# ============================================================================
# UTILS - small generic helpers used across the sector indicator scripts
# ============================================================================

library(dplyr)
library(purrr)
library(rlang)
library(glue)

#' Check that columns are numeric, try to convert them if not, stop if that fails
are_cols_numeric <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    abort(glue("The following columns are missing from the dataframe: {glue_collapse(missing_cols, sep = ', ')}"))
  }

  non_numeric_cols <- cols[!map_lgl(df[cols], is.numeric)]

  if (length(non_numeric_cols) > 0) {
    df[non_numeric_cols] <- lapply(df[non_numeric_cols], function(col) {
      converted_col <- as.numeric(col)
      if (any(is.na(converted_col) & !is.na(col))) {
        abort(c(
          "Some columns could not be converted to numeric.",
          "i" = glue("The following columns could not be converted without introducing NAs: {glue_collapse(non_numeric_cols, sep = ', ')}")
        ))
      }
      return(converted_col)
    })
  }

  if (!all(map_lgl(df[cols], is.numeric))) {
    abort(c(
      "Some columns are still not numeric after attempted conversion.",
      "i" = glue("The following columns are not numeric: {glue_collapse(non_numeric_cols, sep = ', ')}")
    ))
  }

  return(df)
}

#' Check that numeric columns fall within [lower, upper], stop if not
are_values_in_range <- function(df, cols, lower = 0, upper = 7) {

  are_cols_numeric(df, cols)

  ranges <- purrr::map_lgl(
    dplyr::select(df, dplyr::all_of(cols)),
    \(x) sum(x < lower | x > upper, na.rm = TRUE) >= 1
  )

  cols <- cols[ranges]

  if (all(ranges)) {
    rlang::abort(c(
      glue::glue("All columns must be between {lower} and {upper}."),
      "i" = glue::glue(
        "The following columns have values outside the range Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      )
    ))
  }

  return(TRUE)
}

subvec_in <- function(vector, set) {
  vector[vector %in% set]
}

subvec_not_in <- function(vector, set) {
  vector[!(vector %in% set)]
}

convert_columns_to_numeric <- function(df, cols) {
  if (!all(cols %in% colnames(df))) {
    missing_cols <- cols[!cols %in% colnames(df)]
    stop(paste("The following columns are missing in the dataframe:", paste(missing_cols, collapse = ", ")))
  }

  df[cols] <- lapply(df[cols], function(col) {
    as.numeric(as.character(col))
  })

  return(df)
}

#' Left join with control over what happens to duplicated (non-key) columns.
#' duplicated_columns: 1 = just join, 2 = drop df_b's overlapping cols first,
#' 3/4 = join with a ".dup_col" suffix then drop the duplicate if the two
#' versions of the column actually agree (3 treats NA-vs-value as a mismatch,
#' 4 treats NA-vs-NA as a match). Used by left_joints_dup_modifier() below.
#' Relies on if_not_in_stop() from the impactR.utils package (loaded in
#' 01_packages.R).
left_join_dup_modifier <- function(df_a, df_b, ..., duplicated_columns = 4) {
  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)
  if_not_in_stop(df_a, quoted_cols, "df_a", arg = "...")
  if_not_in_stop(df_b, quoted_cols, "df_b", arg = "...")

  if (duplicated_columns == 1) {
    joined <- dplyr::left_join(df_a, df_b, by = quoted_cols)
  } else if (duplicated_columns == 2) {
    df_b <- df_diff(df_b, df_a, ...)
    joined <- dplyr::left_join(df_a, df_b, by = quoted_cols)
  } else if (duplicated_columns %in% c(3, 4)) {
    joined <- dplyr::left_join(df_a, df_b, by = quoted_cols, suffix = c("", ".dup_col"))
    nn <- intersect(names(df_a), names(df_b))
    nn <- nn[!(nn %in% quoted_cols)]
    nn.dup_col <- paste0(nn, ".dup_col")

    if (duplicated_columns == 3) {
      nn_id_all <- purrr::map2_lgl(nn, nn.dup_col, function(x, y) {
        all(joined[[x]] == joined[[y]] | is.na(joined[y]))
      })
    } else if (duplicated_columns == 4) {
      nn_id_all <- purrr::map2_lgl(nn, nn.dup_col, function(x, y) {
        all(joined[[x]] == joined[[y]] | (is.na(joined[[x]]) & is.na(joined[[y]])))
      })
    }

    nn_id_all <- purrr::set_names(nn_id_all, nn)
    nn_id_all <- names(nn_id_all)[which(nn_id_all %in% TRUE)]
    nn_id_all.dup_col <- paste0(nn_id_all, ".dup_col")
    nn_id_all.dup_col <- intersect(nn_id_all.dup_col, names(joined))

    if (length(nn_id_all.dup_col) > 0) {
      joined <- dplyr::select(joined, -dplyr::all_of(nn_id_all.dup_col))
    }
  }
  return(joined)
}

#' Chain left_join_dup_modifier() across a whole list of data frames (used in
#' the MSNI step to merge roster + edu_ind + health_ind + prot_ind + nut_ind +
#' wgss_indiv into a single "loop" data frame).
left_joints_dup_modifier <- function(data_list, ..., duplicated_columns = 4) {

  quoted_cols <- rlang::enquos(...)
  join_cols <- purrr::map_chr(quoted_cols, rlang::as_name)

  if (!is.list(data_list)) {
    stop("`data_list` must be a list of data frames.")
  }
  if (length(data_list) < 2) {
    stop("`data_list` must contain at least two data frames.")
  }

  purrr::walk2(
    data_list,
    names(data_list),
    ~ if_not_in_stop(.x, join_cols, .y, arg = "...")
  )

  joined <- purrr::reduce(
    data_list,
    ~ left_join_dup_modifier(.x, .y, !!!rlang::syms(join_cols), duplicated_columns = duplicated_columns)
  )

  joined
}




#' Add composite score for Ability to Access Rights and Services
#'
#' Computes weighted sums for two questions about household difficulties accessing services
#' and justice/legal resources, then derives an overall severity category (1–4).
#'
#' @param df A data frame containing binary indicator columns for both questions.
#' @param sep Separator between question code and answer option in column names. Default: "/".
#'
#' @param prot_needs_1_services Base name of services question. Default: "prot_needs_1_services".
#' @param yes_healthcare Answer option name for "Yes, accessing healthcare". Default: "yes_healthcare".
#' @param yes_schools Answer option name for "Yes, accessing schools". Default: "yes_schools".
#' @param yes_therapeutic_services Answer option name for "Yes, therapeutic centers". Default: "yes_therapeutic_services".
#' @param yes_edu_facilities Answer option name for "Yes, education facilities". Default: "yes_edu_facilities".
#' @param yes_social_services Answer option name for "Yes, social services including referral services, and safe spaces for children". Default: "yes_social_services".
#' @param yes_gov_services Answer option name for "Yes, accessing governmental services". Default: "yes_gov_services".
#' @param yes_other_services Answer option name for "Yes, accessing other services". Default: "yes_other_services".
#'
#' @param prot_needs_1_justice Base name of justice question. Default: "prot_needs_1_justice".
#' @param yes_identity_documents Answer option name for "Yes, accessing identity and civil documents services". Default: "yes_identity_documents".
#' @param yes_counselling_legal Answer option name for "Yes, individual counselling or legal assistance". Default: "yes_counselling_legal".
#' @param yes_property_docs Answer option name for "Yes, accessing house, land and property documentation". Default: "yes_property_docs".
#' @param yes_birth_certificates Answer option name for "Yes, accessing birth certificates". Default: "yes_birth_certificates".
#' @param yes_gov_services_justice Answer option name for "Yes, accessing governmental services". Default: "yes_gov_services".
#' @param yes_other_services_justice Answer option name for "Yes, accessing other services". Default: "yes_other_services".
#'
#' @param none Answer option name for "None" (services' no-barrier option). Default: "none".
#' @param no Answer option name for "No" (justice's no-barrier option). Default: "no".
#' @param dnk Answer option name for "Don't know"  Default: "dnk".
#' @param pnta Answer option name for "Prefer not to answer" Default: "pnta".
#'
#' @param .keep_weighted Logical; if TRUE, retain intermediate weighted columns with suffix `_w`. Default: FALSE.
#' @return Input data frame with three new composite-score columns:
#'   * `comp_prot_score_prot_needs_1_services`: weighted sum of services options.
#'   * `comp_prot_score_prot_needs_1_justice`: weighted sum of justice options.
#'   * `comp_prot_score_rights`: overall severity (1–4) based on combined score.
#'   Plus optional `_w` columns if `.keep_weighted = TRUE`.
#' @export
add_prot_score_rights2 <- function(
    df,
    sep = "/",
    prot_needs_1_services = "prot_needs_1_services",
    yes_healthcare = "yes_healthcare",
    yes_schools = "yes_schools",
    yes_therapeutic_services = "yes_therapeutic_services",
    yes_edu_facilities = "yes_edu_facilities",
    yes_social_services = "yes_social_services",
    yes_gov_services = "yes_gov_services",
    yes_other_services = "yes_other_services",
    prot_needs_1_justice = "prot_needs_1_justice",
    yes_identity_documents = "yes_identity_documents",
    yes_counselling_legal = "yes_counselling_legal",
    yes_property_docs = "yes_property_docs",
    yes_birth_certificates = "yes_birth_certificates",
    yes_gov_services_justice = "yes_gov_services",
    yes_other_services_justice = "yes_other_services",
    none = "none",
    no = "no",
    dnk = "dnk",
    pnta = "pnta",
    
    .keep_weighted = FALSE
) {
  params <- as.list(environment())
  
  weights_srv <- c(
    yes_healthcare = 2,
    yes_schools = 2,
    yes_therapeutic_services = 1,
    yes_edu_facilities = 2,
    yes_social_services = 1,
    yes_gov_services = 1,
    yes_other_services = NA,
    none = 0,
    dnk = NA,
    pnta = NA
  )
  srv_opts <- params[names(weights_srv)]
  
  srv_raw <- stringr::str_glue(
    "{prot_needs_1_services}{sep}{srv_opts}"
  )
  names(weights_srv) <- srv_raw
  
  weights_jus <- c(
    yes_identity_documents = 2,
    yes_counselling_legal = 1,
    yes_property_docs = 1,
    yes_birth_certificates = 1,
    yes_gov_services_justice = 1,
    yes_other_services_justice = NA,
    no = 0,
    dnk = NA,
    pnta = NA
  )
  
  jus_opts <- params[names(weights_jus)]
  jus_raw <- stringr::str_glue(
    "{prot_needs_1_justice}{sep}{jus_opts}"
  )
  names(weights_jus) <- jus_raw
  
  weights_mapping <- c(weights_srv, weights_jus)
  all_raw <- names(weights_mapping)
  w_cols <- stringr::str_glue("{all_raw}_w")
  
  if_not_in_stop(df, all_raw, "df")
  are_values_in_set(df, all_raw, c(0, 1))
  
  weights_df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(all_raw),
        ~ .x * weights_mapping[[dplyr::cur_column()]],
        .names = "{.col}_w"
      )
    )
  
  srv_w <- stringr::str_glue("{srv_raw}_w")
  jus_w <- stringr::str_glue("{jus_raw}_w")
  weights_df <- sum_vars(
    weights_df,
    vars = srv_w,
    new_colname = "comp_prot_score_prot_needs_1_services",
    na_rm = TRUE
  ) |>
    sum_vars(
      vars = jus_w,
      new_colname = "comp_prot_score_prot_needs_1_justice",
      na_rm = TRUE
    ) |>
    dplyr::mutate(
      comp_prot_score_prot_needs_1_services = dplyr::case_when(
        comp_prot_score_prot_needs_1_services == 0 &
          (.data[[glue::glue("{prot_needs_1_services}{sep}{dnk}")]] == 1 |
             .data[[glue::glue("{prot_needs_1_services}{sep}{pnta}")]] == 1) ~
          NA_real_,
        TRUE ~ comp_prot_score_prot_needs_1_services
      ),
      comp_prot_score_prot_needs_1_justice = dplyr::case_when(
        comp_prot_score_prot_needs_1_justice == 0 &
          (.data[[glue::glue("{prot_needs_1_justice}{sep}{dnk}")]] == 1 |
             .data[[glue::glue("{prot_needs_1_justice}{sep}{pnta}")]] == 1) ~
          NA_real_,
        TRUE ~ comp_prot_score_prot_needs_1_justice
      )
    )
  
  weights_df <- weights_df |>
    dplyr::mutate(
      .both_na = is.na(.data[["comp_prot_score_prot_needs_1_services"]]) &
        is.na(.data[["comp_prot_score_prot_needs_1_justice"]]),
      .sum = rowSums(
        cbind(
          .data[["comp_prot_score_prot_needs_1_services"]],
          .data[["comp_prot_score_prot_needs_1_justice"]]
        ),
        na.rm = TRUE
      ),
      .sum = dplyr::if_else(.data[[".both_na"]], NA_real_, .data[[".sum"]]),
      comp_prot_score_rights = dplyr::case_when(
        .both_na ~ NA_real_,
        .sum >= 4 ~ 4,
        .sum >= 2 ~ 3,
        .sum >= 1 ~ 2,
        .sum == 0 ~ 1,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::select(-dplyr::all_of(c(".sum", ".both_na")))
  
  na_services <- is.na(weights_df$comp_prot_score_prot_needs_1_services)
  na_justice <- is.na(weights_df$comp_prot_score_prot_needs_1_justice)
  na_both <- na_services & na_justice
  
  n_srv <- sum(na_services, na.rm = TRUE)
  n_jus <- sum(na_justice, na.rm = TRUE)
  n_both <- sum(na_both, na.rm = TRUE)
  
  if (n_srv > 0L || n_jus > 0L) {
    cli::cli_warn(c(
      "{.strong Missing input scores detected}",
      "i" = "{.col comp_prot_score_prot_needs_1_services}: {n_srv} NA{cli::qty(n_srv)}.",
      "i" = "{.col comp_prot_score_prot_needs_1_justice}: {n_jus} NA{cli::qty(n_jus)}.",
      if (n_both > 0L) {
        c(
          "x" = "{n_both} row{?s} ha{?s/ve} both inputs NA; {.col comp_prot_score_rights} will be NA for these row{?s}."
        )
      }
    ))
  }
  
  comp_cols <- c(
    "comp_prot_score_prot_needs_1_services",
    "comp_prot_score_prot_needs_1_justice",
    "comp_prot_score_rights"
  )
  new_cols <- if (.keep_weighted) c(w_cols, comp_cols) else comp_cols
  
  df |>
    dplyr::bind_cols(dplyr::select(weights_df, dplyr::all_of(new_cols))) |>
    dplyr::relocate(
      dplyr::all_of(new_cols),
      .after = tail(all_raw, 1)
    )
}




#' @title Stop statement values are not numeric
#'
#' @param df A data frame
#' @param cols A vector of column names (quoted)
#'
#' @return A stop statement
are_cols_numeric <- function(df, cols) {
  #------ Check for missing columns
  if_not_in_stop(df, cols, "df")
  
  classes <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    is.numeric
  )
  
  cols <- cols[!classes]
  
  if (!all(classes)) {
    rlang::abort(c(
      "All columns must be numeric.",
      "i" = glue::glue(
        "The following columns are not numeric. Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      )
    ))
  }
  
  TRUE
}

#' @title Stop statement values are not in range
#'
#' @param df A data frame
#' @param cols A vector of column names (quoted)
#' @param lower Lower bound
#' @param upper Upper bound
#'
#' @return A stop statement
are_values_in_range <- function(df, cols, lower = 0, upper = 7) {
  #------ Only use on numeric columns
  are_cols_numeric(df, cols)
  
  ranges <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    \(x) {
      sum(x < lower | x > upper, na.rm = TRUE) >= 1
    }
  )
  
  cols <- cols[ranges]
  
  if (all(ranges)) {
    rlang::abort(c(
      glue::glue("All columns must be between {lower} and {upper}."),
      "i" = glue::glue(
        "The following columns have values outside the range Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      )
    ))
  }
  
  TRUE
}


#' @title Stop statement values are not in set
#'
#' @param df A data frame
#' @param cols A vector of column names (quoted)
#' @param set A vector of values
#' @param main_message A main message
#'
#' @return A stop statement
are_values_in_set <- function(
    df,
    cols,
    set,
    main_message = "All columns must be in the following set: "
) {
  #------ Check for missing columns
  if_not_in_stop(df, cols, "df")
  
  #------ Values not in set
  values_lgl <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    \(x) {
      !all(stats::na.omit(unique(x)) %in% set)
    }
  )
  
  if (any(values_lgl)) {
    cols <- cols[values_lgl]
    values_chr <- names(values_lgl)
    
    # Get values not in set
    df_cols <- dplyr::select(df, dplyr::all_of(cols))
    values_chr <- purrr::map(df_cols, \(x) {
      x <- unique(x)
      x[!is.na(x) & !(x %in% set)]
    })
    
    values_chr <- purrr::imap_chr(values_chr, \(x, idx) {
      glue::glue("{idx}: {glue::glue_collapse(x, sep = ', ', last = ' and ')}")
    })
    
    rlang::abort(c(
      glue::glue(main_message, glue::glue_collapse(set, sep = ", ")),
      "i" = glue::glue(
        "The following columns have values out of the set Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      ),
      "x" = glue::glue(
        "The values out of the set are:\n",
        glue::glue_collapse(values_chr, sep = "\n")
      )
    ))
  }
  
  TRUE
}


#' @title Subvec in
#'
#' @param vector A vector to subset
#' @param set A set-vector
#'
#' @return A subset of a list or a vector
subvec_in <- function(vector, set) {
  vector[vector %in% set]
}

#' @title Subvec not in
#'
#' @param vector A vector to subset
#' @param set A set-vector
#'
#' @return A subset of vector not in set
subvec_not_in <- function(vector, set) {
  vector[!(vector %in% set)]
}


#' @title Stop statement "If not in colnames" with colnames
#'
#' @param df A data frame
#' @param cols A vector of column names (quoted)
#' @param df_name Provide the tibble name as a character string
#' @param arg Default to NULL.
#'
#' @return A stop statement
if_not_in_stop <- function(df, cols, df_name, arg = NULL) {
  missing_cols <- subvec_not_in(cols, colnames(df))
  
  if (is.null(arg)) {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns are missing in `{df_name}`: ")
    } else {
      msg <- glue::glue("The following column is missing in `{df_name}`: ")
    }
  } else {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue(
        "The following columns from `{arg}` are missing in `{df_name}`: "
      )
    } else {
      msg <- glue::glue(
        "The following column from `{arg}` is missing in `{df_name}`: "
      )
    }
  }
  if (length(missing_cols) >= 1) {
    rlang::abort(
      c(
        "Missing columns",
        "*" = glue::glue(
          msg,
          glue::glue_collapse(
            missing_cols,
            sep = ", ",
            last = ", and "
          )
        )
      )
    )
  }
}


#' @title Drop columns from main that are also computed in loop
#'
#' @description Before joining loop-derived columns into main, drop any
#' column names main already shares with loop (e.g. from a prior run),
#' except the two id columns used for the join.
#'
#' @param main A data frame (household-level)
#' @param loop A data frame (individual-level, already summarized)
#' @param id_col_main Column name for the unique identifier in main
#' @param id_col_loop Column name for the unique identifier in loop
#'
#' @return main, with shared non-id columns dropped
drop_shared_loop_cols <- function(main, loop, id_col_main, id_col_loop) {
  id_cols <- c(id_col_main, id_col_loop)
  shared_cols <- intersect(colnames(loop), colnames(main))
  shared_cols <- setdiff(shared_cols, id_cols)
  dplyr::select(main, -dplyr::all_of(shared_cols))
}




#' add_prot_score_movement
#'
#' @param df Data frame containing the survey data.
#' @param sep Separator for the binary columns, default is "/".
#' @param prot_needs_3_movement Column name
#' @param no_changes_feel_unsafe answer option
#' @param no_safety_concerns answer option
#' @param women_girls_avoid_places answer option
#' @param men_avoid_places answer option
#' @param boys_avoid_places answer option
#' @param women_girls_avoid_night answer option
#' @param men_avoid_night answer option
#' @param boys_avoid_night answer option
#' @param girls_boys_avoid_school answer option
#' @param different_routes answer option
#' @param avoid_markets answer option
#' @param avoid_public_offices answer option
#' @param avoid_fields answer option
#' @param women_girls_boys_avoid_firewood answer option
#' @param women_girls_boys_avoid_places answer option
#' @param other_safety_measures answer option
#' @param dnk answer option
#' @param pnta answer option
#' @param men_avoid_places_weight Numeric weight for `men_avoid_places` (0–2). Default is 1.
#' @param men_avoid_night_weight Numeric weight for `men_avoid_night` (0–2). Default is 1.
#' @param .keep_weighted Logical, whether to keep the weighted columns in the output data frame. Default is FALSE.
#'
#' @return data frame with additional columns:
#' * comp_prot_score_prot_needs_3
#' * comp_prot_score_movement
#' @export
add_prot_score_movement_V <- function(
    df,
    sep = "/",
    prot_needs_3_movement = "prot_needs_3_movement",
    no_changes_feel_unsafe = "no_changes_feel_unsafe",
    no_safety_concerns = "no_safety_concerns",
    women_girls_avoid_places = "women_girls_avoid_places",
    men_avoid_places = "men_avoid_places",
    boys_avoid_places = "boys_avoid_places",
    women_girls_avoid_night = "women_girls_avoid_night",
    men_avoid_night = "men_avoid_night",
    boys_avoid_night = "boys_avoid_night",
    girls_boys_avoid_school = "girls_boys_avoid_school",
    different_routes = "different_routes",
    avoid_markets = "avoid_markets",
    avoid_public_offices = "avoid_public_offices",
    avoid_fields = "avoid_fields",
    women_girls_boys_avoid_firewood = "women_girls_boys_avoid_firewood",
    women_girls_boys_avoid_places = "women_girls_boys_avoid_places",
    other_safety_measures = "other_safety_measures",
    dnk = "dnk",
    pnta = "pnta",
    men_avoid_places_weight = 1,
    men_avoid_night_weight = 1,
    .keep_weighted = FALSE
) {
  # capture all parameters
  params <- as.list(environment())
  
  if (!checkmate::test_number(men_avoid_places_weight, lower = 0, upper = 2)) {
    cli::cli_abort(
      "{.arg men_avoid_places_weight} must be a single number between 0 and 2,
       not {.val {men_avoid_places_weight}}."
    )
  }
  if (!checkmate::test_number(men_avoid_night_weight, lower = 0, upper = 2)) {
    cli::cli_abort(
      "{.arg men_avoid_night_weight} must be a single number between 0 and 2,
       not {.val {men_avoid_night_weight}}."
    )
  }
  
  # mapping of response → weight
  weights_mapping <- c(
    no_changes_feel_unsafe = 1,
    no_safety_concerns = 0,
    women_girls_avoid_places = 2,
    men_avoid_places = men_avoid_places_weight,
    boys_avoid_places = 2,
    women_girls_avoid_night = 2,
    men_avoid_night = men_avoid_night_weight,
    boys_avoid_night = 2,
    girls_boys_avoid_school = 2,
    different_routes = 1,
    avoid_markets = 2,
    avoid_public_offices = 2,
    avoid_fields = 2,
    women_girls_boys_avoid_firewood = 2,
    women_girls_boys_avoid_places = 2,
    other_safety_measures = NA,
    dnk = NA,
    pnta = NA
  )
  
  # build the raw column names
  user_answer_options <- params[names(weights_mapping)]
  sm_col_names <- stringr::str_glue(
    "{prot_needs_3_movement}{sep}{user_answer_options}"
  )
  names(weights_mapping) <- sm_col_names
  
  # sanity checks
  if_not_in_stop(df, sm_col_names, "df")
  are_values_in_set(df, sm_col_names, c(0, 1))
  
  # compute weighted indicators as new columns ending in "_w"
  weighted_cols <- paste0(sm_col_names, "_w")
  weights_df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(sm_col_names),
        ~ .x * weights_mapping[[dplyr::cur_column()]],
        .names = "{.col}_w"
      )
    )
  
  # These columns need to result in an NA score for comp_prot_score_movement
  
  dnk_col <- stringr::str_glue("{prot_needs_3_movement}{sep}{dnk}")
  pnta_col <- stringr::str_glue("{prot_needs_3_movement}{sep}{pnta}")
  other_safety_measures_col <- stringr::str_glue(
    "{prot_needs_3_movement}{sep}{other_safety_measures}"
  )
  
  # aggregate into composite scores
  weights_df <- sum_vars(
    weights_df,
    vars = weighted_cols,
    new_colname = "comp_prot_score_prot_needs_3",
    na_rm = TRUE
  ) |>
    dplyr::mutate(
      comp_prot_score_movement = dplyr::case_when(
        .data[["comp_prot_score_prot_needs_3"]] >= 3 ~ 4,
        .data[["comp_prot_score_prot_needs_3"]] == 2 ~ 3,
        .data[["comp_prot_score_prot_needs_3"]] == 1 ~ 2,
        .data[["comp_prot_score_prot_needs_3"]] == 0 ~ 1,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::mutate(
      comp_prot_score_movement = dplyr::case_when(
        .data[[dnk_col]] == 1 ~ NA_real_,
        .data[[pnta_col]] == 1 ~ NA_real_,
        .data[[other_safety_measures_col]] == 1 &
          .data[["comp_prot_score_prot_needs_3"]] == 0 ~ NA_real_,
        .default = .data[["comp_prot_score_movement"]]
      )
    )
  
  # decide which new columns to bind back
  composite_cols <- c(
    "comp_prot_score_prot_needs_3",
    "comp_prot_score_movement"
  )
  if (.keep_weighted) {
    new_cols <- c(weighted_cols, composite_cols)
  } else {
    new_cols <- composite_cols
  }
  
  # bind and relocate
  df <- dplyr::bind_cols(
    df,
    dplyr::select(weights_df, dplyr::all_of(new_cols))
  ) |>
    dplyr::relocate(
      dplyr::all_of(new_cols),
      .after = tail(sm_col_names, 1)
    )
  
  df
}




#' @title Add Household Water Insecurity Experiences (HWISE) Score
#'
#' @description Computes the total `hwise4_score` (0–12) from the four HWISE
#'   survey items (water quantity), and maps it to a WASH component score
#'   for water quantity (`comp_wash_score_water_quantity`, 1–5). The four
#'   items are internally recoded to numeric scores but are only exposed in
#'   the output (as `_score`-suffixed columns) when `.keep_recoded = TRUE`.
#'
#' @param .df A data frame containing the four HWISE columns.
#' @param wash_hwise_drink Column name for: *"In the last 4 weeks, how
#'   frequently has there NOT been as much water to drink as you would like for
#'   you or anyone in your household?"*
#' @param wash_hwise_hands Column name for: *"In the last 4 weeks, how
#'   frequently have you or anyone in your household had to go without washing
#'   hands after dirty activities (e.g., defecating or changing diapers,
#'   cleaning animal dung) because of problems with water?"*
#' @param wash_hwise_plans Column name for: *"In the last 4 weeks how often did
#'   you have to change schedules or plans because of problems with water?"*
#' @param wash_hwise_worry Column name for: *"In the last 4 weeks how often did
#'   you or anyone in your household worry that you would not have enough water
#'   for all of your needs?"*
#' @param hwise_never Response code for "Never (0 days)" (scored 0).
#' @param hwise_rarely Response code for "Rarely (1–2 days)" (scored 1).
#' @param hwise_sometimes Response code for "Some days (3–10 days)" (scored 2).
#' @param hwise_often Response code for "Often (11–20 days)" (scored 3).
#' @param hwise_always Response code for "Always (more than 20 days)" (scored 3).
#' @param hwise_dnk Response code for "Don't know" (scored NA).
#' @param hwise_pnta Response code for "Prefer not to answer" (scored NA).
#' @param .keep_recoded Logical, whether to keep the recoded numeric HWISE
#'   columns (ending in `_score`) in the output data frame. Default is FALSE.
#'
#' @return `.df` with new columns:
#'
#' * The four HWISE columns recoded to numeric scores (0–3, NA for dnk/pnta),
#'   suffixed with `_score` (only kept in the output if `.keep_recoded = TRUE`).
#' * `hwise4_score`: row sum of the four recoded columns (0–12).
#' * `comp_wash_score_water_quantity`: WASH severity score (1–5):
#'   1 = score 0–3, 2 = 4–6, 3 = 7–8, 4 = 9–10, 5 = 11+.
#'
#' @seealso [add_comp_wash()]
#'
#' @export
#'
#' @examples
#' df <- dplyr::tibble(
#'   wash_hwise_drink = c("never",     "sometimes", "often"),
#'   wash_hwise_hands = c("rarely",    "often",     "always"),
#'   wash_hwise_plans = c("sometimes", "often",     "always"),
#'   wash_hwise_worry = c("never",     "sometimes", "always")
#' )
#'
#' result <- add_hwise(df)
#' dplyr::select(result, hwise4_score, comp_wash_score_water_quantity)
#'
#' # keep the per-item recoded scores (ending in `_score`)
#' result_recoded <- add_hwise(df, .keep_recoded = TRUE)
#' dplyr::select(result_recoded, dplyr::matches("_score$"))
add_hwise_modified <- function(
    .df,
    wash_hwise_drink = "wash_hwise_drink",
    wash_hwise_hands = "wash_hwise_hands",
    wash_hwise_plans = "wash_hwise_plans",
    wash_hwise_worry = "wash_hwise_worry",
    hwise_never = "never",
    hwise_rarely = "rarely",
    hwise_sometimes = "sometimes",
    hwise_often = "often",
    hwise_always = "always",
    hwise_dnk = "dnk",
    hwise_pnta = "pnta",
    .keep_recoded = FALSE
) {
  # Check variable presence in the data frame
  if_not_in_stop(
    .df,
    c(
      wash_hwise_drink,
      wash_hwise_hands,
      wash_hwise_plans,
      wash_hwise_worry
    ),
    ".df"
  )
  
  # Check answer options
  hwise_answer_options <- list(
    hwise_never,
    hwise_rarely,
    hwise_sometimes,
    hwise_often,
    hwise_always,
    hwise_dnk,
    hwise_pnta
  )
  
  all_scalar <- sapply(hwise_answer_options, checkmate::test_scalar)
  if (!all(all_scalar)) {
    cli::cli_abort("All of the {.arg hwise_*} parameters must be scalars")
  }
  all_character <- sapply(hwise_answer_options, checkmate::test_character)
  
  if (!all(all_character)) {
    cli::cli_abort(
      "All of the {.arg hwise_*} parameters must be of type character"
    )
  }
  
  are_values_in_set(
    .df,
    c(wash_hwise_drink, wash_hwise_hands, wash_hwise_plans, wash_hwise_worry),
    c(
      hwise_never,
      hwise_rarely,
      hwise_sometimes,
      hwise_often,
      hwise_always,
      hwise_dnk,
      hwise_pnta
    )
  )
  
  hwise_mapping <- dplyr::tribble(
    ~from           , ~to      ,
    hwise_never     ,        0 ,
    hwise_rarely    ,        1 ,
    hwise_sometimes ,        2 ,
    hwise_often     ,        3 ,
    hwise_always    ,        3 ,
    hwise_dnk       , NA_real_ ,
    hwise_pnta      , NA_real_
  )
  
  hwise_col_names <- c(
    wash_hwise_drink,
    wash_hwise_hands,
    wash_hwise_plans,
    wash_hwise_worry
  )
  recoded_cols <- paste0(hwise_col_names, "_score")
  
  recoded_df <- .df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(hwise_col_names),
        ~ dplyr::recode_values(
          .x,
          from = hwise_mapping$from,
          to = hwise_mapping$to
        ),
        .names = "{.col}_score"
      ),
      hwise4_score = rowSums(
        dplyr::pick(dplyr::all_of(recoded_cols))
      ),
      comp_wash_score_water_quantity = dplyr::case_when(
        hwise4_score >= 11 ~ 5,
        hwise4_score >= 9 ~ 4,
        hwise4_score >= 7 ~ 3,
        hwise4_score >= 4 ~ 2,
        hwise4_score >= 0 ~ 1,
        .default = NA
      )
    )
  
  # decide which new columns to bind back
  composite_cols <- c("hwise4_score", "comp_wash_score_water_quantity")
  new_cols <- if (.keep_recoded) {
    c(recoded_cols, composite_cols)
  } else {
    composite_cols
  }
  
  # bind and relocate
  dplyr::bind_cols(
    .df,
    dplyr::select(recoded_df, dplyr::all_of(new_cols))
  ) |>
    dplyr::relocate(
      dplyr::all_of(new_cols),
      .after = tail(hwise_col_names, 1)
    )
}





add_loop_edu_ind_age_corrected_d_2 <- function(
    loop,
    main,
    id_col_loop = "uuid",
    id_col_main = "uuid",
    survey_start_date = "start",
    school_year_start_month = 9,
    ind_age = "ind_age",
    month = NULL,
    schooling_start_age = 5,
    schooling_end_age = 17
) {
  #------ Initial checks
  
  # Check if the variable is in the data frame
  if_not_in_stop(loop, id_col_loop, "loop")
  if_not_in_stop(main, id_col_main, "main")
  if_not_in_stop(main, survey_start_date, "main")
  if_not_in_stop(loop, ind_age, "loop")
  
  # Check if ind_age is numeric
  are_cols_numeric(loop, ind_age)
  
  checkmate::assert_number(school_year_start_month, lower = 1, upper = 12)
  checkmate::assert_number(schooling_start_age, lower = 3)
  checkmate::assert_number(schooling_end_age, lower = schooling_start_age)
  
  # Check if survey_start_date is in ISO 8601 format
  is_iso8601 <- function(x) {
    tryCatch(
      {
        !is.na(as.Date(x, format = "%Y-%m-%d"))
      },
      error = function(e) FALSE
    )
  }
  
  if (!all(is_iso8601(main[[survey_start_date]]))) {
    stop(
      "The survey_start_date column is not in ISO 8601 format (YYYY-MM-DD). Please correct the format."
    )
  }
  
  #------ Prepare month of data collection var
  
  # Convert the survey start date column to month OR use the provided param 'month'
  if (is.null(month)) {
    # Prepare month date
    main <- dplyr::mutate(
      main,
      month = as.integer(format(as.Date(!!rlang::sym(survey_start_date)), "%m"))
    )
    main <- dplyr::select(main, dplyr::all_of(c(id_col_main, "month")))
    # Check that month values are between 1 and 12, if not abort
    are_values_in_set(main, "month", 1:12)
    # Remove "month" from loop if it exists
    if ("month" %in% colnames(loop)) {
      # warn for removal
      rlang::warn("'month' already exists in 'loop', replacing it.")
      # Remove
      loop <- dplyr::select(loop, -dplyr::all_of("month"))
    }
    # Join main to loop, setNames used to dynamically set the column names for the join operation.
    loop <- dplyr::left_join(
      loop,
      main,
      by = stats::setNames(id_col_main, id_col_loop)
    )
  } else {
    # Check that month is a number between 1 and 12, if no abort
    if (!(month %in% c(1:12))) {
      rlang::abort("month must be between 1 and 12")
    }
    # Add "month" as a column to loop
    loop <- dplyr::mutate(loop, month = month)
  }
  
  # Calculate the adjusted start month number
  school_year_start_month_adj <- ifelse(
    school_year_start_month > 6,
    school_year_start_month - 12,
    school_year_start_month
  )
  
  # Apply the age correction based on the month difference, handling NA in month or age
  loop <- dplyr::mutate(
    loop,
    edu_ind_age_corrected = dplyr::case_when(
      !is.na(!!rlang::sym("month")) &
        !is.na(!!rlang::sym(ind_age)) &
        (!!rlang::sym("month") - school_year_start_month_adj) > 6 ~
        !!rlang::sym(ind_age) - 1,
      .default = !!rlang::sym(ind_age)
    )
  )
  
  #Final classification --- NAing with below schooling_start_age or above schooling_end_age
  loop <- dplyr::mutate(
    loop,
    edu_ind_age_corrected = dplyr::case_when(
      !!rlang::sym("edu_ind_age_corrected") < schooling_start_age |
        !!rlang::sym("edu_ind_age_corrected") > schooling_end_age ~
        NA_real_,
      .default = !!rlang::sym("edu_ind_age_corrected")
    )
  )
  
  # Add a dummy variable, 1 if the individual is a school child, 0 otherwise
  loop <- dplyr::mutate(
    loop,
    edu_ind_age_schooling = dplyr::case_when(
      is.na(!!rlang::sym("edu_ind_age_corrected")) ~ 0,
      !!rlang::sym("edu_ind_age_corrected") < schooling_start_age |
        !!rlang::sym("edu_ind_age_corrected") > schooling_end_age ~
        0,
      .default = 1
    )
  )
  
  loop
}




add_loop_edu_ind_schooling_age_d_to_mainD <- function(
    main,
    loop,
    ind_schooling_age_d = "edu_ind_age_schooling_n",
    id_col_main = "uuid",
    id_col_loop = "uuid"
) {
  #------ Checks
  
  # Check if the variables are in the data frame
  if_not_in_stop(loop, c(ind_schooling_age_d), "loop")
  if_not_in_stop(main, id_col_main, "main")
  if_not_in_stop(loop, id_col_loop, "loop")
  
  # Check if new_colname exists in the dataframe, if yes throw a warning for replacement
  ind_schooling_age_d_n <- paste0(ind_schooling_age_d, "_n")
  
  if (ind_schooling_age_d_n %in% colnames(main)) {
    rlang::warn(paste0(
      ind_schooling_age_d_n,
      " already exists in the data frame. It will be replaced."
    ))
  }
  
  #------ Compute
  
  # Group loop by id_col_loop
  loop <- dplyr::group_by(loop, !!rlang::sym(id_col_loop))
  
  # Sum the schooling age dummy variable
  loop <- dplyr::summarise(
    loop,
    edu_schooling_age_n = sum(!!rlang::sym(ind_schooling_age_d), na.rm = TRUE)
  )
  
  # Join loop to main
  main <- dplyr::left_join(
    main,
    loop,
    by = dplyr::join_by(!!rlang::sym(id_col_main) == !!rlang::sym(id_col_loop))
  )
  
  # The edu group in the XLSForm is relevant only when the household has at
  # least one school-age child (${ind_age_schooling_n} >= 1), so a household
  # with no school-age children has no rows in the education loop and the
  # join above yields NA. Encode it as the true count: 0 school-age
  # children (issue #767).
  main <- dplyr::mutate(
    main,
    edu_schooling_age_n = dplyr::coalesce(.data$edu_schooling_age_n, 0)
  )
  
  main
}
