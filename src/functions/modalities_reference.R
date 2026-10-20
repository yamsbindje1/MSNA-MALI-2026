# ============================================================================
# MODALITIES REFERENCE - expected answer options per variable, used to add
# never-selected options as 0 (valid denominator) and to document N/A cases.
# ============================================================================

#' Questions whose choice list is restricted by a choice_filter.
#' Read from the analysis Kobo tool, or from the "survey" sheet of the clean
#' dataset when the analysis tool has no choice_filter column.
read_filtered_questions <- function(survey, dataset_file = NULL) {
  if ("choice_filter" %in% names(survey)) {
    return(unique(survey$name[!is.na(survey$choice_filter)]))
  }
  if (!is.null(dataset_file) && file.exists(dataset_file) &&
      "survey" %in% readxl::excel_sheets(dataset_file)) {
    deployed <- readxl::read_excel(dataset_file, sheet = "survey")
    if ("choice_filter" %in% names(deployed)) {
      return(unique(deployed$name[!is.na(deployed$choice_filter)]))
    }
  }
  warning("No choice_filter found (analysis tool or dataset 'survey' sheet): filtered lists cannot be detected.")
  character(0)
}

#' One row per variable x expected option.
#' composite_levels: tibble(variable, modality, rule) for derived variables.
#' excluded: tibble(variable, modality, rule) for options the pipeline recodes to NA.
build_modality_reference <- function(survey, choices, filtered = character(0),
                                     composite_levels = NULL, excluded = NULL) {
  questions <- survey %>%
    dplyr::filter(stringr::str_detect(type, "^select_(one|multiple)\\b")) %>%
    dplyr::transmute(
      variable = name,
      question_type = ifelse(stringr::str_detect(type, "^select_one"), "select_one", "select_multiple"),
      list_name = stringr::str_trim(stringr::str_remove(type, "^select_(one|multiple)\\s*"))
    ) %>%
    dplyr::distinct(variable, .keep_all = TRUE)

  kobo <- questions %>%
    dplyr::inner_join(dplyr::distinct(choices, list_name, name), by = "list_name", relationship = "many-to-many") %>%
    dplyr::transmute(variable, question_type, modality = as.character(name), source = "kobo", rule = paste0("choices: ", list_name))

  composite <- NULL
  if (!is.null(composite_levels)) {
    if (!"override" %in% names(composite_levels)) composite_levels$override <- FALSE
    recoded <- unique(composite_levels$variable[composite_levels$override %in% TRUE])
    kobo <- kobo %>% dplyr::filter(!variable %in% recoded)
    composite <- composite_levels %>%
      dplyr::filter(!variable %in% kobo$variable) %>%
      dplyr::transmute(variable, question_type = "select_one", modality = as.character(modality), source = "composite", rule)
  }

  ref <- dplyr::bind_rows(kobo, composite) %>%
    dplyr::distinct(variable, modality, .keep_all = TRUE) %>%
    dplyr::mutate(
      completable = !variable %in% filtered,
      reason = ifelse(completable, NA_character_, "choice_filter: options depend on the respondent or the area")
    )

  if (!is.null(excluded) && nrow(excluded) > 0) {
    ref <- ref %>%
      dplyr::left_join(dplyr::rename(excluded, excluded_rule = rule), by = c("variable", "modality")) %>%
      dplyr::mutate(
        completable = completable & is.na(excluded_rule),
        reason = dplyr::coalesce(reason, ifelse(is.na(excluded_rule), NA_character_, paste0("recoded to NA by the pipeline: ", excluded_rule)))
      ) %>%
      dplyr::select(-excluded_rule)
  }
  ref
}

#' Checks the reference against one dataset and returns the levels to complete:
#' a named list (column name -> expected values) plus the check table.
#' A variable is only completed when every observed value is in its reference.
modality_levels_for_dataset <- function(ref, dataset) {
  obs_values <- function(col) {
    x <- dataset[[col]]
    x <- x[!x %in% values_to_NA]
    unique(as.character(x))
  }

  one <- ref %>% dplyr::filter(question_type == "select_one", variable %in% names(dataset))
  check_one <- one %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      expected = list(modality[completable]),
      filtered = !any(completable) & any(grepl("^choice_filter", reason)),
      unexpected = list(setdiff(obs_values(dplyr::first(variable)), modality)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      status = dplyr::case_when(
        filtered ~ "not completed: choice_filter",
        lengths(unexpected) > 0 ~ paste0("not completed: observed values outside reference (", vapply(unexpected, paste, "", collapse = ", "), ")"),
        lengths(expected) == 0 ~ "not completed: no completable option",
        TRUE ~ "completed"
      )
    )

  multiple <- ref %>%
    dplyr::filter(question_type == "select_multiple") %>%
    dplyr::mutate(column = paste0(variable, separator, modality))
  check_multiple <- multiple %>%
    dplyr::mutate(
      status = dplyr::case_when(
        !column %in% names(dataset) & !variable %in% names(dataset) ~ "not in this dataset",
        !column %in% names(dataset) ~ "not completed: option column missing from the data",
        !completable ~ paste0("not completed: ", reason),
        TRUE ~ "completed"
      )
    )

  levels_one <- check_one %>% dplyr::filter(status == "completed")
  levels <- c(
    stats::setNames(levels_one$expected, levels_one$variable),
    stats::setNames(rep(list(c("0", "1")), sum(check_multiple$status == "completed")),
                    check_multiple$column[check_multiple$status == "completed"])
  )

  checks <- dplyr::bind_rows(
    check_one %>% dplyr::transmute(variable, question_type = "select_one", modality = NA_character_, status),
    check_multiple %>% dplyr::filter(status != "not in this dataset") %>%
      dplyr::transmute(variable, question_type = "select_multiple", modality, status)
  )
  list(levels = levels, checks = checks)
}

#' Control table: one row per expected option not observed in a group.
#' value = 0 when the group has a valid denominator, "N/A" otherwise.
build_modality_control <- function(results, ref, checks, country) {
  pct <- results %>%
    dplyr::filter(analysis_type %in% c("select_one", "select_multiple")) %>%
    dplyr::mutate(
      variable = ifelse(analysis_type == "select_multiple", sub("\\..*$", "", analysis_var), analysis_var),
      modality = ifelse(analysis_type == "select_multiple", sub("^[^.]*\\.", "", analysis_var), as.character(analysis_var_value))
    )

  groups <- pct %>% dplyr::distinct(source_level, group_var, group_var_value)

  added <- pct %>%
    dplyr::filter(as.numeric(n) == 0) %>%
    dplyr::transmute(
      country, source_level, variable, question_type = analysis_type, group_var, group_var_value, modality,
      denominator_n = as.numeric(N), denominator_weighted = as.numeric(n_w_total),
      value = "0",
      justification = "option offered but never selected in this group; valid denominator"
    )

  answered <- pct %>% dplyr::distinct(source_level, variable, group_var, group_var_value)
  analysed <- answered %>% dplyr::distinct(source_level, variable, group_var)
  no_answer <- analysed %>%
    dplyr::inner_join(groups, by = c("source_level", "group_var"), relationship = "many-to-many") %>%
    dplyr::anti_join(answered, by = c("source_level", "variable", "group_var", "group_var_value")) %>%
    dplyr::semi_join(dplyr::distinct(ref, variable), by = "variable") %>%
    dplyr::left_join(dplyr::distinct(ref, variable, question_type), by = "variable") %>%
    dplyr::transmute(
      country, source_level, variable, question_type, group_var, group_var_value, modality = "(all options)",
      denominator_n = 0, denominator_weighted = 0, value = "N/A",
      justification = "no respondent with a valid answer in this group (not applicable or missing)"
    )

  not_completed <- checks %>%
    dplyr::filter(status != "completed") %>%
    dplyr::transmute(
      country, source_level = NA_character_, variable, question_type, group_var = "(all)", group_var_value = "(all)",
      modality = dplyr::coalesce(modality, "(options not observed)"),
      denominator_n = NA_real_, denominator_weighted = NA_real_, value = "N/A",
      justification = status
    ) %>%
    dplyr::distinct()

  never_analysed <- ref %>%
    dplyr::distinct(variable, question_type) %>%
    dplyr::anti_join(dplyr::distinct(pct, variable), by = "variable") %>%
    dplyr::anti_join(dplyr::distinct(checks, variable), by = "variable") %>%
    dplyr::transmute(
      country, source_level = NA_character_, variable, question_type, group_var = "(all)", group_var_value = "(all)",
      modality = "(all options)", denominator_n = NA_real_, denominator_weighted = NA_real_, value = "N/A",
      justification = "variable not found in the analysed datasets"
    )

  dplyr::bind_rows(added, no_answer, not_completed, never_analysed) %>%
    dplyr::arrange(variable, group_var, group_var_value, modality)
}
