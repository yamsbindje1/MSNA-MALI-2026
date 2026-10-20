# ============================================================================
# ANALYSIS ENGINE - weighted frequency/mean/ratio with a 90% confidence
# interval. OPTIMIZED VERSION: weights_sym and disaggregationN_sym are
# computed ONCE per (dataset, weights_column, disaggregations) combination,
# not once per question, which keeps a full run short. The statistics do not
# depend on this: only the setup work is shared between questions.
# ============================================================================

#' Prepares weights_sym + disaggregation1_sym..N_sym ONCE for a given
#' dataset/weights_column/disaggregations combination. Called once per
#' apply_analysis() call (i.e. once per disaggregation, per source_level),
#' instead of once per question inside frequency_analysis/mean_analysis/
#' ratio_analysis as before.
prepare_analysis_dataset <- function(dataset, weights_column = "none", disaggregations = character(0)) {
  
  if (weights_column == "none") {
    dataset$weights_sym <- 1
  } else {
    dataset$weights_sym <- ifelse(
      dataset[[weights_column]] %in% values_to_NA,
      NA,
      dataset[[weights_column]]
    )
  }
  
  n_disagg <- length(disaggregations)
  group_vars <- if (n_disagg == 0) character(0) else paste0("disaggregation", seq_len(n_disagg), "_sym")
  
  for (i in seq_len(n_disagg)) {
    new_name <- group_vars[i]
    if (disaggregations[i] == "none") {
      dataset[[new_name]] <- "all"
    } else {
      dataset[[new_name]] <- ifelse(
        dataset[[disaggregations[i]]] %in% values_to_NA,
        NA,
        dataset[[disaggregations[i]]]
      )
    }
  }
  if (n_disagg == 0) {
    dataset$disaggregation1_sym <- "all"
    group_vars <- "disaggregation1_sym"
  }
  
  list(dataset = dataset, group_vars = group_vars)
}

#' Attaches disaggregationN_by columns to a result table (was done inside
#' each of the 3 analysis functions before; factored out since it's the
#' same 3 lines repeated identically in each).
add_disaggregation_by_columns <- function(analysis, disaggregations) {
  n_disagg <- length(disaggregations)
  if (n_disagg == 0) {
    analysis[["disaggregation1_by"]] <- "none"
  } else {
    for (i in seq_len(n_disagg)) {
      analysis[[paste0("disaggregation", i, "_by")]] <- disaggregations[i]
    }
  }
  analysis
}

#' Adds expected options never selected in a group (n = 0), only inside
#' groups that already have at least one valid answer. Groups without valid
#' answers are left out, so they stay N/A downstream.
complete_modalities <- function(counts, group_vars, levels = NULL) {
  if (is.null(levels) || nrow(counts) == 0) return(counts)
  counts$answer_variable_name_sym <- as.character(counts$answer_variable_name_sym)
  groups <- unique(as.data.frame(counts)[group_vars])
  grid <- groups[rep(seq_len(nrow(groups)), each = length(levels)), , drop = FALSE]
  grid$answer_variable_name_sym <- rep(as.character(levels), times = nrow(groups))
  key <- function(d) do.call(paste, c(unname(as.list(as.data.frame(d)[c(group_vars, "answer_variable_name_sym")])), sep = "\r"))
  missing <- grid[!key(grid) %in% key(counts), , drop = FALSE]
  if (nrow(missing) == 0) return(counts)
  missing$n <- 0L
  missing$n_weighted <- 0
  missing$sum_w2 <- 0
  dplyr::bind_rows(counts, missing)
}

#' Weighted proportion of each answer to a categorical (select_one /
#' select_multiple) question, with a 90% Wilson confidence interval.
#' 'dataset' must already carry weights_sym + the disaggregationN_sym columns
#' named in 'group_vars' -- use prepare_analysis_dataset() once beforehand
#' (apply_analysis() does this automatically).
frequency_analysis <- function(dataset, variable, group_vars, disaggregations, weights_column = "none", levels = NULL) {
  
  z_90 <- qnorm(0.95)
  
  analysis <- dataset %>%
    mutate(
      answer_variable_name_sym = ifelse(
        !!sym(variable) %in% values_to_NA,
        NA,
        !!sym(variable)
      )
    ) %>%
    filter(!is.na(answer_variable_name_sym)) %>%
    group_by(across(all_of(c(group_vars, "answer_variable_name_sym")))) %>%
    summarise(
      n = n(),
      n_weighted = sum(as.numeric(weights_sym)),
      sum_w2 = sum(as.numeric(weights_sym)^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete_modalities(group_vars, levels) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      N = sum(n),
      stat = as.character(round(n_weighted / sum(n_weighted), 10)),
      n_w_total = round(sum(n_weighted), 0),
      n_eff = (sum(n_weighted))^2 / sum(sum_w2),
      wilson_denom = 1 + (z_90^2) / n_eff,
      wilson_center = (as.numeric(stat) + (z_90^2) / (2 * n_eff)) / wilson_denom,
      wilson_margin = (z_90 / wilson_denom) * sqrt(
        (as.numeric(stat) * (1 - as.numeric(stat)) / n_eff) + (z_90^2 / (4 * n_eff^2))
      ),
      stat_low = as.character(round(pmax(0, wilson_center - wilson_margin), 10)),
      stat_upp = as.character(round(pmin(1, wilson_center + wilson_margin), 10))
    ) %>%
    select(-sum_w2, -n_eff, -wilson_denom, -wilson_center, -wilson_margin) %>%
    ungroup() %>%
    mutate(
      n_w = round(n_weighted, 0),
      question_variable_name = variable,
      estimator = "percentage",
      weighted = ifelse(weights_column == "none", "no", "yes")
    ) %>%
    mutate(MoE = (as.numeric(stat_upp) - as.numeric(stat_low)) / 2) %>%
    relocate(stat, stat_low, stat_upp, MoE, n)
  
  analysis <- add_disaggregation_by_columns(analysis, disaggregations)
  names(analysis) <- gsub("_sym", "", names(analysis))
  analysis
}

#' Weighted mean of a numeric (integer) question, with a 90% confidence interval.
#' Same 'dataset' contract as frequency_analysis() above.
#' Weighted mean of a numeric (integer) question, with a 90% confidence interval.
#' Same 'dataset' contract as frequency_analysis() above.
# Médiane pondérée (si pas de pondération, weights_sym = 1 donne la médiane classique)
weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  o  <- order(x)
  x  <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  i  <- which(cw >= 0.5)[1]
  # si on tombe exactement sur 50 %, on fait la moyenne des deux valeurs centrales
  if (isTRUE(all.equal(cw[i], 0.5)) && i < length(x)) (x[i] + x[i + 1]) / 2 else x[i]
}

mean_analysis  <- function(dataset, variable, group_vars, disaggregations, weights_column = "none") {
  
  z_90 <- qnorm(0.95)
  
  base <- dataset %>%
    mutate(
      answer_variable_name_sym = ifelse(
        !!sym(variable) %in% values_to_NA,
        NA,
        !!sym(variable)
      ),
      answer_variable_name_sym = as.numeric(answer_variable_name_sym),
      weights_sym = as.numeric(weights_sym)
    ) %>%
    filter(!is.na(answer_variable_name_sym)) %>%
    group_by(across(all_of(group_vars)))
  
  # ---- Moyenne ----
  mean_res <- base %>%
    summarise(
      n = n(),
      n_weighted = sum(weights_sym),
      stat = round(weighted.mean(answer_variable_name_sym, weights_sym, na.rm = TRUE), 5),
      sum_w = sum(weights_sym, na.rm = TRUE),
      sum_w2 = sum(weights_sym^2, na.rm = TRUE),
      weighted_var = sum(weights_sym * (answer_variable_name_sym - stat)^2, na.rm = TRUE) / sum_w,
      .groups = "drop"
    ) %>%
    mutate(
      n_eff = sum_w^2 / sum_w2,
      se = sqrt(weighted_var / n_eff),
      stat_low = round(stat - z_90 * se, 5),
      stat_upp = round(stat + z_90 * se, 5),
      estimator = "mean"
    ) %>%
    select(-sum_w, -sum_w2, -weighted_var, -n_eff, -se)
  
  # ---- Médiane ----
  median_res <- base %>%
    summarise(
      n = n(),
      n_weighted = sum(weights_sym),
      stat = round(weighted_median(answer_variable_name_sym, weights_sym), 5),
      .groups = "drop"
    ) %>%
    mutate(
      stat_low = NA_real_,
      stat_upp = NA_real_,
      estimator = "median"
    )
  
  # ---- Assemblage ----
  analysis <- bind_rows(mean_res, median_res) %>%
    mutate(
      MoE = (stat_upp - stat_low) / 2,
      stat = as.character(stat),
      stat_low = as.character(stat_low),
      stat_upp = as.character(stat_upp),
      n_w = round(n_weighted, 0),
      question_variable_name = variable,
      answer_variable_name_sym = "",
      weighted = ifelse(weights_column == "none", "no", "yes")
    ) %>%
    relocate(stat, stat_low, stat_upp, MoE, n)
  
  analysis <- add_disaggregation_by_columns(analysis, disaggregations)
  analysis <- analysis %>% relocate(question_variable_name, .after = dplyr::last_col())
  names(analysis) <- gsub("_sym", "", names(analysis))
  analysis
}

#' Weighted ratio between two numeric variables (ratio-of-sums estimator).
#' Same 'dataset' contract as frequency_analysis() above.
ratio_analysis <- function(dataset, variable, denominator, group_vars, disaggregations, weights_column = "none") {
  
  z_90 <- qnorm(0.95)
  
  analysis <- dataset %>%
    mutate(
      numerator_sym = ifelse(
        !!sym(variable) %in% values_to_NA, NA, as.numeric(!!sym(variable))
      ),
      denominator_sym = ifelse(
        !!sym(denominator) %in% values_to_NA, NA, as.numeric(!!sym(denominator))
      )
    ) %>%
    filter(!is.na(numerator_sym), !is.na(denominator_sym)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n = n(),
      n_weighted = sum(as.numeric(weights_sym)),
      sum_w = sum(as.numeric(weights_sym), na.rm = TRUE),
      sum_w2 = sum(as.numeric(weights_sym)^2, na.rm = TRUE),
      stat = round(
        weighted.mean(numerator_sym, as.numeric(weights_sym), na.rm = TRUE) /
          weighted.mean(denominator_sym, as.numeric(weights_sym), na.rm = TRUE),
        5
      ),
      weighted_var = sum(
        as.numeric(weights_sym) * (numerator_sym - stat * denominator_sym)^2,
        na.rm = TRUE
      ) / sum(as.numeric(weights_sym), na.rm = TRUE),
      denom_mean = weighted.mean(denominator_sym, as.numeric(weights_sym), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_eff = (sum_w)^2 / sum_w2,
      se = sqrt(weighted_var / n_eff) / denom_mean,
      stat_low = as.character(round(pmax(0, stat - z_90 * se), 5)),
      stat_upp = as.character(round(pmin(1, stat + z_90 * se), 5)),
      stat = as.character(stat),
      n_w = round(n_weighted, 0),
      question_variable_name = variable,
      answer_variable_name_sym = "",
      estimator = "ratio",
      weighted = ifelse(weights_column == "none", "no", "yes")
    ) %>%
    select(-sum_w, -sum_w2, -n_eff, -se, -denom_mean, -weighted_var) %>%
    mutate(MoE = (as.numeric(stat_upp) - as.numeric(stat_low)) / 2) %>%
    relocate(stat, stat_low, stat_upp, MoE, n)
  
  analysis <- add_disaggregation_by_columns(analysis, disaggregations)
  analysis <- analysis %>% relocate(question_variable_name, .after = dplyr::last_col())
  names(analysis) <- gsub("_sym", "", names(analysis))
  analysis
}

#' Make a column's type consistent across a list of data frames.
harmonize_column_types <- function(df_list) {
  all_cols <- unique(unlist(lapply(df_list, names)))
  for (col in all_cols) {
    classes <- unique(unlist(lapply(df_list, function(df) {
      if (col %in% names(df)) class(df[[col]])[1] else NA
    })))
    classes <- classes[!is.na(classes)]
    if (length(classes) > 1) {
      df_list <- lapply(df_list, function(df) {
        if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
        df
      })
    }
  }
  df_list
}

apply_analysis <- function(select_one_questions,
                           select_multiple_questions,
                           integer_questions,
                           categorical_analysis,
                           numerical_analysis,
                           recoded_var = NULL,
                           ratio_questions = NULL,
                           ratio_analysis_type = "ratio_analysis",
                           dataset = dataset,
                           weights_column = weights_column,
                           disaggregations = character(0),
                           use_parallel = FALSE,
                           modality_levels = NULL) {
  
  # categorical_analysis / numerical_analysis peuvent etre passees comme des
  # chaines ("frequency_analysis") ou comme de vraies fonctions -- match.fun()
  # gere les deux cas, comme le faisait deja lapply() implicitement avant.
  categorical_analysis <- match.fun(categorical_analysis)
  numerical_analysis <- match.fun(numerical_analysis)
  
  apply_fun <- if (use_parallel && requireNamespace("future.apply", quietly = TRUE)) {
    future.apply::future_lapply
  } else {
    lapply
  }
  
  message(
    "apply_analysis() : preparing weights + disaggregation columns once for this call (",
    paste(disaggregations[disaggregations != "none"], collapse = " | "), ")"
  )
  prepped <- prepare_analysis_dataset(dataset, weights_column, disaggregations)
  dataset_ready <- prepped$dataset
  group_vars <- prepped$group_vars
  
  results_list <- list()
  
  #### select one questions ####
  if (length(select_one_questions) == 0) {
    message("No select one question analysed as the select_one_questions vector is empty")
  } else {
    results_list[["select_one"]] <- rbindlist(apply_fun(
      select_one_questions,
      function(v) categorical_analysis(dataset_ready, v, group_vars, disaggregations, weights_column, levels = modality_levels[[v]])
    )) %>% mutate(question_type = "select_one")
  }
  
  #### select multiple questions ####
  if (length(select_multiple_questions) == 0) {
    message("No select multiple question analysed as the select_multiple_questions vector is empty")
  } else {
    results_list[["select_multiple"]] <- rbindlist(apply_fun(
      select_multiple_questions,
      function(v) categorical_analysis(dataset_ready, v, group_vars, disaggregations, weights_column, levels = modality_levels[[v]])
    )) %>% mutate(question_type = "select_multiple")
  }
  
  #### integer questions ####
  if (length(integer_questions) == 0) {
    message("No integer question analysed as the integer_questions vector is empty")
  } else {
    results_list[["integer"]] <- rbindlist(apply_fun(
      integer_questions,
      function(v) numerical_analysis(dataset_ready, v, group_vars, disaggregations, weights_column)
    )) %>% mutate(question_type = "integer")
  }
  
  #### recoded variables ####
  if (length(recoded_var) == 0) {
    message("No recoded variable analysed as the recoded_var vector is empty")
  } else {
    results_list[["recoded_var"]] <- rbindlist(apply_fun(
      recoded_var,
      function(v) categorical_analysis(dataset_ready, v, group_vars, disaggregations, weights_column)
    )) %>% mutate(question_type = "results_recoded_var")
  }
  
  #### ratio questions ####
  if (length(ratio_questions) == 0) {
    message("No ratio question analysed as the ratio_questions vector is empty")
  } else {
    results_list[["ratio"]] <- rbindlist(Map(
      function(num_var, denom_var) {
        match.fun(ratio_analysis_type)(dataset_ready, num_var, denom_var, group_vars, disaggregations, weights_column)
      },
      names(ratio_questions),
      unname(ratio_questions)
    )) %>% mutate(question_type = "ratio")
  }
  
  if (length(results_list) == 0) {
    warning("No question analysed: select_one, select_multiple, integer, ratio and recoded_var are all empty.")
    return(NULL)
  }
  
  results_list <- harmonize_column_types(results_list)
  
  tot_results <- bind_rows(results_list) %>%
    mutate(question_variable_name = as.character(question_variable_name))
  
  tot_results
}