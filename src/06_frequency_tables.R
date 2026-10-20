# 06 - Frequency tables (Excel) from tot_results

niveaux_pcode <- tibble::tribble(
  ~nom_col,        ~pop_col,           ~pcode_col,
  "Admin_1_nom",   "admin1_pop",       "Admin_1",
  "Admin_2_nom",   "admin2_pop",       "Admin_2",
  # "Admin_3_nom", "admin3_pop",       "Admin_3",   # not used for this country
  "LZNAME_FINAL",  "LZNAME_FINAL_PoI", NA_character_,
  "zone",          "setting_ML",       NA_character_
)

build_pcode_lookups <- function(main_dataset, niveaux_pcode) {
  lookups <- list()
  for (i in seq_len(nrow(niveaux_pcode))) {
    nom_col   <- niveaux_pcode$nom_col[i]
    pop_col   <- niveaux_pcode$pop_col[i]
    pcode_col <- niveaux_pcode$pcode_col[i]
    
    if (is.na(pcode_col) || !pcode_col %in% names(main_dataset)) next
    
    if (nom_col %in% names(main_dataset)) {
      lookups[[nom_col]] <- main_dataset %>%
        dplyr::distinct(.data[[nom_col]], .data[[pcode_col]]) %>%
        dplyr::filter(!is.na(.data[[nom_col]])) %>%
        dplyr::rename(valeur = 1, pcode = 2)
    }
    if (pop_col %in% names(main_dataset)) {
      lookups[[pop_col]] <- main_dataset %>%
        dplyr::distinct(.data[[pop_col]], .data[[pcode_col]]) %>%
        dplyr::filter(!is.na(.data[[pop_col]])) %>%
        dplyr::rename(valeur = 1, pcode = 2)
    }
  }
  lookups
}

pcode_lookups <- build_pcode_lookups(main_dataset, niveaux_pcode)

group_pcode_generic <- function(group_var, group_var_value, lookups) {
  pcode <- rep(NA_character_, length(group_var))
  for (niveau in names(lookups)) {
    idx <- group_var == niveau
    if (!any(idx)) next
    pcode[idx] <- lookups[[niveau]]$pcode[match(group_var_value[idx], lookups[[niveau]]$valeur)]
  }
  pcode
}

main_dataset <- main_dataset %>% mutate(Admin_0 = Admin_code, Admin_0_nom = country_name)

## ---- theme_IDD -------------------------------------------------------------
# Same fallback as 07_indesign_exports.R: use theme_IDD_kobo_legacy if theme_IDD is missing.
ensure_theme_idd <- function(df, label) {
  if (!"theme" %in% names(df)) {
    if ("theme_IDD_kobo_legacy" %in% names(df)) {
      message(
        "06_frequency_tables.R: column 'theme_IDD' missing from ", label, " - ",
        "using 'theme_IDD_kobo_legacy' instead for the by-theme export."
      )
      df <- df %>% mutate(theme_IDD = theme_IDD_kobo_legacy)
    } else {
      stop(
        "06_frequency_tables.R: neither 'theme_IDD' nor 'theme_IDD_kobo_legacy' ",
        "in ", label, " - the by-theme export cannot be built. ",
        "Check indicator_metadata / the join in 05_analyses.R."
      )
    }
  }
  df
}

presentresults_table <- tot_results %>%
  mutate(
    analysis_type      = analysis_type,
    analysis_var        = analysis_var,
    analysis_var_value  = analysis_var_value,
    group_var            = group_var,
    group_var_value       = group_var_value,
    stat                  = suppressWarnings(as.numeric(stat)),
    n_unweighted          = suppressWarnings(as.numeric(n)),
    n_total_unweighted    = suppressWarnings(as.numeric(N))
  ) %>% mutate(
    group_var_value_pcode = group_pcode_generic(group_var, group_var_value, pcode_lookups),
    group_var_value_disambig = ifelse(
      is.na(group_var_value_pcode),
      group_var_value,
      paste0(group_var_value, "_", group_var_value_pcode)
    ),
    analysis_key = ifelse(
      is.na(group_var) | group_var %in% c("none", "", NA),
      paste0(analysis_type, " @/@ ", analysis_var, " %/% ", analysis_var_value, " @/@ ", group_var, " %/% ", group_var_value),
      paste0(analysis_type, " @/@ ", analysis_var, " %/% ", analysis_var_value, " @/@ ", group_var, " %/% ", group_var_value,
             " -/- ", group_var, " %/% ", group_var_value_disambig)
    )
  ) %>%
  select(-group_var_value_pcode, -group_var_value_disambig)

presentresults_table <- ensure_theme_idd(presentresults_table, "presentresults_table")

# Admin_0 / Admin_0_nom: country constants, also added to main_dataset above
presentresults_table <- presentresults_table %>%
  mutate(Admin_0 = Admin_code, Admin_0_nom = country_name)

## ---- Export columns (from disaggregation in run.R) ----


colonnes_niveaux <- disaggregation

# pcode columns only for the analysed levels

pcodes_a_ajouter <- niveaux_pcode %>%
  dplyr::filter(!is.na(pcode_col), nom_col %in% disaggregation | pop_col %in% disaggregation) %>%
  dplyr::pull(pcode_col)

presentresults_export_columns <- c(
  "group_var", "Admin_0", "Admin_0_nom", "country",
  pcodes_a_ajouter, colonnes_niveaux,
  "theme", "module", "indicator", "analysis_type", "question.label", "before_dot",
  "analysis_var", "analysis_var_value", "choice.label", "stat", "n", "N",
  "stat_upp", "stat_low", "MoE", "n_weighted", "n_w_total", "estimator",
  "weighted", "theme", "analysis_key"
)

## ---- spread_group_var(), columns taken from 'disaggregation' --------------

groups_spread <- c(country = "country", setNames(disaggregation, disaggregation))

presentresults_table_export <- presentresults_table %>%
  spread_group_var(groups = groups_spread)

# rebuild the pcode columns, only for analysed levels that have a pcode
for (i in seq_len(nrow(niveaux_pcode))) {
  pcode_col <- niveaux_pcode$pcode_col[i]
  nom_col   <- niveaux_pcode$nom_col[i]
  if (is.na(pcode_col) || !nom_col %in% disaggregation || is.null(pcode_lookups[[nom_col]])) next
  
  presentresults_table_export[[pcode_col]] <- pcode_lookups[[nom_col]]$pcode[
    match(presentresults_table_export[[nom_col]], pcode_lookups[[nom_col]]$valeur)
  ]
}

presentresults_table_export <- presentresults_table_export %>%
  select(all_of(presentresults_export_columns))

writexl::write_xlsx(tot_results, out_path("Tables frequences", paste0("results_table_for_transormation_", run_date, ".xlsx")))



## ---- Final "variable x group" table, by theme ------------------------------

# NA stays NA (nobody answered); options never chosen are already 0
my_results_table <- presentresults_table

# create_table_variable_x_group() pivots on analysis_key - check it's unique first
dup_keys <- my_results_table %>%
  dplyr::count(analysis_key, name = "n_occurrences") %>%
  dplyr::filter(n_occurrences > 1)

if (nrow(dup_keys) > 0) {
  dup_detail <- my_results_table %>%
    dplyr::filter(analysis_key %in% dup_keys$analysis_key) %>%
    dplyr::distinct(analysis_key, analysis_var) %>%
    dplyr::arrange(analysis_key)

  warning(
    nrow(dup_keys), " duplicate analysis_key(s) in my_results_table (",
    sum(dup_keys$n_occurrences), " rows affected). Usually means the same ",
    "variable exists in several datasets (household + loop) analysed at the ",
    "same disaggregation level. Preview: ",
    paste(utils::head(dup_keys$analysis_key, 5), collapse = " | ")
  )
  print(utils::head(dup_detail, 20))

  my_results_table <- my_results_table %>% dplyr::distinct(analysis_key, .keep_all = TRUE)
}

table_variable_x_group <- my_results_table %>%
  create_table_variable_x_group(analysis_key = "analysis_key", value_columns = "stat") %>%
  # drop the "0" modality, but only for select_multiple questions
  filter(!(analysis_type == "select_multiple" & analysis_var_value == "0")) %>%
  rename_with(~ sub("^.*%/% ", "", .x), .cols = matches("%/%")) %>%
  # split variable and modality apart for select_multiple questions
  mutate(
    analysis_var_value = if_else(analysis_type == "select_multiple", sub("^[^.]*\\.", "", analysis_var), analysis_var_value),
    analysis_var        = if_else(analysis_type == "select_multiple", sub("\\..*", "", analysis_var), analysis_var)
  ) %>%
  left_join(indicator_metadata, by = c("analysis_var" = "name")) %>%
  relocate(theme, indicator, .after = analysis_type)

table_variable_x_group <- ensure_theme_idd(table_variable_x_group, "table_variable_x_group")

## ---- Percentage formatting (with the % sign) for the export -----------------

# Percentages only for proportions; means / medians keep their unit; NA -> "N/A"
format_stat <- function(x, is_pct) {
  dplyr::case_when(
    is.na(x) ~ "N/A",
    is_pct ~ paste0(formatC(x * 100, format = "f", digits = 1), "%"),
    TRUE ~ formatC(x, format = "f", digits = 2)
  )
}

is_pct_row <- table_variable_x_group$analysis_type %in% c("select_one", "select_multiple", "ratio")

colonnes_a_formater <- table_variable_x_group %>%
  dplyr::select(where(is.numeric)) %>%
  names()

table_variable_x_group <- table_variable_x_group %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(colonnes_a_formater), ~ format_stat(.x, is_pct_row)))

themes_uniques <- table_variable_x_group %>%
  dplyr::pull(theme) %>%
  unique()

themes_uniques <- themes_uniques[!is.na(themes_uniques) & themes_uniques != ""]

for (theme_val in themes_uniques) {

  theme_slug <- tolower(theme_val)
  theme_slug <- gsub("[^a-z0-9]+", "_", theme_slug)
  theme_slug <- gsub("^_|_$", "", theme_slug)

  table_variable_x_group %>%
    dplyr::filter(theme == theme_val) %>%
    create_xlsx_variable_x_group_by_theme(
      file_path = out_path(
        "Tables frequences",
        paste0("tableau_", theme_slug, "_", run_date, ".xlsx")
      ),
      theme_column = "theme",
      value_columns = "stat",
      overwrite = TRUE
    )
}

table_variable_x_group %>%
  create_xlsx_variable_x_group_all(
    file_path = out_path("Tables frequences", paste0("tableau_All_Thematique_", run_date, ".xlsx")),
    value_columns = "stat",
    overwrite = TRUE
  )
