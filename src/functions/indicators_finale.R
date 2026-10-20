# ============================================================================
# INDICATORS FINALE - derived indicators requested in the indicator base and
# not produced elsewhere. Additive only: existing columns are never modified.
# Each new variable is also declared in tool.survey so it is analysed like
# the others, and its expected values go to finale_levels.
# ============================================================================

finale_rows <- list()
finale_levels <- tibble::tibble(variable = character(), modality = character(), rule = character())

# theme / theme_IDD are taken from the source variable in the Kobo tool when available
declare_finale <- function(name, label, theme, theme_IDD, module, levels, rule, type = "select_one", source = NULL) {
  i <- match(source, tool.survey$name)
  if (length(i) == 1 && !is.na(i)) {
    if ("theme" %in% names(tool.survey) && !is.na(tool.survey$theme[i])) theme <- tool.survey$theme[i]
    if ("theme_IDD" %in% names(tool.survey) && !is.na(tool.survey$theme_IDD[i])) theme_IDD <- tool.survey$theme_IDD[i]
  }
  finale_rows[[name]] <<- tibble::tibble(
    type = type, name = name, label = label, theme = theme, theme_IDD = theme_IDD,
    module = module, indicator = label
  )
  if (!is.null(levels)) {
    finale_levels <<- dplyr::bind_rows(
      finale_levels,
      tibble::tibble(variable = name, modality = as.character(levels), rule = rule)
    )
  }
}

sector_info <- tibble::tribble(
  ~sector,   ~label,                     ~theme_IDD,
  "snfi",    "ABNA",                     "ABNA",
  "wash",    "EHA",                      "WASH",
  "edu",     "Education",                "Education",
  "health",  "Health",                   "Sant\u00e9",
  "foodsec", "Food security",            "SECAL",
  "prot",    "Protection",               "Protection"
)
theme_of <- function(var) {
  s <- sector_info$theme_IDD[match(sub("^comp_([a-z]+)_.*$", "\\1", var), sector_info$sector)]
  ifelse(is.na(s), NA_character_, s)  # no sector: theme_IDD left empty
}

## ---- 1. LSG sub-determinants: severity 3+ and 4+ ----------------------------

sub_scores <- c(
  "comp_snfi_score_shelter_type_cat", "comp_snfi_score_shelter_issue_cat",
  "comp_snfi_score_tenure_security_cat", "comp_snfi_score_fds_cannot_cat",
  "comp_snfi_score_shelter_damage_cat",
  "comp_wash_score_water_quantity", "comp_wash_score_water_quality",
  "comp_wash_score_sanitation", "comp_wash_score_hygiene",
  "comp_prot_score_movement", "comp_prot_score_practices", "comp_prot_score_rights",
  "comp_edu_score_disrupted", "comp_edu_score_attendance"
)

for (score in intersect(sub_scores, names(main_dataset))) {
  if (!score %in% tool.survey$name) {
    declare_finale(
      score, paste0("% of households by severity level (", score, ")"),
      "MSNI framework", theme_of(score), "MSNI framework - LSG sub-determinants",
      NULL, "humind sub-score, levels in composite_levels.R", source = paste0(sub("_score_.*$", "", score), "_score")
    )
  }
  x <- suppressWarnings(as.numeric(main_dataset[[score]]))
  for (k in c(3, 4)) {
    new_col <- paste0(score, if (k == 3) "_345" else "_45")
    if (new_col %in% names(main_dataset)) next
    main_dataset[[new_col]] <- ifelse(is.na(x), NA_integer_, as.integer(x >= k))
    declare_finale(
      new_col,
      paste0("% of households with severity ", k, "+ (", score, ")"),
      "MSNI framework", theme_of(score), "MSNI framework - LSG aggregates",
      0:1, paste0("1 if ", score, " >= ", k, ", 0 if lower, NA if score missing"), source = score
    )
  }
}

## ---- 2. Sector need x lowest income quantile ---------------------------------
# Same rule as cm_foodsec_in_need_low_income (indicators_new_2026_additions.R):
# lowest category of cm_income_total_qtl, among households in need.

need_vars <- c(paste0("comp_", sector_info$sector, "_in_need"), "msni_in_need")

if ("cm_income_total_qtl" %in% names(main_dataset)) {
  lowest_qtl <- main_dataset$cm_income_total_qtl %>% unique() %>% na.omit() %>% sort() %>% dplyr::first()
  for (v in intersect(need_vars, names(main_dataset))) {
    new_col <- paste0(v, "_low_income")
    main_dataset[[new_col]] <- dplyr::case_when(
      is.na(main_dataset[[v]]) | main_dataset[[v]] != 1 ~ NA_character_,
      is.na(main_dataset$cm_income_total_qtl) ~ NA_character_,
      main_dataset$cm_income_total_qtl == lowest_qtl ~ "yes",
      TRUE ~ "no"
    )
    declare_finale(
      new_col,
      paste0("% of households in need (", v, ") with a total income in the lowest quantile (", lowest_qtl, ")"),
      "MSNI framework", theme_of(v), "MSNI framework - need x income",
      c("yes", "no"), paste0("among ", v, " == 1: yes if cm_income_total_qtl == '", lowest_qtl, "'"), source = v
    )
  }
}

## ---- 3. Sector need x no assistance in the last 12 months --------------------

if ("aap_received_assistance_12m" %in% names(main_dataset)) {
  for (v in intersect(c("msni_in_need", "comp_foodsec_in_need"), names(main_dataset))) {
    new_col <- paste0(v, "_no_assistance")
    main_dataset[[new_col]] <- dplyr::case_when(
      is.na(main_dataset[[v]]) | main_dataset[[v]] != 1 ~ NA_character_,
      main_dataset$aap_received_assistance_12m == "no" ~ "yes",
      main_dataset$aap_received_assistance_12m == "yes" ~ "no",
      TRUE ~ NA_character_
    )
    declare_finale(
      new_col,
      paste0("% of households in need (", v, ") that received no humanitarian assistance in the last 12 months"),
      "Accountability to Affected Populations (AAP)", "AAP", "AAP - need x assistance",
      c("yes", "no"), paste0("among ", v, " == 1: yes if aap_received_assistance_12m == 'no', no if 'yes', NA otherwise (dnk/pnta)"), source = v
    )
  }
}

## ---- 4. Sector need by sex of the head of household ---------------------------

if ("hoh_gender" %in% names(main_dataset)) {
  by_sex <- list(comp_health_score = 1:3)
  for (v in need_vars) by_sex[[v]] <- 0:1
  for (v in intersect(names(by_sex), names(main_dataset))) {
    for (g in c("female", "male")) {
      new_col <- paste0(v, "_hoh_", g)
      main_dataset[[new_col]] <- ifelse(main_dataset$hoh_gender %in% g, main_dataset[[v]], NA)
      declare_finale(
        new_col,
        paste0("% of ", g, "-headed households by ", v),
        "MSNI framework", theme_of(v), "MSNI framework - by sex of the head of household",
        by_sex[[v]], paste0(v, " restricted to hoh_gender == '", g, "'"), source = v
      )
    }
  }
}

## ---- 5. Latrines shared with 20 households or more ---------------------------

if ("wash_sharing_sanitation_facility_n_ind" %in% names(main_dataset)) {
  main_dataset$wash_sharing_20plus <- dplyr::case_when(
    main_dataset$wash_sharing_sanitation_facility_n_ind %in% c("20_to_49", "50_and_above") ~ "yes",
    main_dataset$wash_sharing_sanitation_facility_n_ind == "19_and_below" ~ "no",
    TRUE ~ NA_character_
  )
  declare_finale(
    "wash_sharing_20plus",
    "% of households sharing their sanitation facility with 20 households or more (among sharing households)",
    "WASH", "WASH", "WASH - sanitation", c("yes", "no"),
    "yes if wash_sharing_sanitation_facility_n_ind in 20_to_49 / 50_and_above, no if 19_and_below",
    source = "wash_sharing_sanitation_facility_n_ind"
  )
}

## ---- 6. HWISE score (mean) -----------------------------------------------------

if ("hwise4_score" %in% names(main_dataset) && !"hwise4_score" %in% tool.survey$name) {
  declare_finale(
    "hwise4_score", "HWISE-4 score (mean, 0-12)", "WASH", "WASH", "WASH - HWISE",
    NULL, "humind::add_hwise", type = "integer", source = "comp_wash_score_water_quantity"
  )
}

## ---- 7. Food security categories typed "calculate" in the Kobo tool ----------
# They are needed by the indicator base; analysed as select_one like in the
# other countries' tools.

for (v in c("fsl_fcs_cat", "fsl_rcsi_cat", "fsl_hhs_cat", "fsl_lcsi_cat")) {
  rows <- which(tool.survey$name == v)
  if (v %in% names(main_dataset) && length(rows) > 0 && !any(grepl("^select_one", tool.survey$type[rows]))) {
    tool.survey$type[rows[1]] <- "select_one"
    if ("theme_IDD" %in% names(tool.survey) && is.na(tool.survey$theme_IDD[rows[1]])) tool.survey$theme_IDD[rows[1]] <- "SECAL"
    if ("theme" %in% names(tool.survey) && is.na(tool.survey$theme[rows[1]])) tool.survey$theme[rows[1]] <- "Food Security"
    message("indicators_finale.R: ", v, " analysed as select_one (typed calculate in the Kobo tool).")
  }
}

## ---- Declare the new variables in the Kobo tool used for the analysis ---------

if (length(finale_rows) > 0) {
  new_rows <- dplyr::bind_rows(finale_rows) %>% dplyr::filter(!name %in% tool.survey$name)
  tool.survey <- dplyr::bind_rows(tool.survey, new_rows)
  message("indicators_finale.R: ", nrow(new_rows), " derived indicator(s) added.")
}
