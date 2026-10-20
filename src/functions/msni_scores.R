# ============================================================================
# MSNI - builds the sectoral composite scores + the final MSNI score
# ============================================================================
# package_version("humind") # 2026
data <- readxl::excel_sheets(intermediate_datasets_file) %>%
  map(~ readxl::read_excel(
    path = intermediate_datasets_file,
    sheet = .x,
    guess = 50000
  )) |>
  set_names(readxl::excel_sheets(intermediate_datasets_file))

loop <- left_joints_dup_modifier(
  list(
    roster       = data$clean_roster,
    edu_ind    = data$clean_edu_ind,
    health_ind = data$clean_health_ind,
    # prot_ind   = data$clean_prot_ind,
    nut_ind    = data$clean_nut_ind,
    wgss_indiv = data$clean_wgss_indiv
  ),
  person_id,
  uuid,
  duplicated_columns = 4
)

main_dataset <- data$`clean_main_dataset`

cols <- c("resp_age", "hoh_age_final")
main_dataset <- convert_columns_to_numeric(main_dataset, cols)
main_dataset <- add_age_cat(
  main_dataset,
  age_col = "hoh_age_final",
  breaks = c(0, 18, 60, 120),
  labels = NULL,
  int_undefined = c(-999, 999),
  char_undefined = "undefined",
  new_colname = "hoh_age_cat"
)

## ---- Clean up food security indicators -------------------------------------

# HHS calculation should not contain Don't know or Prefer not to answer
main_dataset <- mutate(
  main_dataset,
  across(
    c("fsl_hhs_nofoodhh", "fsl_hhs_sleephungry", "fsl_hhs_alldaynight"),
    \(x) case_when(
      x %in% c("dnk", "pnta") ~ NA_character_,
      .default = x
    )
  )
)

## ---- LSG --------------------------------------------

main_dataset <- main_dataset |>
  humind::add_comp_foodsec() |>
  humind::add_comp_snfi() |>
  humind::add_comp_prot()|>
  humind::add_comp_health() |>
  humind::add_comp_wash() |>
  humind::add_comp_edu()

## ---- MSNI --------------------------------------------
main_dataset <- main_dataset |>
  humind::add_msni()

## ---- Add the _1.._5 / _345 / _45 breakdown columns for every score ---------

add_score_suffixes <- function(df, score_var, in_need_var, severe_var) {

  score <- df[[score_var]]

  base <- sub("_score$", "", score_var)

  df[[paste0(base, "_1")]]   <- as.integer(score == 1)
  df[[paste0(base, "_2")]]   <- as.integer(score == 2)
  df[[paste0(base, "_3")]]   <- as.integer(score == 3)
  df[[paste0(base, "_4")]]   <- as.integer(score == 4)
  df[[paste0(base, "_5")]]   <- as.integer(score == 5)
  df[[paste0(base, "_345")]] <- df[[in_need_var]]
  df[[paste0(base, "_45")]]  <- df[[severe_var]]

  df
}

mapping <- tribble(
  ~score,               ~in_need,                       ~severe,
  "comp_snfi_score",    "comp_snfi_in_need",           "comp_snfi_in_severe_need",
  "comp_foodsec_score", "comp_foodsec_in_need",         "comp_foodsec_in_severe_need",
  "comp_prot_score",    "comp_prot_in_need",           "comp_prot_in_severe_need",
  "comp_health_score",  "comp_health_in_need",         "comp_health_in_severe_need",
  "comp_wash_score",    "comp_wash_in_need",           "comp_wash_in_severe_need",
  "comp_edu_score",     "comp_edu_in_need",           "comp_edu_in_severe_need",
  "msni_score",         "msni_in_need",               "msni_in_severe_need"
)

for (i in seq_len(nrow(mapping))) {
  main_dataset <- add_score_suffixes(
    main_dataset,
    mapping$score[i],
    mapping$in_need[i],
    mapping$severe[i]
  )
}

ajouter_colonnes_communes <- function(df) {
  df %>%
    rename(PoI = Pol) %>%
    mutate(
      PoI = recode(
        PoI,
        idp      = "PDI",
        host     = "PND",
        retourne = "RET",
        refugie  = "REF"
      ),
      country_pcode = country_name,
      CONCATENATE_1 = paste(country_pcode, PoI, sep = "_"),
      CONCATENATE_2 = paste(Admin_3, PoI, sep = "_")
    )
}

main_dataset <- main_dataset %>%
  ajouter_colonnes_communes() %>%
  group_by(CONCATENATE_2) %>%
  mutate(
    msni_345_n  = sum(msni_345 == 1, na.rm = TRUE),
    msni_45_n   = sum(msni_45 == 1, na.rm = TRUE),
    msni_n      = sum(msni_score > 0, na.rm = TRUE),
    lsg_edu_n   = sum(comp_edu_score > 0, na.rm = TRUE),
    lsg_eha_n   = sum(comp_wash_score > 0, na.rm = TRUE),
    lsg_abna_n  = sum(comp_snfi_score > 0, na.rm = TRUE),
    lsg_sante_n = sum(comp_health_score > 0, na.rm = TRUE),
    lsg_secal_n = sum(comp_foodsec_score > 0, na.rm = TRUE),
    lsg_pro_n   = sum(comp_prot_score > 0, na.rm = TRUE)
  ) %>%
  ungroup()


## ---- Save the full indicator dataset

openxlsx::write.xlsx(
  list(
    main_dataset = main_dataset,
    loop = loop
  ),
  file = out_path("Dataset used and dictionnaire_indi", "clean_dataset_and_all_indicators.xlsx"),
  overwrite = TRUE
)
