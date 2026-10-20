# ============================================================================
# FOOD SECURITY (FSL) INDICATORS
# ============================================================================
# ---- Food Consumption Score (FCS) ------------------------------------------
main_dataset <- humind::add_fcs(
  main_dataset,
  cutoffs = c("normal"),
  fsl_fcs_cereal  = "fsl_fcs_cereal",
  fsl_fcs_legumes = "fsl_fcs_legumes",
  fsl_fcs_veg     = "fsl_fcs_veg",
  fsl_fcs_fruit   = "fsl_fcs_fruit",
  fsl_fcs_meat    = "fsl_fcs_meat",
  fsl_fcs_dairy   = "fsl_fcs_dairy",
  fsl_fcs_sugar   = "fsl_fcs_sugar",
  fsl_fcs_oil     = "fsl_fcs_oil"
)


# ---- Livelihood Coping Strategy Index (LCSI) -------------------------------
main_dataset <- humind::add_lcsi(
  main_dataset,
  fsl_lcsi_stress1 = "fsl_lcsi_stress1",
  fsl_lcsi_stress2 = "fsl_lcsi_stress2",
  fsl_lcsi_stress3 = "fsl_lcsi_stress3",
  fsl_lcsi_stress4 = "fsl_lcsi_stress4",
  fsl_lcsi_crisis1 = "fsl_lcsi_crisis1",
  fsl_lcsi_crisis2 = "fsl_lcsi_crisis2",
  fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
  fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
  fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
  fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
  yes_val = "yes",
  no_val = "no_had_no_need",
  exhausted_val = "no_exhausted",
  not_applicable_val = "not_applicable"
)

# ---- Household Hunger Scale (HHS) ------------------------------------------
# HHS: the clean data is already consistent (dnk / pnta recoded, "yes" without frequency set to empty,
# see hh_cleaning_log, check hhs_oui_sans_frequence)
# HHS columns already in the cleaned dataset are replaced (add_hhs() stops on duplicated names)
hhs_out <- c(paste0(c("fsl_hhs_nofoodhh", "fsl_hhs_nofoodhh_freq", "fsl_hhs_sleephungry", "fsl_hhs_sleephungry_freq",
                      "fsl_hhs_alldaynight", "fsl_hhs_alldaynight_freq"), "_recoded"),
             "fsl_hhs_comp1", "fsl_hhs_comp2", "fsl_hhs_comp3", "fsl_hhs_score", "fsl_hhs_cat_ipc", "fsl_hhs_cat")
main_dataset <- main_dataset[, setdiff(names(main_dataset), hhs_out)]
main_dataset <- humind::add_hhs(
  main_dataset,
  fsl_hhs_nofoodhh          = "fsl_hhs_nofoodhh",
  fsl_hhs_nofoodhh_freq     = "fsl_hhs_nofoodhh_freq",
  fsl_hhs_sleephungry       = "fsl_hhs_sleephungry",
  fsl_hhs_sleephungry_freq  = "fsl_hhs_sleephungry_freq",
  fsl_hhs_alldaynight       = "fsl_hhs_alldaynight",
  fsl_hhs_alldaynight_freq  = "fsl_hhs_alldaynight_freq",
  yes_answer      = "yes",
  no_answer       = "no",
  rarely_answer   = "rarely",
  sometimes_answer = "sometimes",
  often_answer    = "often"
)

main_dataset <- main_dataset %>%
  humind::add_rcsi(
    fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
    fsl_rcsi_borrow       = "fsl_rcsi_borrow",
    fsl_rcsi_mealsize     = "fsl_rcsi_mealsize",
    fsl_rcsi_mealadult    = "fsl_rcsi_mealadult",
    fsl_rcsi_mealnb       = "fsl_rcsi_mealnb"
  )

# ---- Food Consumption Matrix (FCM) phase, combined step by step -----------
main_dataset <- main_dataset %>%
  humind::add_fcm_phase(
    hdds_column_name = "fsl_hdds_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name  = "fsl_hhs_cat_ipc",
    hdds_categories_low = "Low", hdds_categories_medium = "Medium", hdds_categories_high = "High",
    rcsi_categories_low = "No to Low", rcsi_categories_medium = "Medium", rcsi_categories_high = "High",
    hhs_categories_none = "None", hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate", hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )

main_dataset <- main_dataset %>%
  humind::add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    fcs_categories_acceptable = "Acceptable", fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    hhs_categories_none = "None", hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate", hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )

main_dataset <- main_dataset %>%
  humind::add_fcm_phase(
    hdds_column_name = "fsl_hdds_cat",
    hhs_column_name  = "fsl_hhs_cat_ipc",
    hdds_categories_low = "Low", hdds_categories_medium = "Medium", hdds_categories_high = "High",
    hhs_categories_none = "None", hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate", hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )

# ---- Food Consumption - Livelihood Coping Matrix (FCLCM) ------------------
main_dataset <- humind::add_fclcm_phase(
  main_dataset,
  fc_phase_var = "fsl_fc_phase",
  fc_phase_1 = "Phase 1 FC",
  fc_phase_2 = "Phase 2 FC",
  fc_phase_3 = "Phase 3 FC",
  fc_phase_4 = "Phase 4 FC",
  fc_phase_5 = "Phase 5 FC",
  lcs_cat_var = "fsl_lcsi_cat",
  lcs_cat_none = "None",
  lcs_cat_stress = "Stress",
  lcs_cat_crisis = "Crisis",
  lcs_cat_emergency = "Emergency"
)




# ==============================================================================
# Creation de colonnes dupliquees,

lcsi_rename_map <- c(
  fsl_lcsi_stress1    = "Lcs_stress_DomAsset",
  fsl_lcsi_stress2    = "Lcs_stress_Saving",
  fsl_lcsi_stress3    = "Lcs_stress_BorrowCash",
  fsl_lcsi_stress4    = "LcsR_stress_Animals",
  fsl_lcsi_crisis1    = "Lcs_crisis_ProdAsset",
  fsl_lcsi_crisis2  = "Lcs_crisis_Health",
  fsl_lcsi_crisis3    = "LcsR_crisis_Seed",
  fsl_lcsi_emergency1 = "Lcs_em_ResAsset",
  fsl_lcsi_emergency2 = "Lcs_em_IllegalAct",
  fsl_lcsi_emergency3 = "Lcs_em_Begged"
)


ajouter_colonnes_lcsi_officielles <- function(df, rename_map = lcsi_rename_map) {
  
  presentes <- intersect(names(rename_map), names(df))
  absentes  <- setdiff(names(rename_map), names(df))
  if (length(absentes) > 0) {
    warning(
      "Variable(s) absente(s) du dataset, colonne officielle non creee pour : ",
      paste(absentes, collapse = ", ")
    )
  }
  
  for (ancien_nom in presentes) {
    nouveau_nom <- rename_map[[ancien_nom]]
    df[[nouveau_nom]] <- df[[ancien_nom]]
  }
  
  df
}
# Creation
main_dataset <- ajouter_colonnes_lcsi_officielles(main_dataset)
