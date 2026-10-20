
if (exists("loop") && all(c("prot_child_work", "prot_ind_age", "uuid") %in% names(loop))) {

  loop <- loop %>%
    dplyr::mutate(
      prot_ind_age_num = suppressWarnings(as.numeric(prot_ind_age)),
      .prot_child_work_5_17 = dplyr::case_when(
        is.na(prot_ind_age_num) | prot_ind_age_num < 5 | prot_ind_age_num >= 18 ~ NA_real_,
        prot_child_work == "yes" ~ 1,
        prot_child_work == "no"  ~ 0,
        TRUE ~ NA_real_
      )
    )

  hh_child_work_5_17 <- loop %>%
    dplyr::filter(!is.na(.prot_child_work_5_17)) %>%
    dplyr::group_by(uuid) %>%
    dplyr::summarise(
      prot_hh_child_work_5_17_num = as.numeric(any(.prot_child_work_5_17 == 1)),
      .groups = "drop"
    )

  # Colonne technique (préfixe ".", valeurs 0/1/NA) 
  loop <- loop %>% dplyr::select(-.prot_child_work_5_17)

  main_dataset <- main_dataset %>%
    dplyr::left_join(hh_child_work_5_17, by = "uuid") %>%
    dplyr::mutate(
      prot_hh_child_work_5_17 = dplyr::case_when(
        prot_hh_child_work_5_17_num == 1 ~ "yes",
        prot_hh_child_work_5_17_num == 0 ~ "no",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-prot_hh_child_work_5_17_num)


} else {
}



if (exists("loop") && all(c("edu_other_yn", "uuid") %in% names(loop))) {

  loop <- loop %>%
    dplyr::mutate(
      .edu_other_yn_num = dplyr::case_when(
        edu_other_yn == "yes" ~ 1,
        edu_other_yn == "no"  ~ 0,
        TRUE ~ NA_real_
      )
    )

  hh_edu_other <- loop %>%
    dplyr::filter(!is.na(.edu_other_yn_num)) %>%
    dplyr::group_by(uuid) %>%
    dplyr::summarise(
      edu_hh_other_education_access_num = as.numeric(any(.edu_other_yn_num == 1)),
      .groups = "drop"
    )

  
  loop <- loop %>% dplyr::select(-.edu_other_yn_num)

  main_dataset <- main_dataset %>%
    dplyr::left_join(hh_edu_other, by = "uuid") %>%
    dplyr::mutate(
      edu_hh_other_education_access = dplyr::case_when(
        edu_hh_other_education_access_num == 1 ~ "yes",
        edu_hh_other_education_access_num == 0 ~ "no",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-edu_hh_other_education_access_num)


} else {
}


## ---- 2. Croisement besoin (SECA) x revenu ------------------------------------


if (all(c("comp_foodsec_in_need", "cm_income_total_qtl") %in% names(main_dataset))) {

  lowest_qtl <- main_dataset$cm_income_total_qtl %>% unique() %>% na.omit() %>% sort() %>% dplyr::first()

  main_dataset <- main_dataset %>%
    dplyr::mutate(
      cm_foodsec_in_need_low_income = dplyr::case_when(
        is.na(comp_foodsec_in_need) | comp_foodsec_in_need != 1 ~ NA_character_,
        is.na(cm_income_total_qtl) ~ NA_character_,
        cm_income_total_qtl == lowest_qtl ~ "yes",
        TRUE ~ "no"
      )
    )

  message(
    "Nouvel indicateur ajouté : main_dataset$cm_foodsec_in_need_low_income ",
    "(parmi les ménages en besoin SECA, % avec un revenu dans le quintile le ",
    "plus bas '", lowest_qtl, "' de cm_income_total_qtl). ",
    "A VERIFIER : la catégorie la plus basse a été détectée automatiquement ",
    "par tri alphabétique des valeurs existantes de cm_income_total_qtl - ",
    "confirmer qu'elle correspond bien au quintile 0-20% avant publication."
  )

} else {
  warning("cm_foodsec_in_need_low_income NON calculé : comp_foodsec_in_need ou cm_income_total_qtl introuvable dans main_dataset à ce stade du pipeline.")
}


## ---- 3. HWISE (Score HWISE) -------------------------------------------------


if (all(c("hwise4_score", "comp_wash_score_water_quantity") %in% names(main_dataset))) {

  main_dataset <- main_dataset %>%
    dplyr::mutate(
      wash_hwise4_score = hwise4_score,
      wash_hwise4_cat = dplyr::case_when(
        is.na(hwise4_score) ~ NA_character_,
        hwise4_score >= 4    ~ "water_insecure",
        TRUE                  ~ "water_secure"
      ),
      wash_hwise4_severity_cat = dplyr::case_when(
        is.na(comp_wash_score_water_quantity) ~ NA_character_,
        TRUE ~ as.character(comp_wash_score_water_quantity)
      )
    )

  message("Nouvelles colonnes ajoutées : main_dataset$wash_hwise4_score / wash_hwise4_cat / wash_hwise4_severity_cat (dérivées de hwise4_score / comp_wash_score_water_quantity, déjà calculés par add_hwise() dans indicators_wash.R).")

} else {
  warning("wash_hwise4_score/cat/severity_cat NON calculés : hwise4_score ou comp_wash_score_water_quantity introuvable dans main_dataset. Vérifier que add_hwise() dans indicators_wash.R a bien été exécuté avant ce script.")
}


## ---- 4. HESPER - alias de colonnes sans point pour compatibilité export -----

hesper_sector_alias_map <- c(
  "at_least_one_hesper_item_sector.snfi"       = "hesper_need_sector_abna",
  "at_least_one_hesper_item_sector.education"  = "hesper_need_sector_education",
  "at_least_one_hesper_item_sector.wash"       = "hesper_need_sector_eha",
  "at_least_one_hesper_item_sector.health"     = "hesper_need_sector_sante",
  "at_least_one_hesper_item_sector.fsl"        = "hesper_need_sector_seca",
  "at_least_one_hesper_item_sector.protection" = "hesper_need_sector_protection",
  "at_least_one_hesper_item_sector.aap"        = "hesper_need_sector_aap"
)

for (src_col in names(hesper_sector_alias_map)) {
  new_col <- hesper_sector_alias_map[[src_col]]
  if (src_col %in% names(main_dataset)) {
    main_dataset[[new_col]] <- dplyr::case_when(
      main_dataset[[src_col]] %in% TRUE  ~ "yes",
      main_dataset[[src_col]] %in% FALSE ~ "no",
      TRUE ~ NA_character_
    )
  } else {
    warning(paste0(
      "Alias HESPER non créé pour '", new_col, "' : colonne source '", src_col,
      "' introuvable dans main_dataset. Vérifier que hesper_apply.R a bien ",
      "calculé add_hesper_cat(list_group = sector, ...)."
    ))
  }
}


if ("hesper_in_need" %in% names(main_dataset)) {
  main_dataset <- main_dataset %>%
    dplyr::mutate(
      hesper_in_need_yn = dplyr::case_when(
        hesper_in_need == 1 ~ "yes",
        hesper_in_need == 0 ~ "no",
        TRUE ~ NA_character_
      )
    )
  message("Alias créé : main_dataset$hesper_in_need_yn (copie lisible yes/no de hesper_in_need, déjà calculé par hesper_cameroun.R) pour l'export InDiSign.")
} else {
  warning("hesper_in_need introuvable dans main_dataset : vérifier hesper_apply.R.")
}


