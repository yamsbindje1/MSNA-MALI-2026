# ============================================================================
# TRANSVERSAL - fonctions communes, démographie (roster), AAP, quantiles de

## ---- Fonctions utilitaires (catégories d'âge) -----------------------------

add_age_cat <- function(df, age_col, breaks = c(seq(0, 115, by = 5), 120), labels = NULL, int_undefined = c(-999, 999), char_undefined = "undefined", new_colname = NULL) {
  df <- num_cat(
    df = df,
    num_col = age_col,
    breaks = breaks,
    labels = labels,
    int_undefined = int_undefined,
    char_undefined = char_undefined,
    new_colname = new_colname,
    plus_last = FALSE
  )
  return(df)
}

add_age_18_cat <- function(df, age_col, int_undefined = c(-999, 999), char_undefined = "undefined", new_colname = NULL) {

  if (is.null(new_colname)) new_colname <- paste0(age_col, "_18_cat")
  new_colname_d <- paste0(new_colname, "_d")

  df <- dplyr::mutate(
    df,
    "{new_colname}"  := dplyr::case_when(
      !!rlang::sym(age_col) %in% int_undefined ~ char_undefined,
      !!rlang::sym(age_col) < 18 ~ "Inferieur a 18 ans",
      !!rlang::sym(age_col) >= 18 ~ "Superieur a 18 ans",
      .default = NA_character_
    ),
    "{new_colname_d}"  := dplyr::case_when(
      !!rlang::sym(new_colname) == char_undefined ~ NA_integer_,
      !!rlang::sym(new_colname) == "above_18" ~ 1,
      !!rlang::sym(new_colname) == "below_18" ~ 0,
      .default = NA_integer_
    )
  )

  return(df)
}

## ---- Chef de ménage final (répondant vs chef de ménage déclaré) ----------

main_dataset <- humind::add_hoh_final(
  main_dataset,
  resp_hoh_yn = "resp_hoh_yn",
  yes = "yes",
  no = "no",
  hoh_gender = "hoh_gender",
  hoh_age = "hoh_age",
  resp_gender = "resp_gender",
  resp_age = "resp_age"
)

## ---- Roster : dummies âge/genre pour la boucle démographique -------------

roster <- add_loop_age_dummy(
  roster,
  ind_age = "ind_age",
  lb = 5,
  ub = 18,
  new_colname = NULL
)

roster <- humind::add_loop_age_gender_dummy(
  roster,
  ind_age = "ind_age",
  lb = 5,
  ub = 18,
  ind_gender = "ind_gender",
  gender = "female",
  new_colname = NULL
)

main_dataset <- humind::add_loop_age_gender_dummy_to_main(
  main_dataset,
  roster,
  ind_age_gender_dummy = "ind_age_female_5_18",
  id_col_main = "uuid",
  id_col_loop = "uuid",
  new_colname = NULL
)

main_dataset <- humind::add_loop_age_dummy_to_main(
  main_dataset,
  roster,
  ind_age_dummy = "ind_age_5_18",
  id_col_main = "uuid",
  id_col_loop = "uuid",
  new_colname = NULL
)

## ---- Nutrition (nut_ind) --------------------------------------------------


cols <- c("nut_ind_age")
nut_ind <- convert_columns_to_numeric(nut_ind, cols)

nut_ind <- humind::add_loop_age_dummy(
  nut_ind,
  ind_age = "nut_ind_age",
  lb = 5,
  ub = 18,
  new_colname = NULL
)

nut_ind <- add_age_cat(
  nut_ind,
  age_col = "nut_ind_age",
  int_undefined = c(-999, 999),
  char_undefined = "undefined",
  new_colname = "ind_age_18_cat_d"
)

## ---- AAP (redevabilité envers les populations affectées) -----------------

main_dataset <- add_received_assistance(
  main_dataset,
  received_assistance_12m = "aap_received_assistance_12m",
  yes = "yes",
  no = "no",
  undefined = c("dnk", "pnta"),
  received_assistance_date = "aap_received_assistance_date",
  date_past_30d = "past_30d",
  date_1_3_months = "1_3_months",
  date_4_6_months = "4_6_months",
  date_7_12_months = "7_12_months",
  date_undefined = c("dnk", "pnta")
)

## ---- Catégories d'âge (répondant / chef de ménage / roster) --------------

cols <- c("resp_age", "hoh_age_final")
main_dataset <- convert_columns_to_numeric(main_dataset, cols)
main_dataset <- add_age_cat(main_dataset, age_col = "resp_age", new_colname = "resp_age_cat")
main_dataset <- add_age_cat(main_dataset, age_col = "hoh_age_final", new_colname = "hoh_age_final_cat")

main_dataset <- main_dataset %>% dplyr::mutate(
  resp_age_cat_dis = dplyr::case_when(
    resp_age < 60 ~ "18-59",
    resp_age >= 60 ~ "60 ans et plus",
    .default = NA_character_
  ),
  hoh_age_final_cat_dis = dplyr::case_when(
    hoh_age_final < 60 ~ "18-59",
    hoh_age_final >= 60 ~ "60 ans et plus",
    .default = NA_character_
  )
)

roster <- add_age_cat(
  roster,
  age_col = "ind_age",
  breaks = c(0, 18, 60),
  labels = NULL,
  int_undefined = c(-999, 999),
  char_undefined = "undefined",
  new_colname = "ind_age_cat"
)
# table(roster$ind_age_cat)
## ---- Quantiles de revenu ---------------------------------------------------

vars <- c("cm_income_total")

main_dataset <- add_quantile_interval(
  main_dataset,
  vars,
  cut_offs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  weight = "weights"
)

## ---- Emploi / moyens de subsistance du ménage -----------------------------

# main_dataset <- main_dataset %>%
#   mutate(
#     # Au moins un adulte dans chaque type d'emploi
#     cm_job_permanent_yn = as.integer(
#       !is.na(cm_job_permanent_m_n) & cm_job_permanent_m_n > 0
#     ),
# 
#     cm_job_temp_yn = as.integer(
#       !is.na(cm_job_temp_m_n) & cm_job_temp_m_n > 0
#     ),
# 
#     cm_job_daily_yn = as.integer(
#       !is.na(cm_job_daily_m_n) & cm_job_daily_m_n > 0
#     ),
# 
#     cm_job_own_yn = as.integer(
#       !is.na(cm_job_own_m_n) & cm_job_own_m_n > 0
#     ),
# 
#     cm_job_other_yn = as.integer(
#       !is.na(cm_job_other_m_n) & cm_job_other_m_n > 0
#     ),
# 
#     # Nombre de types d'emploi dans le ménage
#     cm_job_types_n = rowSums(
#       across(
#         c(
#           cm_job_permanent_yn,
#           cm_job_temp_yn,
#           cm_job_daily_yn,
#           cm_job_own_yn,
#           cm_job_other_yn
#         ),
#         ~ replace_na(., 0)
#       )
#     ),
# 
#     # Au moins deux types d'emploi
#     cm_job_types_2plus_yn = as.integer(
#       cm_job_types_n >= 2
#     ),
# 
#     # Au moins un emploi ou une source de revenu majeure perdue
#     cm_income_source_loss_yn = as.integer(
#       !is.na(cm_income_source_loss_n) &
#         cm_income_source_loss_n > 0
#     )
#   )



# sessionInfo()




# wash_handwashing_facility was collected as a select_multiple, while the HUMIND
# function expects a single response. Convert it to select_one using a defined
# priority rule when more than one type of handwashing facility was reported.
# If "none" is selected together with an available facility, prioritize the
# reported facility over "none".
# 
# main_dataset <- main_dataset %>%
#   dplyr::mutate(
#     wash_handwashing_facility = dplyr::case_when(
#       
#       stringr::str_detect(
#         wash_handwashing_facility,
#         "(^|\\s)available_fixed_in_dwelling($|\\s)"
#       ) ~ "available_fixed_in_dwelling",
#       
#       stringr::str_detect(
#         wash_handwashing_facility,
#         "(^|\\s)available_fixed_in_plot($|\\s)"
#       ) ~ "available_fixed_in_plot",
#       
#       stringr::str_detect(
#         wash_handwashing_facility,
#         "(^|\\s)available_mobile($|\\s)"
#       ) ~ "available_mobile",
#       
#       stringr::str_detect(
#         wash_handwashing_facility,
#         "(^|\\s)no_permission($|\\s)"
#       ) ~ "no_permission",
#       
#       stringr::str_detect(
#         wash_handwashing_facility,
#         "(^|\\s)none($|\\s)"
#       ) ~ "none",
#       
#       stringr::str_detect(
#         wash_handwashing_facility,
#         "(^|\\s)other($|\\s)"
#       ) ~ "other",
#       
#       TRUE ~ NA_character_
#     )
#   )
# 
# # l_wash_soap_observed contains additional response options, while the HUMIND
# # function expects only soap_available and soap_not_available.
# # Recode "no" as soap_not_available because the household reported not having
# # soap or detergent for handwashing. Keep soap_available and soap_not_available
# # unchanged, and set dnk and pnta to NA because they do not confirm availability.
# 
# main_dataset2x<-main_dataset
# main_dataset <- main_dataset %>%
#   dplyr::mutate(
#     wash_soap_observed_yn = dplyr::case_when(
#       wash_soap_observed_yn == "soap_available" ~ "soap_available",
#       
#       wash_soap_observed_yn %in% c(
#         "soap_not_available",
#         "no"
#       ) ~ "soap_not_available",
#       
#       wash_soap_observed_yn %in% c(
#         "dnk",
#         "pnta"
#       ) ~ NA_character_,
#       
#       TRUE ~ NA_character_
#     )
#   )
# 
# main_dataset <- main_dataset %>%
#   dplyr::mutate(
#     wash_soap_reported_yn = dplyr::if_else(
#       wash_soap_reported_yn == "soap_not_available",
#       "no",
#       wash_soap_reported_yn
#     )
#   )
# 
# 
# # wash_soap_observed_type was collected as a select_multiple, while the HUMIND
# # function expects a single soap type. When multiple types were observed,
# # responses are recoded using the following priority: soap > detergent >
# # ash_mud_sand > other > dnk > pnta. Soap and detergent are prioritized because
# # they are classified as appropriate handwashing agents in the HUMIND function.
# main_dataset <- main_dataset %>%
#   dplyr::mutate(
#     wash_soap_observed_type = dplyr::case_when(
#       
#       # If soap was observed, prioritize soap
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)soap($|\\s)"
#       ) ~ "soap",
#       
#       # Otherwise, prioritize detergent
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)detergent($|\\s)"
#       ) ~ "detergent",
#       
#       # Ash/mud/sand is retained only when soap or detergent was not observed
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)ash_mud_sand($|\\s)"
#       ) ~ "ash_mud_sand",
#       
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)other($|\\s)"
#       ) ~ "other",
#       
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)dnk($|\\s)"
#       ) ~ "dnk",
#       
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)pnta($|\\s)"
#       ) ~ "pnta",
#       
#       TRUE ~ NA_character_
#     )
#   )
# 
# 
# main_dataset <- main_dataset %>%
#   dplyr::mutate(
#     wash_soap_reported_type = dplyr::case_when(
#       
#       # If soap was observed, prioritize soap
#       stringr::str_detect(
#         wash_soap_reported_type,
#         "(^|\\s)soap($|\\s)"
#       ) ~ "soap",
#       
#       # Otherwise, prioritize detergent
#       stringr::str_detect(
#         wash_soap_reported_type,
#         "(^|\\s)detergent($|\\s)"
#       ) ~ "detergent",
#       
#       # Ash/mud/sand is retained only when soap or detergent was not observed
#       stringr::str_detect(
#         wash_soap_observed_type,
#         "(^|\\s)ash_mud_sand($|\\s)"
#       ) ~ "ash_mud_sand",
#       
#       stringr::str_detect(
#         wash_soap_reported_type,
#         "(^|\\s)other($|\\s)"
#       ) ~ "other",
#       
#       stringr::str_detect(
#         wash_soap_reported_type,
#         "(^|\\s)dnk($|\\s)"
#       ) ~ "dnk",
#       
#       stringr::str_detect(
#         wash_soap_reported_type,
#         "(^|\\s)pnta($|\\s)"
#       ) ~ "pnta",
#       
#       TRUE ~ NA_character_
#     )
#   )






# wash_handwashing_facility was collected as a select_multiple, while the HUMIND
# function expects a single response. Convert it to select_one using a defined
# priority rule when more than one type of handwashing facility was reported.
# If "none" is selected together with an available facility, prioritize the
# reported facility over "none".

main_dataset <- main_dataset %>%
  dplyr::mutate(
    wash_handwashing_facility = dplyr::case_when(
      
      stringr::str_detect(
        wash_handwashing_facility,
        "(^|\\s)available_fixed_in_dwelling($|\\s)"
      ) ~ "available_fixed_in_dwelling",
      
      stringr::str_detect(
        wash_handwashing_facility,
        "(^|\\s)available_fixed_in_plot($|\\s)"
      ) ~ "available_fixed_in_plot",
      
      stringr::str_detect(
        wash_handwashing_facility,
        "(^|\\s)available_mobile($|\\s)"
      ) ~ "available_mobile",
      
      stringr::str_detect(
        wash_handwashing_facility,
        "(^|\\s)no_permission($|\\s)"
      ) ~ "no_permission",
      
      stringr::str_detect(
        wash_handwashing_facility,
        "(^|\\s)none($|\\s)"
      ) ~ "none",
      
      stringr::str_detect(
        wash_handwashing_facility,
        "(^|\\s)other($|\\s)"
      ) ~ "other",
      
      TRUE ~ NA_character_
    )
  )

# l_wash_soap_observed contains additional response options, while the HUMIND
# function expects only soap_available and soap_not_available.
# Recode "no" as soap_not_available because the household reported not having
# soap or detergent for handwashing. Keep soap_available and soap_not_available
# unchanged, and set dnk and pnta to NA because they do not confirm availability.

main_dataset2x<-main_dataset
main_dataset <- main_dataset %>%
  dplyr::mutate(
    wash_soap_observed_yn = dplyr::case_when(
      wash_soap_observed_yn == "soap_available" ~ "soap_available",
      
      wash_soap_observed_yn %in% c(
        "soap_not_available",
        "no"
      ) ~ "soap_not_available",
      
      wash_soap_observed_yn %in% c(
        "dnk",
        "pnta"
      ) ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  )

main_dataset <- main_dataset %>%
  dplyr::mutate(
    wash_soap_reported_yn = dplyr::if_else(
      wash_soap_reported_yn == "soap_not_available",
      "no",
      wash_soap_reported_yn
    )
  )


# wash_soap_observed_type was collected as a select_multiple, while the HUMIND
# function expects a single soap type. When multiple types were observed,
# responses are recoded using the following priority: soap > detergent >
# ash_mud_sand > other > dnk > pnta. Soap and detergent are prioritized because
# they are classified as appropriate handwashing agents in the HUMIND function.
main_dataset <- main_dataset %>%
  dplyr::mutate(
    wash_soap_observed_type = dplyr::case_when(
      
      # If soap was observed, prioritize soap
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)soap($|\\s)"
      ) ~ "soap",
      
      # Otherwise, prioritize detergent
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)detergent($|\\s)"
      ) ~ "detergent",
      
      # Ash/mud/sand is retained only when soap or detergent was not observed
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)ash_mud_sand($|\\s)"
      ) ~ "ash_mud_sand",
      
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)other($|\\s)"
      ) ~ "other",
      
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)dnk($|\\s)"
      ) ~ "dnk",
      
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)pnta($|\\s)"
      ) ~ "pnta",
      
      TRUE ~ NA_character_
    )
  )


main_dataset <- main_dataset %>%
  dplyr::mutate(
    wash_soap_reported_type = dplyr::case_when(
      
      # If soap was observed, prioritize soap
      stringr::str_detect(
        wash_soap_reported_type,
        "(^|\\s)soap($|\\s)"
      ) ~ "soap",
      
      # Otherwise, prioritize detergent
      stringr::str_detect(
        wash_soap_reported_type,
        "(^|\\s)detergent($|\\s)"
      ) ~ "detergent",
      
      # Ash/mud/sand is retained only when soap or detergent was not observed
      stringr::str_detect(
        wash_soap_observed_type,
        "(^|\\s)ash_mud_sand($|\\s)"
      ) ~ "ash_mud_sand",
      
      stringr::str_detect(
        wash_soap_reported_type,
        "(^|\\s)other($|\\s)"
      ) ~ "other",
      
      stringr::str_detect(
        wash_soap_reported_type,
        "(^|\\s)dnk($|\\s)"
      ) ~ "dnk",
      
      stringr::str_detect(
        wash_soap_reported_type,
        "(^|\\s)pnta($|\\s)"
      ) ~ "pnta",
      
      TRUE ~ NA_character_
    )
  )
