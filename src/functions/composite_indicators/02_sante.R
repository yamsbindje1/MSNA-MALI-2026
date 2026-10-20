# ============================================================================
# SANTÉ


cols <- c("health_ind_age")
health_ind <- convert_columns_to_numeric(health_ind, cols)
health_ind <- add_loop_age_dummy(
  health_ind,
  ind_age = "health_ind_age",
  lb = 5,
  ub = 18,
  new_colname = NULL
)

health_ind <- add_loop_age_gender_dummy(
  health_ind,
  ind_age = "health_ind_age",
  lb = 5,
  ub = 18,
  ind_gender = "health_ind_gender",
  gender = "female",
  new_colname = NULL
)

cols <- c("ind_age")
wgss_indiv <- convert_columns_to_numeric(roster, cols)

wgss_indiv <- add_loop_age_dummy(
  wgss_indiv,
  ind_age = "ind_age",
  lb = 5,
  ub = 18,
  new_colname = NULL
)

## ---- Besoins de soins de santé --------------------------------------------

health_ind <- add_loop_healthcare_needed_cat(
  health_ind,
  ind_healthcare_needed = "health_ind_healthcare_needed",
  ind_healthcare_needed_no = "no",
  ind_healthcare_needed_yes = "yes",
  ind_healthcare_needed_dnk = "dnk",
  ind_healthcare_needed_pnta = "pnta",
  ind_healthcare_received = "health_ind_healthcare_received",
  ind_healthcare_received_no = "no",
  ind_healthcare_received_yes = "yes",
  ind_healthcare_received_dnk = "dnk",
  ind_healthcare_received_pnta = "pnta",
  ind_age = "health_ind_age"
)

main_dataset <- add_loop_healthcare_needed_cat_to_main(
  main_dataset,
  health_ind,
  ind_healthcare_needed_no = "health_ind_healthcare_needed_no",
  ind_healthcare_needed_yes_unmet = "health_ind_healthcare_needed_yes_unmet",
  ind_healthcare_needed_yes_met = "health_ind_healthcare_needed_yes_met",
  id_col_main = "uuid",
  id_col_loop = "uuid"
)

## ---- Handicap (Washington Group Short Set) --------------------------------

wgss_indiv <- add_loop_wgq_ss(
  wgss_indiv,
  ind_age = "ind_age",
  vision = "wgq_vision",
  hearing = "wgq_hearing",
  mobility = "wgq_mobility",
  cognition = "wgq_cognition",
  self_care = "wgq_self_care",
  communication = "wgq_communication",
  no_difficulty = "no_difficulty",
  some_difficulty = "some_difficulty",
  lot_of_difficulty = "a_lot_of_difficulty",
  cannot_do = "cannot_do_at_all",
  undefined = c("dnk", "pnta")
)

main_dataset <- add_loop_wgq_ss_to_main(
  main_dataset,
  wgss_indiv,
  wgq_dis_4 = "wgq_dis_4",
  wgq_dis_3 = "wgq_dis_3",
  wgq_dis_2 = "wgq_dis_2",
  wgq_dis_1 = "wgq_dis_1",
  ind_age_above_5 = "ind_age_above_5",
  id_col_main = "uuid",
  id_col_loop = "uuid"
)

# Catégorie d'âge sur la boucle handicap
wgss_indiv <- add_age_cat(
  wgss_indiv,
  age_col = "ind_age",
  breaks = c(0, 18, 60, 120),
  labels = NULL,
  int_undefined = c(-999, 999),
  char_undefined = "undefined",
  new_colname = "ind_age_cat"
)
