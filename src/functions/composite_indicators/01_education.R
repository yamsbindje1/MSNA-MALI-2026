# ============================================================================
# EDUCATION
# Nécessite : 00_transversal.R exécuté avant (fonction add_age_18_cat()).
# ============================================================================

cols <- c("edu_ind_age")
edu_ind <- convert_columns_to_numeric(edu_ind, cols)
edu_ind <- add_loop_age_dummy(
  edu_ind,
  ind_age = "edu_ind_age",
  lb = 5,
  ub = 18,
  new_colname = NULL
)

cols <- c("edu_ind_age_schooling")
edu_ind <- convert_columns_to_numeric(edu_ind, cols)
# School age: age corrected to the start of the school year, used by every step below
# (access, barriers, disruption and edu_schooling_age_n)
cols <- c("edu_ind_age")
edu_ind <- convert_columns_to_numeric(edu_ind, cols)
edu_ind <- add_loop_edu_ind_age_corrected_d_2(
  edu_ind,
  main_dataset,
  id_col_loop = "uuid",
  id_col_main = "uuid",
  survey_start_date = "today",
  school_year_start_month = 9,
  ind_age = "edu_ind_age",
  month = NULL,
  schooling_start_age = 5,
  schooling_end_age = 17
)

edu_ind <- humind::add_loop_edu_access_d(
  edu_ind,
  ind_access = "edu_access",
  yes = "yes",
  no = "no",
  pnta = "pnta",
  dnk = "dnk",
  ind_schooling_age_d = "edu_ind_age_schooling"
)

main_dataset <- humind::add_loop_edu_access_d_to_main(
  main_dataset,
  edu_ind,
  ind_access_d = "edu_ind_access_d",
  ind_no_access_d = "edu_ind_no_access_d",
  id_col_main = "uuid",
  id_col_loop = "uuid"
)


add_loop_edu_barrier_protection_d_mali <- function(
    loop,
    barriers = "edu_barrier",
    protection_issues = c(
      "protection_at_school",
      "protection_travel_school",
      "child_work_home",
      "child_work_outside",
      "child_armed_group",
      "child_marriage",
      "child_pregnancy",
      "ban",
      "enroll_lack_documentation",
      "discrimination"
    ),
    ind_schooling_age_d = "edu_ind_age_schooling",
    barriers_undefined = c("dnk", "pnta", "other"),
    non_protection_issues = c(
      "costs",
      "lack_interest",
      "not_priority",
      "lack_accessible_school",
      "lack_classrooms",
      "lack_wash_facilities",
      "school_closed",
      "lack_teacher",
      "curriculum_not_useful",
      "child_health",
      "language",
      "enroll_displacement"
    )
) {
  #----- Checks
  
  # Check if the variable is in the data frame
  if_not_in_stop(loop, barriers, "loop")
  if_not_in_stop(loop, ind_schooling_age_d, "loop")
  
  # Check that the configurable barrier sets are mutually disjoint
  checkmate::assert_disjunct(
    protection_issues,
    non_protection_issues,
    .var.name = "protection_issues"
  )
  checkmate::assert_disjunct(
    protection_issues,
    barriers_undefined,
    .var.name = "protection_issues"
  )
  checkmate::assert_disjunct(
    non_protection_issues,
    barriers_undefined,
    .var.name = "non_protection_issues"
  )
  
  # Check that edu_barrier values are known barrier response codes
  # are_values_in_set(
  #   loop,
  #   barriers,
  #   c(protection_issues, non_protection_issues, barriers_undefined),
  #   main_message = glue::glue(
  #     "Column '{barriers}' values must be in the following set: "
  #   )
  # )
  
  # Check that ind_schooling_age 0:1
  are_values_in_range(loop, ind_schooling_age_d, 0, 1)
  
  # Warn if edu_ind_barrier_cat exists in loop and will be replaced
  if ("edu_ind_barrier_child_prot_d" %in% colnames(loop)) {
    rlang::warn(
      "edu_ind_barrier_child_prot_d already exists in loop. It will be replaced."
    )
  }
  
  #------ Recode
  
  loop <- dplyr::mutate(
    loop,
    # Level 4-5 categories --- dummy for protection and unable_severe
    "edu_ind_barrier_protection_d" := dplyr::case_when(
      !!rlang::sym(ind_schooling_age_d) == 0 ~ NA_real_,
      !!rlang::sym(barriers) %in% protection_issues ~ 1,
      .default = 0
    )
  )
  
  loop
}


edu_ind <- add_loop_edu_barrier_protection_d_mali(
  edu_ind,
    barriers = "edu_barrier",
    protection_issues = c(
      "protection_at_school",
      "protection_travel_school",
      "child_work_home",
      "child_work_outside",
      "child_armed_group",
      "child_marriage",
      "child_pregnancy",
      "ban",
      "enroll_lack_documentation",
      "discrimination"
    ),
    ind_schooling_age_d = "edu_ind_age_schooling",
    barriers_undefined = c("dnk", "pnta", "other"),
    non_protection_issues = c(
      "costs",
      "lack_interest",
      "not_priority",
      "lack_accessible_school",
      "lack_classrooms",
      "lack_wash_facilities",
      "school_closed",
      "lack_teacher",
      "curriculum_not_useful",
      "child_health",
      "language",
      "enroll_displacement"
    )
)


main_dataset <- add_loop_edu_barrier_protection_d_to_main(
  main_dataset,
  edu_ind,
  ind_barrier_protection_d = "edu_ind_barrier_protection_d",
  id_col_main = "uuid",
  id_col_loop = "uuid"
)

# Disruption de la scolarité (attaque / aléas / déplacement / enseignant), individu puis ménage
edu_ind <- add_loop_edu_disrupted_d(
  edu_ind,
  attack = "edu_disrupted_attack",
  hazards = "edu_disrupted_hazards",
  displaced = "edu_disrupted_displaced",
  teacher = "edu_disrupted_teacher",
  levels = c("yes", "no", "dnk", "pnta"),
  ind_schooling_age_d = "edu_ind_age_schooling"
)

main_dataset <- add_loop_edu_disrupted_d_to_main(
  main_dataset,
  edu_ind,
  attack_d = "edu_disrupted_attack_d",
  hazards_d = "edu_disrupted_hazards_d",
  displaced_d = "edu_disrupted_displaced_d",
  teacher_d = "edu_disrupted_teacher_d",
  id_col_main = "uuid",
  id_col_loop = "uuid"
)


main_dataset <- add_loop_edu_ind_schooling_age_d_to_main(
  main_dataset,
  edu_ind,
  ind_schooling_age_d = "edu_ind_age_schooling",
  id_col_main = "uuid",
  id_col_loop = "uuid"
)

# Catégorie d'âge (>18 / <18) sur la boucle éducation
edu_ind <- add_age_18_cat(
  edu_ind,
  age_col = "edu_ind_age",
  int_undefined = c(-999, 999),
  char_undefined = "undefined",
  new_colname = "ind_age_18_cat"
)

