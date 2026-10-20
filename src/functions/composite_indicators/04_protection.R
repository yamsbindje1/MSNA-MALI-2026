# ============================================================================
# PROTECTION

# cols <- c("prot_ind_age")
# prot_ind <- convert_columns_to_numeric(prot_ind, cols)
# 
# prot_ind <- add_loop_age_dummy(
#   prot_ind,
#   ind_age = "prot_ind_age",
#   lb = 5,
#   ub = 18,
#   new_colname = NULL
# )
# 
# prot_ind <- add_age_cat(
#   prot_ind,
#   age_col = "prot_ind_age",
#   breaks = c(0, 18, 60, 120),
#   labels = NULL,
#   int_undefined = c(-999, 999),
#   char_undefined = "undefined",
#   new_colname = "ind_age_cat"
# )
main_dataset <- main_dataset %>%
  dplyr::mutate(
    hlp_risk_eviction = dplyr::if_else(
      hlp_risk_eviction == "No",
      "no",
      hlp_risk_eviction
    )
  )
## ---- HLP : sécurité d'occupation -------------------------------------------
# main_dataset<-main_dataset %>% rename
# Type d'occupation +
main_dataset <- add_occupancy_cat(
  main_dataset,
  occupancy = "hlp_occupancy",
  occupancy_high_risk = c("no_agreement"),
  occupancy_medium_risk = c("rented", "hosted_free"),
  occupancy_low_risk = c("ownership"),
  occupancy_undefined = c("dnk", "pnta", "other"),
  eviction = "hlp_risk_eviction",
  eviction_high_risk = "yes",
  eviction_low_risk = "no",
  eviction_undefined = c("dnk", "pnta")
)

# ## ---- Séparation des enfants ------------------------------------------------
# 
# add_child_sep_cat_modifier <- function(
#     df,
#     child_sep = "prot_child_sep",
#     child_sep_yes = "yes",
#     child_sep_no = "no",
#     child_sep_undefined = c("pnta", "dnk"),
#     child_sep_reason = "prot_child_sep_reason",
#     child_sep_reason_non_severe = c("study"),
#     child_sep_reason_severe = c("employment", "marriage"),
#     child_sep_reason_very_severe = c("armed_groups", "kidnapping", "missing", "detained", "stayed_behind", "separated"),
#     child_sep_reason_undefined = c("other", "dnk", "pnta"),
#     sep = "/") {
# 
#   child_sep_reason_d_severe <- paste0(child_sep_reason, sep, child_sep_reason_severe)
#   child_sep_reason_d_very_severe <- paste0(child_sep_reason, sep, child_sep_reason_very_severe)
#   child_sep_reason_d_undefined <- paste0(child_sep_reason, sep, child_sep_reason_undefined)
#   child_sep_reason_d_non_severe <- paste0(child_sep_reason, sep, child_sep_reason_non_severe)
# 
#   if (length(child_sep_yes) != 1 | length(child_sep_no) != 1) {
#     rlang::abort("yes and no must be of length 1.")
#   }
# 
#   df <- dplyr::mutate(
#     df,
#     prot_child_sep_cat = dplyr::case_when(
#       !!rlang::sym(child_sep) == child_sep_no ~ "none",
#       !!rlang::sym(child_sep) %in% child_sep_undefined ~ "undefined",
#       !!rlang::sym(child_sep) == child_sep_yes & dplyr::if_any(dplyr::all_of(child_sep_reason_d_undefined), \(x) x == 1) ~ "undefined",
#       !!rlang::sym(child_sep) == child_sep_yes & dplyr::if_any(dplyr::all_of(child_sep_reason_d_very_severe), \(x) x == 1) ~ "at_least_one_very_severe",
#       !!rlang::sym(child_sep) == child_sep_yes & dplyr::if_any(dplyr::all_of(child_sep_reason_d_severe), \(x) x == 1) ~ "at_least_one_severe",
#       !!rlang::sym(child_sep) == child_sep_yes & dplyr::if_any(dplyr::all_of(child_sep_reason_d_non_severe), \(x) x == 1) ~ "at_least_one_non_severe",
#       .default = NA_character_
#     )
#   )
# 
#   return(df)
# }
# 
# main_dataset <- add_child_sep_cat_modifier(
#   main_dataset,
#   child_sep = "prot_child_sep",
#   child_sep_yes = "yes",
#   child_sep_no = "no",
#   child_sep_undefined = c("pnta", "dnk"),
#   child_sep_reason = "prot_child_sep_reason",
#   child_sep_reason_non_severe = c("study"),
#   child_sep_reason_severe = c("employment", "marriage"),
#   child_sep_reason_very_severe = c("armed_groups", "kidnapping", "missing", "detained", "stayed_behind", "separated"),
#   child_sep_reason_undefined = c("other", "dnk", "pnta"),
#   sep = "."
# )

## ---- Scores de protection (mouvement / pratiques / droits) ---------------

# main_dataset <- add_prot_score_movement_V(
#   main_dataset,
#   sep = ".",
#   prot_needs_3_movement = "prot_needs_3_movement",
#   no_changes_feel_unsafe = "no_changes_feel_unsafe",
#   no_safety_concerns = "no_safety_concerns",
#   women_girls_avoid_places = "women_girls_avoid_places",
#   men_avoid_places = "men_avoid_places",
#   boys_avoid_places = "boys_avoid_places",
#   women_girls_avoid_night = "women_girls_avoid_night",
#   men_avoid_night = "men_avoid_night",
#   boys_avoid_night = "boys_avoid_night",
#   girls_boys_avoid_school = "girls_boys_avoid_school",
#   different_routes = "different_routes",
#   avoid_markets = "avoid_markets",
#   avoid_public_offices = "avoid_public_offices",
#   avoid_fields = "avoid_fields",
#   women_girls_boys_avoid_firewood = "women_girls_boys_avoid_firewood",
#   women_girls_boys_avoid_places = "women_girls_boys_avoid_places",
#   other_safety_measures = "other_safety_measures",
#   dnk = "dnk",
#   pnta = "pnta",
#   men_avoid_places_weight = 1,
#   men_avoid_night_weight = 1,
#   .keep_weighted = FALSE
# )



main_dataset <- add_prot_score_movement(
  main_dataset,
    sep = ".",
    prot_needs_3_movement = "prot_needs_3_movement",
    no_changes_feel_unsafe = "no_changes_feel_unsafe",
    no_safety_concerns = "no_safety_concerns",
    women_girls_avoid_places = "women_girls_avoid_places",
    men_avoid_places = "men_avoid_places",
    boys_avoid_places = "boys_avoid_places",
    women_girls_avoid_night = "women_girls_avoid_night",
    men_avoid_night = "men_avoid_night",
    boys_avoid_night = "boys_avoid_night",
    girls_boys_avoid_school = "girls_boys_avoid_school",
    different_routes = "different_routes",
    avoid_markets = "avoid_markets",
    avoid_public_offices = "avoid_public_offices",
    avoid_fields = "avoid_fields",
    women_girls_boys_avoid_firewood = "women_girls_boys_avoid_firewood",
    women_girls_boys_avoid_places = "women_girls_boys_avoid_places",
    other_safety_measures = "other_safety_measures",
    dnk = "dnk",
    pnta = "pnta",
    men_avoid_places_weight = 1,
    men_avoid_night_weight = 1,
    .keep_weighted = FALSE
)

main_dataset <- add_prot_score_practices(
  main_dataset,
  sep = ".",
  prot_needs_2_activities = "prot_needs_2_activities",
  yes_work = "yes_work",
  yes_livelihood = "yes_livelihood",
  yes_safety = "yes_safety",
  yes_farm = "yes_farm",
  yes_water = "yes_water",
  yes_other_activities = "yes_other_activities",
  yes_free_choices = "yes_free_choices",
  prot_needs_2_social = "prot_needs_2_social",
  yes_visiting_family = "yes_visiting_family",
  yes_visiting_friends = "yes_visiting_friends",
  yes_community_events = "yes_community_events",
  yes_joining_groups = "yes_joining_groups",
  yes_other_social = "yes_other_social",
  yes_child_recreation = "yes_child_recreation",
  yes_decision_making = "yes_decision_making",
  no = "no",
  dnk = "dnk",
  pnta = "pnta",
  .keep_weighted = FALSE
)

main_dataset <- add_prot_score_rights2(
  main_dataset,
  sep = ".",
  prot_needs_1_services = "prot_needs_1_services",
  yes_healthcare = "yes_healthcare",
  yes_schools = "yes_schools",
  yes_therapeutic_services = "yes_therapeutic_services",
  yes_edu_facilities = "yes_edu_facilities",
  yes_social_services = "yes_social_services",
  yes_gov_services = "yes_gov_services",
  yes_other_services = "yes_other_services",
  prot_needs_1_justice = "prot_needs_1_justice",
  yes_identity_documents = "yes_identity_documents",
  yes_counselling_legal = "yes_counselling_legal",
  yes_property_docs = "yes_property_docs",
  yes_birth_certificates = "yes_birth_certificates",
  yes_gov_services_justice = "yes_gov_services",
  yes_other_services_justice = "yes_other_services",
  none = "none",
  no = "no",
  dnk = "dnk",
  pnta = "pnta",
  .keep_weighted = FALSE
)

