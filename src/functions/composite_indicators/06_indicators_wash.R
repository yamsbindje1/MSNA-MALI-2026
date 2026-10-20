# ============================================================================
# WASH INDICATORS
# ============================================================================
# Expects main_dataset to already be loaded (02_load_data.R).
# ============================================================================
# 
# main_dataset <- main_dataset %>% mutate(
#   health_facility_time_cat = dplyr::case_when(
#     wash_drinking_water_time_int < 30 ~ "under_30_min",
#     wash_drinking_water_time_int >= 30 & wash_drinking_water_time_int <= 60 ~ "30min_1hr",
#     wash_drinking_water_time_int > 60 & wash_drinking_water_time_int <= 120 ~ "1hr_2hr",
#     wash_drinking_water_time_int > 120 ~ "more_than_2hr",
#     TRUE ~ NA_character_
#   )
# )

main_dataset <- humind::add_drinking_water_source_cat(
  main_dataset,
    drinking_water_source = "wash_drinking_water_source",
    drinking_water_source_cat_improved = c(
      "piped_dwelling",
      "piped_compound",
      "piped_neighbour",
      "tap",
      "borehole",
      "protected_well",
      "well_spring",
      "rainwater_collection",
      "tank_truck",
      "cart_tank",
      "kiosk",
      "bottled_water",
      "sachet_water"
    ),
    drinking_water_source_cat_unimproved = c(
      "unprotected_well",
      "unprotected_spring"
    ),
    drinking_water_source_cat_surface_water = "surface_water",
    drinking_water_source_cat_undefined = c("dnk", "pnta", "other")
)


main_dataset <- humind::add_drinking_water_time_cat(
  main_dataset,
    drinking_water_time_yn = "wash_drinking_water_time_yn",
    water_on_premises = c("water_in_dwelling", "water_in_plot"),
    number_minutes = "number_minutes",
    dnk = "dnk",
    undefined = "pnta",
    drinking_water_time_int = "wash_drinking_water_time_int",
    max_minutes = 600,
    drinking_water_time_sl = "wash_drinking_water_time_sl",
    sl_under_30_min = c("5min_or_less", "5min_15min", "15min_30min"),
    sl_30min_1hr = "30min_1hr",
    sl_more_than_1hr = "more_than_1hr",
    sl_undefined = c("dnk", "pnta"),
    drinking_water_source = "wash_drinking_water_source",
    skipped_drinking_water_source_premises = c("piped_dwelling"),
    skipped_drinking_water_source_undefined = c("dnk", "pnta", "other")
)



main_dataset <- humind::add_drinking_water_time_threshold_cat(
  main_dataset,
  drinking_water_time_30min_cat = "wash_drinking_water_time_cat",
  drinking_water_time_30min_cat_premises = "premises",
  drinking_water_time_30min_cat_under_30min = c("under_30_min"),
  drinking_water_time_30min_cat_above_30min = c("30min_1hr", "more_than_1hr"),
  drinking_water_time_30min_cat_undefined = "undefined"
)



main_dataset <- humind::add_handwashing_facility_cat(
  main_dataset,
    survey_modality = "survey_modality",
    survey_modality_in_person = c("in_person"),
    survey_modality_remote = c("remote"),
    facility = "wash_handwashing_facility",
    facility_yes = c(
      "available_fixed_in_dwelling",
      "available_fixed_in_plot",
      "available_mobile"
    ),
    facility_no = "none",
    facility_no_permission = "no_permission",
    facility_undefined = "other",
    facility_observed_water = "wash_handwashing_facility_observed_water_yn",
    facility_observed_water_yes = "water_available",
    facility_observed_water_no = c("water_not_available"),
    facility_observed_soap = "wash_soap_observed_yn",
    facility_observed_soap_yes = "soap_available",
    facility_observed_soap_no = "soap_not_available",
    facility_observed_soap_undefined = c("other", "pnta"),
    facility_reported = "wash_handwashing_facility_reported",
    facility_reported_yes = c("fixed_dwelling", "fixed_yard", "mobile"),
    facility_reported_no = c("none"),
    facility_reported_undefined = c("other", "dnk", "pnta"),
    facility_reported_water = "wash_handwashing_facility_water_reported_yn",
    facility_reported_water_yes = "yes",
    facility_reported_water_no = c("no"),
    facility_reported_water_undefined = c("dnk", "pnta"),
    facility_reported_soap = "wash_soap_reported_yn",
    facility_reported_soap_yes = "yes",
    facility_reported_soap_no = c("no"),
    facility_reported_soap_undefined = c("dnk", "pnta"),
    soap_type_observed = "wash_soap_observed_type",
    soap_type_observed_yes = c("soap", "detergent"),
    soap_type_observed_no = c("ash_mud_sand"),
    soap_type_observed_undefined = c("dnk", "pnta", "other"),
    soap_type_reported = "wash_soap_reported_type",
    soap_type_reported_yes = c("soap", "detergent"),
    soap_type_reported_no = c("ash_mud_sand"),
    soap_type_reported_undefined = c("dnk", "pnta", "other")
)

main_dataset <- humind::add_sanitation_facility_cat(
  main_dataset,
  sanitation_facility = "wash_sanitation_facility",
  improved = c(
    "flush_piped_sewer",
    "flush_septic_tank",
    "flush_pit_latrine",
    # "flush_pit_other_location",
    "flush_dnk_where",
    "pit_latrine_slab",
    "composting_toilet",
    "twin_pit_latrine_slab",
    "compost"
  ),
  unimproved = c(
    "flush_open_drain",
    "pit_latrine_wo_slab",
    "plastic_bag",
    "bucket",
    "hanging_toilet",
    "twin_pit_latrine_wo_slab",
    "other_container",
    "flush_pit_other_location"
  ),
  none = "none",

  undefined =c("dnk", "pnta","other") 
)

main_dataset <- humind::add_sharing_sanitation_facility_cat(
  main_dataset,
  sharing_sanitation_facility = "wash_sanitation_facility_sharing_yn",
  yes = "yes",
  no = "no",
  undefined = c("dnk", "pnta"),
  sanitation_facility = "wash_sanitation_facility",
  skipped_sanitation_facility = NULL
)

main_dataset <- humind::add_sharing_sanitation_facility_n_ind(
  main_dataset,
  sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
  sharing_sanitation_facility_cat_shared = "shared",
  sharing_sanitation_facility_cat_not_shared = "not_shared",
  sharing_sanitation_facility_cat_not_applicable = "not_applicable",
  sharing_sanitation_facility_cat_undefined = "undefined",
  sanitation_facility_sharing_n = "wash_sanitation_facility_sharing_n",
  hh_size = "hh_size",

  weight = "weights"
)

main_dataset <- humind::add_sanitation_facility_jmp_cat(
  main_dataset,
  sanitation_facility_cat = "wash_sanitation_facility_cat",
  sanitation_facility_cat_improved = "improved",
  sanitation_facility_cat_unimproved = "unimproved",
  sanitation_facility_cat_none = "none",
  sanitation_facility_cat_undefined = "undefined",
  sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
  sharing_sanitation_facility_cat_shared = "shared",
  sharing_sanitation_facility_cat_not_shared = "not_shared",
  sharing_sanitation_facility_cat_not_applicable = "not_applicable",
  sharing_sanitation_facility_cat_undefined = "undefined"
)




main_dataset <- humind::add_drinking_water_quality_jmp_cat(
  main_dataset,
  drinking_water_source_cat = "wash_drinking_water_source_cat",
  drinking_water_source_cat_improved = "improved",
  drinking_water_source_cat_unimproved = "unimproved",
  drinking_water_source_cat_surface_water = "surface_water",
  drinking_water_source_cat_undefined = "undefined",
  drinking_water_time_30min_cat = "wash_drinking_water_time_30min_cat",
  drinking_water_time_30min_cat_premises = "premises",
  drinking_water_time_30min_cat_under_30min = "under_30min",
  drinking_water_time_30min_cat_above_30min = "above_30min",
  drinking_water_time_30min_cat_undefined = "undefined"
)


main_dataset <- humind::add_drinking_water_time_threshold_cat(
  main_dataset,
  drinking_water_time_30min_cat = "wash_drinking_water_time_cat",
  drinking_water_time_30min_cat_premises = "premises",
  drinking_water_time_30min_cat_under_30min = c("under_30_min"),
  drinking_water_time_30min_cat_above_30min = c("30min_1hr", "more_than_1hr"),
  drinking_water_time_30min_cat_undefined = "undefined"
)




# main_dataset <-phrindicators::add_hwise(
#   main_dataset,
#   wash_hwise_worry_col = "wash_hwise_worry",
#   wash_hwise_plans_col = "wash_hwise_plans",
#   wash_hwise_hands_col = "wash_hwise_hands",
#   wash_hwise_drink_col = "wash_hwise_drink",
#   wash_hwise_interrupt_col = NULL,
#   wash_hwise_clothes_col = NULL,
#   wash_hwise_food_col = NULL,
#   wash_hwise_body_col = NULL,
#   wash_hwise_angry_col = NULL,
#   wash_hwise_sleep_col = NULL,
#   wash_hwise_none_col = NULL,
#   wash_hwise_shame_col = NULL,
#   never_val = "never",
#   rarely_val = "rarely",
#   sometimes_val = "sometimes",
#   often_val = "often",
#   always_val = "always"
# )


main_dataset <- humind::add_hwise(
  main_dataset,
  wash_hwise_drink = "wash_hwise_drink",
  wash_hwise_hands = "wash_hwise_hands",
  wash_hwise_plans = "wash_hwise_plans",
  wash_hwise_worry = "wash_hwise_worry",
  hwise_never = "never",
  hwise_rarely = "rarely",
  hwise_sometimes = "sometimes",
  hwise_often = "often",
  hwise_always = "always",
  hwise_dnk = "dnk",
  hwise_pnta = "pnta",
  .keep_recoded = FALSE
)
# 
