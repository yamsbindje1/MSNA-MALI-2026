# ============================================================================
# COMPOSITE LEVELS - expected values of derived variables (no Kobo choice list).
# Taken from the calculation rules (humind v2026.4.1 and this project's scripts),
# never from observed values. Variables not listed here are not completed.
# ============================================================================

levels_rows <- function(variable, modality, rule) {
  tidyr::expand_grid(variable = variable, modality = as.character(modality)) %>%
    dplyr::mutate(rule = rule)
}

sectors <- c("edu", "foodsec", "health", "prot", "snfi", "wash")
score_dummies <- c(paste0("comp_", rep(sectors, each = 9), "_", c(1:5, "345", "45", "in_need", "in_severe_need")),
                   paste0("msni_", c(1:5, "345", "45", "in_need", "in_severe_need")))

composite_levels <- dplyr::bind_rows(
  levels_rows(score_dummies, 0:1, "0/1: as.integer(score == k), is_in_need(), is_in_severe_need() (msni script / humind)"),
  levels_rows(c("comp_foodsec_score", "comp_snfi_score", "comp_wash_score", "msni_score"), 1:5, "humind: severity score 1-5 (pmax of sub-scores)"),
  levels_rows(c("comp_edu_score", "comp_prot_score"), 1:4, "humind: pmax of sub-scores, each 1-4"),
  levels_rows("comp_health_score", 1:3, "humind::add_comp_health: case_when 1-3"),
  levels_rows("comp_edu_score_disrupted", 1:4, "humind::add_comp_edu"),
  levels_rows("comp_edu_score_attendance", c(1, 3, 4), "humind::add_comp_edu"),
  levels_rows(c("comp_prot_score_movement", "comp_prot_score_practices", "comp_prot_score_rights"), 1:4, "humind::add_prot_score_*"),
  levels_rows("comp_snfi_score_shelter_type_cat", c(1, 3, 5), "humind::add_comp_snfi"),
  levels_rows(c("comp_snfi_score_shelter_issue_cat", "comp_snfi_score_fds_cannot_cat"), 1:4, "humind::add_comp_snfi"),
  levels_rows("comp_snfi_score_tenure_security_cat", 1:3, "humind::add_comp_snfi"),
  levels_rows("comp_snfi_score_shelter_damage_cat", c(1, 3, 4, 5), "humind::add_comp_snfi"),
  levels_rows("comp_wash_score_water_quality", 1:4, "humind::add_comp_wash"),
  levels_rows(c("comp_wash_score_water_quantity", "comp_wash_score_sanitation"), 1:5, "humind::add_comp_wash / add_hwise"),
  levels_rows("comp_wash_score_hygiene", 1:3, "humind::add_comp_wash"),
  levels_rows("fsl_fcs_cat", c("Poor", "Borderline", "Acceptable"), "humind::add_fcs"),
  levels_rows("fsl_rcsi_cat", c("No to Low", "Medium", "High"), "humind::add_rcsi"),
  levels_rows("fsl_hhs_cat", c("Little to No", "Moderate", "Severe"), "humind::add_hhs"),
  levels_rows("fsl_hhs_cat_ipc", c("None", "Little", "Moderate", "Severe", "Very Severe"), "humind::add_hhs"),
  levels_rows(c("fsl_lcsi_cat", "fsl_lcsi_cat_exhaust"), c("None", "Stress", "Crisis", "Emergency"), "humind::add_lcsi"),
  levels_rows("fsl_fc_phase", paste("Phase", 1:5, "FC"), "humind::add_fcm_phase"),
  levels_rows("fclcm_phase", paste("Phase", 1:5, "FCLC"), "humind::add_fclcm_phase"),
  levels_rows("health_ind_healthcare_needed_cat", c("no_need", "yes_met_need", "yes_unmet_need"), "humind::add_loop_healthcare_needed_cat"),
  levels_rows(c("health_ind_healthcare_needed_yes_met", "health_ind_healthcare_needed_yes_unmet", "edu_ind_access_d", "edu_ind_no_access_d"), 0:1, "humind: 0/1 dummy"),
  levels_rows("hlp_occupancy_cat", c("low_risk", "medium_risk", "high_risk", "undefined"), "humind::add_occupancy_cat"),
  levels_rows("hlp_eviction_cat", c("low_risk", "high_risk", "undefined"), "humind::add_occupancy_cat"),
  levels_rows("snfi_fds_cannot_cat", c("none", "1_task", "2_to_3_tasks", "4_tasks"), "humind::add_fds_cannot_cat"),
  levels_rows("snfi_shelter_damage_cat", c("none", "part", "damaged", "total", "undefined"), "humind::add_shelter_damage_cat"),
  levels_rows("snfi_shelter_issue_cat", c("none", "1_to_3", "4_to_7", "8_to_11", "other", "undefined"), "humind::add_shelter_issue_cat"),
  levels_rows("snfi_shelter_type_cat", c("adequate", "inadequate", "none", "undefined"), "humind::add_shelter_type_cat"),
  levels_rows("wash_drinking_water_quality_jmp_cat", c("basic", "limited", "unimproved", "surface_water", "undefined"), "humind::add_drinking_water_source_cat"),
  levels_rows("wash_drinking_water_source_cat", c("improved", "unimproved", "surface_water", "undefined"), "humind::add_drinking_water_source_cat"),
  levels_rows("wash_drinking_water_time_30min_cat", c("premises", "under_30min", "above_30min", "undefined"), "humind::add_drinking_water_source_cat"),
  levels_rows("wash_sanitation_facility_cat", c("improved", "unimproved", "none", "undefined"), "humind::add_sanitation_facility_cat"),
  levels_rows("wash_sanitation_facility_jmp_cat", c("basic", "limited", "unimproved", "open_defecation", "undefined"), "humind::add_sanitation_facility_cat"),
  levels_rows("wash_sharing_sanitation_facility_cat", c("not_shared", "shared", "not_applicable", "undefined"), "humind::add_sanitation_facility_cat"),
  levels_rows("wash_sharing_sanitation_facility_n_ind", c("19_and_below", "20_to_49", "50_and_above"), "humind::add_sanitation_facility_cat"),
  levels_rows("wash_handwashing_facility_jmp_cat", c("basic", "limited", "no_facility", "undefined"), "humind::add_handwashing_facility_cat"),
  levels_rows("aap_received_assistance", c("past_30d", "1_3_months", "4_6_months", "7_12_months", "no", "undefined"), "humind::add_received_assistance"),
  levels_rows(paste0("hesper_need_sector_", c("abna", "education", "eha", "sante", "seca", "protection", "aap")), 0:1,
              "hesper_cameroun.R: copy of at_least_one_hesper_item_sector.* (0/1)"),
  levels_rows("hesper_in_need", 0:1, "hesper_cameroun.R: 0/1")
)

# Kobo answers recoded in place by humind: these levels replace the Kobo list
composite_levels <- dplyr::bind_rows(
  composite_levels,
  levels_rows("snfi_fds_cooking", c("yes", "no", "no_no_need", "undefined"), "humind::add_fds_cannot_cat recodes the Kobo answer"),
  levels_rows(c("snfi_fds_sleeping", "snfi_fds_storing"), c("yes", "no", "undefined"), "humind::add_fds_cannot_cat recodes the Kobo answer")
) %>%
  dplyr::mutate(override = variable %in% c("snfi_fds_cooking", "snfi_fds_sleeping", "snfi_fds_storing"))

# HHS: dnk / pnta are set to NA before humind::add_hhs() in the msni script.
modality_exclusions <- levels_rows(
  c("fsl_hhs_nofoodhh", "fsl_hhs_sleephungry", "fsl_hhs_alldaynight"), c("dnk", "pnta"),
  "msni script sets dnk/pnta to NA before the HHS calculation"
)

# Rules defined in this project's own scripts
composite_levels <- dplyr::bind_rows(
  composite_levels,
  levels_rows(c("resp_age_cat_dis", "hoh_age_final_cat_dis"), c("18-59", "60 ans et plus"), "00_transversal.R"),
  levels_rows("hesper_in_need_yn", 0:1, "hesper_cameroun.R: copy of hesper_in_need (0/1)")
)
