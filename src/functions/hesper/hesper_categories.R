# ============================================================================
# HESPER - item -> category mapping (section_wide / section / area_wellbeing / sector)
# ============================================================================

# ============================================================================

section_wide <- list(
  physical = c(
    "hesper_drinking_water", "hesper_food", "hesper_shelter", "hesper_toilet",
    "hesper_clean", "hesper_clean_women", "hesper_clothes_etc",
    "hesper_health", "hesper_health_care_men", "hesper_health_care_women"
  ),
  non_physical = c(
    "hesper_distress", "hesper_safety", "hesper_education", "hesper_care",
    "hesper_support", "hesper_separation", "hesper_displaced",
    "hesper_information", "hesper_aid", "hesper_respect", "hesper_movement",
    "hesper_time", "hesper_law", "hesper_gbv", "hesper_drug",
    "hesper_mental_health", "hesper_care_community"
  )
)

# ---- section (5 categories, 28/28 items) ----------------------------------
section <- list(
  livelihood = c("hesper_income_livelihood"),
  physical   = section_wide$physical,
  protection = c(
    "hesper_safety", "hesper_separation", "hesper_displaced",
    "hesper_movement", "hesper_law", "hesper_gbv", "hesper_drug"
  ),
  well_being = c(
    "hesper_distress", "hesper_education", "hesper_care", "hesper_support",
    "hesper_respect", "hesper_time", "hesper_mental_health",
    "hesper_care_community"
  ),
  other = c("hesper_information", "hesper_aid")
)

# ---- area_wellbeing (7 categories, 28/28 items) ---------------------------
area_wellbeing <- list(
  agency = c("hesper_information", "hesper_time"),
  basic_need = c(section_wide$physical, "hesper_education"),
  livelihood = c("hesper_income_livelihood"),
  relationship_with_others = c(
    "hesper_care", "hesper_support", "hesper_separation", "hesper_respect",
    "hesper_drug", "hesper_mental_health", "hesper_care_community"
  ),
  safety_security = c(
    "hesper_safety", "hesper_displaced", "hesper_movement", "hesper_law",
    "hesper_gbv"
  ),
  well_being = c("hesper_distress"),
  other = c("hesper_aid")
)

# ---- sector (9 categories, 28/28 items) -----------------------------------
sector <- list(
  aap = c("hesper_information", "hesper_aid"),
  education = c("hesper_education"),
  fsl = c("hesper_food"),
  health = c("hesper_health", "hesper_health_care_men", "hesper_health_care_women"),
  livelihood = c("hesper_income_livelihood"),
  protection = c(
    "hesper_distress", "hesper_safety", "hesper_separation",
    "hesper_displaced", "hesper_respect", "hesper_movement", "hesper_law",
    "hesper_gbv", "hesper_drug"
  ),
  snfi = c("hesper_shelter", "hesper_clothes_etc"),
  wash = c("hesper_drinking_water", "hesper_toilet", "hesper_clean", "hesper_clean_women"),
  wellbeing = c(
    "hesper_care", "hesper_support", "hesper_time", "hesper_mental_health",
    "hesper_care_community"
  )
)

# ---- All 4 groupings + readable names for labels --------------------------
hesper_groupings <- list(
  section_wide   = section_wide,
  section        = section,
  area_wellbeing = area_wellbeing,
  sector         = sector
)

# ---- "priority" variants ---------------------------------------------------

hesper_priority_col <- "hesper_top_three_priorities"

to_priority_cols <- function(items) paste0(hesper_priority_col, ".", items)

section_wide_priority   <- lapply(section_wide, to_priority_cols)
section_priority        <- lapply(section, to_priority_cols)
area_wellbeing_priority <- lapply(area_wellbeing, to_priority_cols)
sector_priority         <- lapply(sector, to_priority_cols)

hesper_groupings_priority <- list(
  section_wide_priority   = section_wide_priority,
  section_priority        = section_priority,
  area_wellbeing_priority = area_wellbeing_priority,
  sector_priority         = sector_priority
)

# ---- All 28 HESPER items, flat (for reference) -----------------------------
hesper_all_28_items <- unique(c(section_wide$physical, section_wide$non_physical, "hesper_income_livelihood"))
