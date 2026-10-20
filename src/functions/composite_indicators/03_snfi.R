# ============================================================================
# SNFI (Abris et Articles Non Alimentaires)
# ============================================================================

main_dataset <- humind::add_fds_cannot_cat(
  main_dataset,
  fds_cooking = "snfi_fds_cooking",
  fds_cooking_can = "yes",
  fds_cooking_cannot = "no",
  fds_cooking_no_need = "no_need",
  fds_cooking_undefined = "pnta",
  fds_sleeping = "snfi_fds_sleeping",
  fds_sleeping_can = "yes",
  fds_sleeping_cannot = "no",
  fds_sleeping_undefined = "pnta",
  fds_storing = "snfi_fds_storing",
  fds_storing_cannot = "no",
  fds_storing_can = c("yes"),
  fds_storing_undefined = "pnta",
  lighting_source = "energy_lighting_source",
  lighting_source_none = "none",
  lighting_source_undefined = c("pnta", "dnk")
)

## ---- Abris : dommages / problèmes / type ----------------------------------

main_dataset <- add_shelter_damage_cat(
  main_dataset,
  sep = ".",
  snfi_shelter_damage = "snfi_shelter_damage",
  none = "none",
  minor = "minor_roof",
  major = "major_roof",
  damage_windows_doors = "windows_doors",
  damage_floors = "floors",

  damage_walls = "walls",
  total_collapse = "total_collapse",

  other = c("other"),
  dnk = "dnk",
  pnta = "pnta"
)

main_dataset <- add_shelter_issue_cat(
  main_dataset,
  shelter_issue = "snfi_shelter_issue",
  none = "none",
  issues = c(
    "lack_privacy",
    "lack_space",
    "temperature",
    "ventilation",
    "vectors",
    "no_natural_light",
    "leak",
    "lock",
    "lack_lighting",
    "difficulty_move",
    "lack_space_laundry"
  ),
  undefined = c("dnk", "pnta"),
  other = c("other"),
  sep = "."
)

main_dataset <- add_shelter_type_cat(
  main_dataset,
  shelter_type = "snfi_shelter_type",
  sl_none = "none",
  sl_collective_center = c("collective_center","hosting","hosted"),
  sl_undefined = "pnta",
  shelter_type_individual = "snfi_shelter_type_individual",

  adequate = c("house", "apartment"),
  inadequate = c("makeshift", "tent","unfinished_building"),
  undefined = c("pnta", "other", "dnk")
)
