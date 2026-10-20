# ============================================================================
# IYCF / NUTRITION - Minimum Dietary Diversity
# ============================================================================
# mddw_vars <- c(
#   "fs_mddw_cereals", "fs_mddw_roots", "fs_mddw_pulses", "fs_mddw_nuts",
#   "fs_mddw_dairy", "fs_mddw_organ_meat", "fs_mddw_meat_poultry", "fs_mddw_fish",
#   "fs_mddw_eggs", "fs_mddw_leafy_veg", "fs_mddw_roots_vitamina",
#   "fs_mddw_fruits_vitamina", "fs_mddw_other_veg", "fs_mddw_other_fruits"
# )
# 
# if (length(setdiff(mddw_vars, colnames(main_dataset))) == 0) {
# 
#   # ---- Recode yes/no/dnk/pnta -> 1/0/NA -------------------------------------
#   main_dataset <- main_dataset %>%
#     mutate(across(
#       all_of(mddw_vars),
#       ~ dplyr::case_when(. == "yes" ~ 1, . == "no" ~ 0, TRUE ~ NA_real_),
#       .names = "{.col}_num"
#     ))
# 
#   combine_or <- function(...) {
#     mat <- cbind(...)
#     dplyr::case_when(
#       rowSums(mat == 1, na.rm = TRUE) > 0 ~ 1,
#       rowSums(!is.na(mat)) == 0 ~ NA_real_,
#       TRUE ~ 0
#     )
#   }
# 
#   main_dataset <- main_dataset %>%
#     mutate(
#       # 1. Grains, white roots and tubers, and plantains
#       mddw_g1_grains_roots_tubers = combine_or(fs_mddw_cereals_num, fs_mddw_roots_num),
#       # 2. Pulses (beans, peas, lentils)
#       mddw_g2_pulses              = fs_mddw_pulses_num,
#       # 3. Nuts and seeds
#       mddw_g3_nuts_seeds          = fs_mddw_nuts_num,
#       # 4. Dairy
#       mddw_g4_dairy               = fs_mddw_dairy_num,
#       # 5. Meat, poultry and fish (organ meat, other meat/poultry, fish/seafood)
#       mddw_g5_meat_poultry_fish   = combine_or(fs_mddw_organ_meat_num, fs_mddw_meat_poultry_num, fs_mddw_fish_num),
#       # 6. Eggs
#       mddw_g6_eggs                = fs_mddw_eggs_num,
#       # 7. Dark green leafy vegetables
#       mddw_g7_leafy_veg           = fs_mddw_leafy_veg_num,
#       # 8. Other vitamin A-rich fruits and vegetables (roots/tubers + fruits)
#       mddw_g8_vitamina_fruits_veg = combine_or(fs_mddw_roots_vitamina_num, fs_mddw_fruits_vitamina_num),
#       # 9. Other vegetables
#       mddw_g9_other_veg           = fs_mddw_other_veg_num,
#       # 10. Other fruits
#       mddw_g10_other_fruits       = fs_mddw_other_fruits_num
#     )
# 
#   mddw_group_vars <- c(
#     "mddw_g1_grains_roots_tubers", "mddw_g2_pulses", "mddw_g3_nuts_seeds",
#     "mddw_g4_dairy", "mddw_g5_meat_poultry_fish", "mddw_g6_eggs",
#     "mddw_g7_leafy_veg", "mddw_g8_vitamina_fruits_veg", "mddw_g9_other_veg",
#     "mddw_g10_other_fruits"
#   )
# 
#   main_dataset <- main_dataset %>%
#     mutate(
#       # Number of valid (non-NA) food groups - used to decide if the score is usable
#       mddw_n_groups_answered = rowSums(!is.na(across(all_of(mddw_group_vars)))),
#       # MDD-W score: number of food groups (out of 10) consumed the previous day
#       mddw_score = ifelse(
#         mddw_n_groups_answered == 0,
#         NA_real_,
#         rowSums(across(all_of(mddw_group_vars)), na.rm = TRUE)
#       ),
#       # MDD achieved: consumed >= 5 of the 10 food groups (FAO/FANTA, 2016)
#       mddw_cat = dplyr::case_when(
#         is.na(mddw_score) ~ NA_character_,
#         mddw_score >= 5 ~ "Diversité minimale atteinte",
#         TRUE ~ "Diversité minimale non atteinte"
#       )
#     ) %>%
#     select(-all_of(paste0(mddw_vars, "_num")))
# 
# } else {
#   warning(
#     "MDD-W indicator not computed: the following fs_mddw_* variables are missing from main_dataset: ",
#     paste(setdiff(mddw_vars, colnames(main_dataset)), collapse = ", ")
#   )
# }
