# ============================================================================
# HESPER - applied to the Cameroun 2026 data
# ============================================================================


## ---- 1. Find the HESPER items in the Cameroun Kobo --------------------------

hesper_survey <- tool.survey %>%
  dplyr::mutate(.row = dplyr::row_number()) %>%
  tidyr::separate(type, into = c("q_type", "q_list_name"), sep = " ", remove = FALSE, extra = "merge", fill = "right")

# HESPER items = select_one questions starting with "hesper_", excluding the
# priority questions (handled separately) and "hesper_top_three_priorities"
# (created by add_hesper_main(), doesn't exist yet in the raw kobo at this point).
hesper_items_survey <- hesper_survey %>%
  dplyr::filter(
    q_type == "select_one",
    stringr::str_detect(name, "^hesper_"),
    !stringr::str_detect(name, "^hesper_priority_"),
    name != "hesper_top_three_priorities",
    name %in% names(main_dataset)
  )

hesper_col_items <- hesper_items_survey %>% dplyr::pull(name) %>% unique()

# Priority questions (1st/2nd/3rd HESPER priority)
hesper_cols_priority <- hesper_survey %>%
  dplyr::filter(stringr::str_detect(name, "^hesper_priority_"), name %in% names(main_dataset)) %>%
  dplyr::pull(name) %>% unique()

if (length(hesper_col_items) == 0) {
  warning(
    "hesper_apply.R: no 'hesper_*' column found in both tool.survey and ",
    "main_dataset. Skipping the HESPER module - check that the Cameroun Kobo ",
    "has a HESPER section and that its column names start with 'hesper_'."
  )
} else {

  ## ---- 2. Work out the choice codes (serious/no_serious/dnk/pnta/na) -----
  # Based on the list_name of the first HESPER item found (all HESPER items
  # normally share the same choice list in a standard Kobo).

  hesper_list_name <- hesper_items_survey$q_list_name[1]

  hesper_choices <- tool.choices %>%
    dplyr::filter(list_name == hesper_list_name) %>%
    dplyr::pull(name) %>% unique()

  pick_choice <- function(candidates, pattern_yes, pattern_exclude = NULL) {
    hit <- candidates[stringr::str_detect(candidates, stringr::regex(pattern_yes, ignore_case = TRUE))]
    if (!is.null(pattern_exclude)) hit <- hit[!stringr::str_detect(hit, stringr::regex(pattern_exclude, ignore_case = TRUE))]
    hit
  }

  # Matches against the Kobo choice CODE, not the label. Cameroun uses short
  # codes (problem/no_problem/dnk/pnta/na) instead of the descriptive ones
  # these patterns were written for, so "^problem$"/"^na$" are added as
  # fallbacks (the descriptive patterns stay for other country tools).
  hesper_choice_no_serious <- pick_choice(hesper_choices, "no.?serious|not.?serious|no.?problem")
  hesper_choice_serious    <- pick_choice(hesper_choices, "serious|^problem$", pattern_exclude = "no.?serious|not.?serious|no.?problem")
  hesper_choice_dnk        <- pick_choice(hesper_choices, "^dnk$|do.?not.?know|dont.?know|ne sait pas")
  hesper_choice_pnta       <- pick_choice(hesper_choices, "^pnta$|decline|prefer.?not|ne (se )?prononce")
  hesper_choice_na         <- pick_choice(hesper_choices, "not.?applicable|non.?applicable|^na$")

  hesper_choice_map <- list(
    serious_problem    = hesper_choice_serious,
    no_serious_problem = hesper_choice_no_serious,
    dnk                = hesper_choice_dnk,
    pnta               = hesper_choice_pnta,
    not_applicable     = hesper_choice_na
  )
  missing_map <- names(hesper_choice_map)[lengths(hesper_choice_map) != 1]

  if (length(missing_map) > 0) {
    warning(
      "hesper_apply.R: could not automatically determine the choice code(s) ",
      "for the following in list '", hesper_list_name, "': ",
      paste(missing_map, collapse = ", "),
      ". Values available in the Kobo for this list: ",
      paste(hesper_choices, collapse = ", "),
      ". Fill them in by hand in hesper_choice_map above before re-running ",
      "(otherwise the HESPER indicators will be computed with empty/wrong codes)."
    )
  } else {

    ## ---- 3. Subgroup config (gender / displacement) -----------------------
    # Fill in if the Cameroun Kobo has gender/displacement-specific items.
    # Left empty - every item is treated the same for everyone.
    hesper_item_male          <- character(0)  # e.g. c("hesper_health_care_men")
    hesper_item_female        <- character(0)  # e.g. c("hesper_clean_women", "hesper_health_care_women")
    hesper_item_displaced     <- character(0)  # e.g. c("hesper_displaced")
    hesper_item_non_displaced <- character(0)

    # Respondent gender: column already used elsewhere in the pipeline
    # (src/functions/indicators_household.R uses resp_gender / "female").
    hesper_col_gender    <- "resp_gender"
    hesper_choice_male   <- "male"
    hesper_choice_female <- "female"

    # Household displacement status: column confirmed (hoh_dis, used for
    # weighting). The exact codes ("idp"/"refugee"/...) are derived
    # automatically from tool.choices, and it falls back to no subgroup
    # applied (no crash) if it can't.
    hesper_col_displacement <- if ("Pol" %in% names(main_dataset)) "Pol" else NA_character_

    hesper_choices_displaced <- character(0)
    hesper_choices_host <- character(0)
    if (!is.na(hesper_col_displacement)) {
      hoh_dis_list_name <- hesper_survey %>%
        dplyr::filter(name == hesper_col_displacement) %>%
        dplyr::pull(q_list_name) %>% dplyr::first()
      if (!is.na(hoh_dis_list_name) && length(hoh_dis_list_name) > 0) {
        hoh_dis_choices <- tool.choices %>%
          dplyr::filter(list_name == hoh_dis_list_name) %>%
          dplyr::pull(name) %>% unique()
        hesper_choices_displaced <- hoh_dis_choices[stringr::str_detect(hoh_dis_choices, stringr::regex("idp|refug|deplac|displac", ignore_case = TRUE))]
        hesper_choices_host      <- hoh_dis_choices[stringr::str_detect(hoh_dis_choices, stringr::regex("^host$|hote|retourn|return", ignore_case = TRUE))]
      }
    }
    if (length(hesper_choices_displaced) == 0 || length(hesper_choices_host) == 0) {
      # No reliable derivation -> no displacement subgroup applied (avoids
      # silently crashing or using wrong codes).
      hesper_col_displacement <- NA_character_
    }

    ## ---- 4. Apply add_hesper_main() ---------------------------------------

    main_dataset <- main_dataset %>%
      add_hesper_main(
        col_items                  = hesper_col_items,
        choice_serious              = hesper_choice_serious,
        choice_no_serious           = hesper_choice_no_serious,
        choice_dnk                  = hesper_choice_dnk,
        choice_pnta                 = hesper_choice_pnta,
        choice_na                   = hesper_choice_na,
        cols_priority                = if (length(hesper_cols_priority) == 3) hesper_cols_priority else NULL,
        col_name_hesper_top_three   = "hesper_top_three_priorities",
        col_gender                  = hesper_col_gender,
        choice_male                 = hesper_choice_male,
        choice_female                = hesper_choice_female,
        hesper_item_male            = if (length(hesper_item_male) > 0) hesper_item_male else NULL,
        hesper_item_female          = if (length(hesper_item_female) > 0) hesper_item_female else NULL,
        col_displacement            = if (!is.na(hesper_col_displacement)) hesper_col_displacement else "resp_gender",
        choices_displaced           = if (length(hesper_choices_displaced) > 0) hesper_choices_displaced else "__none__",
        choices_non_displaced        = if (length(hesper_choices_host) > 0) hesper_choices_host else "__none__",
        col_hesper_subset            = c(hesper_item_male, hesper_item_female, hesper_item_displaced, hesper_item_non_displaced),
        hesper_item_displaced       = if (length(hesper_item_displaced) > 0) hesper_item_displaced else NULL,
        hesper_item_non_displaced   = if (length(hesper_item_non_displaced) > 0) hesper_item_non_displaced else NULL,
        add_binaries                 = TRUE,
        add_binaries_subset          = TRUE,
        subset                       = TRUE
      )

    ## ---- 5. "In need" HESPER flag (MSNI-style) -----------------------------
    # A household is "in need" as soon as at least one applicable item is
    # reported as a serious problem.
    main_dataset <- main_dataset %>%
      dplyr::mutate(
        hesper_in_need = dplyr::case_when(
          prop_hesper.all > 0  ~ 1,
          prop_hesper.all == 0 ~ 0,
          .default = NA_real_
        )
      )

    ## ---- 6. Per-category indicators (section_wide/section/area_wellbeing/sector)

    main_dataset <- main_dataset %>%
      add_hesper_cat(
        list_group = section_wide,
        choice_serious = hesper_choice_serious, choice_no_serious = hesper_choice_no_serious,
        choice_dnk = hesper_choice_dnk, choice_pnta = hesper_choice_pnta, choice_na = hesper_choice_na
      ) %>%
      add_hesper_cat(
        list_group = section,
        choice_serious = hesper_choice_serious, choice_no_serious = hesper_choice_no_serious,
        choice_dnk = hesper_choice_dnk, choice_pnta = hesper_choice_pnta, choice_na = hesper_choice_na
      ) %>%
      add_hesper_cat(
        list_group = area_wellbeing,
        choice_serious = hesper_choice_serious, choice_no_serious = hesper_choice_no_serious,
        choice_dnk = hesper_choice_dnk, choice_pnta = hesper_choice_pnta, choice_na = hesper_choice_na
      ) %>%
      add_hesper_cat(
        list_group = sector,
        choice_serious = hesper_choice_serious, choice_no_serious = hesper_choice_no_serious,
        choice_dnk = hesper_choice_dnk, choice_pnta = hesper_choice_pnta, choice_na = hesper_choice_na
      )

    ## ---- 7. Same categories, applied to the 3 HESPER priorities -----------

    hesper_priority_ready <- length(hesper_cols_priority) == 3 &&
      paste0(hesper_priority_col, ".", hesper_col_items[1]) %in% names(main_dataset)

    if (hesper_priority_ready) {
      main_dataset <- main_dataset %>%
        add_hesper_cat(list_group = section_wide_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA_real_) %>%
        add_hesper_cat(list_group = section_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA_real_) %>%
        add_hesper_cat(list_group = area_wellbeing_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA_real_) %>%
        add_hesper_cat(list_group = sector_priority, choice_serious = 1, choice_no_serious = 0, choice_na = NA_real_)
    } else {

    }


  }
}

# Indicateurs :
#   hesper_need_sector_abna        % avec >=1 pb serieux HESPER secteur ABNA/Shelter
#   hesper_need_sector_education   % avec >=1 pb serieux HESPER secteur Education
#   hesper_need_sector_eha         % avec >=1 pb serieux HESPER secteur EHA/WASH
#   hesper_need_sector_sante       % avec >=1 pb serieux HESPER secteur Sante
#   hesper_need_sector_seca        % avec >=1 pb serieux HESPER secteur SECAL
#   hesper_need_sector_protection  % avec >=1 pb serieux HESPER secteur Protection
#   hesper_need_sector_aap         % avec >=1 pb serieux HESPER secteur AAP
#   hesper_in_need_yn              % avec >=1 pb serieux HESPER, tous secteurs confondus
#

# ============================================================================
# HESPER - "need by sector" indicators (official names)
# Uses at_least_one_hesper_item_sector.* and hesper_in_need created above by
# add_hesper_cat() / add_hesper_main() (hesper_functions.R).
# ============================================================================
#
# Indicators (% of households with >= 1 serious HESPER problem):
#   hesper_need_sector_abna / _education / _eha / _sante / _seca /
#   _protection / _aap (by sector), hesper_in_need_yn (all sectors).
# Values 0/1/NA (not "yes"/"no"), as the rest of the HESPER module.
# ============================================================================

# HESPER sector (hesper_categories.R) -> official indicator name


hesper_sector_to_indicator <- c(
  snfi       = "hesper_need_sector_abna",
  education  = "hesper_need_sector_education",
  wash       = "hesper_need_sector_eha",
  health     = "hesper_need_sector_sante",
  fsl        = "hesper_need_sector_seca",
  protection = "hesper_need_sector_protection",
  aap        = "hesper_need_sector_aap"
)

for (hesper_sector in names(hesper_sector_to_indicator)) {
  
  source_col <- paste0("at_least_one_hesper_item_sector.", hesper_sector)
  target_col <- hesper_sector_to_indicator[[hesper_sector]]
  
  if (source_col %in% names(main_dataset)) {
    main_dataset[[target_col]] <- main_dataset[[source_col]]
  } else {
    warning(
      "hesper_apply.R: column '", source_col, "' missing from main_dataset ",
      "(sector missing from the HESPER grid of add_hesper_cat(list_group = sector)) - ",
      "'", target_col, "' set to NA."
    )
    main_dataset[[target_col]] <- NA_real_
  }
}

# hesper_in_need_yn: copy of hesper_in_need (section 5 "In need HESPER flag"),
# all sectors.
if ("hesper_in_need" %in% names(main_dataset)) {
  main_dataset$hesper_in_need_yn <- main_dataset$hesper_in_need
} else {
  warning(
    "hesper_apply.R: column 'hesper_in_need' missing from main_dataset - ",
    "'hesper_in_need_yn' set to NA. Check section 5 of this script."
  )
  main_dataset$hesper_in_need_yn <- NA_real_
  main_dataset <- main_dataset %>%dplyr::select(-dplyr::ends_with(".binary"))
}



# openxlsx::write.xlsx(main_dataset,"main_dataset.xlsx")
