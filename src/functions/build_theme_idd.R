# ============================================================================
# BUILD_THEME_IDD - derive theme_IDD automatically from the enriched
# indicator base, instead of maintaining it by hand in the Kobo tool or in a
# hard-coded R vector.
# ============================================================================
# Logic requested:
#   Kobo (survey$name) -> Base_indicateurs_enrichie$question_variable_name
#                       -> Base_indicateurs_enrichie$sector -> theme_IDD
#
# Only Base_indicateurs_enrichie rows with match_status == "Found" are used to
# assign a sector automatically: "Partial" / "Composite/cross-sector" /
# "Missing" rows are NOT reliable enough for an automatic, silent match (cf.
# consigne : "ne fais pas une correspondance approximative sans signaler les
# cas problematiques"). Those indicators are flagged "needs_review" instead of
# being guessed.
#
# An indicator legitimately referenced under more than one sector (e.g. an
# indicator shown on both the Education and the Protection report pages) is
# NOT arbitrarily collapsed to a single sector: it is kept as one row per
# sector in $lookup (long format), so it can correctly appear in every
# relevant per-theme CSV. $display collapses it to a single "A / B" string
# (same convention already used inside Base_indicateurs_enrichie itself) for
# contexts that need exactly one theme_IDD value per indicator
# (indicator_metadata, 06_frequency_tables.R's by-theme workbook).
#
# No sector/theme name is hard-coded anywhere in this function: the set of
# themes is entirely whatever distinct, non-NA values exist in
# Base_indicateurs_enrichie$sector at the time the pipeline runs. Adding a new
# sector to that column is enough to get a new theme_IDD and a new export CSV.
# ============================================================================

# build_theme_idd <- function(kobo_survey,
#                              enriched_indicators_path,
#                              enriched_sheet = "Base_indicateurs_enrichie",
#                              known_indicators = NULL,
#                              output_dir = NULL) {
# 
#   stopifnot(is.data.frame(kobo_survey))
# 
#   if (!file.exists(enriched_indicators_path)) {
#     stop(
#       "build_theme_idd(): enriched indicator file not found at:\n  ",
#       enriched_indicators_path,
#       "\nCheck 'enriched_indicators_path' in run.R."
#     )
#   }
# 
#   ## ---- 1. Kobo indicator universe ------------------------------------------
# 
#   is_group_marker <- grepl(
#     "begin_group|end_group|begin_repeat|end_repeat",
#     kobo_survey$type
#   )
# 
#   kobo_indicators <- kobo_survey %>%
#     dplyr::filter(!is_group_marker, !is.na(name)) %>%
#     dplyr::mutate(name = trimws(name)) %>%
#     dplyr::distinct(name, .keep_all = TRUE) %>%
#     dplyr::select(name, dplyr::any_of(c("theme", "theme_IDD", "module"))) %>%
#     dplyr::rename(dplyr::any_of(c(theme_IDD_kobo_actuel = "theme_IDD")))
# 
#   if (!is.null(known_indicators)) {
#     known_indicators <- trimws(as.character(known_indicators))
#     # Union: every name actually analysed in tot_results_final (before_dot),
#     # plus every name declared in the Kobo survey (covers indicators that
#     # exist in the tool but were, for some reason, never analysed - still
#     # worth reporting on so nothing silently disappears from the diagnostic).
#     names_only_in_known <- setdiff(known_indicators, kobo_indicators$name)
#     kobo_indicators <- kobo_indicators %>%
#       dplyr::filter(name %in% known_indicators) %>%
#       dplyr::bind_rows(tibble::tibble(name = names_only_in_known)) %>%
#       dplyr::distinct(name, .keep_all = TRUE)
#   }
# 
#   ## ---- 2. Base_indicateurs_enrichie -----------------------------------------
# 
#   enriched <- readxl::read_excel(enriched_indicators_path, sheet = enriched_sheet)
# 
#   required_cols <- c("sector", "question_variable_name", "match_status")
#   missing_cols <- setdiff(required_cols, names(enriched))
#   if (length(missing_cols) > 0) {
#     stop(
#       "build_theme_idd(): the sheet '", enriched_sheet, "' in\n  ",
#       enriched_indicators_path,
#       "\nis missing required column(s): ", paste(missing_cols, collapse = ", "),
#       ". Cannot derive theme_IDD from 'sector'."
#     )
#   }
# 
#   enriched <- enriched %>%
#     dplyr::mutate(
#       question_variable_name = trimws(as.character(question_variable_name)),
#       sector = trimws(as.character(sector))
#     )
# 
#   found <- enriched %>%
#     dplyr::filter(match_status == "Found", !is.na(question_variable_name), question_variable_name != "")
# 
#   pairs <- found %>% dplyr::distinct(question_variable_name, sector)
# 
#   conflict_names <- pairs %>%
#     dplyr::count(question_variable_name, name = "n_sectors") %>%
#     dplyr::filter(n_sectors > 1) %>%
#     dplyr::pull(question_variable_name)
# 
#   ## ---- 3. Long lookup (1 row per indicator x matched sector) ----------------
#   # Used by the per-theme export loop: an indicator matched to several
#   # sectors gets one row per sector here, so it is correctly included in
#   # every relevant sectoral CSV (nothing is silently dropped).
# 
#   lookup <- pairs %>%
#     dplyr::rename(indicator = question_variable_name, theme_IDD = sector) %>%
#     dplyr::arrange(indicator, theme_IDD)
# 
#   ## ---- 4. Single-value display version (indicator_metadata, 06_frequency_tables) -
# 
#   display <- lookup %>%
#     dplyr::group_by(indicator) %>%
#     dplyr::summarise(theme_IDD = paste(sort(unique(theme_IDD)), collapse = " / "), .groups = "drop")
# 
#   ## ---- 5. Indicators present in the base but NOT reliably matched ----------
# 
#   non_found_names <- enriched %>%
#     dplyr::filter(match_status != "Found", !is.na(question_variable_name), question_variable_name != "") %>%
#     dplyr::distinct(question_variable_name) %>%
#     dplyr::filter(!question_variable_name %in% lookup$indicator) %>%
#     dplyr::pull(question_variable_name)
# 
#   ## ---- 6. QC control table (one row per Kobo indicator) --------------------
# 
#   qc <- kobo_indicators %>%
#     dplyr::mutate(
#       status = dplyr::case_when(
#         name %in% conflict_names               ~ "multiple_matches",
#         name %in% lookup$indicator              ~ "matched",
#         name %in% non_found_names               ~ "needs_review (Partial/Composite/Missing dans la base)",
#         TRUE                                     ~ "not_found"
#       )
#     ) %>%
#     dplyr::left_join(display, by = c("name" = "indicator")) %>%
#     dplyr::rename(indicator = name) %>%
#     dplyr::relocate(indicator, theme_IDD, status)
# 
#   ## ---- 7. Console diagnostic --------------------------------------------------
# 
#   status_counts <- qc %>% dplyr::count(status, name = "n") %>% dplyr::arrange(dplyr::desc(n))
#   sector_counts <- lookup %>% dplyr::count(theme_IDD, name = "n_indicateurs") %>% dplyr::arrange(dplyr::desc(n_indicateurs))
# 
#   message("=== build_theme_idd() : diagnostic Kobo -> sector -> theme_IDD ===")
#   message(nrow(qc), " indicateur(s) Kobo controle(s).")
#   for (i in seq_len(nrow(status_counts))) {
#     message("  - ", status_counts$status[i], " : ", status_counts$n[i])
#   }
#   message(length(unique(lookup$theme_IDD)), " sector(s)/theme_IDD distinct(s) utilise(s) : ",
#           paste(sort(unique(lookup$theme_IDD)), collapse = ", "))
#   for (i in seq_len(nrow(sector_counts))) {
#     message("  - ", sector_counts$theme_IDD[i], " : ", sector_counts$n_indicateurs[i], " indicateur(s)")
#   }
# 
#   if (length(conflict_names) > 0) {
#     warning(
#       length(conflict_names), " indicateur(s) associe(s) a PLUSIEURS sector distincts dans ",
#       "Base_indicateurs_enrichie (match_status == 'Found') : ",
#       paste(conflict_names, collapse = ", "),
#       ". Ils sont exportes dans CHACUN des sectors concernes (voir $lookup) plutot que ",
#       "d'etre assignes arbitrairement a un seul. Verifier que c'est bien voulu."
#     )
#   }
# 
#   n_needs_review <- sum(qc$status == "needs_review (Partial/Composite/Missing dans la base)")
#   if (n_needs_review > 0) {
#     warning(
#       n_needs_review, " indicateur(s) present(s) dans Base_indicateurs_enrichie mais sans ",
#       "match_status == 'Found' (Partial/Composite/Missing) : aucun theme_IDD ne leur a ete ",
#       "assigne automatiquement. Ils n'apparaitront dans AUCUN export sectoriel tant que la ",
#       "base indicateurs n'aura pas ete corrigee pour ces lignes."
#     )
#   }
# 
#   n_not_found <- sum(qc$status == "not_found")
#   message(
#     n_not_found, " indicateur(s) Kobo n'ont aucune correspondance dans Base_indicateurs_enrichie ",
#     "(non references dans la base indicateurs / hors perimetre des 7 pages sectorielles) : ",
#     "ils n'auront pas de theme_IDD et n'apparaitront dans aucun export sectoriel (mais restent ",
#     "dans All_themes.csv, qui ne depend pas de theme_IDD)."
#   )
# 
#   ## ---- 8. Optional export of the QC table -----------------------------------
# 
#   if (!is.null(output_dir)) {
#     dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
#     qc_path <- file.path(output_dir, paste0("QC_theme_IDD_", Sys.Date(), ".csv"))
#     data.table::fwrite(qc, qc_path, bom = TRUE)
#     message("Table de controle theme_IDD ecrite dans : ", qc_path)
#   }
# 
#   list(
#     lookup = lookup,             # long format: indicator, theme_IDD (1 row per matched sector)
#     display = display,           # 1 row per indicator: theme_IDD as "A / B" if multiple
#     qc = qc,                     # full control table requested (indicator | sector | status | ...)
#     conflict_names = conflict_names,
#     status_counts = status_counts,
#     sector_counts = sector_counts
#   )
# }
