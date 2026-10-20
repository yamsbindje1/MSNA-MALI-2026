# GIS map values - national, Admin 1 and Admin 2 values for the maps, from the results of a run.
# Not run by run.R: open the project, set the block below, then source("src/tools/export_gis_map_values.R").
# The fields come from the GIS field dictionary (one row per map field); sources that do not exist in this
# country are left empty and listed at the end.

suppressPackageStartupMessages({ library(dplyr); library(readxl); library(openxlsx) })

## ---- Settings ------------------------------------------------------------------
gis_country      <- if (exists("country_name")) country_name else basename(list.dirs("Output", recursive = FALSE))[1]
gis_country_code <- if (exists("Admin_code")) Admin_code else NA   # e.g. "NE", "ML", "CM" (from run.R when loaded)
# results of the run: the most recent tot_results table of Output/<country>/Tables frequences/<date>/
gis_results_file <- NULL
# GIS field dictionary: a workbook of inputs/ with a sheet named "...Field_Dictionary"
gis_dictionary_file <- NULL
# optional template: same workbook, sheets GIS_admin<1|2>_map_values (fid, pcode, name, parent_pcode of the zones)
gis_template_file <- NULL
# levels to export: 0 = national, 1 = Admin 1, 2 = Admin 2 (Admin 2 must be analysed in the run)
gis_levels <- c(0, 1, 2)
# Kobo tool, for the names of the zones (lists l_admin1, l_admin2)
gis_kobo_file <- if (exists("kobo_tool")) kobo_tool else NULL

# How each indicator_id of the dictionary is read from the results, when the name is not a variable of the
# results. type: levels (score levels summed), select_one (answers summed), sm_option (one option of a
# select_multiple), sm_any (at least one option: 1 - the share of the options given), median.
gis_sources <- list(
  msni_severity          = list(type = "levels", var = "msni_score"),
  food_security_severity = list(type = "levels", var = "comp_foodsec_score"),
  education_severity     = list(type = "levels", var = "comp_edu_score"),
  wash_severity          = list(type = "levels", var = "comp_wash_score"),
  snfi_severity          = list(type = "levels", var = "comp_snfi_score"),
  protection_severity    = list(type = "levels", var = "comp_prot_score"),
  health_severity        = list(type = "levels", var = "comp_health_score"),
  hazards_env                 = list(type = "sm_any", var = "hazard_env_type", not = c("none", "dnk", "pnta")),
  hazards_env_flood           = list(type = "sm_option", var = "hazard_env_type", code = "flood"),
  hazards_env_drought         = list(type = "sm_option", var = "hazard_env_type", code = "drought"),
  hazards_env_wildfire        = list(type = "sm_option", var = "hazard_env_type", code = "wildfire"),
  hazards_env_water_scarcity  = list(type = "sm_option", var = "hazard_env_type", code = "water_scarcity"),
  hazards_env_degradation     = NULL,   # no matching option in the Kobo tool: left empty (to check)
  hazards_sp                  = list(type = "sm_any", var = "hazard_sp_type", not = c("none", "dnk", "pnta")),
  aap_received_assistance_12m = list(type = "select_one", var = "aap_received_assistance_12m", codes = "yes"),
  l_aap_relevance_assistance  = list(type = "select_one", var = "aap_relevance_assistance"),
  l_aap_satisfaction_assistance = list(type = "select_one", var = "aap_satisfaction_assistance"),
  I_cm_income_total           = list(type = "median", var = "cm_income_total"),
  hesper_toilet               = list(type = "select_one", var = "hesper_toilet", codes = "problem")   # serious problem
)

# Categories of the dictionary that stand for several answer codes
gis_aliases <- list(relevant = c("all", "majority"), satisfied = c("better", "as_expected"))   # satisfied: to check

## ---- Files ----------------------------------------------------------------------
out_root <- file.path("Output", gis_country)
if (is.null(gis_results_file)) {
  d <- sort(list.dirs(file.path(out_root, "Tables frequences"), recursive = FALSE), decreasing = TRUE)
  f <- unlist(lapply(d, function(x) list.files(x, pattern = "tot_results.*\\.xlsx$", full.names = TRUE)))
  if (!length(f)) stop("GIS: no tot_results table in ", file.path(out_root, "Tables frequences"))
  gis_results_file <- f[1]
}
if (is.null(gis_dictionary_file)) {
  f <- list.files("inputs", pattern = "\\.xlsx$", full.names = TRUE)
  f <- f[vapply(f, function(x) any(grepl("Field_Dictionary", excel_sheets(x))), logical(1))]
  if (!length(f)) stop("GIS: put the workbook with the GIS field dictionary in inputs/")
  gis_dictionary_file <- f[1]
}
if (is.null(gis_template_file)) {
  f <- unique(c(gis_dictionary_file, list.files("inputs", pattern = "\\.xlsx$", full.names = TRUE)))
  f <- f[vapply(f, function(x) any(grepl("^GIS_admin[0-9]_map_values$", excel_sheets(x))), logical(1))]
  gis_template_file <- if (length(f)) f[1] else gis_dictionary_file
}
message("GIS: results ", gis_results_file, "\nGIS: dictionary ", gis_dictionary_file)

dico_sheet <- grep("Field_Dictionary", excel_sheets(gis_dictionary_file), value = TRUE)[1]
dico <- read_excel(gis_dictionary_file, sheet = dico_sheet) %>%
  filter(is.na(enabled) | as.logical(toupper(as.character(enabled)) %in% c("TRUE", "VRAI", "1")))
# n field: a typo in the dictionary falls back on n_<name>
dico$sample_n_field <- ifelse(grepl("^n_", trimws(dico$sample_n_field)), trimws(dico$sample_n_field), sub("^v_", "n_", dico$field_name))

res <- read_excel(gis_results_file, guess_max = 10000)
res$analysis_var_value <- as.character(res$analysis_var_value)
res$group_var_value <- as.character(res$group_var_value)
for (v in c("stat", "n", "N")) res[[v]] <- suppressWarnings(as.numeric(res[[v]]))
# national zone: the country code when it is known (the results may hold the country name)
nat_value <- unique(res$group_var_value[res$group_var == "country"])[1]
if (!is.na(gis_country_code)) res$group_var_value[res$group_var == "country"] <- gis_country_code

## ---- Zones (pcode, name, parent) -------------------------------------------------
level_var <- c(`0` = "country", `1` = "Admin_1", `2` = "Admin_2")
kobo_names <- NULL
if (!is.null(gis_kobo_file) && file.exists(gis_kobo_file)) {
  ch <- read_excel(gis_kobo_file, sheet = "choices")
  lab <- grep("^label", names(ch), value = TRUE)[1]
  kobo_names <- ch %>% filter(grepl("admin[12]", list_name)) %>% transmute(pcode = as.character(name), name = .data[[lab]]) %>% distinct(pcode, .keep_all = TRUE)
}
template <- function(lv) {
  sh <- paste0("GIS_admin", lv, "_map_values")
  if (!sh %in% excel_sheets(gis_template_file)) return(NULL)
  t <- read_excel(gis_template_file, sheet = sh)
  t <- t[!is.na(t$pcode), intersect(c("fid", "country_code", "pcode", "name", "parent_pcode"), names(t))]
  t$pcode <- as.character(t$pcode); t
}

## ---- Value of one field in every zone ---------------------------------------------------
cats <- function(x) {
  x <- trimws(strsplit(as.character(x), ";")[[1]]); x <- ifelse(x == "4+", "5", x)
  unlist(lapply(x, function(k) if (k %in% names(gis_aliases)) gis_aliases[[k]] else k))
}
field_value <- function(r, R) {
  src <- if (r$indicator_id %in% names(gis_sources)) gis_sources[[r$indicator_id]] else list(type = "select_one", var = r$indicator_id, codes = cats(r$categories))
  if (is.null(src)) return(NULL)
  if (src$type %in% c("levels", "select_one") && is.null(src$codes)) src$codes <- cats(r$categories)
  # a 0/1 indicator analysed as <variable>.binary (e.g. hesper_toilet.binary)
  if (src$type == "select_one" && !src$var %in% R$analysis_var && paste0(src$var, ".binary") %in% R$analysis_var) {
    src$var <- paste0(src$var, ".binary") }
  # a <variable>.binary row holds the share of 1 with an empty answer code, whatever the categories say
  if (src$type == "select_one" && grepl("\\.binary$", src$var)) src$codes <- c("1", "", NA)
  out <- switch(src$type,
    levels = , select_one = R %>% filter(analysis_var == src$var) %>% group_by(group_var_value) %>%
      summarise(v = sum(stat[analysis_var_value %in% src$codes]), nn = max(N, na.rm = TRUE), .groups = "drop"),
    sm_option = R %>% filter(analysis_var == paste0(src$var, ".", src$code)) %>% group_by(group_var_value) %>%
      summarise(v = sum(stat), nn = max(N, na.rm = TRUE), .groups = "drop"),
    sm_any = R %>% filter(startsWith(analysis_var, paste0(src$var, "."))) %>% group_by(group_var_value) %>%
      summarise(v = 1 - sum(stat[sub("^[^.]*\\.", "", analysis_var) %in% src$not]), nn = max(N, na.rm = TRUE), .groups = "drop"),
    median = R %>% filter(analysis_var == src$var, estimator == "median") %>% group_by(group_var_value) %>%
      summarise(v = first(stat), nn = first(n), .groups = "drop"))
  if (!nrow(out)) return(NULL)
  out
}

## ---- One sheet per level ------------------------------------------------------------
wb <- createWorkbook(); missing <- character()
for (lv in gis_levels) {
  R <- res %>% filter(group_var == level_var[[as.character(lv)]])
  if (!nrow(R)) { message("GIS: level ", lv, " not in the results (not analysed in this run): skipped"); next }
  zones <- tibble(pcode = sort(unique(R$group_var_value)))
  tp <- template(lv)
  if (!is.null(tp)) zones <- full_join(tp, zones, by = "pcode")
  # template zones of another country (the model workbook holds Cameroon examples) are dropped
  zones <- zones[zones$pcode %in% R$group_var_value | (!is.na(gis_country_code) & startsWith(zones$pcode, gis_country_code)), ]
  if (!"name" %in% names(zones)) zones$name <- NA_character_
  if (!is.null(kobo_names)) zones$name <- coalesce(as.character(zones$name), kobo_names$name[match(zones$pcode, kobo_names$pcode)])
  if (lv == 0) zones$name <- coalesce(as.character(zones$name), nat_value)
  if (!"parent_pcode" %in% names(zones)) zones$parent_pcode <- NA_character_
  if (lv == 1) zones$parent_pcode <- coalesce(as.character(zones$parent_pcode), unique(res$group_var_value[res$group_var == "country"])[1])
  if (lv == 2) { a1 <- unique(res$group_var_value[res$group_var == "Admin_1"]); zones$parent_pcode <- coalesce(as.character(zones$parent_pcode), vapply(zones$pcode, function(p) { m <- a1[startsWith(p, a1)]; if (length(m)) m[1] else NA_character_ }, "")) }
  sheet <- tibble(fid = if ("fid" %in% names(zones)) zones$fid else NA,
                  country_code = if ("country_code" %in% names(zones)) coalesce(as.character(zones$country_code), gis_country_code) else gis_country_code,
                  admin_level = lv, pcode = zones$pcode, name = zones$name, parent_pcode = zones$parent_pcode,
                  assessed = ifelse(zones$pcode %in% R$group_var_value, "vrai", "faux"), value_field_count = nrow(dico))
  vals <- list(); ns <- list()
  for (i in seq_len(nrow(dico))) {
    r <- dico[i, ]; fv <- field_value(r, R)
    if (is.null(fv)) { missing <- union(missing, r$field_name); fv <- tibble(group_var_value = character(), v = numeric(), nn = numeric()) }
    vals[[r$field_name]] <- fv$v[match(sheet$pcode, fv$group_var_value)]
    ns[[r$sample_n_field]] <- fv$nn[match(sheet$pcode, fv$group_var_value)]
  }
  sheet <- bind_cols(sheet, as_tibble(vals), as_tibble(ns))
  sh <- paste0("GIS_admin", lv, "_map_values")
  addWorksheet(wb, sh); writeData(wb, sh, sheet); freezePane(wb, sh, firstRow = TRUE)
  message("GIS: ", sh, " - ", nrow(sheet), " zones")
}
addWorksheet(wb, dico_sheet); writeData(wb, dico_sheet, dico)

out_dir <- file.path(out_root, "GIS", format(Sys.Date(), "%Y-%m-%d")); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("GIS_map_values_", gis_country, "_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
saveWorkbook(wb, out_file, overwrite = TRUE)
message("GIS: written ", out_file)
if (length(missing)) message("GIS: no source in this country, left empty: ", paste(missing, collapse = ", "))
