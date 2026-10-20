# 10 - Complementary indicators
# Built from the tag dictionary (READY_FOR_R), plus a few rules written by hand at the end.
# 1 = numerator, 0 = denominator only, NA = no answer. An indicator missing in a country is skipped, not an error.

c10_na <- if (exists("values_to_NA")) values_to_NA else c(NA, "NA", "", "SL", -999, 999)

# ---- helpers ---------------------------------------------------------------
c10_chr <- function(x) { x <- as.character(x); x[x %in% c10_na] <- NA_character_; x }
c10_num <- function(x) suppressWarnings(as.numeric(c10_chr(x)))
# 1 if the answer is one of codes, 0 for another answer, NA if no answer
c10_one_of <- function(x, codes) { x <- c10_chr(x); ifelse(is.na(x), NA_real_, as.numeric(x %in% codes)) }
# select_multiple: 1 if at least one option outside 'exclude' is ticked (dummies <var>.<code>)
c10_any_dummy <- function(d, var, exclude) {
  parent <- c10_chr(d[[var]])
  dummies <- grep(paste0("^", gsub(".", "\\.", var, fixed = TRUE), "\\."), names(d), value = TRUE)
  dummies <- dummies[!sub("^[^.]*\\.", "", dummies) %in% exclude]
  if (length(dummies) == 0) stop("10: no dummy column ", var, ".<code>")
  m <- sapply(dummies, function(v) c10_num(d[[v]]) %in% 1)
  if (is.null(dim(m))) m <- matrix(m, ncol = length(dummies))
  ifelse(is.na(parent), NA_real_, as.numeric(rowSums(m) > 0))
}
# select_multiple: 1 if option 'code' is ticked
c10_dummy <- function(d, var, code) {
  v <- paste0(var, ".", code)
  if (!v %in% names(d)) stop("10: column ", v, " not found")
  ifelse(is.na(c10_chr(d[[var]])), NA_real_, as.numeric(c10_num(d[[v]]) %in% 1))
}
# "yes" / "no" / "dnk_pnta" from one or more select_multiple; dnk and pnta stay in the denominator
c10_yes_no <- function(d, vars, no_codes, undefined = c("dnk", "pnta")) {
  yes <- no <- ans <- rep(FALSE, nrow(d))
  for (var in vars) {
    dummies <- grep(paste0("^", gsub(".", "\\.", var, fixed = TRUE), "\\."), names(d), value = TRUE)
    if (length(dummies) == 0) stop("10: no dummy column ", var, ".<code>")
    codes <- sub("^[^.]*\\.", "", dummies)
    ticked <- function(cs) { v <- dummies[codes %in% cs]; if (!length(v)) return(rep(FALSE, nrow(d))); rowSums(sapply(v, function(x) c10_num(d[[x]]) %in% 1)) > 0 }
    yes <- yes | ticked(setdiff(codes, c(no_codes, undefined)))
    no <- no | ticked(no_codes)
    ans <- ans | !is.na(c10_chr(d[[var]]))
  }
  ifelse(yes, "yes", ifelse(no, "no", ifelse(ans, "dnk_pnta", NA_character_)))
}
# copy of a select_multiple, kept only where keep is TRUE
c10_add_sm <- function(df, name, var, keep) {
  dummies <- grep(paste0("^", gsub(".", "\\.", var, fixed = TRUE), "\\."), names(df), value = TRUE)
  if (length(dummies) == 0 || !var %in% names(df)) { c10_skip(name, paste0("no column ", var)); return(df) }
  keep <- keep %in% TRUE
  if (!name %in% names(df)) df[[name]] <- ifelse(keep, c10_chr(df[[var]]), NA_character_)
  for (v in dummies) { nm <- paste0(name, ".", sub("^[^.]*\\.", "", v)); if (!nm %in% names(df)) df[[nm]] <- ifelse(keep, df[[v]], NA) }
  df
}
# k-th answer of a ranked question ("none" when fewer answers)
c10_rank_pos <- function(x, k) {
  x <- c10_chr(x)
  r <- vapply(strsplit(ifelse(is.na(x), "", x), " +"), function(z) { z <- z[nzchar(z)]; if (length(z) >= k) z[k] else "none" }, "")
  ifelse(is.na(x), NA_character_, r)
}
# household value from the loop: 1 if anyone is 1, 0 if someone answered, NA otherwise
c10_hh_from_loop <- function(ind) {
  dt <- data.table::data.table(uuid = as.character(loop$uuid), x = ind)[!is.na(x)]
  agg <- dt[, .(x = as.numeric(any(x == 1))), by = uuid]
  agg$x[match(as.character(main_dataset$uuid), agg$uuid)]
}

# ---- sub-groups for the disaggregations (column of the dataset, codes kept) ----
c10_subgroups <- list(
  female     = list(col = "ind_gender",          values = "female"),
  male       = list(col = "ind_gender",          values = "male"),
  hoh_female = list(col = "hoh_gender",          values = "female"),
  hoh_male   = list(col = "hoh_gender",          values = "male"),
  qtl_0_20   = list(col = "cm_income_total_qtl", values = "0%-20%"),
  qtl_60_100 = list(col = "cm_income_total_qtl", values = c("60%-80%", "80%-100%")),
  wgq_yes    = list(col = "wgq_dis_3",           values = "1"),
  age_0_5    = list(col = "ind_age_wgq_cat",     values = "0_to_5_y"),
  age_6_17   = list(col = "ind_age_wgq_cat",     values = "6_to_17_y"),
  age_18_59  = list(col = "ind_age_wgq_cat",     values = "18_to_59_y"),
  age_60_plus = list(col = "ind_age_wgq_cat",    values = "60_more"),
  resp_female = list(col = "resp_gender",        values = "female"),
  resp_male   = list(col = "resp_gender",        values = "male"),
  rural         = list(col = "setting",          values = "rural"),
  urban         = list(col = "setting",          values = "urban"),
  camp_formal   = list(col = "setting",          values = "camp_formal"),
  camp_informal = list(col = "setting",          values = "camp_informal"),
  # household health-care need (health_need_hh_cat, built in part 3)
  health_unmet  = list(col = "health_need_hh_cat", values = "unmet"),
  health_met    = list(col = "health_need_hh_cat", values = "met"),
  health_yes    = list(col = "health_need_hh_cat", values = c("unmet", "met")),
  health_no     = list(col = "health_need_hh_cat", values = "no")
)

# adds column 'name' (value x) and one column <name>_<sub-group> per sub-group in 'disagg'
c10_skipped <- character(0)
c10_skip <- function(name, why) { c10_skipped <<- c(c10_skipped, paste0(name, " (", why, ")")); invisible(NULL) }
c10_add <- function(df, name, x, disagg = character(0)) {
  if (!name %in% names(df)) {
    val <- tryCatch(x, error = function(e) e)
    if (inherits(val, "error")) { c10_skip(name, conditionMessage(val)); return(df) }
    if (length(val) != nrow(df)) { c10_skip(name, "source variable not in this dataset"); return(df) }
    if (all(is.na(val))) { c10_skip(name, "no answer in this dataset"); return(df) }
    df[[name]] <- val
  }
  for (g in disagg) {
    r <- c10_subgroups[[g]]
    if (is.null(r)) stop("10: unknown sub-group ", g)
    if (!r$col %in% names(df)) { c10_skip(paste0(name, "_", g), paste0("column ", r$col, " not in this dataset")); next }
    nm <- paste0(name, "_", g)
    if (!nm %in% names(df)) df[[nm]] <- ifelse(as.character(df[[r$col]]) %in% r$values, df[[name]], NA)
  }
  df
}

# ---- 1. auxiliary variables ------------------------------------------------------
lp <- loop
# Disability (WG-SS) - age classes of the health fiche: 0-5, 6-17, 18-59, 60+
lp <- c10_add(lp, "ind_age_wgq_cat", { a <- c10_num(lp$ind_age)
  dplyr::case_when(is.na(a) ~ NA_character_, a <= 5 ~ "0_to_5_y", a <= 17 ~ "6_to_17_y", a <= 59 ~ "18_to_59_y", TRUE ~ "60_more") })

# ---- 2. indicators built from the dictionary ------------------------------------------
source("src/functions/indicators_from_dictionary.R")
stopifnot(exists("dictionary_file"))
c10_res <- c10_from_dictionary(main_dataset, lp, dictionary_file, tool.survey, tool.choices)
md <- c10_res$md; lp <- c10_res$lp
if (exists("output_root")) {
  dir.create(file.path(output_root, "QA"), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(c10_res$log, file.path(output_root, "QA", paste0("10_indicateurs_dictionnaire_", Sys.Date(), ".csv")), row.names = FALSE)
}
message("10: ", sum(c10_res$log$statut == "created"), " indicators built from the dictionary, ",
        sum(c10_res$log$statut != "created"), " skipped (see Output/<country>/QA/10_indicateurs_dictionnaire_*.csv)")

# ---- 3. rules written by hand ----
# Shelter - at least one shelter issue (households with a shelter only)
md <- c10_add(md, "snfi_shelter_issue_n_1_ou_plus", { x <- c10_num(md$snfi_shelter_issue_n); t <- c10_chr(md$snfi_shelter_type)
  ifelse(is.na(x) | is.na(t) | t == "none", NA_real_, as.numeric(x >= 1)) })
# Livelihoods - expenses higher than income (last 30 days), also by sex of the HoH
md <- c10_add(md, "cm_expenditure_above_income", { e <- c10_num(md$cm_expenditure_tot); i <- c10_num(md$cm_income_total); ifelse(is.na(e) | is.na(i), NA_real_, as.numeric(e > i)) },
              c("hoh_female", "hoh_male"))
# Health - in need / in severe need by sex of the HoH and income quintile
md <- c10_add(md, "comp_health_in_need", md$comp_health_in_need, c("hoh_female", "qtl_0_20", "qtl_60_100"))
md <- c10_add(md, "comp_health_in_severe_need", md$comp_health_in_severe_need, c("hoh_female", "qtl_0_20", "qtl_60_100"))
# Health - walking time to the nearest health facility (minutes)
md <- c10_add(md, "health_facility_time_cat", { x <- c10_num(md$health_facility_time)
  dplyr::case_when(is.na(x) ~ NA_character_, x < 30 ~ "under_30_min", x <= 60 ~ "30min_1hr", x <= 120 ~ "1hr_2hr", TRUE ~ "more_than_2hr") })
# Food - 1st, 2nd and 3rd source ("none" when fewer sources)
md <- c10_add(md, "fsl_source_food_rank1", c10_rank_pos(md$fsl_source_food_ranked, 1))
md <- c10_add(md, "fsl_source_food_rank2", c10_rank_pos(md$fsl_source_food_ranked, 2))
md <- c10_add(md, "fsl_source_food_rank3", c10_rank_pos(md$fsl_source_food_ranked, 3))
# Mali: three separate questions; a source already given at a higher rank becomes "none"
if (!"fsl_source_food_rank1" %in% names(md) && all(paste0("fsl_source_food_", 1:3) %in% names(md))) {
  s <- sapply(1:3, function(k) c10_chr(md[[paste0("fsl_source_food_", k)]]))
  for (k in 1:3) {
    r <- s[, k]
    if (k > 1) r[!is.na(r) & sapply(seq_len(nrow(s)), function(i) r[i] %in% s[i, seq_len(k - 1)])] <- "none"
    r[is.na(r) & !is.na(s[, 1])] <- "none"
    md <- c10_add(md, paste0("fsl_source_food_rank", k), r)
  }
  rm(s, r)
}
# Food - same, grouped (market, own production, aid, gift, other)
c10_food_grp <- function(x) dplyr::case_when(is.na(x) ~ NA_character_, x %in% c("purchase_cash", "purchase_credit") ~ "market", x == "own_production" ~ "own_production",
  x %in% c("assistance_in_kind", "assistance_cva") ~ "aid", x == "gift" ~ "donations", x == "none" ~ "none", TRUE ~ "other")
md <- c10_add(md, "fsl_source_food_rank1_grp", c10_food_grp(md$fsl_source_food_rank1))
md <- c10_add(md, "fsl_source_food_rank2_grp", c10_food_grp(md$fsl_source_food_rank2))
md <- c10_add(md, "fsl_source_food_rank3_grp", c10_food_grp(md$fsl_source_food_rank3))
# AAP - priority needs, by preferred modality
for (m in setdiff(unique(c10_chr(md$cpa_preferred_modality_1)), NA))
  md <- c10_add_sm(md, paste0("cpa_priority_support_ngo_1_mod_", m), "cpa_priority_support_ngo_1", c10_chr(md$cpa_preferred_modality_1) %in% m)
# Child protection - children 5-17 in hazardous work (0 for the other children asked)
lp <- c10_add(lp, "prot_child_labour_any", { h <- c10_any_dummy(lp, "prot_child_labour", c("none", "dnk", "pnta")); w <- c10_chr(lp$prot_child_work)
  ifelse(!c10_num(lp$ind_age_5_17) %in% 1, NA_real_, ifelse(w %in% "no", 0, ifelse(w %in% "yes", h, NA_real_))) })

# WASH - HWISE-4 category (4 or more = water insecure), only if the country pipeline did not create it
if (!"wash_hwise4_cat" %in% names(md) && "hwise4_score" %in% names(md))
  md <- c10_add(md, "wash_hwise4_cat", { s <- c10_num(md$hwise4_score); ifelse(is.na(s), NA_character_, ifelse(s >= 4, "water_insecure", "water_secure")) })

# ---- variables without tag ----
# Livelihoods - lowest income quintile (0-20 %)
# Health - health-care need of the household, from the members counts added by humind:
# at least one unmet need -> "unmet", else at least one met need -> "met", else at least one "no" -> "no"
md <- c10_add(md, "health_need_hh_cat", {
  u <- c10_num(md$health_ind_healthcare_needed_yes_unmet_n); m <- c10_num(md$health_ind_healthcare_needed_yes_met_n)
  n <- c10_num(md$health_ind_healthcare_needed_no_n)
  dplyr::case_when(u >= 1 ~ "unmet", m >= 1 ~ "met", n >= 1 ~ "no", TRUE ~ NA_character_) })
md <- c10_add(md, "cm_income_qtl_0_20", c10_one_of(md$cm_income_total_qtl, "0%-20%"),
              c("health_unmet", "health_met", "health_yes", "health_no"))
# Livelihoods - income quintile of the household, by health-care need and by sex of the HoH (quintile kept inside the sub-group)
for (g in c("health_unmet", "health_met", "health_yes", "health_no", "hoh_female", "hoh_male"))
  md <- c10_add(md, paste0("cm_income_total_qtl_", g), {
    r <- c10_subgroups[[g]]
    ifelse(as.character(md[[r$col]]) %in% r$values, c10_chr(md$cm_income_total_qtl), NA_character_) })
# Child protection - at least one child 5-17 who works
md <- c10_add(md, "prot_hh_child_work_5_17", { x <- c10_hh_from_loop(ifelse(c10_num(loop$ind_age_5_17) %in% 1, c10_one_of(loop$prot_child_work, "yes"), NA_real_)); ifelse(is.na(x), NA_character_, ifelse(x == 1, "yes", "no")) })
# Education - school level of the children attending (Mali grade codes mapped too)
lp <- c10_add(lp, "edu_level_grade_cat", {
  x <- c10_chr(lp$edu_level_grade)
  dplyr::case_when(is.na(x) ~ NA_character_, x %in% c("maternelle", "level1_grade0") ~ "maternelle", grepl("^premier_|^level1_grade[1-9]", x) ~ "primaire",
                   grepl("^deuxieme_cycle_|^level2_", x) ~ "college", grepl("^lycee_|^level3_", x) ~ "lycee", TRUE ~ "autre")
})

main_dataset <- md
loop <- lp
rm(md, lp, c10_food_grp, c10_res)
message("10: complementary indicators added to main_dataset and loop")
c10_skipped <- c10_skipped[!sub(" \\(.*", "", c10_skipped) %in% c(names(main_dataset), names(loop))]   # created by a country rule after a first attempt
if (length(c10_skipped)) message("10: ", length(c10_skipped), " indicator(s) not created for this country: ", paste(c10_skipped, collapse = "; "))
