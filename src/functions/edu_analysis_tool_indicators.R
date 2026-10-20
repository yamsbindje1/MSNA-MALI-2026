# Education indicators of the EduAnalysisTool (github.com/martivit/EduAnalysisTool), on the loop.
# Children aged 5-17 after the age correction only; called from 05.

# Mali: Kobo grade codes -> names of the ISCED file
edu_grade_to_isced <- tibble::tribble(
  ~kobo_code,               ~name_level_grade,
  "level1_grade0",          "maternelle",
  "level1_grade1",          "premier_cycle_annee1",
  "level1_grade2",          "premier_cycle_annee2",
  "level1_grade3",          "premier_cycle_annee3",
  "level1_grade4",          "premier_cycle_annee4",
  "level1_grade5",          "premier_cycle_annee5",
  "level2_grade_1",         "deuxieme_cycle_annee6",
  "level2_grade_2",         "deuxieme_cycle_annee7",
  "level2_grade_3",         "deuxieme_cycle_annee8",
  "level2_grade_4",         "deuxieme_cycle_annee9",
  "level3_grade_1",         "lycee_annee10",
  "level3_grade_2",         "lycee_annee11",
  "level3_grade_3",         "lycee_annee12"
)

# Education answers of the loop, analysed on the tool population only
edu_loop_questions <- c("edu_access", "edu_level_grade", "edu_disrupted_hazards", "edu_disrupted_teacher",
                        "edu_disrupted_displaced", "edu_disrupted_attack", "edu_barrier", "edu_financial_barrier",
                        "edu_other_yn", "edu_other_type", "edu_ind_barrier_protection_d")

## ---- School structure (same reading as read_ISCED_info) ----------------------

edu_read_isced <- function(path, country_code) {
  isced <- readxl::read_excel(path, sheet = "Compiled_Levels_Grades") %>%
    dplyr::filter(tolower(`country code`) == tolower(country_code)) %>%
    dplyr::mutate(`theoretical start age` = as.integer(`theoretical start age`))
  if (nrow(isced) == 0) stop("edu_analysis_tool_indicators.R: no row for '", country_code, "' in ", path)

  levels <- isced %>%
    dplyr::group_by(level_code = `level code`) %>%
    dplyr::summarise(starting_age = min(`theoretical start age`), duration = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(level_code)
  if (all(c("level0", "level1") %in% levels$level_code)) {
    levels$duration[levels$level_code == "level0"] <-
      levels$starting_age[levels$level_code == "level1"] - levels$starting_age[levels$level_code == "level0"]
  }

  grades <- isced %>%
    dplyr::transmute(name_level_grade = `name -- for kobo`, level_code = `level code`,
                     grade_start_age = `theoretical start age`)
  list(levels = levels, grades = grades)
}

## ---- Indicators ----
# returns list(new, replaced, in_scope)

edu_tool_indicators <- function(loop, main, isced, survey_start_date = "today", school_year_start_month = 9,
                                schooling_start_age = 5, schooling_end_age = 17) {
  lv <- isced$levels
  start_of <- function(l) lv$starting_age[lv$level_code == l]
  last_level <- max(lv$level_code)
  # age range of each level (the last level keeps one extra year, as in the tool)
  lv$end_age_level <- lv$starting_age + lv$duration - ifelse(lv$level_code == last_level, 0, 1)
  lv$end_age_cycle <- lv$starting_age + lv$duration - 1
  primary_start <- start_of("level1")
  higher_levels <- setdiff(lv$level_code, "level0")

  # age corrected to the start of the school year
  month_of <- main %>%
    dplyr::transmute(uuid, edu_month = as.integer(format(as.Date(.data[[survey_start_date]]), "%m")))
  start_adj <- ifelse(school_year_start_month > 6, school_year_start_month - 12, school_year_start_month)

  yes_no <- function(x) dplyr::case_when(x == "yes" ~ 1, x == "no" ~ 0, .default = NA_real_)

  d <- loop %>%
    dplyr::select(uuid, edu_ind_age, edu_ind_gender, edu_access, edu_level_grade, edu_other_yn,
                  edu_disrupted_teacher, edu_disrupted_hazards, edu_disrupted_displaced, edu_disrupted_attack) %>%
    dplyr::left_join(month_of, by = "uuid") %>%
    dplyr::mutate(
      age = suppressWarnings(as.numeric(edu_ind_age)),
      age = dplyr::if_else(!is.na(edu_month) & !is.na(age) & (edu_month - start_adj) > 6, age - 1, age),
      age = dplyr::if_else(age < schooling_start_age | age > schooling_end_age, NA_real_, age),
      in_scope = !is.na(age),
      access = yes_no(edu_access),
      gender = dplyr::case_when(edu_ind_gender == "female" ~ "female", edu_ind_gender == "male" ~ "male", .default = NA_character_),
      name_level_grade = edu_grade_to_isced$name_level_grade[match(as.character(edu_level_grade), edu_grade_to_isced$kobo_code)]
    ) %>%
    dplyr::left_join(isced$grades, by = "name_level_grade") %>%
    dplyr::mutate(limit_age = grade_start_age + 2)
  stopifnot(nrow(d) == nrow(loop))

  unmatched <- setdiff(unique(stats::na.omit(d$edu_level_grade[d$in_scope])), c(edu_grade_to_isced$kobo_code, "other", "dnk", "pnta"))
  if (length(unmatched) > 0) warning("edu_analysis_tool_indicators.R: grade code(s) without ISCED match: ", paste(unmatched, collapse = ", "))

  # age-assigned school cycle (add_edu_school_cycle)
  d$cycle <- NA_character_
  for (i in seq_len(nrow(lv))) {
    d$cycle[d$age >= lv$starting_age[i] & d$age <= lv$end_age_cycle[i] & is.na(d$cycle)] <- lv$level_code[i]
  }

  rank_of <- stats::setNames(seq_along(higher_levels), higher_levels)
  out <- tibble::tibble(.rows = nrow(d))

  # early childhood attendance: children one year younger than the primary entry age
  minus_one <- d$age == primary_start - 1
  out$edu_attending_level0_level1_and_level1_minus_one_age_d <- dplyr::case_when(
    !minus_one ~ NA_real_,
    is.na(d$level_code) ~ 0,
    d$level_code %in% c("level0", "level1") ~ 1
  )
  out$edu_attending_level1_and_level1_minus_one_age_d <- dplyr::case_when(
    !minus_one ~ NA_real_,
    is.na(d$level_code) ~ 0,
    d$level_code == "level0" ~ 0,
    d$level_code == "level1" ~ 1
  )
  # preschool only (attending level0, not yet in primary): with the primary one above it adds up to the total
  out$edu_attending_level0_and_level1_minus_one_age_d <- dplyr::case_when(
    !minus_one ~ NA_real_,
    is.na(d$level_code) ~ 0,
    d$level_code == "level0" ~ 1,
    d$level_code == "level1" ~ 0
  )

  # adjusted net attendance (official age, same level or higher)
  for (l in higher_levels) {
    i <- which(lv$level_code == l)
    age_ok <- d$age >= lv$starting_age[i] & d$age <= lv$end_age_level[i]
    above <- higher_levels[higher_levels >= l]
    col <- paste0("edu_attending_level", paste(sub("level", "", above), collapse = ""), "_and_", l, "_age_d")
    out[[col]] <- dplyr::case_when(
      !age_ok ~ NA_real_,
      is.na(d$level_code) ~ 0,
      d$level_code == "level0" ~ 0,
      rank_of[d$level_code] >= rank_of[[l]] ~ 1,
      rank_of[d$level_code] < rank_of[[l]] ~ 0
    )
  }

  # overage learners: attending a level at least 2 years above the intended age of the grade
  for (l in intersect(c("level1", "level2", "level3"), higher_levels)) {
    out[[paste0("edu_", l, "_overage_learners_d")]] <- ifelse(d$level_code == l, as.numeric(d$age - d$limit_age >= 0), NA_real_)
  }

  # attendance by age-assigned cycle (edu_ind_access_d grouped by edu_school_cycle_d in the tool)
  cycle_names <- c(level0 = "prescolaire", level1 = "primaire", level2 = "premiercycle", level3 = "deuxiemecycle")
  for (l in intersect(names(cycle_names), lv$level_code)) {
    out[[paste0("edu_ind_access_", cycle_names[[l]], "_d")]] <- ifelse(d$cycle %in% l, d$access, NA_real_)
  }

  # access and non-formal education (add_loop_edu_access_d, add_loop_edu_optional_nonformal_d)
  replaced <- tibble::tibble(
    edu_ind_access_d = d$access,
    edu_ind_no_access_d = 1 - d$access,
    edu_disrupted_teacher_d = yes_no(d$edu_disrupted_teacher),
    edu_disrupted_hazards_d = yes_no(d$edu_disrupted_hazards),
    edu_disrupted_displaced_d = yes_no(d$edu_disrupted_displaced),
    edu_disrupted_attack_d = yes_no(d$edu_disrupted_attack)
  )
  out$edu_other_yn_d <- yes_no(d$edu_other_yn)
  out$edu_ind_access_sex_d <- d$access
  out$edu_ind_no_access_sex_d <- 1 - d$access

  out[!d$in_scope, ] <- NA_real_
  replaced[!d$in_scope, ] <- NA_real_

  # girls / boys versions (group_var child_gender_d in the tool)
  by_sex <- setdiff(names(out), "edu_other_yn_d")
  for (v in by_sex) {
    stem <- sub("(_sex)?_d$", "", v)
    out[[paste0(stem, "_female_d")]] <- ifelse(d$gender %in% "female", out[[v]], NA_real_)
    out[[paste0(stem, "_male_d")]] <- ifelse(d$gender %in% "male", out[[v]], NA_real_)
  }
  out <- out[, setdiff(names(out), c("edu_ind_access_sex_d", "edu_ind_no_access_sex_d"))]
  list(new = out, replaced = replaced, in_scope = d$in_scope)
}

## ---- Apply to the pipeline objects ---------------------------------------------

edu_isced <- edu_read_isced(edu_isced_file, edu_isced_country)
edu_tool <- edu_tool_indicators(loop, main_dataset, edu_isced, school_year_start_month = edu_school_year_start_month)

clash <- intersect(names(edu_tool$new), names(loop))
if (length(clash) > 0) stop("edu_analysis_tool_indicators.R: column(s) already in loop: ", paste(clash, collapse = ", "))
loop <- dplyr::bind_cols(loop, edu_tool$new)
loop[names(edu_tool$replaced)] <- edu_tool$replaced

edu_question_cols <- names(loop)[sub("\\..*$", "", names(loop)) %in% edu_loop_questions]
loop[!edu_tool$in_scope, edu_question_cols] <- NA

edu_tool_vars <- names(edu_tool$new)
edu_missing_meta <- setdiff(edu_tool_vars, tool.survey$name)
if (length(edu_missing_meta) > 0) {
  warning("edu_analysis_tool_indicators.R: not in the survey sheet of ", basename(kobo_tool), ", not analysed: ",
          paste(edu_missing_meta, collapse = ", "))
}
message("edu_analysis_tool_indicators.R: ", length(edu_tool_vars), " education indicator(s) added to loop, ",
        sum(edu_tool$in_scope), " children aged 5-17 (age corrected to the start of the school year)")
