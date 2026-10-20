# 05 - Analyses
# Weighted analysis of households and individuals for each level set in run.R -> tot_results

## ---- Derived indicators (sub-scores, need x income, need x sex of HoHH, ...) ----

source("src/functions/indicators_finale.R")

## ---- Education indicators, EduAnalysisTool methodology (new columns of loop) ----

source("src/functions/edu_analysis_tool_indicators.R")

## ---- Complementary indicators (10) ----

source("src/10_complementary_indicators.R")

## ---- Duplicate header safety check -----------------------------------------

source("src/functions/check_duplicate_headers.R")

check_for_duplicate_headers_list <- check_for_duplicate_headers(
  dataset = main_dataset,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column,
  ordered = T
)

main_dataset <- check_for_duplicate_headers_list[[1]]
tool.survey  <- check_for_duplicate_headers_list[[2]]

## ---- Analysis engine ---------------------------------------------------

source("src/functions/select_from_dataset.R")
source("src/functions/analysis_engine.R")
source("src/functions/modalities_reference.R")
source("src/functions/composite_levels.R")

## ---- Expected modalities -------------------------------------------------

filtered_questions <- read_filtered_questions(tool.survey, file_name_dataset)
modality_reference <- build_modality_reference(
  tool.survey, tool.choices,
  filtered = filtered_questions,
  composite_levels = dplyr::bind_rows(composite_levels, finale_levels),
  excluded = modality_exclusions
)

# for integer variables:
# - mean_analysis calculates the arithmetic mean

categorical_analysis <- "frequency_analysis"
numerical_analysis <- "mean_analysis"
ratio_analysis_type <- "ratio_analysis"

# Variables we don't analyse

variable_non_analys <- c(
  "in_person",
  "chef_team",
  "enum_id",
  "enum_gender",
  "admin1",
  "admin2",
  "admin3",
  "admin4",
  "check1",
  
  # # HHS
  # "fsl_hhs_nofoodhh",
  # "fsl_hhs_nofoodhh_freq",
  # "fsl_hhs_sleephungry",
  # "fsl_hhs_sleephungry_freq",
  # "fsl_hhs_alldaynight",
  # "fsl_hhs_alldaynight_freq",
  # 
  
  # Data collection / control variables
  "code_hh",
  "total_hh_member",
  "survey_modality",
  "partenaire",
  "admin4_reserve",
  "autres_admin4_name",
  "zone_enquete",
  "code_hh_reserve",
  "check_compo_hh",
  "code_host",
  
  # FIES
  "fsl_fies_worry",
  "fsl_fies_healthy",
  "fsl_fies_fewkinds",
  "fsl_fies_mealskip",
  "fsl_fies_mealsize",
  "fsl_fies_mealsize_freq",
  "fsl_fies_nofood",
  "fsl_fies_nofood_freq",
  "fsl_fies_hungry",
  "fsl_fies_hungry_freq",
  
  # Checks
  "check_time_water_more",
  "check_time_water_less"
)

## ---- Analysis runner (one call per level) ----
# Admin 3 levels use weights_column_admin3, the others weights_column

run_analysis <- function(dataset, disagg1, label, levels = NULL) {
  result <- apply_analysis(
    select_one_questions,
    select_multiple_questions,
    integer_questions,
    categorical_analysis,
    numerical_analysis,
    ratio_questions = ratio_questions,
    ratio_analysis_type = ratio_analysis_type,
    dataset = dataset,
    weights_column = weights_column,
    disaggregations = disagg1,
    modality_levels = levels
  )
  if (!is.null(result)) result$source_level <- label
  result
}

## ---- Household analysis (main_dataset) ----------------------------------

select_one_questions <- select_one_from_dataset(
  dataset = main_dataset,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column
)

integer_questions <- select_integer_from_dataset(
  dataset = main_dataset,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column
)

select_multiple_questions <- select_multiple_from_dataset(
  dataset = main_dataset,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  separator = separator
)

# Ratio indicators (numerator = "name", denominator = "analysis_var_denominator"
# in the questions sheet, cf. select_ratio_from_dataset()) - household level
ratio_questions <- select_ratio_from_dataset(
  dataset = main_dataset,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column
)

menage_levels <- modality_levels_for_dataset(modality_reference, main_dataset)

menage_results <- setNames(
  lapply(disaggregation, function(d) run_analysis(main_dataset, d, "Menage", menage_levels$levels)),
  paste0("tot_results_", disaggregation)
)
# also splits out tot_results_country, tot_results_admin1, ... as individual
# variables, handy to inspect one disaggregation at a time
list2env(menage_results, envir = environment())

main_menage <- bind_rows(menage_results)

# The loop keeps the population group under the name PoI, as main_dataset
loop<-loop %>% rename(PoI=Pol)
## ---- Individual-level analysis (loop) -----------------------------------
# Same disaggregations (disaggregation), applied to the loop dataset this time.

select_one_questions <- select_one_from_dataset(
  dataset = loop,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column
)

integer_questions <- select_integer_from_dataset(
  dataset = loop,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column
)

select_multiple_questions <- select_multiple_from_dataset(
  dataset = loop,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  separator = separator
)

# Household columns copied into the loop (e.g. setting) are analysed at household level only
hh_cols_in_loop <- setdiff(intersect(names(main_dataset), names(loop)), "uuid")
dropped_hh <- intersect(c(select_one_questions, integer_questions, sub("\\..*", "", select_multiple_questions)), hh_cols_in_loop)
if (length(dropped_hh)) message("05: household variable(s) not analysed at individual level: ", paste(dropped_hh, collapse = ", "))
select_one_questions <- setdiff(select_one_questions, hh_cols_in_loop)
integer_questions <- setdiff(integer_questions, hh_cols_in_loop)
select_multiple_questions <- select_multiple_questions[!sub("\\..*", "", select_multiple_questions) %in% hh_cols_in_loop]

# Ratio indicators - individual (loop) level
ratio_questions <- select_ratio_from_dataset(
  dataset = loop,
  questions = tool.survey,
  choices = tool.choices,
  name_or_label_headers = name_or_label_headers,
  label_column = label_column
)

loop_levels <- modality_levels_for_dataset(modality_reference, loop)

loop_results <- setNames(
  lapply(disaggregation, function(d) run_analysis(loop, d, "Loop", loop_levels$levels)),
  paste0("tot_results_", disaggregation, "_loop")
)
list2env(loop_results, envir = environment())

main_loop <- bind_rows(loop_results)

## ---- Household + loop combined -------------------------------------------

tot_results_final <- bind_rows(main_menage, main_loop) %>%
  filter(!question_variable_name %in% variable_non_analys) %>%
  filter(!(question_type == "select_multiple" & answer_variable_name == "0")) %>%
  mutate(before_dot = sub("\\..*", "", question_variable_name))

# Indicator metadata from the Kobo survey sheet
indicator_metadata <- tool.survey %>%
  select(name, any_of(c("theme", "module", "indicator", "label", "theme_IDD", ""))) %>%
  # a name can appear twice in the Kobo: keep its first non-empty theme_IDD
  group_by(name) %>%
  mutate(theme_IDD = { x <- trimws(as.character(theme_IDD)); x[!is.na(x) & x != ""][1] }) %>%
  ungroup() %>%
  distinct(name, .keep_all = TRUE) %>%
  rename(question.label = label, theme_IDD_kobo_legacy = theme_IDD)

tot_results_final <- tot_results_final %>%
  left_join(indicator_metadata, by = c("before_dot" = "name")) %>%
  mutate(sous_section = NA_character_) %>%
  rename(choice.name = answer_variable_name)

## ---- Response labels (choice.label) ---------------------------------------

tool.survey_labelled  <- tool.survey %>% rename(`description variable` = label)
tool.choices_labelled <- tool.choices %>% rename(`choice.label` = label)

# choice codes listed twice in a list: first label kept, duplicates saved to a CSV

choices_dup <- tool.choices_labelled %>%
  dplyr::count(list_name, name, name = "n_labels") %>%
  dplyr::filter(n_labels > 1)

if (nrow(choices_dup) > 0) {
  dup_detail <- tool.choices_labelled %>%
    dplyr::semi_join(choices_dup, by = c("list_name", "name")) %>%
    dplyr::arrange(list_name, name)
  dup_path <- file.path(output_root, "InDiSign", run_date, paste0("AMBIGUOUS_CHOICES_kobo_tool_", run_date, ".csv"))
  dir.create(dirname(dup_path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(dup_detail, dup_path, bom = TRUE)
  warning(
    nrow(choices_dup), " code(s) de choix ont plusieurs labels differents dans l'onglet 'choices' du kobo tool (ex: ",
    paste(utils::head(paste0(choices_dup$list_name, "/", choices_dup$name), 5), collapse = ", "),
    "). Le premier label rencontre est garde pour chaque code, sinon les jointures fabriquent des lignes en double. ",
    "A corriger dans le kobo tool - detail complet : ", dup_path
  )
  tool.choices_labelled <- tool.choices_labelled %>%
    dplyr::distinct(list_name, name, .keep_all = TRUE)
}

tool.survey_labelled$question <- gsub(".* ", "", tool.survey_labelled$type)

fichier_correspondance <- tool.survey_labelled %>%
  select(type, question, name, `description variable`) %>%
  left_join(
    tool.choices_labelled %>% select(list_name, name, `choice.label`),
    by = c("question" = "list_name")
  ) %>%
  distinct() %>%
  arrange(name.x) %>%
  select(-question) %>%
  rename(`question en HML` = name.x, choice.name = name.y) %>%
  filter(!is.na(type), !grepl("(begin|end)_group", type))

selec_one <- fichier_correspondance %>%
  filter(grepl("select_(one)", type)) %>%
  mutate(question_variable_name = `question en HML`) %>%
  select(question_variable_name, `choice.label`, choice.name)

select_multiple_lbl <- fichier_correspondance %>%
  filter(grepl("select_(multiple)", type)) %>%
  mutate(question_variable_name = paste(`question en HML`, choice.name, sep = separator)) %>%
  select(question_variable_name, `choice.label`, `question en HML`, choice.name)

tot_results_one <- tot_results_final %>%
  filter(question_type == "select_one") %>%
  left_join(selec_one, by = c("before_dot" = "question_variable_name", "choice.name"))

tot_results_multiple <- tot_results_final %>%
  filter(question_type == "select_multiple") %>%
  left_join(select_multiple_lbl, by = c("question_variable_name" = "question_variable_name")) %>%
  rename(choice.name = choice.name.y)

tot_results_integer <- tot_results_final %>%
  filter(question_type == "integer") %>%
  mutate(`choice.label` = "")

tot_results_ratio <- tot_results_final %>%
  filter(question_type == "ratio") %>%
  mutate(`choice.label` = "")

tot_results <- bind_rows(tot_results_one, tot_results_multiple, tot_results_integer, tot_results_ratio) %>%
  rename(
    analysis_var_value = choice.name,
    analysis_var = question_variable_name,
    group_var = disaggregation1_by,
    group_var_value = disaggregation1,
    analysis_type = question_type
  )

## ---- Modalities control table --------------------------------------------

modality_checks <- dplyr::bind_rows(
  dplyr::mutate(menage_levels$checks, source_level = "Menage"),
  dplyr::mutate(loop_levels$checks, source_level = "Loop")
)
modality_control <- build_modality_control(tot_results, modality_reference, modality_checks, country_name)

writexl::write_xlsx(
  list(
    controle_modalites = modality_control,
    referentiel_modalites = modality_reference,
    verification_referentiel = modality_checks
  ),
  out_path("Tables frequences", paste0("controle_modalites_", run_date, ".xlsx"))
)

