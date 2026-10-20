# 02 - Load data
# Kobo tool, sampling frame, datasets, disaggregation columns and weights.

## ---- Output folders ------------------------------------------------------------

for (category in c("Tables frequences", "InDiSign", "Dataset used and dictionnaire_indi")) {
  dir.create(file.path(output_root, category, run_date), recursive = TRUE, showWarnings = FALSE)
}

out_path <- function(category, filename) file.path(output_root, category, run_date, filename)

## ---- Kobo tool ---------------------------------------------------------------

tool.survey <- readxl::read_excel(kobo_tool, sheet = "survey")
tool.choices <- readxl::read_excel(kobo_tool, sheet = "choices") %>% dplyr::select(list_name, name, label)

## ---- Sampling frame -----------------------------------------------------------

sampling_frame_raw <- readxl::read_excel(sampling_frame_file, sheet = sampling_frame_sheet)

## ---- Household dataset -----------------------------------------------------------
# Every disaggregation level listed in run.R must be a column built here.

main_dataset <- readxl::read_xlsx(path = file_name_dataset, sheet = dataset_sheets[["household"]]) %>%
  dplyr::rename(
    Admin_1 = admin1, Admin_1_nom = admin1_name,
    Admin_2 = admin2, Admin_2_nom = admin2_name,
    Admin_3 = admin3, Admin_3_nom = admin3_name,
    Admin_4 = admin4,
    Pol = dplyr::all_of(population_column)
  ) %>%
  dplyr::mutate(
    country = Admin_code,
    admin1_pop = paste(Admin_1, Pol, sep = "_"),
    admin2_pop = paste(Admin_2, Pol, sep = "_"),
    Admin_2_nom_pop = paste(Admin_2_nom, Pol, sep = "_"),
    country_pop = paste(country, Pol, sep = "_"),
    strate_admin2 = paste(Admin_2, Pol, sep = "-"),
    setting_PoI = paste(setting, Pol, sep = "_")
  )

## ---- Livelihood zones -------------------------------------------------------------

ZM <- readxl::read_excel(lhz_file) %>%
  dplyr::select(LZ_FINAL, LZNAME_FINAL, uuid)

main_dataset <- main_dataset %>%
  dplyr::left_join(ZM, by = "uuid") %>%
  dplyr::mutate(
    LZ_FINAL_PoI = paste(LZNAME_FINAL, Pol, sep = "_")
  )

## ---- Weights (strata = Admin 2 x population group) --------------------------------

main_dataset <- main_dataset %>%
  analysistools::add_weights(
    sampling_frame_raw,
    strata_column_dataset = "strate_admin2",
    strata_column_sample = "strate",
    population_column = "population"
  )

## ---- Household variables joined to the individual datasets ------------------------

hh_variables <- main_dataset %>%
  dplyr::select(
    uuid, Pol,
    Admin_1, Admin_1_nom,
    Admin_2, Admin_2_nom,
    Admin_3, Admin_3_nom,
    Admin_4,
    country,
    admin1_pop, admin2_pop, Admin_2_nom_pop, country_pop,
    strate_admin2, LZ_FINAL, LZ_FINAL_PoI,
    setting, setting_PoI,
    weights
  )

## ---- Individual datasets -------------------------------------------------------

roster <- readxl::read_xlsx(file_name_dataset, sheet = dataset_sheets[["roster"]]) %>%
  dplyr::mutate(uuid_personne = person_id) %>%
  dplyr::left_join(hh_variables, by = "uuid") %>%
  dplyr::filter(!is.na(country))

nut_ind <- readxl::read_xlsx(file_name_dataset, sheet = dataset_sheets[["nut"]]) %>%
  dplyr::mutate(uuid_personne = nut_person_id) %>%
  dplyr::left_join(hh_variables, by = "uuid") %>%
  dplyr::filter(!is.na(country))

health_ind <- readxl::read_xlsx(file_name_dataset, sheet = dataset_sheets[["health"]]) %>%
  dplyr::mutate(uuid_personne = health_person_id) %>%
  dplyr::left_join(hh_variables, by = "uuid") %>%
  dplyr::filter(!is.na(country))

edu_ind <- readxl::read_xlsx(file_name_dataset, sheet = dataset_sheets[["edu"]]) %>%
  dplyr::mutate(uuid_personne = edu_person_id) %>%
  dplyr::left_join(hh_variables, by = "uuid") %>%
  dplyr::filter(!is.na(country))

## ---- Column name helper ------------------------------------------------------------

clean_column_names <- function(df) {
  names(df) <- gsub("/", ".", names(df))
  df
}
