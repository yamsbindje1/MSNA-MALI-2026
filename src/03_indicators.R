# 03 - Sector indicators (composite_indicators/, IYCF/MDD, HESPER), saved for 04

source("src/functions/utils.R")

source("src/functions/00_run_all.R")                 # sector indicators (composite_indicators/)
source("src/functions/indicators_iycf_mddw.R")       # MDD (minimum dietary diversity)
source("src/functions/hesper/hesper_functions.R")    # add_hesper_main / add_hesper_cat
source("src/functions/hesper/hesper_utils.R")
source("src/functions/hesper/hesper_plots.R")
source("src/functions/hesper/hesper_categories.R")   # item -> category mapping
source("src/functions/hesper/hesper_apply.R")        # applies HESPER to main_dataset

source("src/functions/export_helpers.R")

## ---- Datasets used by the MSNI step ------------------------------------------------
# 04 reads this file back: the re-read sets the column types used downstream.

openxlsx::write.xlsx(
  list(
    clean_main_dataset = main_dataset,
    clean_roster       = roster,
    clean_nut_ind      = nut_ind %>% rename(person_id = nut_person_id),
    clean_wgss_indiv   = wgss_indiv %>% rename(person_id = person_id),
    clean_health_ind   = health_ind %>% rename(person_id = health_person_id),
    clean_edu_ind      = edu_ind %>% rename(person_id = edu_person_id)
  ),
  intermediate_datasets_file,
  overwrite = TRUE
)
