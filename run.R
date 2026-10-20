# MSNA Mali 2026 - run.R
# Check the settings of section 1, then source this file (details in README.txt).

rm(list = ls())
if (!require("here")) install.packages("here"); library("here")

# ============================================================================
# 1. SETTINGS - check before every run
# ============================================================================

## ---- Country -----------------------------------------------------------------
country_name <- "MALI"   # also names the output folder Output/<country_name>
Admin_code   <- "ML"     # value of the 'country' column

## ---- Input files (folder inputs/) ----------------------------------------------
input_dir <- here("inputs")

dataset_file_name        <- "MSNA_2026_MALI_Clean_Data_2026-09-21.xlsx"
kobo_tool_file_name      <- "Copy of Kobo_MLI_theme_indicator_completes.xlsx"
sampling_frame_file_name <- "Sampling_frame_organise.xlsx"
lhz_file_name            <- "MSNA_2026_KoboGPS_LHZ_16_09_2026 (2).xlsx"   # livelihood zones by uuid
dictionary_file_name     <- "dictionnaire_balises_Mali.xlsx"             # tag dictionary: new indicators (10) and InDesign fiches (11)

file_name_dataset   <- file.path(input_dir, dataset_file_name)
kobo_tool           <- file.path(input_dir, kobo_tool_file_name)
sampling_frame_file <- file.path(input_dir, sampling_frame_file_name)
lhz_file            <- file.path(input_dir, lhz_file_name)
dictionary_file     <- file.path(input_dir, dictionary_file_name)
edu_isced_file      <- file.path(input_dir, "UNESCO ISCED Mappings_MSNAcountries_consolidated.xlsx")  # school structure (EduAnalysisTool)

# Intermediate file written by 03 and read back by 04 (do not edit by hand)
intermediate_datasets_file <- file.path(input_dir, "exported_datasets.xlsx")

## ---- Sheets ---------------------------------------------------------------------
sampling_frame_sheet <- "sampling_frame_admin3 (2)"
dataset_sheets <- c(
  
  
  household = "hh_clean_data",
  roster    = "roster_clean_data",
  nut       = "nut_clean_data",
  health    = "health_clean_data",
  edu       = "edu_clean_data"
)
# Mali: no WG-SS or protection sheet (the WG-SS questions are in the roster)

## ---- Education indicators (EduAnalysisTool, src/functions/edu_analysis_tool_indicators.R)
edu_isced_country           <- "MLI"   # country code in the ISCED file
edu_school_year_start_month <- 9       # school year start month, same value as 01_education.R

## ---- Population group and weights ---------------------------------------------
population_column <- "stratification"   # dataset column renamed to 'Pol' in 02
weights_column    <- "weights"          # created in 02 by analysistools::add_weights()

## ---- Analysis options ------------------------------------------------------------
name_or_label_headers <- "name"
name_or_label_values  <- "name"
label_column          <- "label"
separator             <- "."
values_to_NA          <- c(NA, "NA", "", "SL", -999, 999)

## ---- InDesign fiches (11) ----------------------------------------------------------
fiche_texte_na <- ""      # no respondent (N = 0) or no value: nothing is written in the fiche (and nothing drawn in the charts)
fiche_seuil_n  <- 0       # 0 = every value is shown whatever the number of respondents

## ---- Disaggregation levels (TRUE = analysed) ------------------------------------
# A new level must also exist as a column of main_dataset (built in 02).
# country, country_pop, Admin_1 and admin1_pop are needed by the NAT and REG fiches.
disaggregation_Admin_1_nom      <- TRUE    # Admin 1
disaggregation_admin1_pop       <- TRUE    # Admin 1 x population group
disaggregation_Admin_2_nom      <- FALSE   # Admin 2
disaggregation_admin2_pop       <- FALSE   # Admin 2 x population group
disaggregation_country_pop      <- TRUE    # country x population group
disaggregation_country          <- TRUE   # country
disaggregation_PoI              <- FALSE   # population group
disaggregation_LZNAME_FINAL     <- FALSE   # livelihood zone
disaggregation_LZNAME_FINAL_PoI <- FALSE   # livelihood zone x population group
disaggregation_setting          <- FALSE   # setting (urban/rural/camp)
disaggregation_setting_PoI      <- FALSE   # setting x population group

flag <- function(name) isTRUE(get0(name, ifnotfound = FALSE))
disaggregation_flags <- c(
  Admin_1      = flag("disaggregation_Admin_1_nom"),
  admin1_pop   = flag("disaggregation_admin1_pop"),
  Admin_2      = flag("disaggregation_Admin_2_nom"),
  admin2_pop   = flag("disaggregation_admin2_pop"),
  country_pop  = flag("disaggregation_country_pop"),
  country      = flag("disaggregation_country"),
  PoI          = flag("disaggregation_PoI"),
  LZ_FINAL     = flag("disaggregation_LZNAME_FINAL"),
  LZ_FINAL_PoI = flag("disaggregation_LZNAME_FINAL_PoI"),
  setting      = flag("disaggregation_setting"),
  setting_PoI  = flag("disaggregation_setting_PoI")
)
disaggregation <- names(disaggregation_flags)[disaggregation_flags]

## ---- Outputs ---------------------------------------------------------------------
run_date    <- Sys.Date()
output_root <- file.path("Output", country_name)

## ---- Optional steps ------------------------------------------------------------
run_bulletin                 <- FALSE    # 09: bulletin inputs (Excel)

# ============================================================================
# 2. PIPELINE - do not edit below
# ============================================================================
source(here("src", "01_packages.R"))                  # packages
source(here("src", "02_load_data.R"))                 # output folders, Kobo, data, weights
source(here("src", "03_indicators.R"))                # sector indicators, HESPER, IYCF
source(here("src", "04_msni.R"))                      # sector composites + MSNI
source(here("src", "05_analyses.R"))                  # 10 (indicators on request) + weighted analysis -> tot_results
source(here("src", "06_frequency_tables.R"))          # Excel frequency tables
# InDesign steps (07 tag values, 08 tag dictionary, 11 fiche CSVs): kept out of the public repository, run when present
for (step in c("07_indesign_exports.R", "08_indicator_dictionary.R", "11_fiches_indesign.R"))
  if (file.exists(here("src", step))) source(here("src", step))
if (run_bulletin) source(here("src", "09_bulletin.R"))  # always last
