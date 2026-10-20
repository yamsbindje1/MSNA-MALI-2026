# ============================================================================
# 09 - BULLETIN
# ============================================================================
# Creates Excel inputs required for the MSNA bulletin.
#
# Depends on:
#   04_msni.R
#       -> creates MSNI, sector and component indicators
#
#   analysis_engine.R
#       -> provides frequency_analysis()
#
# Outputs:
#
# PAGE 1
#   MSNI + sector severity distributions
#   - Severity 1 / 2 / 3 / 4 / 5
#   - Country / Admin 1 / Admin 2
#   - All / PND / PDI / REF / RET
#
# PAGE 2a
#   Underlying sector components
#   - Exact severity/category distribution
#   - % in need (severity 3-5)
#   - % in severe need (severity 4-5)
#   - Country / Admin 1 / Admin 2
#   - All / PND / PDI / REF / RET
#
# PAGE 2b
#   Exact combinations of sectoral needs
#   - 64 exact combinations across 6 sectors
#   - % HHs in exact combination at severity 3-5
#   - % HHs in exact combination at severity 4-5
#   - Country / Admin 1
#   - All / PND / PDI / REF / RET
#
# PAGE 3a
#   Demographic and socioeconomic disaggregations
#   - MSNI + 6 sector scores
#   - % HHs at severity 3-5 and severity 4-5
#   - 10 disaggregations
#   - Country / Admin 1 / Admin 2
#   - All / PND / PDI / REF / RET
#   - Unweighted and weighted n
#
# PAGE 3b
#   Compact MSNI + sector need table
#   - % HHs at severity 3-5 and severity 4-5
#   - Country / Admin 1
#   - All / PND / PDI / REF / RET
#
# PAGE 4
#   AAP analysis
#   - Part A: preferred modalities and priority needs
#   - Part B: modality x priority-need heat-map source
#   - Part C: assistance experience and relevance
#   - Country / Admin 1 / Admin 2
#   - All / PND / PDI / REF / RET
#   - Weighted and unweighted n
# ============================================================================
#
# INTEGRATION IN THE PIPELINE (run.R)
#   Sourced by run.R as the LAST step (run_bulletin = TRUE).
#   It only reads the objects already in memory (main_dataset, wgss_indiv or
#   roster, tool.survey, tool.choices, output_root, run_date, values_to_NA)
#   and writes ONLY to Output/<pays>/Bulletin/<run_date>/. It does not change
#   any object or file produced by the earlier steps: main_dataset is restored at the end.
#   Integration-only adaptations (calculation rules unchanged):
#     - the pipeline engine (src/functions/analysis_engine.R) uses the
#       prepare_analysis_dataset() + frequency_analysis(group_vars, ...) API;
#       bulletin_frequency_analysis() below calls it with the same inputs;
#     - columns expected by this script are taken from the pipeline:
#       weights = pipeline weight column (weight_admin2 for Cameroon, weights
#       for Mali/Niger), Pol = PoI (population group codes created by 04_msni.R);
#     - missing closing brace in the wgq_dis_3 block (the file did not parse);
#     - MALI version: WG-SS source = roster (wgq_* questions, age = ind_age),
#       as there is no WG-SS loop in the Mali dataset.
# ============================================================================


# Resolve the current country before building output paths.
if (!exists("country_name") || is.null(country_name) || length(country_name) != 1) {
  if (exists("main_dataset") && "country" %in% names(main_dataset)) {
    .cc <- unique(as.character(main_dataset$country[!is.na(main_dataset$country)]))
    if (length(.cc) == 1) country_name <- .cc
  }
}
if (!exists("country_name") || is.null(country_name) || length(country_name) != 1) {
  stop("country_name is not available. Run the country settings/data step first.")
}


# ============================================================================
# 0. OUTPUT SETTINGS
# ============================================================================

bulletin_output_dir <- file.path(
  output_root,
  "Bulletin",
  run_date
)

dir.create(
  bulletin_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

bulletin_file <- file.path(
  bulletin_output_dir,
  paste0(
    "Bulletin_inputs_",
    country_name,
    "_",
    run_date,
    ".xlsx"
  )
)


# ============================================================================
# 1. GENERAL SAFETY CHECKS
# ============================================================================

if (!exists("main_dataset")) {
  stop(
    "main_dataset not found. Run 04_msni.R first."
  )
}


# Automatically load analysis engine if needed

if (!exists("frequency_analysis")) {

  source(
    here::here(
      "src",
      "functions",
      "analysis_engine.R"
    )
  )
}


# Pipeline integration: same call as before (dataset, variable,
# weights_column, disaggregation1..3). With the pipeline engine, the weights
# and disaggregation columns are prepared by prepare_analysis_dataset(), then
# frequency_analysis() computes the same weighted proportions. With an engine
# that still has the disaggregation1..N arguments, it is called unchanged.

bulletin_frequency_analysis <- function(
    dataset,
    variable,
    weights_column = "none",
    disaggregation1 = "none",
    disaggregation2 = "none",
    disaggregation3 = "none"
) {

  disaggregations <- c(
    disaggregation1,
    disaggregation2,
    disaggregation3
  )

  if ("group_vars" %in% names(formals(frequency_analysis))) {

    prepared <- prepare_analysis_dataset(
      dataset,
      weights_column = weights_column,
      disaggregations = disaggregations
    )

    frequency_analysis(
      prepared$dataset,
      variable,
      group_vars = prepared$group_vars,
      disaggregations = disaggregations,
      weights_column = weights_column
    )

  } else {

    frequency_analysis(
      dataset = dataset,
      variable = variable,
      weights_column = weights_column,
      disaggregation1 = disaggregation1,
      disaggregation2 = disaggregation2,
      disaggregation3 = disaggregation3
    )
  }
}


# Pipeline integration: keep the pipeline main_dataset unchanged (restored at
# the end of this script) and add the two columns this script expects.

bulletin_main_dataset_pipeline <- main_dataset

if (!"weights" %in% names(main_dataset)) {

  # Same weight as 05_analyses.R for country / Admin 1 / Admin 2 levels.
  bulletin_weights_source <- if (
    exists("weights_column") &&
    length(weights_column) == 1 &&
    weights_column %in% names(main_dataset)
  ) {
    weights_column
  } else {
    "weight_admin2"
  }

  if (!bulletin_weights_source %in% names(main_dataset)) {
    stop(
      "08_Bulletins: no weight column found (weights / weight_admin2)."
    )
  }

  main_dataset$weights <- main_dataset[[bulletin_weights_source]]

  message(
    ">>> Bulletin weights: ",
    bulletin_weights_source
  )
}

if (!"Pol" %in% names(main_dataset) && "PoI" %in% names(main_dataset)) {

  # 04_msni.R renames Pol to PoI (codes PND / PDI / REF / RET).
  main_dataset$Pol <- main_dataset$PoI
}


# Needed by frequency_analysis()

if (!exists("values_to_NA")) {
  
  values_to_NA <- c(
    NA,
    "NA",
    "",
    "SL",
    -999,
    999
  )
}


required_general_columns <- c(
  "weights",
  "Pol",
  "country",
  "Admin_1_nom",
  "Admin_2_nom"
)

missing_general_columns <- setdiff(
  required_general_columns,
  names(main_dataset)
)

if (length(missing_general_columns) > 0) {
  
  stop(
    paste0(
      "The following required columns are missing from main_dataset: ",
      paste(
        missing_general_columns,
        collapse = ", "
      )
    )
  )
}


bulletin_country <- if (exists("country_name") && length(country_name) == 1) {
  as.character(country_name)
} else {
  cc <- unique(as.character(main_dataset$country[!is.na(main_dataset$country)]))
  if (length(cc) != 1) {
    stop("08_Bulletins.R could not determine a single current country.")
  }
  cc
}

message(">>> Building bulletin inputs for: ", bulletin_country)


# ============================================================================
# 2. POPULATION GROUP MAPPING — MULTI-COUNTRY
# ============================================================================
#
# The bulletin uses standard display codes:
#   PND = non-displaced / host population
#   PDI = internally displaced population
#   REF = refugees
#   RET = returnees
#
# The raw `Pol` codes are not assumed to be identical across countries.
# We first match common aliases against what is actually present in the dataset.
# Only population groups present in the current country are analysed.
# ============================================================================

normalise_population_code <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

population_aliases <- list(
  PND = c(
    "host", "pnd", "resident", "residents", "non_displaced",
    "non_deplace", "non_deplacee", "population_hote", "communaute_hote",
    "host_community"
  ),
  PDI = c(
    "idp", "pdi", "internally_displaced", "deplace", "deplacee",
    "deplace_interne", "deplaces_internes"
  ),
  REF = c(
    "refugie", "refugiee", "refugee", "refugees", "ref"
  ),
  RET = c(
    "retourne", "retournee", "returnee", "returnees", "ret"
  )
)

available_population_groups <- sort(
  unique(
    as.character(
      main_dataset$Pol[
        !is.na(main_dataset$Pol) &
          trimws(as.character(main_dataset$Pol)) != ""
      ]
    )
  )
)

available_population_groups_norm <- normalise_population_code(
  available_population_groups
)

population_map <- character(0)

for (std_code in names(population_aliases)) {
  alias_norm <- unique(normalise_population_code(population_aliases[[std_code]]))
  hit <- which(available_population_groups_norm %in% alias_norm)

  if (length(hit) > 0) {
    population_map[std_code] <- available_population_groups[hit[1]]
  }
}

# If no alias matched but the dataset already uses the bulletin codes directly,
# catch those explicitly.
for (std_code in c("PND", "PDI", "REF", "RET")) {
  if (!std_code %in% names(population_map)) {
    hit <- which(toupper(trimws(available_population_groups)) == std_code)
    if (length(hit) > 0) population_map[std_code] <- available_population_groups[hit[1]]
  }
}

if (length(population_map) == 0) {
  stop(
    paste0(
      "\nCould not map any population groups from `Pol`.\n",
      "Available values are:\n",
      paste(available_population_groups, collapse = ", "),
      "\n\nAdd the relevant raw code(s) to `population_aliases` near the top of ",
      "08_Bulletins.R."
    )
  )
}

missing_standard_populations <- setdiff(
  c("PND", "PDI", "REF", "RET"),
  names(population_map)
)

if (length(missing_standard_populations) > 0) {
  warning(
    paste0(
      "The following bulletin population group(s) are not present in this country ",
      "and will be omitted: ",
      paste(missing_standard_populations, collapse = ", "),
      ". Raw Pol values found: ",
      paste(available_population_groups, collapse = ", ")
    )
  )
}

message(
  ">>> Bulletin population mapping: ",
  paste(
    paste0(names(population_map), "=", unname(population_map)),
    collapse = " | "
  )
)

population_reverse_map <- setNames(
  names(population_map),
  unname(population_map)
)


# ============================================================================
# 3. GEOGRAPHIC SETTINGS
# ============================================================================

geography_map <- list(
  
  list(
    variable = "country",
    label = "Country"
  ),
  
  list(
    variable = "Admin_1_nom",
    label = "Admin 1"
  ),
  
  list(
    variable = "Admin_2_nom",
    label = "Admin 2"
  )
)


admin_level_order <- c(
  "Country",
  "Admin 1",
  "Admin 2"
)


population_order <- c(
  "All",
  names(population_map)
)


# ============================================================================
# 4. COMMON HELPER:
#    BINARY VARIABLE BY GEOGRAPHY + POPULATION
# ============================================================================

analyse_binary_geography <- function(
    dataset,
    variable,
    geo_var,
    admin_level
) {
  
  # --------------------------------------------------------------------------
  # Geographic universe
  # --------------------------------------------------------------------------
  
  area_values <- dataset %>%
    
    dplyr::filter(
      !is.na(.data[[geo_var]]),
      as.character(.data[[geo_var]]) != ""
    ) %>%
    
    dplyr::transmute(
      area = as.character(
        .data[[geo_var]]
      )
    ) %>%
    
    dplyr::distinct() %>%
    
    dplyr::arrange(
      area
    )
  
  
  # ==========================================================================
  # A. ALL POPULATIONS
  # ==========================================================================
  
  result_all <- bulletin_frequency_analysis(
    dataset = dataset,
    variable = variable,
    weights_column = "weights",
    disaggregation1 = geo_var,
    disaggregation2 = "none",
    disaggregation3 = "none"
  )
  
  
  valid_all <- result_all %>%
    
    dplyr::transmute(
      area = as.character(
        disaggregation1
      ),
      has_valid_data = TRUE
    ) %>%
    
    dplyr::distinct()
  
  
  positive_all <- result_all %>%
    
    dplyr::filter(
      as.character(
        answer_variable_name
      ) == "1"
    ) %>%
    
    dplyr::transmute(
      area = as.character(
        disaggregation1
      ),
      percentage = as.numeric(
        stat
      )
    )
  
  
  all_results <- area_values %>%
    
    dplyr::left_join(
      valid_all,
      by = "area"
    ) %>%
    
    dplyr::left_join(
      positive_all,
      by = "area"
    ) %>%
    
    dplyr::mutate(
      
      population = "All",
      
      percentage = dplyr::case_when(
        
        is.na(has_valid_data) ~ NA_real_,
        
        is.na(percentage) ~ 0,
        
        TRUE ~ percentage
      ),
      
      admin_level = admin_level
    ) %>%
    
    dplyr::select(
      admin_level,
      area,
      population,
      percentage
    )
  
  
  # ==========================================================================
  # B. BY POPULATION
  # ==========================================================================
  
  result_population <- bulletin_frequency_analysis(
    dataset = dataset,
    variable = variable,
    weights_column = "weights",
    disaggregation1 = geo_var,
    disaggregation2 = "Pol",
    disaggregation3 = "none"
  )
  
  
  # Full desired area x population matrix
  
  population_universe <- tidyr::crossing(
    area = area_values$area,
    population = names(population_map)
  )
  
  
  valid_population <- result_population %>%
    
    dplyr::filter(
      as.character(
        disaggregation2
      ) %in% unname(population_map)
    ) %>%
    
    dplyr::transmute(
      
      area = as.character(
        disaggregation1
      ),
      
      population_raw = as.character(
        disaggregation2
      ),
      
      population = unname(
        population_reverse_map[
          population_raw
        ]
      ),
      
      has_valid_data = TRUE
      
    ) %>%
    
    dplyr::distinct(
      area,
      population,
      .keep_all = TRUE
    ) %>%
    
    dplyr::select(
      area,
      population,
      has_valid_data
    )
  
  
  positive_population <- result_population %>%
    
    dplyr::filter(
      
      as.character(
        answer_variable_name
      ) == "1",
      
      as.character(
        disaggregation2
      ) %in% unname(population_map)
    ) %>%
    
    dplyr::transmute(
      
      area = as.character(
        disaggregation1
      ),
      
      population_raw = as.character(
        disaggregation2
      ),
      
      population = unname(
        population_reverse_map[
          population_raw
        ]
      ),
      
      percentage = as.numeric(
        stat
      )
      
    ) %>%
    
    dplyr::select(
      area,
      population,
      percentage
    )
  
  
  population_results <- population_universe %>%
    
    dplyr::left_join(
      valid_population,
      by = c(
        "area",
        "population"
      )
    ) %>%
    
    dplyr::left_join(
      positive_population,
      by = c(
        "area",
        "population"
      )
    ) %>%
    
    dplyr::mutate(
      
      percentage = dplyr::case_when(
        
        is.na(has_valid_data) ~ NA_real_,
        
        is.na(percentage) ~ 0,
        
        TRUE ~ percentage
      ),
      
      admin_level = admin_level
    ) %>%
    
    dplyr::select(
      admin_level,
      area,
      population,
      percentage
    )
  
  
  # ==========================================================================
  # C. COMBINE
  # ==========================================================================
  
  dplyr::bind_rows(
    all_results,
    population_results
  )
}


# ============================================================================
# 5. COMMON HELPER:
#    FULL SEVERITY/CATEGORY DISTRIBUTION
# ============================================================================

analyse_distribution_geography <- function(
    dataset,
    variable,
    geo_var,
    admin_level,
    expected_categories
) {
  
  expected_categories <- as.character(
    expected_categories
  )
  
  
  # --------------------------------------------------------------------------
  # Geographic universe
  # --------------------------------------------------------------------------
  
  area_values <- dataset %>%
    
    dplyr::filter(
      !is.na(.data[[geo_var]]),
      as.character(.data[[geo_var]]) != ""
    ) %>%
    
    dplyr::transmute(
      area = as.character(
        .data[[geo_var]]
      )
    ) %>%
    
    dplyr::distinct() %>%
    
    dplyr::arrange(
      area
    )
  
  
  # ==========================================================================
  # A. ALL POPULATIONS
  # ==========================================================================
  
  result_all <- bulletin_frequency_analysis(
    dataset = dataset,
    variable = variable,
    weights_column = "weights",
    disaggregation1 = geo_var,
    disaggregation2 = "none",
    disaggregation3 = "none"
  )
  
  
  all_universe <- tidyr::crossing(
    area = area_values$area,
    severity_category = expected_categories
  )
  
  
  valid_all <- result_all %>%
    
    dplyr::transmute(
      area = as.character(
        disaggregation1
      ),
      has_valid_data = TRUE
    ) %>%
    
    dplyr::distinct()
  
  
  distribution_all <- result_all %>%
    
    dplyr::transmute(
      
      area = as.character(
        disaggregation1
      ),
      
      severity_category = as.character(
        answer_variable_name
      ),
      
      severity_percentage = as.numeric(
        stat
      )
    )
  
  
  all_results <- all_universe %>%
    
    dplyr::left_join(
      valid_all,
      by = "area"
    ) %>%
    
    dplyr::left_join(
      distribution_all,
      by = c(
        "area",
        "severity_category"
      )
    ) %>%
    
    dplyr::mutate(
      
      population = "All",
      
      severity_percentage = dplyr::case_when(
        
        is.na(has_valid_data) ~ NA_real_,
        
        is.na(severity_percentage) ~ 0,
        
        TRUE ~ severity_percentage
      ),
      
      admin_level = admin_level
    ) %>%
    
    dplyr::select(
      admin_level,
      area,
      population,
      severity_category,
      severity_percentage
    )
  
  
  # ==========================================================================
  # B. BY POPULATION
  # ==========================================================================
  
  result_population <- bulletin_frequency_analysis(
    dataset = dataset,
    variable = variable,
    weights_column = "weights",
    disaggregation1 = geo_var,
    disaggregation2 = "Pol",
    disaggregation3 = "none"
  )
  
  
  population_universe <- tidyr::crossing(
    area = area_values$area,
    population = names(population_map),
    severity_category = expected_categories
  )
  
  
  valid_population <- result_population %>%
    
    dplyr::filter(
      as.character(
        disaggregation2
      ) %in% unname(population_map)
    ) %>%
    
    dplyr::transmute(
      
      area = as.character(
        disaggregation1
      ),
      
      population_raw = as.character(
        disaggregation2
      ),
      
      population = unname(
        population_reverse_map[
          population_raw
        ]
      ),
      
      has_valid_data = TRUE
      
    ) %>%
    
    dplyr::distinct(
      area,
      population,
      .keep_all = TRUE
    ) %>%
    
    dplyr::select(
      area,
      population,
      has_valid_data
    )
  
  
  distribution_population <- result_population %>%
    
    dplyr::filter(
      as.character(
        disaggregation2
      ) %in% unname(population_map)
    ) %>%
    
    dplyr::transmute(
      
      area = as.character(
        disaggregation1
      ),
      
      population_raw = as.character(
        disaggregation2
      ),
      
      population = unname(
        population_reverse_map[
          population_raw
        ]
      ),
      
      severity_category = as.character(
        answer_variable_name
      ),
      
      severity_percentage = as.numeric(
        stat
      )
      
    ) %>%
    
    dplyr::select(
      area,
      population,
      severity_category,
      severity_percentage
    )
  
  
  population_results <- population_universe %>%
    
    dplyr::left_join(
      valid_population,
      by = c(
        "area",
        "population"
      )
    ) %>%
    
    dplyr::left_join(
      distribution_population,
      by = c(
        "area",
        "population",
        "severity_category"
      )
    ) %>%
    
    dplyr::mutate(
      
      severity_percentage = dplyr::case_when(
        
        is.na(has_valid_data) ~ NA_real_,
        
        is.na(severity_percentage) ~ 0,
        
        TRUE ~ severity_percentage
      ),
      
      admin_level = admin_level
    ) %>%
    
    dplyr::select(
      admin_level,
      area,
      population,
      severity_category,
      severity_percentage
    )
  
  
  # ==========================================================================
  # C. COMBINE
  # ==========================================================================
  
  dplyr::bind_rows(
    all_results,
    population_results
  ) %>%
    
    dplyr::mutate(
      severity_order = match(
        severity_category,
        expected_categories
      )
    )
}


# ============================================================================
# ============================================================================
# PAGE 1
# MSNI + SECTOR SEVERITY
# ============================================================================
# ============================================================================


# ============================================================================
# 6. PAGE 1 - SECTOR DEFINITIONS
# ============================================================================

page1_sector_map <- tibble::tribble(
  
  ~sector_code, ~sector,         ~internal_prefix,
  
  "msni",       "MSNI",          "msni",
  "fs",         "Food Security", "comp_foodsec",
  "wash",       "WASH",          "comp_wash",
  "snfi",       "SNFI",          "comp_snfi",
  "education",  "Education",     "comp_edu",
  "protection", "Protection",    "comp_prot",
  "health",     "Health",        "comp_health"
)


# ============================================================================
# 7. PAGE 1 - CHECK SEVERITY VARIABLES
# ============================================================================

expected_severity_vars <- unlist(
  
  lapply(
    page1_sector_map$internal_prefix,
    
    function(prefix) {
      
      paste0(
        prefix,
        "_",
        1:5
      )
    }
  )
)


missing_severity_vars <- setdiff(
  expected_severity_vars,
  names(main_dataset)
)


if (length(missing_severity_vars) > 0) {
  
  stop(
    paste0(
      "\nThe following severity variables are missing:\n\n",
      paste(
        missing_severity_vars,
        collapse = "\n"
      ),
      "\n\nCheck the country MSNI derivation used by 04_msni.R."
    )
  )
}


# ============================================================================
# 8. PAGE 1 - CALCULATIONS
# ============================================================================

page1_results <- list()

page1_counter <- 1


for (i in seq_len(nrow(page1_sector_map))) {
  
  sector_code <- page1_sector_map$sector_code[i]
  
  sector_name <- page1_sector_map$sector[i]
  
  internal_prefix <- page1_sector_map$internal_prefix[i]
  
  
  for (severity in 1:5) {
    
    severity_variable <- paste0(
      internal_prefix,
      "_",
      severity
    )
    
    
    message(
      "Page 1: calculating ",
      sector_code,
      " severity ",
      severity
    )
    
    
    geography_results <- list()
    
    
    for (g in seq_along(geography_map)) {
      
      geo_var <- geography_map[[g]]$variable
      
      geo_label <- geography_map[[g]]$label
      
      
      geography_results[[g]] <- analyse_binary_geography(
        
        dataset = main_dataset,
        
        variable = severity_variable,
        
        geo_var = geo_var,
        
        admin_level = geo_label
      )
    }
    
    
    severity_results <- dplyr::bind_rows(
      geography_results
    ) %>%
      
      dplyr::mutate(
        
        sector_code = sector_code,
        
        sector = sector_name,
        
        severity = severity,
        
        internal_variable = severity_variable,
        
        code = dplyr::if_else(
          
          population == "All",
          
          paste0(
            sector_code,
            "_",
            severity
          ),
          
          paste0(
            sector_code,
            "_",
            severity,
            "_",
            population
          )
        )
      ) %>%
      
      dplyr::select(
        sector_code,
        sector,
        severity,
        internal_variable,
        code,
        admin_level,
        area,
        population,
        percentage
      )
    
    
    page1_results[[page1_counter]] <- severity_results
    
    page1_counter <- page1_counter + 1
  }
}


page1 <- dplyr::bind_rows(
  page1_results
)


# ============================================================================
# 9. PAGE 1 - ADD CONCATENATE
# ============================================================================

# Equivalent to:
#
# CONCATENATE(code, ".", admin_level)
#
# Examples:
#   wash_4.Country
#   wash_4_PDI.Country
#   wash_4.Admin 1
#   wash_4_REF.Admin 2


page1 <- page1 %>%
  
  dplyr::mutate(
    
    concatenate = paste(
      code,
      admin_level,
      sep = "."
    )
  )


# ============================================================================
# 10. PAGE 1 - ORDER
# ============================================================================

page1 <- page1 %>%
  
  dplyr::mutate(
    
    sector_order_internal = match(
      sector_code,
      page1_sector_map$sector_code
    ),
    
    admin_order_internal = match(
      admin_level,
      admin_level_order
    ),
    
    population_order_internal = match(
      population,
      population_order
    )
  ) %>%
  
  dplyr::arrange(
    sector_order_internal,
    severity,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  
  dplyr::select(
    
    sector_code,
    sector,
    severity,
    internal_variable,
    code,
    admin_level,
    concatenate,
    area,
    population,
    percentage
  )


# ============================================================================
# 11. PAGE 1 - VALIDATION
# ============================================================================

if (
  any(
    page1$percentage < 0 |
    page1$percentage > 1,
    na.rm = TRUE
  )
) {
  
  stop(
    "Page 1 contains percentages outside the range 0-100%."
  )
}


page1_duplicate_check <- page1 %>%
  
  dplyr::count(
    sector_code,
    severity,
    admin_level,
    area,
    population
  ) %>%
  
  dplyr::filter(
    n > 1
  )


if (nrow(page1_duplicate_check) > 0) {
  
  stop(
    "Duplicate severity/geography/population rows found on Page 1."
  )
}


# ============================================================================
# ============================================================================
# PAGE 2A
# COMPONENT DISTRIBUTIONS + NEED + SEVERE NEED
# ============================================================================
# ============================================================================


# ============================================================================
# 12. PAGE 2A - COMPONENT DEFINITIONS
# ============================================================================

# scale_type:
#
# numeric_1_5
#   Standard MSNI-style numeric component severity.
#
# food_fcs / food_hhs / food_rcsi / food_lcsi
#   Native Food Security classifications.
#
#
# severe_need_available:
#
# TRUE
#   A genuine >=4 component severity can be calculated.
#
# FALSE
#   No common 1-5 component severity exists.
#   severe_need_percentage is consequently reported as 0.


component_map <- tibble::tribble(
  
  ~sector,
  ~component_code,
  ~component,
  ~source_variable,
  ~scale_type,
  ~severe_need_available,
  
  
  # --------------------------------------------------------------------------
  # FOOD SECURITY
  # --------------------------------------------------------------------------
  
  "Food Security",
  "fs_fcs",
  "Food Consumption Score (FCS)",
  "fsl_fcs_cat",
  "food_fcs",
  FALSE,
  
  "Food Security",
  "fs_hhs",
  "Household Hunger Scale (HHS)",
  "fsl_hhs_cat_ipc",
  "food_hhs",
  FALSE,
  
  "Food Security",
  "fs_rcsi",
  "Reduced Coping Strategy Index (rCSI)",
  "fsl_rcsi_cat",
  "food_rcsi",
  FALSE,
  
  "Food Security",
  "fs_lcsi",
  "Livelihood Coping Strategy Index (LCSI)",
  "fsl_lcsi_cat",
  "food_lcsi",
  FALSE,
  
  
  # --------------------------------------------------------------------------
  # WASH
  # --------------------------------------------------------------------------
  
  "WASH",
  "wash_water_quantity",
  "Water Quantity",
  "comp_wash_score_water_quantity",
  "numeric_1_5",
  TRUE,
  
  "WASH",
  "wash_water_quality",
  "Water Quality",
  "comp_wash_score_water_quality",
  "numeric_1_5",
  TRUE,
  
  "WASH",
  "wash_sanitation",
  "Sanitation",
  "comp_wash_score_sanitation",
  "numeric_1_5",
  TRUE,
  
  "WASH",
  "wash_hygiene",
  "Hygiene",
  "comp_wash_score_hygiene",
  "numeric_1_5",
  TRUE,
  
  
  # --------------------------------------------------------------------------
  # SNFI
  # --------------------------------------------------------------------------
  
  "SNFI",
  "snfi_shelter_type",
  "Shelter Type",
  "comp_snfi_score_shelter_type_cat",
  "numeric_1_5",
  TRUE,
  
  "SNFI",
  "snfi_shelter_issues",
  "Shelter Issues",
  "comp_snfi_score_shelter_issue_cat",
  "numeric_1_5",
  TRUE,
  
  "SNFI",
  "snfi_tenure",
  "Security of Tenure",
  "comp_snfi_score_tenure_security_cat",
  "numeric_1_5",
  TRUE,
  
  "SNFI",
  "snfi_fds",
  "Functional Domestic Space",
  "comp_snfi_score_fds_cannot_cat",
  "numeric_1_5",
  TRUE,
  
  "SNFI",
  "snfi_shelter_damage",
  "Shelter Damages",
  "comp_snfi_score_shelter_damage_cat",
  "numeric_1_5",
  TRUE,
  
  
  # --------------------------------------------------------------------------
  # EDUCATION
  # --------------------------------------------------------------------------
  
  "Education",
  "education_access",
  "Access and barriers to access education",
  "comp_edu_score_attendance",
  "numeric_1_5",
  TRUE,
  
  "Education",
  "education_disruption",
  "Education Disruption",
  "comp_edu_score_disrupted",
  "numeric_1_5",
  TRUE,
  
  
  # --------------------------------------------------------------------------
  # PROTECTION
  # --------------------------------------------------------------------------
  
  "Protection",
  "protection_movement",
  "Ability to move and access public spaces",
  "comp_prot_score_movement",
  "numeric_1_5",
  TRUE,
  
  "Protection",
  "protection_practices",
  "Ability to participate in safe practices and activities",
  "comp_prot_score_practices",
  "numeric_1_5",
  TRUE,
  
  "Protection",
  "protection_rights",
  "Ability to access rights and services",
  "comp_prot_score_rights",
  "numeric_1_5",
  TRUE,
  
  
  # --------------------------------------------------------------------------
  # HEALTH
  # --------------------------------------------------------------------------
  
  "Health",
  "health_needs",
  "Health Needs",
  "comp_health_score",
  "numeric_1_5",
  TRUE
)


# ============================================================================
# 13. PAGE 2A - EXPECTED SEVERITY/CATEGORY VALUES
# ============================================================================

get_component_categories <- function(
    scale_type
) {
  
  switch(
    
    scale_type,
    
    numeric_1_5 = as.character(
      1:5
    ),
    
    food_fcs = c(
      "Acceptable",
      "Borderline",
      "Poor"
    ),
    
    food_hhs = c(
      "None",
      "Little",
      "Moderate",
      "Severe",
      "Very Severe"
    ),
    
    food_rcsi = c(
      "No to Low",
      "Medium",
      "High"
    ),
    
    food_lcsi = c(
      "None",
      "Stress",
      "Crisis",
      "Emergency"
    ),
    
    stop(
      "Unknown scale_type: ",
      scale_type
    )
  )
}


# ============================================================================
# 14. PAGE 2A - CHECK SOURCE VARIABLES
# ============================================================================

missing_component_variables <- setdiff(
  component_map$source_variable,
  names(main_dataset)
)


if (length(missing_component_variables) > 0) {
  
  stop(
    paste0(
      "\nThe following Page 2a component variables are missing:\n\n",
      paste(
        missing_component_variables,
        collapse = "\n"
      ),
      "\n\nCheck the outputs created by 04_msni.R."
    )
  )
}


# ============================================================================
# 15. PAGE 2A - VALIDATE FOOD SECURITY CATEGORIES
# ============================================================================

for (i in seq_len(nrow(component_map))) {
  
  scale_type <- component_map$scale_type[i]
  
  
  if (
    scale_type %in%
    c(
      "food_fcs",
      "food_hhs",
      "food_rcsi",
      "food_lcsi"
    )
  ) {
    
    source_var <- component_map$source_variable[i]
    
    expected_values <- get_component_categories(
      scale_type
    )
    
    
    observed_values <- unique(
      as.character(
        main_dataset[[source_var]][
          !is.na(
            main_dataset[[source_var]]
          )
        ]
      )
    )
    
    
    unexpected_values <- setdiff(
      observed_values,
      expected_values
    )
    
    
    if (length(unexpected_values) > 0) {
      
      stop(
        paste0(
          "\nUnexpected values found in ",
          source_var,
          ":\n",
          paste(
            unexpected_values,
            collapse = ", "
          )
        )
      )
    }
  }
}


# ============================================================================
# 16. PAGE 2A - CREATE NEED + SEVERE NEED VARIABLES
# ============================================================================

bulletin_dataset <- main_dataset


for (i in seq_len(nrow(component_map))) {
  
  component_code <- component_map$component_code[i]
  
  source_var <- component_map$source_variable[i]
  
  scale_type <- component_map$scale_type[i]
  
  
  need_var <- paste0(
    "bulletin_need_",
    component_code
  )
  
  severe_var <- paste0(
    "bulletin_severe_",
    component_code
  )
  
  
  # ==========================================================================
  # STANDARD NUMERIC COMPONENT
  #
  # Need        = Severity 3 / 4 / 5
  # Severe need = Severity 4 / 5
  # ==========================================================================
  
  if (scale_type == "numeric_1_5") {
    
    source_numeric <- suppressWarnings(
      as.numeric(
        as.character(
          bulletin_dataset[[source_var]]
        )
      )
    )
    
    
    bulletin_dataset[[need_var]] <- dplyr::case_when(
      
      is.na(source_numeric) ~ NA_integer_,
      
      source_numeric %in% 3:5 ~ 1L,
      
      source_numeric %in% 1:2 ~ 0L,
      
      TRUE ~ NA_integer_
    )
    
    
    bulletin_dataset[[severe_var]] <- dplyr::case_when(
      
      is.na(source_numeric) ~ NA_integer_,
      
      source_numeric %in% 4:5 ~ 1L,
      
      source_numeric %in% 1:3 ~ 0L,
      
      TRUE ~ NA_integer_
    )
  }
  
  
  # ==========================================================================
  # FOOD SECURITY - FCS
  # ==========================================================================
  
  if (scale_type == "food_fcs") {
    
    bulletin_dataset[[need_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      bulletin_dataset[[source_var]] ==
        "Poor" ~ 1L,
      
      bulletin_dataset[[source_var]] %in%
        c(
          "Acceptable",
          "Borderline"
        ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
    
    
    bulletin_dataset[[severe_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      TRUE ~ 0L
    )
  }
  
  
  # ==========================================================================
  # FOOD SECURITY - HHS
  # ==========================================================================
  
  if (scale_type == "food_hhs") {
    
    bulletin_dataset[[need_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      bulletin_dataset[[source_var]] %in%
        c(
          "Moderate",
          "Severe",
          "Very Severe"
        ) ~ 1L,
      
      bulletin_dataset[[source_var]] %in%
        c(
          "None",
          "Little"
        ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
    
    
    bulletin_dataset[[severe_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      TRUE ~ 0L
    )
  }
  
  
  # ==========================================================================
  # FOOD SECURITY - rCSI
  # ==========================================================================
  
  if (scale_type == "food_rcsi") {
    
    bulletin_dataset[[need_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      bulletin_dataset[[source_var]] ==
        "High" ~ 1L,
      
      bulletin_dataset[[source_var]] %in%
        c(
          "No to Low",
          "Medium"
        ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
    
    
    bulletin_dataset[[severe_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      TRUE ~ 0L
    )
  }
  
  
  # ==========================================================================
  # FOOD SECURITY - LCSI
  # ==========================================================================
  
  if (scale_type == "food_lcsi") {
    
    bulletin_dataset[[need_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      bulletin_dataset[[source_var]] %in%
        c(
          "Crisis",
          "Emergency"
        ) ~ 1L,
      
      bulletin_dataset[[source_var]] %in%
        c(
          "None",
          "Stress"
        ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
    
    
    bulletin_dataset[[severe_var]] <- dplyr::case_when(
      
      is.na(
        bulletin_dataset[[source_var]]
      ) ~ NA_integer_,
      
      TRUE ~ 0L
    )
  }
}


# ============================================================================
# 17. PAGE 2A - CALCULATE ALL COMPONENTS
# ============================================================================

page2a_results <- list()

page2a_counter <- 1


for (i in seq_len(nrow(component_map))) {
  
  sector_name <- component_map$sector[i]
  
  component_code <- component_map$component_code[i]
  
  component_name <- component_map$component[i]
  
  source_var <- component_map$source_variable[i]
  
  scale_type <- component_map$scale_type[i]
  
  severe_available <- component_map$severe_need_available[i]
  
  
  expected_categories <- get_component_categories(
    scale_type
  )
  
  
  need_var <- paste0(
    "bulletin_need_",
    component_code
  )
  
  severe_var <- paste0(
    "bulletin_severe_",
    component_code
  )
  
  
  message(
    "Page 2a: calculating ",
    component_code
  )
  
  
  # ==========================================================================
  # A. EXACT SEVERITY / CATEGORY DISTRIBUTION
  # ==========================================================================
  
  distribution_results <- list()
  
  
  for (g in seq_along(geography_map)) {
    
    geo_var <- geography_map[[g]]$variable
    
    geo_label <- geography_map[[g]]$label
    
    
    distribution_results[[g]] <- analyse_distribution_geography(
      
      dataset = bulletin_dataset,
      
      variable = source_var,
      
      geo_var = geo_var,
      
      admin_level = geo_label,
      
      expected_categories = expected_categories
    )
  }
  
  
  distribution_results <- dplyr::bind_rows(
    distribution_results
  )
  
  
  # ==========================================================================
  # B. % IN NEED
  # ==========================================================================
  
  need_results <- list()
  
  
  for (g in seq_along(geography_map)) {
    
    geo_var <- geography_map[[g]]$variable
    
    geo_label <- geography_map[[g]]$label
    
    
    need_results[[g]] <- analyse_binary_geography(
      
      dataset = bulletin_dataset,
      
      variable = need_var,
      
      geo_var = geo_var,
      
      admin_level = geo_label
    )
  }
  
  
  need_results <- dplyr::bind_rows(
    need_results
  ) %>%
    
    dplyr::rename(
      in_need_percentage = percentage
    )
  
  
  # ==========================================================================
  # C. % IN SEVERE NEED
  # ==========================================================================
  
  severe_results <- list()
  
  
  for (g in seq_along(geography_map)) {
    
    geo_var <- geography_map[[g]]$variable
    
    geo_label <- geography_map[[g]]$label
    
    
    severe_results[[g]] <- analyse_binary_geography(
      
      dataset = bulletin_dataset,
      
      variable = severe_var,
      
      geo_var = geo_var,
      
      admin_level = geo_label
    )
  }
  
  
  severe_results <- dplyr::bind_rows(
    severe_results
  ) %>%
    
    dplyr::rename(
      severe_need_percentage = percentage
    )
  
  
  # ==========================================================================
  # D. COMBINE
  # ==========================================================================
  
  component_results <- distribution_results %>%
    
    dplyr::left_join(
      
      need_results,
      
      by = c(
        "admin_level",
        "area",
        "population"
      )
    ) %>%
    
    dplyr::left_join(
      
      severe_results,
      
      by = c(
        "admin_level",
        "area",
        "population"
      )
    ) %>%
    
    dplyr::mutate(
      
      sector = sector_name,
      
      component_code = component_code,
      
      component = component_name,
      
      source_variable = source_var,
      
      severity_scale = dplyr::if_else(
        scale_type == "numeric_1_5",
        "1-5",
        "Native category"
      ),
      
      severe_need_available = severe_available
      
    ) %>%
    
    dplyr::select(
      sector,
      component_code,
      component,
      source_variable,
      severity_scale,
      severity_category,
      admin_level,
      area,
      population,
      severity_percentage,
      in_need_percentage,
      severe_need_percentage,
      severe_need_available,
      severity_order
    )
  
  
  page2a_results[[page2a_counter]] <- component_results
  
  page2a_counter <- page2a_counter + 1
}


page2a <- dplyr::bind_rows(
  page2a_results
)


# ============================================================================
# 18. PAGE 2A - ADD CONCATENATE
# ============================================================================

# Equivalent to:
#
# CONCATENATE(
#   source_variable,
#   ".",
#   admin_level,
#   ".",
#   population
# )
#
# Examples:
#
# comp_wash_score_water_quantity.Country.All
# comp_wash_score_water_quantity.Country.PDI
# comp_wash_score_water_quantity.Admin 1.PND
# comp_snfi_score_shelter_damage_cat.Admin 2.RET


page2a <- page2a %>%
  
  dplyr::mutate(
    
    concatenate = paste(
      source_variable,
      admin_level,
      population,
      sep = "."
    )
  )


# ============================================================================
# 19. PAGE 2A - ORDER
# ============================================================================

component_order <- component_map$component_code


page2a <- page2a %>%
  
  dplyr::mutate(
    
    component_order_internal = match(
      component_code,
      component_order
    ),
    
    admin_order_internal = match(
      admin_level,
      admin_level_order
    ),
    
    population_order_internal = match(
      population,
      population_order
    )
    
  ) %>%
  
  dplyr::arrange(
    component_order_internal,
    admin_order_internal,
    area,
    population_order_internal,
    severity_order
  ) %>%
  
  dplyr::select(
    
    sector,
    component_code,
    component,
    source_variable,
    severity_scale,
    severity_category,
    admin_level,
    area,
    population,
    concatenate,
    severity_percentage,
    in_need_percentage,
    severe_need_percentage,
    severe_need_available
  )


# ============================================================================
# 20. PAGE 2A - VALIDATION
# ============================================================================

if (
  dplyr::n_distinct(
    page2a$component_code
  ) != 19
) {
  
  warning(
    "Page 2a does not contain all 19 expected components."
  )
}


percentage_columns_page2a <- c(
  "severity_percentage",
  "in_need_percentage",
  "severe_need_percentage"
)


for (percentage_column in percentage_columns_page2a) {
  
  if (
    any(
      page2a[[percentage_column]] < 0 |
      page2a[[percentage_column]] > 1,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Page 2a contains invalid values in ",
      percentage_column,
      "."
    )
  }
}


page2a_duplicate_check <- page2a %>%
  
  dplyr::count(
    component_code,
    severity_category,
    admin_level,
    area,
    population
  ) %>%
  
  dplyr::filter(
    n > 1
  )


if (nrow(page2a_duplicate_check) > 0) {
  
  stop(
    "Duplicate component/severity/geography/population rows found on Page 2a."
  )
}


# ============================================================================
# 21. PAGE 2A - QUALITY CHECK
#     SEVERITY/CATEGORY DISTRIBUTIONS SHOULD SUM TO APPROX. 100%
# ============================================================================

page2a_distribution_check <- page2a %>%
  
  dplyr::group_by(
    component_code,
    admin_level,
    area,
    population
  ) %>%
  
  dplyr::summarise(
    
    severity_total = sum(
      severity_percentage,
      na.rm = TRUE
    ),
    
    all_missing = all(
      is.na(
        severity_percentage
      )
    ),
    
    .groups = "drop"
  ) %>%
  
  dplyr::filter(
    !all_missing,
    abs(
      severity_total - 1
    ) > 0.01
  )


if (
  nrow(
    page2a_distribution_check
  ) > 0
) {
  
  warning(
    paste0(
      nrow(
        page2a_distribution_check
      ),
      " Page 2a component distributions do not sum to approximately 100%. ",
      "Inspect object 'page2a_distribution_check'."
    )
  )
}

# ============================================================================
# ============================================================================
# PAGE 2B
# EXACT COMBINATIONS OF SECTORAL NEEDS
# ============================================================================
# ============================================================================
#
# For each household, determine the exact combination of sectors:
#
#   A. In need:
#      sector severity >= 3
#
#   B. Severe need:
#      sector severity >= 4
#
# There are 6 sectors, therefore:
#
#   2^6 = 64 possible combinations
#
# Results are produced for:
#
#   Country
#   Admin 1
#
# and:
#
#   All
#   PND
#   PDI
#   REF
#   RET
#
# ============================================================================


# ============================================================================
# 22. PAGE 2B - SECTOR DEFINITIONS
# ============================================================================

page2b_sector_map <- tibble::tribble(
  
  ~sector_flag,   ~sector_label,   ~score_variable,
  
  "secal",        "SECAL",         "comp_foodsec_score",
  "wash",         "WASH",          "comp_wash_score",
  "snfi",         "SNFI",          "comp_snfi_score",
  "education",    "Education",     "comp_edu_score",
  "protection",   "Protection",    "comp_prot_score",
  "health",       "Health",        "comp_health_score"
)


# ============================================================================
# 23. PAGE 2B - CHECK SECTOR SCORE VARIABLES
# ============================================================================

missing_page2b_scores <- setdiff(
  page2b_sector_map$score_variable,
  names(main_dataset)
)


if (length(missing_page2b_scores) > 0) {
  
  stop(
    paste0(
      "\nThe following sector scores required for Page 2b are missing:\n\n",
      paste(
        missing_page2b_scores,
        collapse = "\n"
      ),
      "\n\nRun 04_msni.R first."
    )
  )
}


# ============================================================================
# 24. PAGE 2B - CREATE THE 64 POSSIBLE COMBINATIONS
# ============================================================================

sector_labels_page2b <- page2b_sector_map$sector_label


# Generate combinations in the same order as the table:
#
# None
# singles
# pairs
# triples
# quadruples
# quintuples
# all six

option_sets <- list()


for (number_of_sectors in 0:length(sector_labels_page2b)) {
  
  if (number_of_sectors == 0) {
    
    option_sets <- c(
      option_sets,
      list(character(0))
    )
    
  } else {
    
    new_sets <- utils::combn(
      sector_labels_page2b,
      number_of_sectors,
      simplify = FALSE
    )
    
    option_sets <- c(
      option_sets,
      new_sets
    )
  }
}


# Safety check

if (length(option_sets) != 64) {
  
  stop(
    "Page 2b should generate exactly 64 sector combinations."
  )
}


# ============================================================================
# 25. PAGE 2B - CREATE OPTION LOOKUP TABLE
# ============================================================================

option_map <- tibble::tibble(
  
  option_number = seq_along(
    option_sets
  ),
  
  option_code = sprintf(
    "option_%02d",
    seq_along(option_sets)
  ),
  
  option_label = vapply(
    
    option_sets,
    
    function(x) {
      
      if (length(x) == 0) {
        
        "None"
        
      } else {
        
        paste(
          x,
          collapse = " + "
        )
      }
    },
    
    FUN.VALUE = character(1)
  )
)


# Add six binary columns corresponding to your table

for (sector_label in sector_labels_page2b) {
  
  new_column <- tolower(
    sector_label
  )
  
  option_map[[new_column]] <- vapply(
    
    option_sets,
    
    function(x) {
      
      as.integer(
        sector_label %in% x
      )
    },
    
    FUN.VALUE = integer(1)
  )
}


# Explicitly put columns in desired order

option_map <- option_map %>%
  
  dplyr::rename(
    SECAL = secal,
    WASH = wash,
    SNFI = snfi,
    Education = education,
    Protection = protection,
    Health = health
  )


# Create six-digit signature
#
# Examples:
#
# None:
#   000000
#
# SECAL:
#   100000
#
# SECAL + WASH:
#   110000
#
# All:
#   111111

option_map$signature <- apply(
  
  option_map %>%
    dplyr::select(
      SECAL,
      WASH,
      SNFI,
      Education,
      Protection,
      Health
    ),
  
  1,
  
  paste0,
  
  collapse = ""
)


# ============================================================================
# 26. PAGE 2B - PREPARE HOUSEHOLD SECTOR SCORES
# ============================================================================

page2b_dataset <- bulletin_dataset


page2b_score_vars <- page2b_sector_map$score_variable


# Convert scores safely to numeric

page2b_score_matrix <- sapply(
  
  page2b_score_vars,
  
  function(variable_name) {
    
    suppressWarnings(
      as.numeric(
        as.character(
          page2b_dataset[[variable_name]]
        )
      )
    )
  }
)


colnames(
  page2b_score_matrix
) <- sector_labels_page2b


# ============================================================================
# 27. PAGE 2B - HOUSEHOLDS WITH COMPLETE SIX-SECTOR INFORMATION
# ============================================================================

page2b_complete_sector_scores <- apply(
  
  !is.na(
    page2b_score_matrix
  ),
  
  1,
  
  all
)


number_incomplete_page2b <- sum(
  !page2b_complete_sector_scores
)


if (number_incomplete_page2b > 0) {
  
  warning(
    paste0(
      number_incomplete_page2b,
      " households do not have valid scores for all six sectors. ",
      "Their Page 2b combination is therefore treated as NA."
    )
  )
}


# ============================================================================
# 28. PAGE 2B - CREATE 3-5 NEED COMBINATION
# ============================================================================

flags_345 <- ifelse(
  
  is.na(
    page2b_score_matrix
  ),
  
  0L,
  
  ifelse(
    page2b_score_matrix >= 3,
    1L,
    0L
  )
)


signature_345 <- apply(
  flags_345,
  1,
  paste0,
  collapse = ""
)


signature_345[
  !page2b_complete_sector_scores
] <- NA_character_


signature_to_option <- stats::setNames(
  option_map$option_code,
  option_map$signature
)


page2b_dataset$bulletin_combination_345 <- unname(
  signature_to_option[
    signature_345
  ]
)


# ============================================================================
# 29. PAGE 2B - CREATE 4-5 SEVERE-NEED COMBINATION
# ============================================================================

flags_45 <- ifelse(
  
  is.na(
    page2b_score_matrix
  ),
  
  0L,
  
  ifelse(
    page2b_score_matrix >= 4,
    1L,
    0L
  )
)


signature_45 <- apply(
  flags_45,
  1,
  paste0,
  collapse = ""
)


signature_45[
  !page2b_complete_sector_scores
] <- NA_character_


page2b_dataset$bulletin_combination_45 <- unname(
  signature_to_option[
    signature_45
  ]
)


# ============================================================================
# 30. PAGE 2B - GEOGRAPHIC LEVELS
# ============================================================================

# As requested:
#
# Country
# Admin 1
#
# No Admin 2 on Page 2b at this stage.

page2b_geography_map <- list(
  
  list(
    variable = "country",
    label = "Country"
  ),
  
  list(
    variable = "Admin_1_nom",
    label = "Admin 1"
  )
)


page2b_admin_order <- c(
  "Country",
  "Admin 1"
)


# ============================================================================
# 31. PAGE 2B - HELPER TO CALCULATE COMBINATION DISTRIBUTION
# ============================================================================

calculate_page2b_distribution <- function(
    combination_variable
) {
  
  results <- list()
  
  
  for (g in seq_along(page2b_geography_map)) {
    
    geo_var <- page2b_geography_map[[g]]$variable
    
    geo_label <- page2b_geography_map[[g]]$label
    
    
    results[[g]] <- analyse_distribution_geography(
      
      dataset = page2b_dataset,
      
      variable = combination_variable,
      
      geo_var = geo_var,
      
      admin_level = geo_label,
      
      expected_categories = option_map$option_code
    )
  }
  
  
  dplyr::bind_rows(
    results
  ) %>%
    
    dplyr::rename(
      option_code = severity_category
    ) %>%
    
    dplyr::select(
      admin_level,
      area,
      population,
      option_code,
      severity_percentage
    )
}


# ============================================================================
# 32. PAGE 2B - CALCULATE 3-5 COMBINATIONS
# ============================================================================

message(
  "Page 2b: calculating exact combinations for severity 3-5"
)


page2b_345 <- calculate_page2b_distribution(
  "bulletin_combination_345"
) %>%
  
  dplyr::rename(
    need_345_percentage = severity_percentage
  )


# ============================================================================
# 33. PAGE 2B - CALCULATE 4-5 COMBINATIONS
# ============================================================================

message(
  "Page 2b: calculating exact combinations for severity 4-5"
)


page2b_45 <- calculate_page2b_distribution(
  "bulletin_combination_45"
) %>%
  
  dplyr::rename(
    severe_45_percentage = severity_percentage
  )


# ============================================================================
# 34. PAGE 2B - COMBINE 3-5 AND 4-5 RESULTS
# ============================================================================

page2b <- page2b_345 %>%
  
  dplyr::full_join(
    
    page2b_45,
    
    by = c(
      "admin_level",
      "area",
      "population",
      "option_code"
    )
  ) %>%
  
  dplyr::left_join(
    
    option_map %>%
      
      dplyr::select(
        option_number,
        option_code,
        option_label,
        SECAL,
        WASH,
        SNFI,
        Education,
        Protection,
        Health
      ),
    
    by = "option_code"
  )


# ============================================================================
# 35. PAGE 2B - CONCATENATE
# ============================================================================

# Use AREA rather than generic admin_level so the lookup is unique.
#
# Examples:
#
# option_08.<country>.All
# option_08.<country>.PDI
# option_08.Diffa.All
# option_08.Diffa.PDI

page2b <- page2b %>%
  
  dplyr::mutate(
    
    concatenate = paste(
      option_code,
      area,
      population,
      sep = "."
    )
  )


# ============================================================================
# 36. PAGE 2B - ORDER
# ============================================================================

page2b <- page2b %>%
  
  dplyr::mutate(
    
    admin_order_internal = match(
      admin_level,
      page2b_admin_order
    ),
    
    population_order_internal = match(
      population,
      population_order
    )
    
  ) %>%
  
  dplyr::arrange(
    option_number,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  
  dplyr::select(
    
    option_number,
    option_code,
    option_label,
    
    SECAL,
    WASH,
    SNFI,
    Education,
    Protection,
    Health,
    
    admin_level,
    area,
    population,
    
    concatenate,
    
    need_345_percentage,
    severe_45_percentage
  )


# ============================================================================
# 37. PAGE 2B - VALIDATION
# ============================================================================

if (
  dplyr::n_distinct(
    page2b$option_code
  ) != 64
) {
  
  stop(
    "Page 2b does not contain all 64 expected combinations."
  )
}


# Check percentages are valid

for (
  percentage_variable in
  c(
    "need_345_percentage",
    "severe_45_percentage"
  )
) {
  
  if (
    any(
      page2b[[percentage_variable]] < 0 |
      page2b[[percentage_variable]] > 1,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Invalid percentage found in ",
      percentage_variable,
      "."
    )
  }
}


# Check duplicate rows

page2b_duplicate_check <- page2b %>%
  
  dplyr::count(
    option_code,
    admin_level,
    area,
    population
  ) %>%
  
  dplyr::filter(
    n > 1
  )


if (
  nrow(
    page2b_duplicate_check
  ) > 0
) {
  
  stop(
    "Duplicate option/geography/population rows found on Page 2b."
  )
}


# ============================================================================
# 38. PAGE 2B - QUALITY CHECK:
#     ALL 64 OPTIONS MUST SUM TO 100%
# ============================================================================

page2b_sum_check <- page2b %>%
  
  dplyr::group_by(
    admin_level,
    area,
    population
  ) %>%
  
  dplyr::summarise(
    
    total_345 = sum(
      need_345_percentage,
      na.rm = TRUE
    ),
    
    total_45 = sum(
      severe_45_percentage,
      na.rm = TRUE
    ),
    
    all_345_missing = all(
      is.na(
        need_345_percentage
      )
    ),
    
    all_45_missing = all(
      is.na(
        severe_45_percentage
      )
    ),
    
    .groups = "drop"
  )


page2b_sum_problems <- page2b_sum_check %>%
  
  dplyr::filter(
    
    (
      !all_345_missing &
        abs(
          total_345 - 1
        ) > 0.01
    ) |
      
      (
        !all_45_missing &
          abs(
            total_45 - 1
          ) > 0.01
      )
  )


if (
  nrow(
    page2b_sum_problems
  ) > 0
) {
  
  warning(
    paste0(
      nrow(
        page2b_sum_problems
      ),
      " Page 2b distributions do not sum to approximately 100%. ",
      "Inspect object 'page2b_sum_problems'."
    )
  )
}

# ============================================================================
# ============================================================================
# PAGE 3A
# DEMOGRAPHIC / SOCIOECONOMIC DISAGGREGATIONS
# ============================================================================
# ============================================================================
#
# For MSNI + each sector:
#
#   need_345_percentage  = % HHs at severity 3, 4 or 5
#   severe_45_percentage = % HHs at severity 4 or 5
#
# Disaggregated by:
#
#   1. Gender of HoHH
#   2. Age of HoHH
#   3. Civil status of HoHH
#   4. Education of HoHH
#   5. Household size
#   6. Household member with disability
#   7. Main language
#   8. Weighted national income quintile
#   9. Crop/livestock/fishing activity
#  10. Setting
#
# Geography:
#   Country
#   Admin 1
#   Admin 2
#
# Population:
#   All
#   PND
#   PDI
#   REF
#   RET
#
# Also provides:
#   n_unweighted
#   n_weighted
# ============================================================================


# ============================================================================
# PAGE 3A - SAFETY CHECKS
# ============================================================================

# wgq_dis_3 is deliberately not required here.
# If absent, Page 3a recreates the household disability indicator from
# the WG-SS individual loop.

page3_required_columns <- c(
  "uuid",
  "weights",
  "Pol",
  "country",
  "Admin_1_nom",
  "Admin_2_nom",
  "hoh_gender_final",
  "hoh_age_final",
  "hoh_civil_status",
  "hoh_edu",
  "hh_size",
  "hh_main_language_spoken",
  "cm_income_total",
  "fsl_liv_act",
  "setting",
  "msni_score",
  "comp_foodsec_score",
  "comp_wash_score",
  "comp_snfi_score",
  "comp_edu_score",
  "comp_prot_score",
  "comp_health_score"
)

missing_page3_columns <- setdiff(
  page3_required_columns,
  names(main_dataset)
)

if (length(missing_page3_columns) > 0) {
  stop(
    paste0(
      "\nThe following Page 3a variables are missing:\n\n",
      paste(missing_page3_columns, collapse = "\n"),
      "\n\nCheck 04_msni.R and the input dataset."
    )
  )
}


# ============================================================================
# PAGE 3A - CREATE HOUSEHOLD DISABILITY INDICATOR IF NEEDED
# ============================================================================

if (!"wgq_dis_3" %in% names(main_dataset)) {
  
  message(
    "Page 3a: wgq_dis_3 not found in main_dataset. ",
    "Recreating household disability indicator from wgss_indiv."
  )
  
  # MALI: no WG-SS loop in the Mali dataset; the WG-SS questions (wgq_*) are
  # asked in the roster and the person's age is ind_age. The roster is used
  # first (the wgss_indiv object built in 03 from the roster has no
  # wgss_ind_age column).
  if (
    exists("roster") &&
    all(
      c(
        "uuid",
        "ind_age",
        "wgq_vision",
        "wgq_hearing",
        "wgq_mobility",
        "wgq_cognition",
        "wgq_self_care",
        "wgq_communication"
      ) %in% gsub("/", ".", names(roster))
    )
  ) {

    message("Page 3a: WG-SS questions taken from the roster (ind_age).")
    wgss_page3 <- roster
    names(wgss_page3) <- gsub("/", ".", names(wgss_page3))
    wgss_page3$wgss_ind_age <- wgss_page3$ind_age

  } else if (exists("wgss_indiv")) {
    wgss_page3 <- wgss_indiv
  } else {

    if (!exists("file_name_dataset")) {
      stop(
        paste0(
          "Page 3a needs the WG-SS individual loop to recreate wgq_dis_3, ",
          "but neither `wgss_indiv` nor `file_name_dataset` is available. ",
          "Run the country-specific 02_load_data.R first."
        )
      )
    }

    wgss_page3 <- readxl::read_xlsx(
      path = file_name_dataset,
      sheet = "wgss_indiv"
    )
  }

  names(wgss_page3) <- gsub("/", ".", names(wgss_page3))
  
  wgss_domains <- c(
    "wgq_vision",
    "wgq_hearing",
    "wgq_mobility",
    "wgq_cognition",
    "wgq_self_care",
    "wgq_communication"
  )
  
  wgss_required_columns <- c(
    "uuid",
    "wgss_ind_age",
    wgss_domains
  )
  
  missing_wgss_columns <- setdiff(
    wgss_required_columns,
    names(wgss_page3)
  )
  
  if (length(missing_wgss_columns) > 0) {
    stop(
      paste0(
        "\nCannot recreate wgq_dis_3. The following variables are missing ",
        "from wgss_indiv:\n\n",
        paste(missing_wgss_columns, collapse = "\n")
      )
    )
  }
  
  valid_wgss_answers <- c(
    "no_difficulty",
    "some_difficulty",
    "a_lot_of_difficulty",
    "cannot_do_at_all"
  )
  
  severe_wgss_answers <- c(
    "a_lot_of_difficulty",
    "cannot_do_at_all"
  )
  
  wgss_page3 <- wgss_page3 %>%
    dplyr::mutate(
      wgss_age_numeric = suppressWarnings(
        as.numeric(as.character(wgss_ind_age))
      ),
      wgss_has_valid_response =
        wgss_age_numeric >= 5 &
        dplyr::if_any(
          dplyr::all_of(wgss_domains),
          ~ as.character(.x) %in% valid_wgss_answers
        ),
      wgss_disability_person =
        wgss_age_numeric >= 5 &
        dplyr::if_any(
          dplyr::all_of(wgss_domains),
          ~ as.character(.x) %in% severe_wgss_answers
        )
    )
  
  page3_disability_hh <- wgss_page3 %>%
    dplyr::filter(
      !is.na(uuid),
      as.character(uuid) != ""
    ) %>%
    dplyr::group_by(uuid) %>%
    dplyr::summarise(
      wgq_dis_3 = dplyr::case_when(
        any(wgss_disability_person, na.rm = TRUE) ~ 1L,
        any(wgss_has_valid_response, na.rm = TRUE) ~ 0L,
        TRUE ~ NA_integer_
      ),
      .groups = "drop"
    )
  
  main_dataset <- main_dataset %>%
    dplyr::left_join(
      page3_disability_hh,
      by = "uuid"
    )
  
  message("Page 3a: wgq_dis_3 successfully recreated.")
}

if (!"wgq_dis_3" %in% names(main_dataset)) {
  stop("Page 3a could not create wgq_dis_3.")
}


# ============================================================================
# PAGE 3A - START DATASET
# ============================================================================

page3_dataset <- main_dataset


# ============================================================================
# PAGE 3A - HELPER: KEEP ONLY EXPECTED CATEGORIES
# ============================================================================

clean_page3_category <- function(x, allowed, variable_name) {

  x <- as.character(x)
  x[x %in% c("", "NA")] <- NA_character_

  unexpected <- setdiff(
    unique(x[!is.na(x)]),
    allowed
  )

  if (length(unexpected) > 0) {
    message(
      paste0(
        "Page 3a: additional country-specific value(s) in ",
        variable_name,
        ": ",
        paste(unexpected, collapse = ", "),
        ". They will be retained and added to the category universe."
      )
    )
  }

  # `allowed` is a preferred order only. Country-specific values are retained.
  x
}


# ============================================================================
# PAGE 3A - 1. GENDER OF HoHH
# ============================================================================

page3_dataset$page3_hoh_gender <- clean_page3_category(
  page3_dataset$hoh_gender_final,
  allowed = c("male", "female", "pnta"),
  variable_name = "hoh_gender_final"
)


# ============================================================================
# PAGE 3A - 2. AGE OF HoHH
# ============================================================================

page3_hoh_age_numeric <- suppressWarnings(
  as.numeric(as.character(page3_dataset$hoh_age_final))
)

invalid_page3_ages <- sum(
  !is.na(page3_hoh_age_numeric) &
    (page3_hoh_age_numeric < 18 | page3_hoh_age_numeric > 120)
)

if (invalid_page3_ages > 0) {
  warning(
    invalid_page3_ages,
    " HoHH age value(s) fall outside 18-120 and will not be included ",
    "in the Page 3a age disaggregation."
  )
}

page3_dataset$page3_hoh_age <- dplyr::case_when(
  page3_hoh_age_numeric >= 18 & page3_hoh_age_numeric <= 24 ~ "18-24",
  page3_hoh_age_numeric >= 25 & page3_hoh_age_numeric <= 59 ~ "25-59",
  page3_hoh_age_numeric >= 60 & page3_hoh_age_numeric <= 120 ~ "60+",
  TRUE ~ NA_character_
)


# ============================================================================
# PAGE 3A - 3. CIVIL STATUS OF HoHH
# ============================================================================

page3_dataset$page3_hoh_civil_status <- clean_page3_category(
  page3_dataset$hoh_civil_status,
  allowed = c(
    "single",
    "married",
    "divorced",
    "widowed",
    "other",
    "dnk",
    "pnta"
  ),
  variable_name = "hoh_civil_status"
)


# ============================================================================
# PAGE 3A - 4. EDUCATION OF HoHH
# ============================================================================

page3_dataset$page3_hoh_education <- clean_page3_category(
  page3_dataset$hoh_edu,
  allowed = c(
    "none",
    "literate_quranic_education",
    "lower_elementary_school",
    "upper_elementary_school",
    "secondary_school",
    "higher_education"
  ),
  variable_name = "hoh_edu"
)


# ============================================================================
# PAGE 3A - 5. HOUSEHOLD SIZE
# ============================================================================

page3_hh_size_numeric <- suppressWarnings(
  as.numeric(as.character(page3_dataset$hh_size))
)

page3_dataset$page3_hh_size <- dplyr::case_when(
  page3_hh_size_numeric >= 1 & page3_hh_size_numeric <= 4 ~ "1-4",
  page3_hh_size_numeric >= 5 & page3_hh_size_numeric <= 8 ~ "5-8",
  page3_hh_size_numeric >= 9 & page3_hh_size_numeric <= 12 ~ "9-12",
  page3_hh_size_numeric >= 13 ~ "13+",
  TRUE ~ NA_character_
)


# ============================================================================
# PAGE 3A - 6. HOUSEHOLD MEMBER WITH DISABILITY
# ============================================================================

page3_disability_raw <- tolower(
  as.character(page3_dataset$wgq_dis_3)
)

page3_dataset$page3_disability <- dplyr::case_when(
  page3_disability_raw %in% c("1", "true", "yes") ~ "yes",
  page3_disability_raw %in% c("0", "false", "no") ~ "no",
  TRUE ~ NA_character_
)


# ============================================================================
# PAGE 3A - 7. MAIN LANGUAGE SPOKEN
# ============================================================================

page3_dataset$page3_language <- clean_page3_category(
  page3_dataset$hh_main_language_spoken,
  allowed = c(
    "haoussa",
    "zarma",
    "tamasheq",
    "kanuri",
    "fulfude",
    "arabe",
    "songhai",
    "toubou",
    "boudouma",
    "gurmanchema",
    "tegui",
    "francais",
    "braille",
    "other",
    "dnk",
    "pnta"
  ),
  variable_name = "hh_main_language_spoken"
)


# ============================================================================
# PAGE 3A - 8. WEIGHTED NATIONAL INCOME QUINTILE
# ============================================================================

# First use cm_income_total_qtl when it contains recognisable labels.
# Otherwise calculate weighted national cut-points directly.

use_existing_income_quintile <- FALSE

if ("cm_income_total_qtl" %in% names(page3_dataset)) {
  
  existing_income_qtl <- tolower(
    as.character(page3_dataset$cm_income_total_qtl)
  )
  
  observed_income_qtl <- unique(
    existing_income_qtl[!is.na(existing_income_qtl)]
  )
  
  recognised_income_qtl <- c(
    "poorest",
    "poorer",
    "middle",
    "richer",
    "richest"
  )
  
  if (
    length(observed_income_qtl) > 0 &&
    all(observed_income_qtl %in% recognised_income_qtl)
  ) {
    
    use_existing_income_quintile <- TRUE
    
    page3_dataset$page3_income_quintile <- dplyr::recode(
      existing_income_qtl,
      "poorest" = "poorest",
      "poorer" = "poor",
      "middle" = "middle",
      "richer" = "rich",
      "richest" = "richest",
      .default = NA_character_
    )
  }
}

if (!use_existing_income_quintile) {
  
  message("Page 3a: calculating weighted national income quintiles directly.")
  
  page3_income_numeric <- suppressWarnings(
    as.numeric(as.character(page3_dataset$cm_income_total))
  )
  
  page3_income_numeric[page3_income_numeric == -999] <- NA_real_
  
  page3_weight_numeric <- suppressWarnings(
    as.numeric(as.character(page3_dataset$weights))
  )
  
  weighted_quantile_page3 <- function(x, w, probs) {
    
    valid <-
      !is.na(x) &
      !is.na(w) &
      is.finite(x) &
      is.finite(w) &
      w > 0
    
    if (!any(valid)) {
      return(rep(NA_real_, length(probs)))
    }
    
    x <- x[valid]
    w <- w[valid]
    
    ordering <- order(x)
    x <- x[ordering]
    w <- w[ordering]
    
    cumulative_weight <- cumsum(w) / sum(w)
    
    vapply(
      probs,
      function(p) {
        first_position <- which(cumulative_weight >= p)[1]
        x[first_position]
      },
      FUN.VALUE = numeric(1)
    )
  }
  
  page3_income_cutpoints <- weighted_quantile_page3(
    x = page3_income_numeric,
    w = page3_weight_numeric,
    probs = c(0.20, 0.40, 0.60, 0.80)
  )
  
  if (any(is.na(page3_income_cutpoints))) {
    stop("Page 3a could not calculate weighted income quintile cut-points.")
  }
  
  page3_dataset$page3_income_quintile <- dplyr::case_when(
    is.na(page3_income_numeric) ~ NA_character_,
    page3_income_numeric <= page3_income_cutpoints[1] ~ "poorest",
    page3_income_numeric <= page3_income_cutpoints[2] ~ "poor",
    page3_income_numeric <= page3_income_cutpoints[3] ~ "middle",
    page3_income_numeric <= page3_income_cutpoints[4] ~ "rich",
    page3_income_numeric > page3_income_cutpoints[4] ~ "richest",
    TRUE ~ NA_character_
  )
}


# ============================================================================
# PAGE 3A - 9. CROP / LIVESTOCK / FISHING ACTIVITY
# ============================================================================

page3_dataset$page3_livelihood_activity <- clean_page3_category(
  page3_dataset$fsl_liv_act,
  allowed = c(
    "no",
    "yes_crop_production",
    "livestock",
    "yes_fish_production",
    "crop_livestock",
    "crop_fish",
    "livestock_fish",
    "yes_all_three",
    "dnk",
    "pnta"
  ),
  variable_name = "fsl_liv_act"
)


# ============================================================================
# PAGE 3A - 10. SETTING
# ============================================================================

page3_dataset$page3_setting <- clean_page3_category(
  page3_dataset$setting,
  allowed = c(
    "rural",
    "urban",
    "camp_formal",
    "camp_informal"
  ),
  variable_name = "setting"
)


# ============================================================================
# PAGE 3A - MULTI-COUNTRY CATEGORY ORDER HELPER
# ============================================================================

bulletin_get_kobo_choices <- function(question_name, observed = character(0)) {

  observed <- unique(as.character(observed))
  observed <- observed[!is.na(observed) & observed != "" & observed != "NA"]

  # Prefer the Kobo choice-list order when survey metadata are available.
  if (exists("tool.survey") && exists("tool.choices")) {

    survey_row <- tool.survey %>%
      dplyr::filter(name == question_name) %>%
      dplyr::slice(1)

    if (nrow(survey_row) == 1 && "type" %in% names(survey_row)) {

      qtype <- as.character(survey_row$type[1])

      if (grepl("^select_(one|multiple)\\s+", qtype)) {

        list_name <- sub("^select_(one|multiple)\\s+", "", qtype)

        choice_codes <- tool.choices %>%
          dplyr::filter(.data$list_name == .env$list_name) %>%
          dplyr::pull(name) %>%
          as.character()

        choice_codes <- unique(choice_codes[!is.na(choice_codes) & choice_codes != ""])

        if (length(choice_codes) > 0) {
          # Preserve the questionnaire order, then append any derived/observed
          # code not declared in choices.
          return(c(choice_codes, setdiff(observed, choice_codes)))
        }
      }
    }
  }

  observed
}


bulletin_merge_category_order <- function(preferred, observed, question_name = NULL) {

  observed <- unique(as.character(observed))
  observed <- observed[!is.na(observed) & observed != "" & observed != "NA"]

  if (!is.null(question_name)) {
    kobo <- bulletin_get_kobo_choices(question_name, observed)
    if (length(kobo) > 0) return(kobo)
  }

  c(preferred, setdiff(observed, preferred)) %>% unique()
}


# ============================================================================
# PAGE 3A - DISAGGREGATION MAP
# ============================================================================

page3_disaggregation_map <- tibble::tibble(
  
  disaggregation_order = 1:10,
  
  disaggregation_code = c(
    "hoh_gender",
    "hoh_age",
    "hoh_civil_status",
    "hoh_edu",
    "hh_size",
    "hh_disability",
    "hh_main_language_spoken",
    "income_quintile",
    "fsl_liv_act",
    "setting"
  ),
  
  disaggregation = c(
    "Gender of HoHH",
    "Age of HoHH",
    "Civil status of HoHH",
    "Education level of HoHH",
    "Household size",
    "Household member with disability",
    "Main language spoken",
    "Income quintile",
    "Crop / livestock / fishing activity",
    "Setting"
  ),
  
  page3_variable = c(
    "page3_hoh_gender",
    "page3_hoh_age",
    "page3_hoh_civil_status",
    "page3_hoh_education",
    "page3_hh_size",
    "page3_disability",
    "page3_language",
    "page3_income_quintile",
    "page3_livelihood_activity",
    "page3_setting"
  ),
  
  categories = list(
    c("male", "female", "pnta"),
    c("18-24", "25-59", "60+"),
    c("single", "married", "divorced", "widowed", "other", "dnk", "pnta"),
    c(
      "none",
      "literate_quranic_education",
      "lower_elementary_school",
      "upper_elementary_school",
      "secondary_school",
      "higher_education"
    ),
    c("1-4", "5-8", "9-12", "13+"),
    c("no", "yes"),
    c(
      "haoussa",
      "zarma",
      "tamasheq",
      "kanuri",
      "fulfude",
      "arabe",
      "songhai",
      "toubou",
      "boudouma",
      "gurmanchema",
      "tegui",
      "francais",
      "braille",
      "other",
      "dnk",
      "pnta"
    ),
    c("poorest", "poor", "middle", "rich", "richest"),
    c(
      "no",
      "yes_crop_production",
      "livestock",
      "yes_fish_production",
      "crop_livestock",
      "crop_fish",
      "livestock_fish",
      "yes_all_three",
      "dnk",
      "pnta"
    ),
    c("rural", "urban", "camp_formal", "camp_informal")
  )
)


# Replace Niger-oriented preferred lists with each country's actual category
# universe. Kobo order is used when available; otherwise the preferred order is
# kept and any additional observed categories are appended.

page3_disaggregation_map$categories[[1]] <- bulletin_merge_category_order(
  page3_disaggregation_map$categories[[1]],
  page3_dataset$page3_hoh_gender,
  "hoh_gender_final"
)

page3_disaggregation_map$categories[[3]] <- bulletin_merge_category_order(
  page3_disaggregation_map$categories[[3]],
  page3_dataset$page3_hoh_civil_status,
  "hoh_civil_status"
)

page3_disaggregation_map$categories[[4]] <- bulletin_merge_category_order(
  page3_disaggregation_map$categories[[4]],
  page3_dataset$page3_hoh_education,
  "hoh_edu"
)

page3_disaggregation_map$categories[[7]] <- bulletin_merge_category_order(
  character(0),
  page3_dataset$page3_language,
  "hh_main_language_spoken"
)

page3_disaggregation_map$categories[[9]] <- bulletin_merge_category_order(
  page3_disaggregation_map$categories[[9]],
  page3_dataset$page3_livelihood_activity,
  "fsl_liv_act"
)

page3_disaggregation_map$categories[[10]] <- bulletin_merge_category_order(
  page3_disaggregation_map$categories[[10]],
  page3_dataset$page3_setting,
  "setting"
)

# ============================================================================
# PAGE 3A - SECTOR / MSNI MAP
# ============================================================================

page3_sector_map <- tibble::tribble(
  ~sector_order, ~sector_code, ~sector,         ~score_variable,
  1,             "msni",       "MSNI",          "msni_score",
  2,             "fs",         "Food Security", "comp_foodsec_score",
  3,             "wash",       "WASH",          "comp_wash_score",
  4,             "snfi",       "SNFI",          "comp_snfi_score",
  5,             "education",  "Education",     "comp_edu_score",
  6,             "protection", "Protection",    "comp_prot_score",
  7,             "health",     "Health",        "comp_health_score"
)


# ============================================================================
# PAGE 3A - GEOGRAPHY MAP
# ============================================================================

page3_geography_map <- list(
  list(variable = "country",     label = "Country"),
  list(variable = "Admin_1_nom", label = "Admin 1"),
  list(variable = "Admin_2_nom", label = "Admin 2")
)

page3_admin_order <- c(
  "Country",
  "Admin 1",
  "Admin 2"
)


# ============================================================================
# PAGE 3A - CORE ANALYSIS FUNCTION
# ============================================================================

analyse_page3_group <- function(
    dataset,
    score_variable,
    disaggregation_variable,
    expected_categories,
    geography_variable,
    admin_level
) {
  
  expected_categories <- as.character(expected_categories)
  
  work <- dataset %>%
    dplyr::transmute(
      area = as.character(.data[[geography_variable]]),
      category = as.character(.data[[disaggregation_variable]]),
      population_raw = as.character(Pol),
      score = suppressWarnings(
        as.numeric(as.character(.data[[score_variable]]))
      ),
      weights = suppressWarnings(
        as.numeric(as.character(weights))
      )
    ) %>%
    dplyr::filter(
      !is.na(area),
      area != "",
      !is.na(category),
      category %in% expected_categories,
      !is.na(score),
      score %in% 1:5,
      !is.na(weights),
      is.finite(weights),
      weights > 0
    )
  
  area_universe <- dataset %>%
    dplyr::transmute(
      area = as.character(.data[[geography_variable]])
    ) %>%
    dplyr::filter(
      !is.na(area),
      area != ""
    ) %>%
    dplyr::distinct()
  
  
  # --------------------------------------------------------------------------
  # A. ALL POPULATIONS
  # --------------------------------------------------------------------------
  
  all_summary <- work %>%
    dplyr::group_by(area, category) %>%
    dplyr::summarise(
      n_unweighted = dplyr::n(),
      n_weighted = sum(weights),
      need_345_percentage = sum(
        weights * as.integer(score >= 3)
      ) / sum(weights),
      severe_45_percentage = sum(
        weights * as.integer(score >= 4)
      ) / sum(weights),
      .groups = "drop"
    )
  
  all_universe <- tidyr::crossing(
    area = area_universe$area,
    category = expected_categories
  )
  
  all_results <- all_universe %>%
    dplyr::left_join(
      all_summary,
      by = c("area", "category")
    ) %>%
    dplyr::mutate(
      population = "All",
      admin_level = admin_level,
      n_unweighted = tidyr::replace_na(n_unweighted, 0L),
      n_weighted = tidyr::replace_na(n_weighted, 0)
    )
  
  
  # --------------------------------------------------------------------------
  # B. BY POPULATION
  # --------------------------------------------------------------------------
  
  population_work <- work %>%
    dplyr::filter(
      population_raw %in% unname(population_map)
    ) %>%
    dplyr::mutate(
      population = unname(
        population_reverse_map[population_raw]
      )
    )
  
  population_summary <- population_work %>%
    dplyr::group_by(area, category, population) %>%
    dplyr::summarise(
      n_unweighted = dplyr::n(),
      n_weighted = sum(weights),
      need_345_percentage = sum(
        weights * as.integer(score >= 3)
      ) / sum(weights),
      severe_45_percentage = sum(
        weights * as.integer(score >= 4)
      ) / sum(weights),
      .groups = "drop"
    )
  
  population_universe <- tidyr::crossing(
    area = area_universe$area,
    category = expected_categories,
    population = names(population_map)
  )
  
  population_results <- population_universe %>%
    dplyr::left_join(
      population_summary,
      by = c("area", "category", "population")
    ) %>%
    dplyr::mutate(
      admin_level = admin_level,
      n_unweighted = tidyr::replace_na(n_unweighted, 0L),
      n_weighted = tidyr::replace_na(n_weighted, 0)
    )
  
  dplyr::bind_rows(
    all_results,
    population_results
  ) %>%
    dplyr::select(
      admin_level,
      area,
      population,
      category,
      need_345_percentage,
      severe_45_percentage,
      n_unweighted,
      n_weighted
    )
}


# ============================================================================
# PAGE 3A - RUN ALL ANALYSES
# ============================================================================

page3_results <- list()
page3_counter <- 1

for (sector_index in seq_len(nrow(page3_sector_map))) {
  
  sector_code <- page3_sector_map$sector_code[sector_index]
  sector_name <- page3_sector_map$sector[sector_index]
  score_variable <- page3_sector_map$score_variable[sector_index]
  sector_order <- page3_sector_map$sector_order[sector_index]
  
  for (disagg_index in seq_len(nrow(page3_disaggregation_map))) {
    
    disaggregation_code <-
      page3_disaggregation_map$disaggregation_code[disagg_index]
    
    disaggregation_name <-
      page3_disaggregation_map$disaggregation[disagg_index]
    
    disaggregation_variable <-
      page3_disaggregation_map$page3_variable[disagg_index]
    
    disaggregation_order <-
      page3_disaggregation_map$disaggregation_order[disagg_index]
    
    expected_categories <-
      page3_disaggregation_map$categories[[disagg_index]]
    
    message(
      "Page 3a: ",
      sector_code,
      " x ",
      disaggregation_code
    )
    
    geography_results <- list()
    
    for (geography_index in seq_along(page3_geography_map)) {
      
      geo_variable <-
        page3_geography_map[[geography_index]]$variable
      
      geo_label <-
        page3_geography_map[[geography_index]]$label
      
      geography_results[[geography_index]] <-
        analyse_page3_group(
          dataset = page3_dataset,
          score_variable = score_variable,
          disaggregation_variable = disaggregation_variable,
          expected_categories = expected_categories,
          geography_variable = geo_variable,
          admin_level = geo_label
        )
    }
    
    current_result <- dplyr::bind_rows(
      geography_results
    ) %>%
      dplyr::mutate(
        sector_order = sector_order,
        sector_code = sector_code,
        sector = sector_name,
        score_variable = score_variable,
        disaggregation_order = disaggregation_order,
        disaggregation_code = disaggregation_code,
        disaggregation = disaggregation_name,
        category_order = match(
          category,
          expected_categories
        )
      )
    
    page3_results[[page3_counter]] <- current_result
    page3_counter <- page3_counter + 1
  }
}

page3 <- dplyr::bind_rows(page3_results)


# ============================================================================
# PAGE 3A - CONCATENATE
# ============================================================================

# Examples:
# wash.hoh_gender.female.Country.<country>.PDI
# wash.hoh_gender.female.Admin 1.Diffa.PDI
# msni.income_quintile.poorest.Admin 2.<area>.All

page3 <- page3 %>%
  dplyr::mutate(
    concatenate = paste(
      sector_code,
      disaggregation_code,
      category,
      admin_level,
      area,
      population,
      sep = "."
    )
  )


# ============================================================================
# PAGE 3A - ORDER
# ============================================================================

page3 <- page3 %>%
  dplyr::mutate(
    admin_order_internal = match(
      admin_level,
      page3_admin_order
    ),
    population_order_internal = match(
      population,
      population_order
    )
  ) %>%
  dplyr::arrange(
    sector_order,
    disaggregation_order,
    category_order,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  dplyr::select(
    sector_code,
    sector,
    score_variable,
    disaggregation_code,
    disaggregation,
    category,
    admin_level,
    area,
    population,
    concatenate,
    need_345_percentage,
    severe_45_percentage,
    n_unweighted,
    n_weighted
  )


# ============================================================================
# PAGE 3A - VALIDATION
# ============================================================================

if (dplyr::n_distinct(page3$sector_code) != 7) {
  stop("Page 3a does not contain all 7 expected sector/MSNI outcomes.")
}

if (dplyr::n_distinct(page3$disaggregation_code) != 10) {
  stop("Page 3a does not contain all 10 expected disaggregations.")
}

for (
  page3_percentage_variable in
  c("need_345_percentage", "severe_45_percentage")
) {
  if (
    any(
      page3[[page3_percentage_variable]] < 0 |
      page3[[page3_percentage_variable]] > 1,
      na.rm = TRUE
    )
  ) {
    stop(
      "Page 3a contains an invalid percentage in ",
      page3_percentage_variable,
      "."
    )
  }
}

page3_duplicate_check <- page3 %>%
  dplyr::count(
    sector_code,
    disaggregation_code,
    category,
    admin_level,
    area,
    population
  ) %>%
  dplyr::filter(n > 1)

if (nrow(page3_duplicate_check) > 0) {
  stop("Duplicate Page 3a rows found.")
}

# ============================================================================
# ============================================================================
# PAGE 3B
# MSNI + SECTOR NEED BY GEOGRAPHY AND POPULATION
# ============================================================================
# ============================================================================
#
# Directly calculated from the MSNI / sector score variables.
#
# need_345_percentage:
#   % HHs with score 3, 4 or 5
#
# severe_45_percentage:
#   % HHs with score 4 or 5
#
# Geography:
#   Country
#   Admin 1
#
# Population:
#   All
#   PND
#   PDI
#   REF
#   RET
#
# ============================================================================


# ============================================================================
# PAGE 3B - SECTOR DEFINITIONS
# ============================================================================

page3b_sector_map <- tibble::tribble(
  
  ~sector_order, ~sector_code, ~sector,         ~score_variable,
  
  1,             "msni",       "MSNI",          "msni_score",
  2,             "fs",         "Food Security", "comp_foodsec_score",
  3,             "wash",       "WASH",          "comp_wash_score",
  4,             "snfi",       "SNFI",          "comp_snfi_score",
  5,             "education",  "Education",     "comp_edu_score",
  6,             "protection", "Protection",    "comp_prot_score",
  7,             "health",     "Health",        "comp_health_score"
)


# ============================================================================
# PAGE 3B - SAFETY CHECK
# ============================================================================

missing_page3b_score_vars <- setdiff(
  page3b_sector_map$score_variable,
  names(main_dataset)
)


if (length(missing_page3b_score_vars) > 0) {
  
  stop(
    paste0(
      "\nThe following Page 3b score variables are missing:\n\n",
      paste(
        missing_page3b_score_vars,
        collapse = "\n"
      )
    )
  )
}


# ============================================================================
# PAGE 3B - GEOGRAPHY
# ============================================================================

page3b_geography_map <- list(
  
  list(
    variable = "country",
    label = "Country"
  ),
  
  list(
    variable = "Admin_1_nom",
    label = "Admin 1"
  )
)


page3b_admin_order <- c(
  "Country",
  "Admin 1"
)


# ============================================================================
# PAGE 3B - WORKING DATASET
# ============================================================================

page3b_dataset <- main_dataset


# ============================================================================
# PAGE 3B - RUN CALCULATIONS
# ============================================================================

page3b_results <- list()

page3b_counter <- 1


for (
  sector_index in
  seq_len(
    nrow(
      page3b_sector_map
    )
  )
) {
  
  sector_order <-
    page3b_sector_map$sector_order[sector_index]
  
  sector_code <-
    page3b_sector_map$sector_code[sector_index]
  
  sector_name <-
    page3b_sector_map$sector[sector_index]
  
  score_variable <-
    page3b_sector_map$score_variable[sector_index]
  
  
  message(
    "Page 3b: calculating ",
    sector_code
  )
  
  
  # --------------------------------------------------------------------------
  # Convert sector score to numeric
  # --------------------------------------------------------------------------
  
  current_score <- suppressWarnings(
    as.numeric(
      as.character(
        page3b_dataset[[score_variable]]
      )
    )
  )
  
  
  # --------------------------------------------------------------------------
  # Create binary 3-5 variable
  # --------------------------------------------------------------------------
  
  page3b_dataset$page3b_need_345 <- dplyr::case_when(
    
    is.na(current_score) ~ NA_integer_,
    
    current_score %in% 3:5 ~ 1L,
    
    current_score %in% 1:2 ~ 0L,
    
    TRUE ~ NA_integer_
  )
  
  
  # --------------------------------------------------------------------------
  # Create binary 4-5 variable
  # --------------------------------------------------------------------------
  
  page3b_dataset$page3b_severe_45 <- dplyr::case_when(
    
    is.na(current_score) ~ NA_integer_,
    
    current_score %in% 4:5 ~ 1L,
    
    current_score %in% 1:3 ~ 0L,
    
    TRUE ~ NA_integer_
  )
  
  
  # ==========================================================================
  # A. 3-5
  # ==========================================================================
  
  need_results <- list()
  
  
  for (
    geography_index in
    seq_along(
      page3b_geography_map
    )
  ) {
    
    geo_variable <-
      page3b_geography_map[[geography_index]]$variable
    
    geo_label <-
      page3b_geography_map[[geography_index]]$label
    
    
    need_results[[geography_index]] <-
      analyse_binary_geography(
        
        dataset = page3b_dataset,
        
        variable = "page3b_need_345",
        
        geo_var = geo_variable,
        
        admin_level = geo_label
      )
  }
  
  
  need_results <- dplyr::bind_rows(
    need_results
  ) %>%
    
    dplyr::rename(
      need_345_percentage = percentage
    )
  
  
  # ==========================================================================
  # B. 4-5
  # ==========================================================================
  
  severe_results <- list()
  
  
  for (
    geography_index in
    seq_along(
      page3b_geography_map
    )
  ) {
    
    geo_variable <-
      page3b_geography_map[[geography_index]]$variable
    
    geo_label <-
      page3b_geography_map[[geography_index]]$label
    
    
    severe_results[[geography_index]] <-
      analyse_binary_geography(
        
        dataset = page3b_dataset,
        
        variable = "page3b_severe_45",
        
        geo_var = geo_variable,
        
        admin_level = geo_label
      )
  }
  
  
  severe_results <- dplyr::bind_rows(
    severe_results
  ) %>%
    
    dplyr::rename(
      severe_45_percentage = percentage
    )
  
  
  # ==========================================================================
  # C. COMBINE
  # ==========================================================================
  
  current_page3b <- need_results %>%
    
    dplyr::left_join(
      
      severe_results,
      
      by = c(
        "admin_level",
        "area",
        "population"
      )
    ) %>%
    
    dplyr::mutate(
      
      sector_order = sector_order,
      
      sector_code = sector_code,
      
      sector = sector_name,
      
      score_variable = score_variable
    )
  
  
  page3b_results[[page3b_counter]] <-
    current_page3b
  
  
  page3b_counter <-
    page3b_counter + 1
}


page3b <- dplyr::bind_rows(
  page3b_results
)


# ============================================================================
# PAGE 3B - CONCATENATE
# ============================================================================

# Examples:
#
# msni.Country.<country>.All
# wash.Country.<country>.PDI
# wash.Admin 1.Diffa.PDI

page3b <- page3b %>%
  
  dplyr::mutate(
    
    concatenate = paste(
      sector_code,
      admin_level,
      area,
      population,
      sep = "."
    )
  )


# ============================================================================
# PAGE 3B - ORDER
# ============================================================================

page3b <- page3b %>%
  
  dplyr::mutate(
    
    admin_order_internal = match(
      admin_level,
      page3b_admin_order
    ),
    
    population_order_internal = match(
      population,
      population_order
    )
  ) %>%
  
  dplyr::arrange(
    sector_order,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  
  dplyr::select(
    
    sector_code,
    sector,
    score_variable,
    
    admin_level,
    area,
    population,
    
    concatenate,
    
    need_345_percentage,
    severe_45_percentage
  )


# ============================================================================
# PAGE 3B - VALIDATION
# ============================================================================

if (
  dplyr::n_distinct(
    page3b$sector_code
  ) != 7
) {
  
  stop(
    "Page 3b does not contain all 7 expected MSNI/sector outcomes."
  )
}


# Small numerical tolerance is allowed for floating-point calculations.

page3b_tolerance <- 1e-8


for (
  page3b_percentage_variable in
  c(
    "need_345_percentage",
    "severe_45_percentage"
  )
) {
  
  if (
    any(
      page3b[[page3b_percentage_variable]] <
      -page3b_tolerance |
      page3b[[page3b_percentage_variable]] >
      1 + page3b_tolerance,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Page 3b contains an invalid percentage in ",
      page3b_percentage_variable,
      "."
    )
  }
}


# ============================================================================
# PAGE 3B - DUPLICATE CHECK
# ============================================================================

page3b_duplicate_check <- page3b %>%
  
  dplyr::count(
    sector_code,
    admin_level,
    area,
    population
  ) %>%
  
  dplyr::filter(
    n > 1
  )


if (
  nrow(
    page3b_duplicate_check
  ) > 0
) {
  
  stop(
    "Duplicate sector/geography/population rows found on Page 3b."
  )
}


# ============================================================================
# PAGE 3B - LOGICAL CHECK
# ============================================================================

# Severity 4-5 must always be <= severity 3-5.

page3b_logic_problems <- page3b %>%
  
  dplyr::filter(
    
    !is.na(
      need_345_percentage
    ),
    
    !is.na(
      severe_45_percentage
    ),
    
    severe_45_percentage >
      need_345_percentage +
      page3b_tolerance
  )


if (
  nrow(
    page3b_logic_problems
  ) > 0
) {
  
  stop(
    paste0(
      "Page 3b contains ",
      nrow(
        page3b_logic_problems
      ),
      " row(s) where severity 4-5 is greater than severity 3-5. ",
      "Inspect 'page3b_logic_problems'."
    )
  )
}

# ============================================================================
# PAGE 4 - AAP
# ============================================================================

# ============================================================================
# PAGE 4 - OVERVIEW
# ============================================================================
#
# PART A
#   Preferred modalities and priority needs:
#   - cpa_preferred_modality_1
#   - cpa_preferred_modality_2
#   - cpa_priority_support_ngo_1
#
#   For every answer option:
#   - % of that subgroup with MSNI severity 3-5
#   - % of that subgroup with MSNI severity 4-5
#   - unweighted n
#   - weighted n
#
# PART B
#   Full heat-map source:
#   - rows    = every cpa_priority_support_ngo_1 option
#   - columns = every modality option, combining first OR second preference
#   - prevalence calculated among households mentioning each need
#   - separate calculations for MSNI 3-5 and MSNI 4-5
#   - separate weighted/unweighted denominators for both severity groups
#
# PART C
#   Assistance experience / relevance:
#   - aap_received_assistance_12m
#   - aap_assistance_coverage
#   - aap_relevance_assistance
#   - aap_relevance_assistance_reason
#
# Geography:
#   Country / Admin 1 / Admin 2
#
# Population:
#   All / PND / PDI / REF / RET
# ============================================================================


# ============================================================================
# PAGE 4 - SAFETY CHECKS
# ============================================================================

page4_required_columns <- c(
  "weights",
  "Pol",
  "country",
  "Admin_1_nom",
  "Admin_2_nom",
  "msni_score",
  "cpa_preferred_modality_1",
  "cpa_preferred_modality_2",
  "cpa_priority_support_ngo_1",
  "aap_received_assistance_12m",
  "aap_assistance_coverage",
  "aap_relevance_assistance",
  "aap_relevance_assistance_reason"
)

missing_page4_columns <- setdiff(
  page4_required_columns,
  names(main_dataset)
)

if (length(missing_page4_columns) > 0) {
  stop(
    paste0(
      "\nThe following Page 4 variables are missing:\n\n",
      paste(missing_page4_columns, collapse = "\n"),
      "\n\nCheck the Kobo input and 04_msni.R."
    )
  )
}

page4_dataset <- main_dataset


# ============================================================================
# PAGE 4 - GEOGRAPHY SETTINGS
# ============================================================================

page4_geography_map <- list(
  list(variable = "country",     label = "Country"),
  list(variable = "Admin_1_nom", label = "Admin 1"),
  list(variable = "Admin_2_nom", label = "Admin 2")
)

page4_admin_order <- c(
  "Country",
  "Admin 1",
  "Admin 2"
)


# ============================================================================
# PAGE 4 - ANSWER OPTION DICTIONARIES — MULTI-COUNTRY
# ============================================================================
#
# Rather than hard-coding Niger's answer list, read each question's choices
# directly from the current country's Kobo tool. If metadata are unavailable,
# fall back to answer codes actually observed in the dataset.
# ============================================================================

bulletin_question_dictionary <- function(
    question_code,
    question_type = c("select_one", "select_multiple")
) {

  question_type <- match.arg(question_type)

  # --------------------------------------------------------------------------
  # A. Kobo metadata (preferred: includes zero-response options and labels)
  # --------------------------------------------------------------------------

  if (exists("tool.survey") && exists("tool.choices")) {

    survey_row <- tool.survey %>%
      dplyr::filter(name == question_code) %>%
      dplyr::slice(1)

    if (nrow(survey_row) == 1 && "type" %in% names(survey_row)) {

      raw_type <- as.character(survey_row$type[1])

      if (grepl("^select_(one|multiple)\\s+", raw_type)) {

        list_name <- sub("^select_(one|multiple)\\s+", "", raw_type)

        choice_df <- tool.choices %>%
          dplyr::filter(.data$list_name == .env$list_name) %>%
          dplyr::transmute(
            code = as.character(name),
            label = as.character(label)
          ) %>%
          dplyr::filter(!is.na(code), code != "") %>%
          dplyr::distinct(code, .keep_all = TRUE)

        if (nrow(choice_df) > 0) {
          labels <- stats::setNames(choice_df$label, choice_df$code)
          return(list(options = choice_df$code, labels = labels))
        }
      }
    }
  }

  # --------------------------------------------------------------------------
  # B. Observed-data fallback
  # --------------------------------------------------------------------------

  if (!question_code %in% names(main_dataset)) {
    return(list(options = character(0), labels = character(0)))
  }

  raw <- as.character(main_dataset[[question_code]])
  raw <- raw[!is.na(raw) & trimws(raw) != ""]

  if (question_type == "select_multiple") {
    raw <- gsub("[,;|]+", " ", raw)
    opts <- unlist(strsplit(raw, "\\s+"))
  } else {
    opts <- raw
  }

  opts <- unique(opts[!is.na(opts) & opts != ""])
  labels <- stats::setNames(opts, opts)

  warning(
    "Page 4: Kobo choice metadata unavailable for '",
    question_code,
    "'. Using observed answer codes only."
  )

  list(options = opts, labels = labels)
}


page4_modality_dict <- bulletin_question_dictionary(
  "cpa_preferred_modality_1",
  "select_one"
)

# First and second preference should normally use the same choice list. Merge
# both in case a country questionnaire uses slightly different lists.
page4_modality2_dict <- bulletin_question_dictionary(
  "cpa_preferred_modality_2",
  "select_one"
)

page4_modality_options <- unique(c(
  page4_modality_dict$options,
  page4_modality2_dict$options
))

page4_modality_labels <- c(
  page4_modality_dict$labels,
  page4_modality2_dict$labels
)
page4_modality_labels <- page4_modality_labels[
  !duplicated(names(page4_modality_labels))
]


page4_priority_need_dict <- bulletin_question_dictionary(
  "cpa_priority_support_ngo_1",
  "select_multiple"
)

page4_priority_need_options <- page4_priority_need_dict$options
page4_priority_need_labels <- page4_priority_need_dict$labels


page4_yes_no_dnk_pnta_dict <- bulletin_question_dictionary(
  "aap_received_assistance_12m",
  "select_one"
)

page4_yes_no_dnk_pnta_options <- page4_yes_no_dnk_pnta_dict$options
page4_yes_no_dnk_pnta_labels <- page4_yes_no_dnk_pnta_dict$labels


page4_coverage_dict <- bulletin_question_dictionary(
  "aap_assistance_coverage",
  "select_one"
)


page4_relevance_dict <- bulletin_question_dictionary(
  "aap_relevance_assistance",
  "select_one"
)

page4_relevance_options <- page4_relevance_dict$options
page4_relevance_labels <- page4_relevance_dict$labels


page4_relevance_reason_dict <- bulletin_question_dictionary(
  "aap_relevance_assistance_reason",
  "select_multiple"
)

page4_relevance_reason_options <- page4_relevance_reason_dict$options
page4_relevance_reason_labels <- page4_relevance_reason_dict$labels


# Safety: no Page 4 analysis can be meaningful without choices.
required_page4_dictionaries <- list(
  modalities = page4_modality_options,
  priority_needs = page4_priority_need_options,
  assistance_received = page4_yes_no_dnk_pnta_options,
  assistance_coverage = page4_coverage_dict$options,
  relevance = page4_relevance_options,
  relevance_reasons = page4_relevance_reason_options
)

empty_page4_dictionaries <- names(required_page4_dictionaries)[
  vapply(required_page4_dictionaries, length, integer(1)) == 0
]

if (length(empty_page4_dictionaries) > 0) {
  stop(
    paste0(
      "Page 4 could not determine answer options for: ",
      paste(empty_page4_dictionaries, collapse = ", "),
      ". Check the current country's Kobo survey/choices metadata."
    )
  )
}


# ============================================================================
# PAGE 4 - HELPER: DETECT SELECTED ANSWER
# ============================================================================

page4_answer_selected <- function(x, answer_code, question_type) {
  
  x <- as.character(x)
  
  if (question_type == "select_one") {
    return(!is.na(x) & x == answer_code)
  }
  
  if (question_type == "select_multiple") {
    
    x[is.na(x)] <- ""
    
    # Normalise common separators to spaces.
    x <- gsub("[,;|]+", " ", x)
    x <- trimws(gsub("\\s+", " ", x))
    
    pattern <- paste0(
      "(^|\\s)",
      answer_code,
      "(\\s|$)"
    )
    
    return(grepl(pattern, x))
  }
  
  stop("Unknown Page 4 question type: ", question_type)
}


# ============================================================================
# PAGE 4 - HELPER: ANALYSE ONE ANSWER OPTION
# ============================================================================

analyse_page4_answer <- function(
    dataset,
    selected_flag,
    geography_variable,
    admin_level
) {
  
  area_values <- dataset %>%
    dplyr::transmute(
      area = as.character(.data[[geography_variable]])
    ) %>%
    dplyr::filter(
      !is.na(area),
      area != ""
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(area)
  
  work <- tibble::tibble(
    area = as.character(dataset[[geography_variable]]),
    population_raw = as.character(dataset$Pol),
    selected = as.logical(selected_flag),
    score = suppressWarnings(
      as.numeric(as.character(dataset$msni_score))
    ),
    weights = suppressWarnings(
      as.numeric(as.character(dataset$weights))
    )
  ) %>%
    dplyr::filter(
      !is.na(area),
      area != "",
      selected,
      !is.na(score),
      score %in% 1:5,
      !is.na(weights),
      is.finite(weights),
      weights > 0
    )
  
  # --------------------------------------------------------------------------
  # ALL POPULATIONS
  # --------------------------------------------------------------------------
  
  all_summary <- work %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(
      n_unweighted = dplyr::n(),
      n_weighted = sum(weights),
      need_345_percentage = sum(
        weights * as.integer(score >= 3)
      ) / sum(weights),
      severe_45_percentage = sum(
        weights * as.integer(score >= 4)
      ) / sum(weights),
      .groups = "drop"
    )
  
  all_results <- area_values %>%
    dplyr::left_join(
      all_summary,
      by = "area"
    ) %>%
    dplyr::mutate(
      population = "All",
      admin_level = admin_level,
      n_unweighted = tidyr::replace_na(n_unweighted, 0L),
      n_weighted = tidyr::replace_na(n_weighted, 0)
    )
  
  # --------------------------------------------------------------------------
  # BY POPULATION
  # --------------------------------------------------------------------------
  
  population_work <- work %>%
    dplyr::filter(
      population_raw %in% unname(population_map)
    ) %>%
    dplyr::mutate(
      population = unname(
        population_reverse_map[population_raw]
      )
    )
  
  population_summary <- population_work %>%
    dplyr::group_by(
      area,
      population
    ) %>%
    dplyr::summarise(
      n_unweighted = dplyr::n(),
      n_weighted = sum(weights),
      need_345_percentage = sum(
        weights * as.integer(score >= 3)
      ) / sum(weights),
      severe_45_percentage = sum(
        weights * as.integer(score >= 4)
      ) / sum(weights),
      .groups = "drop"
    )
  
  population_universe <- tidyr::crossing(
    area = area_values$area,
    population = names(population_map)
  )
  
  population_results <- population_universe %>%
    dplyr::left_join(
      population_summary,
      by = c("area", "population")
    ) %>%
    dplyr::mutate(
      admin_level = admin_level,
      n_unweighted = tidyr::replace_na(n_unweighted, 0L),
      n_weighted = tidyr::replace_na(n_weighted, 0)
    )
  
  dplyr::bind_rows(
    all_results,
    population_results
  ) %>%
    dplyr::select(
      admin_level,
      area,
      population,
      need_345_percentage,
      severe_45_percentage,
      n_unweighted,
      n_weighted
    )
}


# ============================================================================
# PAGE 4 - HELPER: RUN A QUESTION WITH ALL ANSWER OPTIONS
# ============================================================================

run_page4_question <- function(
    dataset,
    section,
    question_order,
    question_code,
    question_label,
    question_type,
    answer_options,
    answer_labels
) {
  
  question_results <- list()
  result_counter <- 1
  
  for (answer_index in seq_along(answer_options)) {
    
    answer_code <- answer_options[answer_index]
    answer_label <- unname(answer_labels[answer_code])
    
    selected_flag <- page4_answer_selected(
      dataset[[question_code]],
      answer_code,
      question_type
    )
    
    geography_results <- list()
    
    for (geography_index in seq_along(page4_geography_map)) {
      
      geography_variable <- page4_geography_map[[geography_index]]$variable
      geography_label <- page4_geography_map[[geography_index]]$label
      
      geography_results[[geography_index]] <- analyse_page4_answer(
        dataset = dataset,
        selected_flag = selected_flag,
        geography_variable = geography_variable,
        admin_level = geography_label
      )
    }
    
    current_result <- dplyr::bind_rows(
      geography_results
    )
    
    current_result$section <- section
    current_result$question_order <- question_order
    current_result$question_code <- question_code
    current_result$question <- question_label
    current_result$question_type <- question_type
    current_result$answer_order <- answer_index
    current_result$answer_code <- answer_code
    current_result$answer_label <- answer_label
    
    question_results[[result_counter]] <- current_result
    result_counter <- result_counter + 1
  }
  
  dplyr::bind_rows(question_results)
}


# ============================================================================
# PAGE 4 PART A - PREFERRED MODALITIES + PRIORITY NEEDS
# ============================================================================

message("Page 4A: preferred modalities and priority needs")

page4a <- dplyr::bind_rows(
  
  run_page4_question(
    dataset = page4_dataset,
    section = "A",
    question_order = 1,
    question_code = "cpa_preferred_modality_1",
    question_label = "First preferred humanitarian assistance modality",
    question_type = "select_one",
    answer_options = page4_modality_options,
    answer_labels = page4_modality_labels
  ),
  
  run_page4_question(
    dataset = page4_dataset,
    section = "A",
    question_order = 2,
    question_code = "cpa_preferred_modality_2",
    question_label = "Second preferred humanitarian assistance modality",
    question_type = "select_one",
    answer_options = page4_modality_options,
    answer_labels = page4_modality_labels
  ),
  
  run_page4_question(
    dataset = page4_dataset,
    section = "A",
    question_order = 3,
    question_code = "cpa_priority_support_ngo_1",
    question_label = "Priority needs to address with preferred assistance",
    question_type = "select_multiple",
    answer_options = page4_priority_need_options,
    answer_labels = page4_priority_need_labels
  )
)

page4a <- page4a %>%
  dplyr::mutate(
    admin_order_internal = match(
      admin_level,
      page4_admin_order
    ),
    population_order_internal = match(
      population,
      population_order
    ),
    concatenate = paste(
      question_code,
      answer_code,
      admin_level,
      area,
      population,
      sep = "."
    )
  ) %>%
  dplyr::arrange(
    question_order,
    answer_order,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  dplyr::select(
    section,
    question_code,
    question,
    question_type,
    answer_code,
    answer_label,
    admin_level,
    area,
    population,
    concatenate,
    need_345_percentage,
    severe_45_percentage,
    n_unweighted,
    n_weighted
  )


# ============================================================================
# PAGE 4 PART B - HEAT-MAP SOURCE
# ============================================================================

# Cell interpretation:
#
#   Among households that selected NEED and are in MSNI 3-5,
#   what % selected MODALITY as either first OR second preference?
#
# and separately:
#
#   Among households that selected NEED and are in MSNI 4-5,
#   what % selected MODALITY as either first OR second preference?

analyse_page4_heatmap_cell <- function(
    dataset,
    need_flag,
    modality_flag,
    geography_variable,
    admin_level
) {
  
  area_values <- dataset %>%
    dplyr::transmute(
      area = as.character(.data[[geography_variable]])
    ) %>%
    dplyr::filter(
      !is.na(area),
      area != ""
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(area)
  
  work <- tibble::tibble(
    area = as.character(dataset[[geography_variable]]),
    population_raw = as.character(dataset$Pol),
    need_selected = as.logical(need_flag),
    modality_selected = as.logical(modality_flag),
    score = suppressWarnings(
      as.numeric(as.character(dataset$msni_score))
    ),
    weights = suppressWarnings(
      as.numeric(as.character(dataset$weights))
    )
  ) %>%
    dplyr::filter(
      !is.na(area),
      area != "",
      need_selected,
      !is.na(score),
      score %in% 1:5,
      !is.na(weights),
      is.finite(weights),
      weights > 0
    )
  
  # --------------------------------------------------------------------------
  # ALL POPULATIONS - MSNI 3-5
  # --------------------------------------------------------------------------
  
  all_345 <- work %>%
    dplyr::filter(score >= 3) %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(
      need_345_percentage = sum(
        weights * as.integer(modality_selected)
      ) / sum(weights),
      n_unweighted_345 = dplyr::n(),
      n_weighted_345 = sum(weights),
      .groups = "drop"
    )
  
  # --------------------------------------------------------------------------
  # ALL POPULATIONS - MSNI 4-5
  # --------------------------------------------------------------------------
  
  all_45 <- work %>%
    dplyr::filter(score >= 4) %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(
      severe_45_percentage = sum(
        weights * as.integer(modality_selected)
      ) / sum(weights),
      n_unweighted_45 = dplyr::n(),
      n_weighted_45 = sum(weights),
      .groups = "drop"
    )
  
  all_results <- area_values %>%
    dplyr::left_join(
      all_345,
      by = "area"
    ) %>%
    dplyr::left_join(
      all_45,
      by = "area"
    ) %>%
    dplyr::mutate(
      population = "All",
      admin_level = admin_level,
      n_unweighted_345 = tidyr::replace_na(n_unweighted_345, 0L),
      n_weighted_345 = tidyr::replace_na(n_weighted_345, 0),
      n_unweighted_45 = tidyr::replace_na(n_unweighted_45, 0L),
      n_weighted_45 = tidyr::replace_na(n_weighted_45, 0)
    )
  
  # --------------------------------------------------------------------------
  # BY POPULATION
  # --------------------------------------------------------------------------
  
  population_work <- work %>%
    dplyr::filter(
      population_raw %in% unname(population_map)
    ) %>%
    dplyr::mutate(
      population = unname(
        population_reverse_map[population_raw]
      )
    )
  
  population_345 <- population_work %>%
    dplyr::filter(score >= 3) %>%
    dplyr::group_by(
      area,
      population
    ) %>%
    dplyr::summarise(
      need_345_percentage = sum(
        weights * as.integer(modality_selected)
      ) / sum(weights),
      n_unweighted_345 = dplyr::n(),
      n_weighted_345 = sum(weights),
      .groups = "drop"
    )
  
  population_45 <- population_work %>%
    dplyr::filter(score >= 4) %>%
    dplyr::group_by(
      area,
      population
    ) %>%
    dplyr::summarise(
      severe_45_percentage = sum(
        weights * as.integer(modality_selected)
      ) / sum(weights),
      n_unweighted_45 = dplyr::n(),
      n_weighted_45 = sum(weights),
      .groups = "drop"
    )
  
  population_universe <- tidyr::crossing(
    area = area_values$area,
    population = names(population_map)
  )
  
  population_results <- population_universe %>%
    dplyr::left_join(
      population_345,
      by = c("area", "population")
    ) %>%
    dplyr::left_join(
      population_45,
      by = c("area", "population")
    ) %>%
    dplyr::mutate(
      admin_level = admin_level,
      n_unweighted_345 = tidyr::replace_na(n_unweighted_345, 0L),
      n_weighted_345 = tidyr::replace_na(n_weighted_345, 0),
      n_unweighted_45 = tidyr::replace_na(n_unweighted_45, 0L),
      n_weighted_45 = tidyr::replace_na(n_weighted_45, 0)
    )
  
  dplyr::bind_rows(
    all_results,
    population_results
  ) %>%
    dplyr::select(
      admin_level,
      area,
      population,
      need_345_percentage,
      severe_45_percentage,
      n_unweighted_345,
      n_weighted_345,
      n_unweighted_45,
      n_weighted_45
    )
}


message("Page 4B: full modality x priority-need heat-map source")

page4b_results <- list()
page4b_counter <- 1

for (need_index in seq_along(page4_priority_need_options)) {
  
  need_code <- page4_priority_need_options[need_index]
  need_label <- unname(page4_priority_need_labels[need_code])
  
  need_flag <- page4_answer_selected(
    page4_dataset$cpa_priority_support_ngo_1,
    need_code,
    "select_multiple"
  )
  
  for (modality_index in seq_along(page4_modality_options)) {
    
    modality_code <- page4_modality_options[modality_index]
    modality_label <- unname(page4_modality_labels[modality_code])
    
    modality_1_flag <- page4_answer_selected(
      page4_dataset$cpa_preferred_modality_1,
      modality_code,
      "select_one"
    )
    
    modality_2_flag <- page4_answer_selected(
      page4_dataset$cpa_preferred_modality_2,
      modality_code,
      "select_one"
    )
    
    modality_flag <- modality_1_flag | modality_2_flag
    
    geography_results <- list()
    
    for (geography_index in seq_along(page4_geography_map)) {
      
      geography_variable <- page4_geography_map[[geography_index]]$variable
      geography_label <- page4_geography_map[[geography_index]]$label
      
      geography_results[[geography_index]] <- analyse_page4_heatmap_cell(
        dataset = page4_dataset,
        need_flag = need_flag,
        modality_flag = modality_flag,
        geography_variable = geography_variable,
        admin_level = geography_label
      )
    }
    
    current_result <- dplyr::bind_rows(
      geography_results
    )
    
    current_result$section <- "B"
    current_result$need_order <- need_index
    current_result$need_code <- need_code
    current_result$need_label <- need_label
    current_result$modality_order <- modality_index
    current_result$modality_code <- modality_code
    current_result$modality_label <- modality_label
    
    page4b_results[[page4b_counter]] <- current_result
    page4b_counter <- page4b_counter + 1
  }
}

page4b <- dplyr::bind_rows(page4b_results) %>%
  dplyr::mutate(
    admin_order_internal = match(
      admin_level,
      page4_admin_order
    ),
    population_order_internal = match(
      population,
      population_order
    ),
    concatenate = paste(
      need_code,
      modality_code,
      admin_level,
      area,
      population,
      sep = "."
    )
  ) %>%
  dplyr::arrange(
    need_order,
    modality_order,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  dplyr::select(
    section,
    need_code,
    need_label,
    modality_code,
    modality_label,
    admin_level,
    area,
    population,
    concatenate,
    need_345_percentage,
    severe_45_percentage,
    n_unweighted_345,
    n_weighted_345,
    n_unweighted_45,
    n_weighted_45
  )


# ============================================================================
# PAGE 4 PART C - ASSISTANCE EXPERIENCE / RELEVANCE
# ============================================================================

message("Page 4C: assistance experience and relevance")

page4c <- dplyr::bind_rows(
  
  run_page4_question(
    dataset = page4_dataset,
    section = "C",
    question_order = 1,
    question_code = "aap_received_assistance_12m",
    question_label = "Received humanitarian assistance in past 12 months",
    question_type = "select_one",
    answer_options = page4_yes_no_dnk_pnta_options,
    answer_labels = page4_yes_no_dnk_pnta_labels
  ),
  
  run_page4_question(
    dataset = page4_dataset,
    section = "C",
    question_order = 2,
    question_code = "aap_assistance_coverage",
    question_label = "Assistance reaches people who need it most",
    question_type = "select_one",
    answer_options = page4_coverage_dict$options,
    answer_labels = page4_coverage_dict$labels
  ),
  
  run_page4_question(
    dataset = page4_dataset,
    section = "C",
    question_order = 3,
    question_code = "aap_relevance_assistance",
    question_label = "Extent assistance met most important needs",
    question_type = "select_one",
    answer_options = page4_relevance_options,
    answer_labels = page4_relevance_labels
  ),
  
  run_page4_question(
    dataset = page4_dataset,
    section = "C",
    question_order = 4,
    question_code = "aap_relevance_assistance_reason",
    question_label = "Reasons assistance did not meet needs",
    question_type = "select_multiple",
    answer_options = page4_relevance_reason_options,
    answer_labels = page4_relevance_reason_labels
  )
)

page4c <- page4c %>%
  dplyr::mutate(
    admin_order_internal = match(
      admin_level,
      page4_admin_order
    ),
    population_order_internal = match(
      population,
      population_order
    ),
    concatenate = paste(
      question_code,
      answer_code,
      admin_level,
      area,
      population,
      sep = "."
    )
  ) %>%
  dplyr::arrange(
    question_order,
    answer_order,
    admin_order_internal,
    area,
    population_order_internal
  ) %>%
  dplyr::select(
    section,
    question_code,
    question,
    question_type,
    answer_code,
    answer_label,
    admin_level,
    area,
    population,
    concatenate,
    need_345_percentage,
    severe_45_percentage,
    n_unweighted,
    n_weighted
  )


# ============================================================================
# PAGE 4 - VALIDATION
# ============================================================================

page4_percentage_check <- function(data, percentage_columns, object_name) {
  
  for (percentage_column in percentage_columns) {
    if (
      any(
        data[[percentage_column]] < 0 |
        data[[percentage_column]] > 1,
        na.rm = TRUE
      )
    ) {
      stop(
        object_name,
        " contains an invalid percentage in ",
        percentage_column,
        "."
      )
    }
  }
}

page4_percentage_check(
  page4a,
  c("need_345_percentage", "severe_45_percentage"),
  "Page 4A"
)

page4_percentage_check(
  page4b,
  c("need_345_percentage", "severe_45_percentage"),
  "Page 4B"
)

page4_percentage_check(
  page4c,
  c("need_345_percentage", "severe_45_percentage"),
  "Page 4C"
)

if (dplyr::n_distinct(page4a$question_code) != 3) {
  stop("Page 4A does not contain all 3 expected questions.")
}

if (dplyr::n_distinct(page4c$question_code) != 4) {
  stop("Page 4C does not contain all 4 expected questions.")
}

if (dplyr::n_distinct(page4b$need_code) != length(page4_priority_need_options)) {
  stop(
    "Page 4B does not contain all expected priority-need options for ",
    country_name, "."
  )
}

if (dplyr::n_distinct(page4b$modality_code) != length(page4_modality_options)) {
  stop(
    "Page 4B does not contain all expected modality options for ",
    country_name, "."
  )
}

page4a_duplicate_check <- page4a %>%
  dplyr::count(
    question_code,
    answer_code,
    admin_level,
    area,
    population
  ) %>%
  dplyr::filter(n > 1)

if (nrow(page4a_duplicate_check) > 0) {
  stop("Duplicate rows found in Page 4A.")
}

page4b_duplicate_check <- page4b %>%
  dplyr::count(
    need_code,
    modality_code,
    admin_level,
    area,
    population
  ) %>%
  dplyr::filter(n > 1)

if (nrow(page4b_duplicate_check) > 0) {
  stop("Duplicate rows found in Page 4B.")
}

page4c_duplicate_check <- page4c %>%
  dplyr::count(
    question_code,
    answer_code,
    admin_level,
    area,
    population
  ) %>%
  dplyr::filter(n > 1)

if (nrow(page4c_duplicate_check) > 0) {
  stop("Duplicate rows found in Page 4C.")
}

# Part A and C use the same subgroup denominator for both severity outputs,
# therefore 4-5 must never exceed 3-5.

page4a_logic_problems <- page4a %>%
  dplyr::filter(
    !is.na(need_345_percentage),
    !is.na(severe_45_percentage),
    severe_45_percentage > need_345_percentage + 1e-8
  )

if (nrow(page4a_logic_problems) > 0) {
  stop("Page 4A contains rows where MSNI 4-5 exceeds MSNI 3-5.")
}

page4c_logic_problems <- page4c %>%
  dplyr::filter(
    !is.na(need_345_percentage),
    !is.na(severe_45_percentage),
    severe_45_percentage > need_345_percentage + 1e-8
  )

if (nrow(page4c_logic_problems) > 0) {
  stop("Page 4C contains rows where MSNI 4-5 exceeds MSNI 3-5.")
}

# In Part B, the 4-5 denominator is a subset of the 3-5 denominator.

page4b_n_problems <- page4b %>%
  dplyr::filter(
    n_unweighted_45 > n_unweighted_345 |
      n_weighted_45 > n_weighted_345 + 1e-8
  )

if (nrow(page4b_n_problems) > 0) {
  warning(
    "Page 4B contains rows where the 4-5 denominator exceeds the 3-5 denominator. ",
    "Inspect 'page4b_n_problems'."
  )
}

# ============================================================================
# CREATE WORKBOOK
# ============================================================================

wb_bulletin <- openxlsx::createWorkbook()


# ============================================================================
# WRITE PAGE 1
# ============================================================================

openxlsx::addWorksheet(
  wb_bulletin,
  "Page 1"
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 1",
  x = page1
)


# ============================================================================
# WRITE PAGE 2A
# ============================================================================

openxlsx::addWorksheet(
  wb_bulletin,
  "Page 2a"
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 2a",
  x = page2a
)


# ============================================================================
# WRITE PAGE 2B
# ============================================================================

openxlsx::addWorksheet(
  wb_bulletin,
  "Page 2b"
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 2b",
  x = page2b
)


# ============================================================================
# WRITE PAGE 3A
# ============================================================================

openxlsx::addWorksheet(
  wb_bulletin,
  "Page 3a"
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 3a",
  x = page3
)


# ============================================================================
# WRITE PAGE 3B
# ============================================================================

openxlsx::addWorksheet(
  wb_bulletin,
  "Page 3b"
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 3b",
  x = page3b
)

# ============================================================================
# WRITE PAGE 4
# ============================================================================

# NOTE: This block assumes wb_bulletin already exists.

# Remove existing Page 4 if this section is being rerun
if ("Page 4" %in% names(wb_bulletin)) {
  openxlsx::removeWorksheet(
    wb_bulletin,
    "Page 4"
  )
}

# Create fresh Page 4
openxlsx::addWorksheet(
  wb_bulletin,
  "Page 4"
)

page4_row_a_title <- 1
page4_row_a_data <- 2

page4_row_b_title <- page4_row_a_data + nrow(page4a) + 3
page4_row_b_data <- page4_row_b_title + 1

page4_row_c_title <- page4_row_b_data + nrow(page4b) + 3
page4_row_c_data <- page4_row_c_title + 1

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 4",
  x = "PART A - Preferred modalities and priority needs",
  startRow = page4_row_a_title,
  startCol = 1,
  colNames = FALSE
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 4",
  x = page4a,
  startRow = page4_row_a_data,
  startCol = 1
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 4",
  x = "PART B - Full modality x priority-need heat-map source",
  startRow = page4_row_b_title,
  startCol = 1,
  colNames = FALSE
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 4",
  x = page4b,
  startRow = page4_row_b_data,
  startCol = 1
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 4",
  x = "PART C - Assistance experience and relevance",
  startRow = page4_row_c_title,
  startCol = 1,
  colNames = FALSE
)

openxlsx::writeData(
  wb_bulletin,
  sheet = "Page 4",
  x = page4c,
  startRow = page4_row_c_data,
  startCol = 1
)

# ============================================================================
# EXCEL FORMATTING
# ============================================================================

percentage_style <- openxlsx::createStyle(
  numFmt = "0.0%"
)

header_style_bulletin <- openxlsx::createStyle(
  textDecoration = "bold",
  wrapText = TRUE
)


# ---------------------------------------------------------------------------
# PAGE 1
# ---------------------------------------------------------------------------

page1_percentage_col <- which(
  names(page1) == "percentage"
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 1",
  style = percentage_style,
  rows = 2:(nrow(page1) + 1),
  cols = page1_percentage_col,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 1",
  style = header_style_bulletin,
  rows = 1,
  cols = 1:ncol(page1),
  gridExpand = TRUE
)

openxlsx::freezePane(
  wb_bulletin,
  sheet = "Page 1",
  firstRow = TRUE
)

openxlsx::addFilter(
  wb_bulletin,
  sheet = "Page 1",
  rows = 1,
  cols = 1:ncol(page1)
)

openxlsx::setColWidths(
  wb_bulletin,
  sheet = "Page 1",
  cols = 1:ncol(page1),
  widths = "auto"
)


# ---------------------------------------------------------------------------
# PAGE 2A
# ---------------------------------------------------------------------------

page2a_percentage_cols <- which(
  names(page2a) %in% c(
    "severity_percentage",
    "in_need_percentage",
    "severe_need_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 2a",
  style = percentage_style,
  rows = 2:(nrow(page2a) + 1),
  cols = page2a_percentage_cols,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 2a",
  style = header_style_bulletin,
  rows = 1,
  cols = 1:ncol(page2a),
  gridExpand = TRUE
)

openxlsx::freezePane(
  wb_bulletin,
  sheet = "Page 2a",
  firstRow = TRUE
)

openxlsx::addFilter(
  wb_bulletin,
  sheet = "Page 2a",
  rows = 1,
  cols = 1:ncol(page2a)
)

openxlsx::setColWidths(
  wb_bulletin,
  sheet = "Page 2a",
  cols = 1:ncol(page2a),
  widths = "auto"
)


# ---------------------------------------------------------------------------
# PAGE 2B
# ---------------------------------------------------------------------------

page2b_percentage_cols <- which(
  names(page2b) %in% c(
    "need_345_percentage",
    "severe_45_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 2b",
  style = percentage_style,
  rows = 2:(nrow(page2b) + 1),
  cols = page2b_percentage_cols,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 2b",
  style = header_style_bulletin,
  rows = 1,
  cols = 1:ncol(page2b),
  gridExpand = TRUE
)

openxlsx::freezePane(
  wb_bulletin,
  sheet = "Page 2b",
  firstRow = TRUE
)

openxlsx::addFilter(
  wb_bulletin,
  sheet = "Page 2b",
  rows = 1,
  cols = 1:ncol(page2b)
)

openxlsx::setColWidths(
  wb_bulletin,
  sheet = "Page 2b",
  cols = 1:ncol(page2b),
  widths = "auto"
)


# ---------------------------------------------------------------------------
# PAGE 3A
# ---------------------------------------------------------------------------

page3_percentage_cols <- which(
  names(page3) %in% c(
    "need_345_percentage",
    "severe_45_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 3a",
  style = percentage_style,
  rows = 2:(nrow(page3) + 1),
  cols = page3_percentage_cols,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 3a",
  style = header_style_bulletin,
  rows = 1,
  cols = 1:ncol(page3),
  gridExpand = TRUE
)

openxlsx::freezePane(
  wb_bulletin,
  sheet = "Page 3a",
  firstRow = TRUE
)

openxlsx::addFilter(
  wb_bulletin,
  sheet = "Page 3a",
  rows = 1,
  cols = 1:ncol(page3)
)

openxlsx::setColWidths(
  wb_bulletin,
  sheet = "Page 3a",
  cols = 1:ncol(page3),
  widths = "auto"
)


# ---------------------------------------------------------------------------
# PAGE 3B
# ---------------------------------------------------------------------------

page3b_percentage_cols <- which(
  names(page3b) %in% c(
    "need_345_percentage",
    "severe_45_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 3b",
  style = percentage_style,
  rows = 2:(nrow(page3b) + 1),
  cols = page3b_percentage_cols,
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 3b",
  style = header_style_bulletin,
  rows = 1,
  cols = 1:ncol(page3b),
  gridExpand = TRUE
)

openxlsx::freezePane(
  wb_bulletin,
  sheet = "Page 3b",
  firstRow = TRUE
)

openxlsx::addFilter(
  wb_bulletin,
  sheet = "Page 3b",
  rows = 1,
  cols = 1:ncol(page3b)
)

openxlsx::setColWidths(
  wb_bulletin,
  sheet = "Page 3b",
  cols = 1:ncol(page3b),
  widths = "auto"
)

# ============================================================================
# PAGE 4 FORMATTING
# ============================================================================

page4_section_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 12
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = page4_section_style,
  rows = c(
    page4_row_a_title,
    page4_row_b_title,
    page4_row_c_title
  ),
  cols = 1,
  gridExpand = FALSE
)

# Headers

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = header_style_bulletin,
  rows = page4_row_a_data,
  cols = 1:ncol(page4a),
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = header_style_bulletin,
  rows = page4_row_b_data,
  cols = 1:ncol(page4b),
  gridExpand = TRUE
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = header_style_bulletin,
  rows = page4_row_c_data,
  cols = 1:ncol(page4c),
  gridExpand = TRUE
)

# Percentage formatting - Part A

page4a_percentage_cols <- which(
  names(page4a) %in% c(
    "need_345_percentage",
    "severe_45_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = percentage_style,
  rows = (page4_row_a_data + 1):(page4_row_a_data + nrow(page4a)),
  cols = page4a_percentage_cols,
  gridExpand = TRUE
)

# Percentage formatting - Part B

page4b_percentage_cols <- which(
  names(page4b) %in% c(
    "need_345_percentage",
    "severe_45_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = percentage_style,
  rows = (page4_row_b_data + 1):(page4_row_b_data + nrow(page4b)),
  cols = page4b_percentage_cols,
  gridExpand = TRUE
)

# Percentage formatting - Part C

page4c_percentage_cols <- which(
  names(page4c) %in% c(
    "need_345_percentage",
    "severe_45_percentage"
  )
)

openxlsx::addStyle(
  wb_bulletin,
  sheet = "Page 4",
  style = percentage_style,
  rows = (page4_row_c_data + 1):(page4_row_c_data + nrow(page4c)),
  cols = page4c_percentage_cols,
  gridExpand = TRUE
)

# Column widths

page4_max_cols <- max(
  ncol(page4a),
  ncol(page4b),
  ncol(page4c)
)

openxlsx::setColWidths(
  wb_bulletin,
  sheet = "Page 4",
  cols = 1:page4_max_cols,
  widths = "auto"
)

openxlsx::freezePane(
  wb_bulletin,
  sheet = "Page 4",
  firstActiveRow = 3
)

# ============================================================================
# FINAL EXPORT
# ============================================================================

openxlsx::saveWorkbook(
  wb_bulletin,
  bulletin_file,
  overwrite = TRUE
)

message(
  ">>> Bulletin export completed: ",
  bulletin_file
)

message(
  ">>> Page 1 contains ",
  nrow(page1),
  " severity/geography/population results."
)

message(
  ">>> Page 2a contains ",
  nrow(page2a),
  " component/severity/geography/population results."
)

message(
  ">>> Page 2b contains ",
  nrow(page2b),
  " option/geography/population results."
)

message(
  ">>> Page 3a contains ",
  nrow(page3),
  " sector/disaggregation/geography/population results."
)

message(
  ">>> Page 3b contains ",
  nrow(page3b),
  " sector/geography/population results."
)

# ============================================================================
# PAGE 4 - FINAL MESSAGES
# ============================================================================

message(
  ">>> Page 4A contains ",
  nrow(page4a),
  " question/answer/geography/population results."
)

message(
  ">>> Page 4B contains ",
  nrow(page4b),
  " need/modality/geography/population heat-map source results."
)

message(
  ">>> Page 4C contains ",
  nrow(page4c),
  " assistance-experience/geography/population results."
)

# ============================================================================
# PIPELINE INTEGRATION - restore main_dataset as produced by 01-07
# ============================================================================

main_dataset <- bulletin_main_dataset_pipeline
rm(bulletin_main_dataset_pipeline)