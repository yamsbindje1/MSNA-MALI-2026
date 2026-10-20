# ============================================================================
# HESPER - core functions (add_hesper_main / add_hesper_cat)
# ============================================================================

#' Compute, per respondent: number of items reported as a serious problem,
#' number of applicable items, and collapse the three priority questions into
#' one select-multiple column.
#'
#' @param df dataframe containing the cleaned data
#' @param col_items vector of column names containing the hesper items.
#' @param choice_serious choice value for "serious problem"
#' @param choice_no_serious choice value for "no serious problem"
#' @param choice_dnk choice value for "don't know"
#' @param choice_pnta choice value for "prefer not to answer"
#' @param choice_na choice value for "not applicable to household"
#' @param cols_priority vector of the 3 priority item columns (1st/2nd/3rd)
#' @param col_gender column name for respondent gender
#' @param choice_male / choice_female choice values for gender
#' @param hesper_item_male / hesper_item_female items specific to a gender subset
#' @param col_displacement column name for displacement status
#' @param choices_displaced / choices_non_displaced choice values for displacement status
#' @param col_hesper_subset items applicable only to a gender/displacement subset
#' @param hesper_item_displaced / hesper_item_non_displaced items specific to a displacement subset
#' @param add_binaries add one binary column per item
#' @param add_binaries_subset add one binary column per subset-only item
#' @param subset apply the gender/displacement subset logic to priority columns too
#'
#' @return dataframe with the item counts, proportions, and the three
#'   priorities collapsed into one select-multiple column

add_hesper_main <- function(df,
                            col_items = c("hesper_drinking_water", "hesper_food", "hesper_shelter", "hesper_toilet", "hesper_clean", "hesper_clean", "hesper_clothes_etc", "hesper_income_livelihood", "hesper_health", "hesper_health",
                                          "hesper_health", "hesper_distress", "hesper_safety", "hesper_education", "hesper_care", "hesper_support", "hesper_separation", "hesper_displaced", "hesper_information", "hesper_aid",
                                          "hesper_respect", "hesper_movement", "hesper_time", "hesper_law", "hesper_gbv", "hesper_drug", "hesper_mental_health", "hesper_care_community"),
                            # Default casing ("serious_problem" etc.) matches
                            # add_hesper_cat() below. hesper_niger.R always
                            # passes the real Kobo values explicitly anyway,
                            # so these defaults aren't actually relied on.
                            choice_serious = "serious_problem",
                            choice_no_serious = "no_serious_problem",
                            choice_dnk = "dnk",
                            choice_pnta = "pnta",
                            choice_na = "not_applicable_to_household",
                            cols_priority = c("hesper_priority_first", "hesper_priority_second", "hesper_priority_third"),
                            col_name_hesper_top_three="hesper_top_three_priorities",
                            col_gender = "gender",
                            choice_male = "men",
                            choice_female = "women",
                            hesper_item_male = c("hesper_health_care_men"),
                            hesper_item_female = c("hesper_clean_women", "hesper_health_care_women"),
                            col_displacement = "pop_group",
                            choices_displaced = c("displaced"),
                            choices_non_displaced = c("host"),
                            col_hesper_subset = c("hesper_health_care_men", "hesper_clean_women", "hesper_health_care_women", "hesper_displaced"),
                            hesper_item_displaced = c("hesper_displaced"),
                            hesper_item_non_displaced = NULL,
                            add_binaries = T,
                            add_binaries_subset = T,
                            subset = T
                            ){

  ## check that all columns are present in dataframe and print the non matching columns
  col_not_matching <- col_items %>% unname %>% unlist %>% keep(!. %in% colnames(df))
  if(length(col_not_matching) > 0){warning(paste("The following columns are not present in the dataframe: ", col_not_matching))}

  ## all response choices that should not be counted in the total nb of items
  choice_applicable <- c(choice_serious, choice_no_serious, choice_dnk, choice_pnta)
  choice_exclude <- c(NA, choice_na, choice_dnk, choice_pnta)

  ## compute total items selected
  df <- df %>%
    mutate(nb_hesper.all = rowSums(across(all_of(col_items), ~. %in% choice_serious), na.rm=T),
           nb_hesper.applicable = rowSums(across(all_of(col_items), ~ . %in% choice_applicable), na.rm=T),
           nb_hesper.pnta = rowSums(across(all_of(col_items), ~ . %in% choice_pnta), na.rm=T),
           nb_hepser.dnk = rowSums(across(all_of(col_items), ~ . %in% choice_dnk), na.rm=T),
           prop_hesper.all = nb_hesper.all / nb_hesper.applicable)

  ## Add binary columns to record serious problem, 0 if no serious problem and NA otherwise for all items across col_items
  ## Column names is original names with _binary suffix appended

  if (add_binaries){
    # 1 = serious problem ; 0 = no serious problem, dnk or pnta (dnk / pnta stay in the denominator) ;
    # NA = no answer (missing or not applicable) - a missing answer is never turned into 0
    df <- df %>% mutate(across(all_of(col_items), ~case_when(is.na(.) | . %in% choice_na ~ NA_real_, . %in% choice_serious ~ 1, . %in% c(choice_no_serious, choice_dnk, choice_pnta) ~ 0, TRUE ~ NA_real_), .names = "{.col}.binary"))
  }

  if (add_binaries_subset){
    df <- df %>% mutate(across(all_of(col_hesper_subset), ~case_when(. %in% choice_serious ~ 1, . %in% choice_no_serious ~ 0, TRUE ~ NA_real_), .names = "{.col}.binary_subset"))
  }

  ## collapse priority columns to have a select multiple column
  if (!is.null(cols_priority)){

    ## stop and display columns that are not in df
    if (sum(!cols_priority %in% colnames(df))>0) stop("The following columns are not present in the dataframe: ", cols_priority[!cols_priority %in% colnames(df)])

    ## unite the thre priority columns to have one select multiple hesper priorities
    df <- df %>%
      unite(!!sym(col_name_hesper_top_three), all_of(cols_priority), sep = " ", remove = F, na.rm = T) %>%
      mutate(!!sym(col_name_hesper_top_three) := ifelse(!!sym(col_name_hesper_top_three)=="", NA, str_replace_all(!!sym(col_name_hesper_top_three), "^ | $", "")))

    ### expand parent column top three priorities and priority without subset
    df <- df %>% expand.select.multiple.vec(c(col_name_hesper_top_three, cols_priority))

    ### ensure that skip logic are respected
    if (subset){

      # mutate all subset binaries with _subset at the end before reworking them
      col_hesper_male <- if (!any(is.null(hesper_item_male)|is.na(hesper_item_male))) paste0(col_name_hesper_top_three, ".", hesper_item_male, "_subset") else NULL
      col_hesper_female <- if (!any(is.null(hesper_item_female)|is.na(hesper_item_female))) paste0(col_name_hesper_top_three, ".", hesper_item_female, "_subset") else NULL
      col_hesper_displaced <- if (!any(is.null(hesper_item_displaced)|is.na(hesper_item_displaced))) paste0(col_name_hesper_top_three, ".", hesper_item_displaced, "_subset") else NULL
      col_hesper_non_displaced <- if (!any(is.null(hesper_item_non_displaced)|is.na(hesper_item_non_displaced))) paste0(col_name_hesper_top_three, ".", hesper_item_non_displaced, "_subset") else NULL
      col_hesper_subset <- c(col_hesper_male, col_hesper_female, col_hesper_displaced, col_hesper_non_displaced)
      col_hesper_item_subset <- col_hesper_subset %>% str_replace_all("_subset$", "")

      df <- df %>% mutate(across(any_of(col_hesper_item_subset), ~ . , .names = "{.col}_subset"))
      df <- df %>% mutate(
        across(any_of(col_hesper_male), ~ case_when(!(!!sym(col_gender) == choice_male) ~ NA_real_, TRUE ~ .)),
        across(any_of(col_hesper_female), ~ case_when(!(!!sym(col_gender) == choice_female) ~ NA_real_, TRUE ~ .)),
        across(any_of(col_hesper_displaced), ~ case_when(!(!!sym(col_displacement) %in% choices_displaced) ~ NA_real_, TRUE ~ .)),
        across(any_of(col_hesper_non_displaced), ~ case_when(!(!!sym(col_displacement) %in% choices_non_displaced) ~ NA_real_, TRUE ~ .))
      )

      ## mutate all subset binaries for priority 1 / 2 / 3
      col_prio_male <- if (!any(is.null(hesper_item_male)|is.na(hesper_item_male))) map(cols_priority, ~ paste0(., ".", hesper_item_male, "_subset")) %>% unlist else NULL
      col_prio_female <- if (!any(is.null(hesper_item_female)|is.na(hesper_item_female))) map(cols_priority, ~ paste0(., ".", hesper_item_female, "_subset")) %>% unlist else NULL
      col_prio_displaced <- if (!any(is.null(hesper_item_displaced)|is.na(hesper_item_displaced))) map(cols_priority, ~ paste0(., ".", hesper_item_displaced, "_subset")) %>% unlist else NULL
      col_prio_non_displaced <- if (!any(is.null(hesper_item_non_displaced)|is.na(hesper_item_non_displaced))) map(cols_priority, ~ paste0(., ".", hesper_item_non_displaced, "_subset")) %>% unlist else NULL
      col_prio_subset <- c(col_prio_male, col_prio_female, col_prio_displaced)
      col_prio_item_subset <- col_prio_subset %>% str_replace_all("_subset$", "")

      df <- df %>% mutate(across(any_of(col_prio_item_subset), ~ . , .names = "{.col}_subset"))
      df <- df %>% mutate(
        across(any_of(col_prio_male), ~ case_when(!(!!sym(col_gender) == choice_male) ~ NA_real_, TRUE ~ .)),
        across(any_of(col_prio_female), ~ case_when(!(!!sym(col_gender) == choice_female) ~ NA_real_, TRUE ~ .)),
        across(any_of(col_prio_displaced), ~ case_when(!(!!sym(col_displacement) %in% choices_displaced) ~ NA_real_, TRUE ~ .)),
        across(any_of(col_prio_non_displaced), ~ case_when(!(!!sym(col_displacement) %in% choices_non_displaced) ~ NA_real_, TRUE ~ .))
      )

    }

  }

  return(df)

}

#' Count items with a serious problem per category, plus proportion/overall-share indicators.
#'
#' @description column naming: nb_hesper_items_{section_name}.{section_category}
#' @param df dataframe containing the data
#' @param list_group named list, one vector of HESPER item column names per category
#' @param choice_serious / choice_no_serious / choice_dnk / choice_pnta / choice_na choice values
#'
#' @return dataframe with, for each category: nb_hesper_items_*, prop_hesper_items_*,
#'   overall_prop_hesper_*, at_least_one_hesper_item_*

add_hesper_cat <- function(df,
                           list_group = section_wide,
                           choice_serious = "serious_problem",
                           choice_no_serious = "no_serious_problem",
                           choice_dnk = "dnk",
                           choice_pnta = "pnta",
                           choice_na = "not_applicable"
){

  ## access the name of the object passed as argument to "list_group"
  list_group_name <- deparse(substitute(list_group))
  list_group_cat <- names(list_group)

  # ## check that all columns are present in dataframe and print the non matching columns
  col_hesper <- list_group %>% unname %>% unlist
  col_not_matching <- col_hesper %>% keep(!. %in% colnames(df))
  if(length(col_not_matching) > 0){warning(paste("The following columns are not present in the dataframe: ", col_not_matching, "\n"))}

  ## all response choices that should not be counted in the total nb of items
  choice_applicable <- c(choice_serious, choice_no_serious, choice_dnk, choice_pnta)
  choice_exclude <- c(NA, choice_na, choice_dnk, choice_pnta)

  ## Add a check if choice_serious and choice_no_serious never appears in any of df[, col_hesper]

  if(df %>% select(any_of(col_hesper)) %>% map(unique) %>% map(~all(!. %in% c(choice_serious, choice_no_serious))) %>% unlist %>% any){
    stop("The specified values for arguments choice_serious and choice_no_serious do not appear in the dataframe. Please check the values.")
  }

  ## for each group category, create the nb_hesper_items_{section_name}.{section_category} to capture number of serious problem recorded
  ## if there is only NAs across list_group[[cat]], the value should be set to NA

  for (cat in list_group_cat){
    df <- df %>%
      mutate(all_serious_items := rowSums(across(any_of(unlist(unname(list_group))), ~ . %in% choice_serious), na.rm=T),

             n_valid := rowSums(across(any_of(list_group[[cat]]), ~ !. %in% choice_exclude)),

             !!paste0("nb_hesper_items_", list_group_name, ".", cat) := case_when(
               n_valid == 0 ~ NA_real_, TRUE ~ rowSums(across(any_of(list_group[[cat]]), ~. %in% c(choice_serious)), na.rm=T)
             ),

             !!paste0("prop_hesper_items_", list_group_name, ".", cat) := !!sym(paste0("nb_hesper_items_", list_group_name, ".", cat)) / n_valid,

             !!paste0("overall_prop_hesper_", list_group_name, ".", cat) := !!sym(paste0("nb_hesper_items_", list_group_name, ".", cat)) / all_serious_items,

             !!paste0("at_least_one_hesper_item_", list_group_name, ".", cat) := case_when(
               n_valid == 0 ~ NA_real_, TRUE ~ rowSums(across(any_of(list_group[[cat]]), ~. %in% c(choice_serious)), na.rm=T) > 0
             )
      )
  }

  df <- df %>% select(-n_valid)

  return(df)
}
