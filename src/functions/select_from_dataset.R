# ============================================================================
# SELECT_*_FROM_DATASET - pick out which columns of a dataset are select_one,

select_one_from_dataset <- function(questions, choices, dataset, name_or_label_headers, label_column) {

  if (!name_or_label_headers %in% c("name", "label") | !name_or_label_values %in% c("name", "label")) {
    stop("\n\nin name_or_label write either name or label, depending if the dataset is in HML or word format")
  }

  else if (any(duplicated(names(dataset)))) {
    stop("\n\nDUPLICATES LABELS FOUND IN COLUMN HEADERS!.
         \n\nPlease be sure to follow the instructions given by the check_for_duplicate_headers function before proceeding.")
  }

  else if (name_or_label_headers %in% c("name")) {
    select_one_questions <- (questions %>% filter(str_detect(type, "select_one") | str_detect(type, "recoded_variable_categorical"), name %in% names(dataset)))$name
    if (length(select_one_questions) == 0) {
      cat("\n\nPossible error! Be sure you selected the correct variables (especially the name_or_label_headers one)")
      select_one_questions <- select_one_questions[select_one_questions %in% names(dataset)]
      select_one_questions
    }
    else if (length(select_one_questions) > 0) {
      dataset[select_one_questions] <- lapply(dataset[select_one_questions], as.character)
      select_one_questions
    }
  }

  else if (name_or_label_headers %in% c("label")) {
    select_one_questions <- names(dataset)[gsub(".*\\Q*%*\\E", "", names(dataset)) %in% (filter(questions, str_detect(type, "select_one")) %>% pull(!!sym(label_column)))]

    if (length((questions %>% filter(str_detect(type, "recoded_variable_categorical"), !!sym(label_column) %in% names(dataset)))$name) > 0) {
      select_one_questions <- c(select_one_questions, (questions %>% filter(str_detect(type, "recoded_variable_categorical"), !!sym(label_column) %in% names(dataset)))[[label_column]])
    }

    if (length(select_one_questions) == 0) {
      print("\n\nPossible error! Be sure you selected the correct variables (especially the name_or_label_headers one)
        ")
      select_one_questions
    }
    else if (length(select_one_questions) > 0) {
      dataset[select_one_questions] <- lapply(dataset[select_one_questions], as.character)
      select_one_questions
    }
  }

}

select_multiple_from_dataset <- function(questions, choices, dataset, name_or_label_headers, separator) {

  # keep only columns that are actually binary (0/1/NA), just to be safe
  binary_variables <- dataset %>%
    purrr::map_lgl(~all(.x %in% c(0, 1, "0", "1", NA, values_to_NA))) %>%
    as.data.frame() %>%
    setNames("values") %>% filter(values == T)

  if (!name_or_label_headers %in% c("name", "label")) {
    stop("\n\nin name_or_label write either name or label, depending if the dataset is in HML or word format")
  }

  # "name" mode: find select_multiple columns from the separator + binary check
  else if (name_or_label_headers %in% c("name")) {

    select_multiple_questions <- names(dataset)[str_detect(names(dataset), paste0("[", separator, "]"))]
    select_multiple_questions <- select_multiple_questions[select_multiple_questions %in% rownames(binary_variables)]
    select_multiple_questions

    # if the survey has an "Analyse?" column, keep only questions flagged for analysis
    if ("Analyse?" %in% names(questions) & !all(is.na(questions$`Analyse?`)))
    {
      select_multiple_in_kobo <- questions %>% filter(str_detect(type, "select_multiple"))
      select_multiple_in_kobo$question <- gsub('select_multiple ', '', select_multiple_in_kobo$type)
      select_multiple_in_kobo <- left_join(select_multiple_in_kobo, choices, by = c("question" = "list_name"), multiple = "all")
      select_multiple_in_kobo <- paste0(select_multiple_in_kobo$name.x, separator, select_multiple_in_kobo$name.y)

      select_multiple_questions <- select_multiple_questions[select_multiple_questions %in% select_multiple_in_kobo]
      select_multiple_questions
    }

    if (length(select_multiple_questions) == 0) {
      print(paste0("\n\n Possible error! No select multiple questions was recognised in the dataset. Please be sure that
                   \n - the separator - ", separator, " - was selected correctly
                   \n - no column headers presents the separator in its name if it is not a select multiple question
                   \n - select multiple questions have binaries values"))
      select_multiple_questions

    } else if (name_or_label_headers %in% c("name")) {select_multiple_questions}
  }

  # "label" mode: build the list from the kobo tool instead (labels are less reliable to detect from column names alone)
  else if (name_or_label_headers %in% c("label")) {

    select_multiple_questions <- questions %>% filter(str_detect(type, "select_multiple"))
    select_multiple_questions$question <- gsub('select_multiple ', '', select_multiple_questions$type)
    select_multiple_questions <- left_join(select_multiple_questions, choices, by = c("question" = "list_name"), multiple = "all")
    select_multiple_questions <- paste0(select_multiple_questions$label.x, separator, select_multiple_questions$label.y)

    if (any(str_detect(names(dataset), pattern = "\\Q*%*\\E"))) {
      select_multiple_in_dataset <- names(dataset)[gsub(".*\\Q*%*\\E", "", names(dataset)) %in% select_multiple_questions]
    }

    if (any(str_detect(names(dataset), pattern = "\\Q*%%*\\E"))) {
      select_multiple_in_dataset <- names(dataset)[gsub(".*\\Q*%%*\\E", "", names(dataset)) %in% select_multiple_questions]
    }

    if (length(select_multiple_in_dataset) == 0) {
      print("\n\n Possible error!
         \n No select multiple questions were identified in the dataset. Be sure the correct separator was selected during the import")
      select_multiple_questions
    }

    else if (length(select_multiple_in_dataset[!select_multiple_in_dataset %in% rownames(binary_variables)]) > 0) {
      print(paste0("\n\n Possible error! The following variables were identified as select multiples but are not binaries", select_multiple_in_dataset[!select_multiple_in_dataset %in% rownames(binary_variables)]))
      select_multiple_questions <- select_multiple_in_dataset
      select_multiple_questions
    }
    else if (length(select_multiple_in_dataset[!select_multiple_in_dataset %in% rownames(binary_variables)]) == 0) {
      select_multiple_questions <- select_multiple_in_dataset
      select_multiple_questions

    }
  }
}

#' Find "ratio" indicators: rows of the questions sheet with type == "ratio",
#' where the numerator ("name") and the denominator
#' ("analysis_var_denominator") are both present in the dataset. Returns a
#' named character vector - names = numerator column, values = denominator
#' column - consumed directly by apply_analysis()'s ratio branch.
select_ratio_from_dataset <- function(questions, choices, dataset, name_or_label_headers, label_column) {

  if (!name_or_label_headers %in% c("name", "label")) {
    stop("\n\nin name_or_label write either name or label, depending if the dataset is in HML or word format")
  }

  else if (any(duplicated(names(dataset)))) {
    stop("\n\nDUPLICATES LABELS FOUND IN COLUMN HEADERS!.
         \n\nPlease be sure to follow the instructions given by the check_for_duplicate_headers function before proceeding.")
  }

  else if (!"analysis_var_denominator" %in% names(questions)) {
    cat("\n\nNo ratio question analysed: the questions sheet has no 'analysis_var_denominator' column.")
    setNames(character(0), character(0))
  }

  else if (name_or_label_headers %in% c("name")) {
    ratio_rows <- questions %>%
      filter(
        type == "ratio",
        name %in% names(dataset),
        !is.na(analysis_var_denominator),
        analysis_var_denominator %in% names(dataset)
      )

    if (nrow(ratio_rows) == 0) {
      cat("\n\nNo ratio question analysed: no 'type == ratio' row in the questions sheet has both its numerator ('name') and its denominator ('analysis_var_denominator') present in the dataset.")
      setNames(character(0), character(0))
    } else {
      setNames(ratio_rows$analysis_var_denominator, ratio_rows$name)
    }
  }

  else if (name_or_label_headers %in% c("label")) {
    cat("\n\nNo ratio question analysed: ratio questions are only supported in 'name' mode (they are matched directly on 'name' / 'analysis_var_denominator' from the questions sheet).")
    setNames(character(0), character(0))
  }

}

select_integer_from_dataset <- function(questions, choices, dataset, name_or_label_headers, label_column) {

  if (!name_or_label_headers %in% c("name", "label") | !name_or_label_values %in% c("name", "label")) {
    stop("\n\nin name_or_label write either name or label, depending if the dataset is in HML or word format")
  }

  else if (any(duplicated(names(dataset)))) {
    stop("\n\nDUPLICATES LABELS FOUND IN COLUMN HEADERS!. \n\nPlease be sure to follow the instructions given by the check_for_duplicate_headers function before proceeding.")
  }

  else if (name_or_label_headers %in% c("name")) {
    integer_question <- (questions %>% filter(str_detect(type, "integer") | str_detect(type, "recoded_variable_numeric"), name %in% names(dataset)))$name
    if (length(integer_question) == 0) {
      cat("\n\nPossible erreur! Be sure you selected the correct variables (especially the name_or_label_headers one)
        ")
      integer_question <- integer_question[integer_question %in% names(dataset)]
      integer_question
    }
    else if (length(integer_question) > 0) {
      dataset[integer_question] <- lapply(dataset[integer_question], as.character)
      integer_question
    }
  }

  else if (name_or_label_headers %in% c("label")) {
    integer_question <- names(dataset)[gsub(".*\\Q*%*\\E", "", names(dataset)) %in% (filter(questions, str_detect(type, "integer")) %>% pull(!!sym(label_column)))]
    if (length((questions %>% filter(str_detect(type, "recoded_variable_numeric"), !!sym(label_column) %in% names(dataset)))$name) > 0) {
      integer_question <- c(integer_question, (questions %>% filter(str_detect(type, "recoded_variable_numeric"), !!sym(label_column) %in% names(dataset)))[[label_column]])
    }
    if (length(integer_question) == 0) {
      print("Possible erreur! Be sure you selected the correct variables (especially the name_or_label_headers one)
        ")
      integer_question
    }
    else if (length(integer_question) > 0) {
      dataset[integer_question] <- lapply(dataset[integer_question], as.numeric)
      integer_question
    }
  }

}
