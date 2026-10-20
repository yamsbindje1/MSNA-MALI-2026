# ============================================================================
# check_for_duplicate_headers() - makes sure the dataset has no duplicated
# column headers before analysis. If headers are "name" format (our case),


check_for_duplicate_headers <- function(questions, choices, dataset, name_or_label_headers, label_column, ordered = NULL, name_or_label_values = name_or_label_headers) {

  if (!name_or_label_headers %in% c("name", "label") | !name_or_label_values %in% c("name", "label")) {
    stop("\n\nin name_or_label write either name or label, depending if the dataset is in HML or word format")
  }

  else if (any(duplicated(str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""))) == F) {
    print("\n\nAll good")

    list(dataset, questions)
  }

  else if (name_or_label_headers == "name" & any(duplicated(str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = "")))) {
    stop("\n\nDUPLICATES LABELS FOUND IN COLUMN HEADERS!. Before importing the dataset, make sure no there are no header duplicates.")
  }

  else if (name_or_label_headers %in% c("label") & any(duplicated(str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""))) & is.null(ordered)) {
    stop("\n\nDUPLICATES LABELS FOUND IN COLUMN HEADERS!
            \nIn order to continue with the analysis, the easiest thing to do is to download the dataset in XML format from Kobo and copy paste its headers to the clean dataset to be analysed.
            \nIf this is not possible for some reason (no internet connection) add - ordered- as argument to this function :
            \n  - ordered == T if the original order of dataset columns has not been changed
            \n  - ordered == F if the dataset is not ordered following kobo
            \nFor next time, please make sure to avoid duplicates in labels when coding the questionnaire in kobo")
  }

  else if (name_or_label_headers %in% c("label") & any(duplicated(str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""))) & !ordered %in% c(T, F)) {
    stop("\n\nYOU HAVE TO SPECIFY THE ARGUMENT - ordered - in the function using either T or F!
         \nPlease specify in the ordered argument if the dataset is ordered following the kobo form (ordered ==T) or not (ordered ==F)")
  }

  else if (name_or_label_headers %in% c("label") & any(duplicated(str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""))) & ordered == T) {
    cat("\n\nDuplicate labels in dataset headers resolved by matching their order with those of the kobo survey document
         \nCheck for -replacement of duplicate column names.xlsx- file in input folder for correspondence")

    new_headers <- data.frame(label = str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""), label_dataset = names(dataset), exist = "T")
    new_headers <- new_headers %>% group_by(label) %>%
      mutate(n = n(), code_question = row_number()) %>% ungroup() %>%
      mutate(header_dataset_duplicates = ifelse(n == 1, label, paste0(label, "_", code_question)))

    all <- expand.grid(LETTERS, LETTERS)
    all <- all[order(all$Var1, all$Var2), ]
    out <- c(LETTERS, do.call('paste0', all))

    new_headers$excel_column <- out[1:nrow(new_headers)]

    fichier_correspondance <- fichier_correspondance_function(
      questions = tool.survey,
      choices = tool.choices, label_column = label_column,
      separator = separator
    )
    fichier_correspondance <- fichier_correspondance %>% filter(!type %in% c("end_group", "begin_group"))
    fichier_correspondance <- left_join(questions %>% select(name), fichier_correspondance, by = c("name" = "questions in HML"), multiple = "all")

    aa <- fichier_correspondance %>% group_by(`label questions`, `label choices`) %>%
      mutate(n = n(), code_question = row_number()) %>% ungroup() %>%
      mutate(header_dataset_duplicates = ifelse(n == 1, `label questions`, paste0(`label questions`, "_", code_question))) %>%
      mutate(header_dataset_duplicates = ifelse(n > 1 & str_detect(type, "select_multiple"), paste0(`label questions`, separator, `label choices`, "_", code_question), header_dataset_duplicates))

    ww <- aa %>% filter(n > 1) %>% right_join(new_headers %>% filter(n > 1), by = c("header_dataset_duplicates"), multiple = "all") %>%
      filter(exist == "T") %>% unique() %>% filter(!is.na(`label questions`)) %>%
      mutate(new_headers = ifelse(`n.x` == 1, label, paste0(name, "*%%*", label)))

    replace_new_headers <- new_headers %>% left_join(ww %>% select(label_dataset, new_headers, type), by = c("label_dataset" = "label_dataset"), multiple = "all") %>%
      unique() %>% mutate(new_headers = ifelse(is.na(new_headers), label_dataset, new_headers)) %>% select(-code_question, -exist, -header_dataset_duplicates) %>%
      relocate(excel_column) %>% relocate(n, .after = "label")

    names(replace_new_headers)[names(replace_new_headers) == "label"] <- "old_column_header"
    names(replace_new_headers)[names(replace_new_headers) == "label_dataset"] <- "column_header_as_imported_in_R"
    names(replace_new_headers)[names(replace_new_headers) == "n"] <- "number_of_column_headers_with_same_name"

    writexl::write_xlsx(replace_new_headers, "input/replacement of duplicate column names.xlsx")
    names(dataset) <- replace_new_headers$new_headers

    ee <- questions %>% filter(str_detect(type, "select_one") | str_detect(type, "select_one") | str_detect(type, "calculate") | str_detect(type, "integer")) %>% group_by(!!sym(label_column)) %>% summarise(count = n()) %>% ungroup() %>%
      filter(count > 1)

    ee <- ee[[label_column]]

    questions[[label_column]] <- ifelse(questions[[label_column]] %in% ee, paste0(questions$name, "*%%*", questions[[label_column]]), questions[[label_column]])
    list(dataset, questions)
  }

  else if (name_or_label_headers %in% c("label") & any(duplicated(str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""))) & ordered == F) {
    cat("\n\nDuplicate labels in dataset headers resolved by matching duplicate headers with the column letter as in excel
         \nCheck for -replacement of duplicate column names.xlsx- file in input folder for correspondence")

    new_headers <- data.frame(label = str_replace(names(dataset), pattern = "[.]{3}(?=[:digit:]).*", replace = ""), label_dataset = names(dataset), exist = "T")
    new_headers <- new_headers %>% group_by(label) %>%
      mutate(n = n(), code_question = row_number()) %>% ungroup() %>%
      mutate(header_dataset_duplicates = ifelse(n == 1, label, paste0(label, "_", code_question)))

    all <- expand.grid(LETTERS, LETTERS)
    all <- all[order(all$Var1, all$Var2), ]
    out <- c(LETTERS, do.call('paste0', all))

    new_headers$excel_column <- out[1:nrow(new_headers)]

    replace_new_headers <- new_headers %>% select(-code_question, -exist, -header_dataset_duplicates) %>%
      relocate(excel_column) %>% relocate(n, .after = "label") %>% mutate(new_headers = ifelse(n == 1, label, paste0("column ", excel_column, "*%*", label)))

    names(replace_new_headers)[names(replace_new_headers) == "label"] <- "old_column_header"
    names(replace_new_headers)[names(replace_new_headers) == "label_dataset"] <- "column_header_as_imported_in_R"
    names(replace_new_headers)[names(replace_new_headers) == "n"] <- "number_of_column_headers_with_same_name"

    write_xlsx(replace_new_headers, paste0("input/replacement of duplicate column names.xlsx"))

    names(dataset) <- replace_new_headers$new_headers
  }
  list(dataset, questions)
}
