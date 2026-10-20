# ============================================================================
# EXPORT HELPERS - openxlsx styles + "variable x group" workbook builders
# ============================================================================

spread_group_var <- function(df, groups = disaggregation, group_var_col = "group_var", value_col = "group_var_value") {
  df <- as.data.frame(df)
  # groups can be a plain vector (column name == group_var value to match) or
  # a named vector (name = output column, value = group_var value to match)
  match_values <- if (is.null(names(groups))) setNames(groups, groups) else groups
  out_names <- names(match_values)
  for (i in seq_along(match_values)) {
    df[[out_names[i]]] <- ifelse(df[[group_var_col]] == match_values[i], df[[value_col]], NA_character_)
  }
  df <- df %>% select(-all_of(value_col)) %>% relocate(all_of(out_names), .after = all_of(group_var_col))
  df
}

## ---- Styles ----------------------------------------------------------------

number_2digits_style <- openxlsx::createStyle(numFmt = "0.00")
number_style <- openxlsx::createStyle(numFmt = "0")
proportion_number_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")

thin_allborder_style <- openxlsx::createStyle(
  borderStyle = "thin",
  border = "TopBottomLeftRight"
)

thick_allborder_style <- openxlsx::createStyle(
  borderStyle = "thick",
  border = "TopBottomLeftRight"
)

thick_left_borderstyle <- openxlsx::createStyle(borderStyle = "thick", border = "left")
thick_top_borderstyle <- openxlsx::createStyle(borderStyle = "thick", border = "top")
thick_right_borderstyle <- openxlsx::createStyle(borderStyle = "thick", border = "right")
thick_bottom_borderstyle <- openxlsx::createStyle(borderStyle = "thick", border = "bottom")

thin_allborder_style <- openxlsx::createStyle(
  borderStyle = "thin",
  border = "TopBottomLeftRight"
)

header_style <- openxlsx::createStyle(
  fontSize = 12,
  fontColour = "#FFFFFF",
  halign = "center",
  valign = "center",
  fontName = "Arial Narrow",
  textDecoration = "bold",
  fgFill = "#ee5859",
  border = "TopBottomLeftRight ",
  borderColour = "#fafafa",
  wrapText = T
)

secondary_grey_cell_style <- openxlsx::createStyle(
  fontSize = 12,
  fontColour = "#58585A",
  fontName = "Arial Narrow",
  fgFill = "#c7c8ca",
  border = "TopBottomLeftRight ",
  wrapText = FALSE
)

secondary_beige_cell_style <- openxlsx::createStyle(
  fontSize = 12,
  fontColour = "#58585A",
  fontName = "Arial Narrow",
  fgFill = "#D2CBB8",
  border = "TopBottomLeftRight",
  wrapText = FALSE
)

secondary_white_cell_style <- openxlsx::createStyle(
  fontSize = 12,
  fontColour = "#58585A",
  fontName = "Arial Narrow",
  fgFill = NULL,
  border = "TopBottomLeftRight",
  wrapText = FALSE
)

## ---- Add one formatted "variable x group" sheet to a workbook -------------

add_variable_x_group_sheet <- function(wb, sheet_name, results_table_group_x_variable,
                                        value_columns = c("stat", "stat_low", "stat_upp"),
                                        total_columns = NULL) {
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::writeData(wb, sheet = sheet_name, x = results_table_group_x_variable)
  openxlsx::addStyle(wb, sheet = sheet_name, header_style,
                     rows = 1, cols = 1:ncol(results_table_group_x_variable),
                     gridExpand = TRUE)
  variable_min_cols_index <- 1
  variable_max_cols_index <- 4
  openxlsx::addStyle(wb, sheet = sheet_name, style = secondary_beige_cell_style,
                     rows = c(1:nrow(results_table_group_x_variable) + 1),
                     cols = c(variable_min_cols_index:variable_max_cols_index),
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name, style = thick_left_borderstyle,
                     rows = c(1:nrow(results_table_group_x_variable) + 1),
                     cols = c(variable_min_cols_index, variable_min_cols_index),
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name, style = thick_top_borderstyle,
                     rows = c(2:2), cols = c(variable_min_cols_index:variable_max_cols_index),
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name, style = thick_right_borderstyle,
                     rows = c(1:nrow(results_table_group_x_variable) + 1),
                     cols = c(variable_max_cols_index:variable_max_cols_index),
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name, style = thick_bottom_borderstyle,
                     rows = c((nrow(results_table_group_x_variable) + 1):(nrow(results_table_group_x_variable) +
                                                                            1)), cols = c(variable_min_cols_index:variable_max_cols_index),
                     gridExpand = TRUE, stack = TRUE)
  stat_min_cols_index <- variable_max_cols_index + 1
  stat_max_cols_index <- ncol(results_table_group_x_variable)
  stat_length <- length(c(value_columns, total_columns))
  stat_total_cols <- stat_max_cols_index - variable_max_cols_index
  stat_sets_number <- stat_total_cols/stat_length
  helper_table <- data.frame(sets = paste0("set", 1:stat_sets_number),
                             min_cols_index = cumsum(c(stat_min_cols_index, rep(stat_length,
                                                                                stat_sets_number - 1))), max_cols_index = cumsum(c(variable_max_cols_index +
                                                                                                                                     stat_length, rep(stat_length, stat_sets_number -
                                                                                                                                                        1))))
  if ((stat_total_cols%%stat_sets_number) != 0) {
    stop("Length of value_columns does not match with the table.")
  }
  if (stat_length == 1) {
    warning("Length of value_columns is one, function cannot checks the number of columns.")
  }
  value_columns_index <- stringr::str_starts(names(results_table_group_x_variable),
                                             stringr::str_c(value_columns, collapse = "|"))
  proportion_lines <- grepl("prop_select", c("title", results_table_group_x_variable$analysis_type))
  openxlsx::addStyle(wb, sheet = sheet_name, style = proportion_number_style,
                     rows = c(1:(nrow(results_table_group_x_variable) + 1))[proportion_lines],
                     cols = c(1:stat_max_cols_index)[value_columns_index],
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name, style = number_2digits_style,
                     rows = c(1:(nrow(results_table_group_x_variable) + 1))[!proportion_lines],
                     cols = c(1:stat_max_cols_index)[value_columns_index],
                     gridExpand = TRUE, stack = TRUE)
  if (!is.null(total_columns)) {
    total_columns_index <- stringr::str_starts(names(results_table_group_x_variable),
                                               stringr::str_c(total_columns, collapse = "|"))
    openxlsx::addStyle(wb, sheet = sheet_name, style = number_style,
                       rows = c(1:(nrow(results_table_group_x_variable) +
                                     1)), cols = c(1:stat_max_cols_index)[total_columns_index],
                       gridExpand = TRUE, stack = TRUE)
  }
  for (i in 1:nrow(helper_table)) {
    if ((i%%2) != 0) {
      openxlsx::addStyle(wb, sheet = sheet_name,
                         style = secondary_white_cell_style, rows = c(1:nrow(results_table_group_x_variable) +
                                                                        1), cols = c(helper_table[i, "min_cols_index"]:helper_table[i,
                                                                                                                                    "max_cols_index"]), gridExpand = TRUE, stack = TRUE)
    }
    else {
      openxlsx::addStyle(wb, sheet = sheet_name,
                         style = secondary_grey_cell_style, rows = c(1:nrow(results_table_group_x_variable) +
                                                                       1), cols = c(helper_table[i, "min_cols_index"]:helper_table[i,
                                                                                                                                   "max_cols_index"]), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(wb, sheet = sheet_name, style = thick_left_borderstyle,
                       rows = c(1:nrow(results_table_group_x_variable) +
                                  1), cols = c(helper_table[i, "min_cols_index"]:helper_table[i,
                                                                                              "min_cols_index"]), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = thick_top_borderstyle,
                       rows = c(2:2), cols = c(helper_table[i, "min_cols_index"]:helper_table[i,
                                                                                              "max_cols_index"]), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = thick_right_borderstyle,
                       rows = c(1:nrow(results_table_group_x_variable) +
                                  1), cols = c(helper_table[i, "max_cols_index"]:helper_table[i,
                                                                                              "max_cols_index"]), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = thick_bottom_borderstyle,
                       rows = c((nrow(results_table_group_x_variable) +
                                   1):(nrow(results_table_group_x_variable) + 1)),
                       cols = c(helper_table[i, "min_cols_index"]:helper_table[i,
                                                                               "max_cols_index"]), gridExpand = TRUE, stack = TRUE)
  }
}

create_xlsx_variable_x_group_modifer <- function (table_group_x_variable, file_path = NULL, table_name = "variable_x_group_table",
          value_columns = c("stat", "stat_low", "stat_upp"), total_columns = NULL,
          readme_sheet_name = "readme", table_sheet_name = "variable_x_group_table",
          overwrite = FALSE)
{
  if (is.data.frame(table_group_x_variable)) {
    results_table_group_x_variable <- table_group_x_variable
    is_list <- FALSE
  }
  else {
    if (!table_name %in% names(table_group_x_variable)) {
      stop("Cannot identify one element of the list.")
    }
    results_table_group_x_variable <- table_group_x_variable[[table_name]]
    is_list <- TRUE
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = readme_sheet_name)
  add_variable_x_group_sheet(wb, table_sheet_name, results_table_group_x_variable,
                              value_columns = value_columns, total_columns = total_columns)
  if (is.null(file_path)) {
    return(wb)
  }
  else {
    if (stringr::str_detect(file_path, "\\.xlsx", negate = TRUE)) {
      stop("file_path does not containts .xlsx")
    }
    openxlsx::saveWorkbook(wb, file_path, overwrite = overwrite)
  }
}

## ---- One workbook, one sheet PER THEME (theme column) ---------------------
create_xlsx_variable_x_group_by_theme <- function(table_group_x_variable,
                                                  file_path,
                                                  theme_column = "theme",
                                                  value_columns = c("stat", "stat_low", "stat_upp"),
                                                  total_columns = NULL,
                                                  overwrite = FALSE) {

  wb <- openxlsx::createWorkbook()

  themes <- unique(table_group_x_variable[[theme_column]])
  themes <- themes[!is.na(themes) & themes != ""]

  if (length(themes) == 0)
    themes <- "sans_thematique"

  used_sheet_names <- character()

  for (th in themes) {

    sheet_data <- if (th == "sans_thematique") {
      table_group_x_variable[is.na(table_group_x_variable[[theme_column]]) |
                               table_group_x_variable[[theme_column]] == "", ]
    } else {
      table_group_x_variable[table_group_x_variable[[theme_column]] == th, ]
    }

    if (nrow(sheet_data) == 0) next

    sheet_name <- substr(gsub("[\\\\/?*\\[\\]:]", "_", th), 1, 31)

    if (sheet_name %in% used_sheet_names) {
      sheet_name <- substr(paste0(sheet_name, "_", length(used_sheet_names)), 1, 31)
    }

    used_sheet_names <- c(used_sheet_names, sheet_name)

    add_variable_x_group_sheet(
      wb = wb,
      sheet_name = sheet_name,
      results_table_group_x_variable  = sheet_data,
      value_columns = value_columns,
      total_columns = total_columns
    )
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = overwrite)

  message(length(used_sheet_names), " sheets exported to ", file_path)
}

## ---- One workbook, all variables in a single "Results" sheet --------------
create_xlsx_variable_x_group_all <- function(table_group_x_variable,
                                         file_path,
                                         value_columns = c("stat", "stat_low", "stat_upp"),
                                         total_columns = NULL,
                                         sheet_name = "Results",
                                         overwrite = FALSE) {

  wb <- openxlsx::createWorkbook()

  add_variable_x_group_sheet(
    wb = wb,
    sheet_name = sheet_name,
    results_table_group_x_variable  = table_group_x_variable,
    value_columns = value_columns,
    total_columns = total_columns
  )

  if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    stop("file_path must end with '.xlsx'")
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = overwrite)

  message("Global export completed: ", file_path)
}
