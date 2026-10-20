# Frequent expenditure


main_dataset <- add_expenditure_type_zero_freq(
    main_dataset,
    expenditure_freq = "cm_expenditure_frequent",
    none = "none",
    undefined = c("dnk", "pnta"),
    expenditure_freq_types = c(
      "cm_expenditure_frequent_food",
      "cm_expenditure_frequent_rent",
      "cm_expenditure_frequent_water",
      "cm_expenditure_frequent_nfi",
      "cm_expenditure_frequent_utilities",
      "cm_expenditure_frequent_fuel",
      "cm_expenditure_frequent_transportation",
      "cm_expenditure_frequent_communication"
    )
) 

main_dataset <- main_dataset %>%
  dplyr::mutate(
    cm_expenditure_frequent_other = as.numeric(cm_expenditure_frequent_other)
  )

main_dataset <- add_expenditure_type_prop_freq(
  main_dataset,
    cm_expenditure_frequent_food = "cm_expenditure_frequent_food",
    cm_expenditure_frequent_rent = "cm_expenditure_frequent_rent",
    cm_expenditure_frequent_water = "cm_expenditure_frequent_water",
    cm_expenditure_frequent_nfi = "cm_expenditure_frequent_nfi",
    cm_expenditure_frequent_utilities = "cm_expenditure_frequent_utilities",
    cm_expenditure_frequent_fuel = "cm_expenditure_frequent_fuel",
    cm_expenditure_frequent_transportation = "cm_expenditure_frequent_transportation",
    cm_expenditure_frequent_communication = "cm_expenditure_frequent_communication",
    cm_expenditure_frequent_other = "cm_expenditure_frequent_other"
)

main_dataset <- add_expenditure_type_freq_rank(
  main_dataset,
    expenditure_freq_types = c(
      "cm_expenditure_frequent_food",
      "cm_expenditure_frequent_rent",
      "cm_expenditure_frequent_water",
      "cm_expenditure_frequent_nfi",
      "cm_expenditure_frequent_utilities",
      "cm_expenditure_frequent_fuel",
      "cm_expenditure_frequent_transportation",
      "cm_expenditure_frequent_communication",
      "cm_expenditure_frequent_other"
    ),
    id_col = "uuid"
)
main_dataset <- main_dataset %>%
  dplyr::mutate(
    cm_expenditure_infrequent_nfi = as.numeric(cm_expenditure_infrequent_nfi),
    cm_expenditure_infrequent_other = as.numeric(cm_expenditure_infrequent_other)
  )
# Infrequent expenditure
main_dataset <- add_expenditure_type_zero_infreq(main_dataset)
main_dataset <- add_expenditure_type_prop_infreq(main_dataset)
main_dataset <- add_expenditure_type_infreq_rank(main_dataset)


