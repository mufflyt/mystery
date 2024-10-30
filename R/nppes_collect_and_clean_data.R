#' Collect and clean the processed data
#'
#' This function collects and cleans the processed data from DuckDB.
#'
#' @details After collecting and cleaning the data, the next step is to save the cleaned data to a CSV file using `nppes_save_data_to_csv()`.
#'
#' @param processed_data A `tbl` object containing the processed data.
#' @return A cleaned data frame.
#' @examples
#' cleaned_data <- nppes_collect_and_clean_data(processed_data)
#' # Next step: nppes_save_data_to_csv()
nppes_collect_and_clean_data <- function(processed_data) {
  base::cat("Collecting and cleaning processed data...\n")

  cleaned_data <- processed_data %>%
    duckplyr::collect() %>%
    dplyr::mutate(
      `Provider Credential Text` = stringr::str_remove_all(`Provider Credential Text`, "[[\\p{P}][\\p{S}]]"),
      `Provider Credential Text` = stringr::str_remove_all(`Provider Credential Text`, "[:blank:]"),
      `Provider Credential Text` = stringr::str_to_upper(`Provider Credential Text`),
      dplyr::across(
        c(`Provider Business Mailing Address Postal Code`, `Provider Business Practice Location Address Postal Code`),
        ~ stringr::str_sub(., 1, 5)
      )
    ) %>%
    dplyr::filter(
      exploratory::str_detect(`Provider Credential Text`, "MD") | exploratory::str_detect(`Provider Credential Text`, stringr::fixed("DO"))
    ) %>%
    dplyr::mutate(
      `Provider Credential Text` = dplyr::recode(
        `Provider Credential Text`,
        "DO" = "DO", "DOFACOG" = "DO", "DOFACOGPA" = "DO", "DOFACOOG" = "DO", "DOFACOSDACFE" = "DO",
        "DOMD" = "DO", "DOMPH" = "DO", "DOMPHFAC" = "DO", "DOMS" = "DO", "DOPA" = "DO", "DOPC" = "DO",
        "DOPLLC" = "DO", .default = "MD"
      ),
      `Provider Gender Code` = dplyr::recode(`Provider Gender Code`, "F" = "Female", "M" = "Male")
    ) %>%
    dplyr::arrange(NPI)

  base::cat("Data collection and cleaning complete.\n")
  base::cat("Next step: nppes_save_data_to_csv()\n")
  return(cleaned_data)
}
