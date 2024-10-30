#' Process an NPI table in DuckDB
#'
#' This function processes an NPI table in DuckDB, filtering by taxonomy codes and cleaning the data.
#'
#' @details After processing the NPI table, the next step is to collect and clean the data using `nppes_collect_and_clean_data()`.
#'
#' @param con A DuckDB connection object.
#' @param table_name A character string specifying the name of the table to process. Default is "npi_2024".
#' @param taxonomy_codes_1 A character vector specifying the taxonomy codes to filter in `Healthcare Provider Taxonomy Code_1`. Default is a predefined set of codes.
#' @param taxonomy_codes_2 A character vector specifying the taxonomy codes to filter in `Healthcare Provider Taxonomy Code_2`. Default is a predefined set of codes.
#' @return A `tbl` object containing the processed data.
#' @examples
#' con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
#' processed_data <- nppes_process_npi_table_chunk(con, "npi_2024", c("207V00000X"), c("207V00000X"))
#' # Next step: nppes_collect_and_clean_data()
#'
#' con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
#' processed_data <- nppes_process_npi_table_chunk(con)
#' # Next step: nppes_collect_and_clean_data()
nppes_process_npi_table_chunk <- function(con,
                                          table_name = "npi_2024",
                                          taxonomy_codes_1 = c(
                                            "390200000X", # students
                                            "174400000X", # specialists
                                            "207V00000X", # OBGYN codes
                                            "207VB0002X",
                                            "207VC0300X",
                                            "207VC0200X",
                                            "207VX0201X",
                                            "207VG0400X",
                                            "207VH0002X",
                                            "207VM0101X",
                                            "207VX0000X",
                                            "207VE0102X",
                                            "207VF0040X"
                                          ),
                                          taxonomy_codes_2 = c(
                                            "390200000X",
                                            "174400000X",
                                            "207V00000X",
                                            "207VB0002X",
                                            "207VC0300X",
                                            "207VC0200X",
                                            "207VX0201X",
                                            "207VG0400X",
                                            "207VH0002X",
                                            "207VM0101X",
                                            "207VX0000X",
                                            "207VE0102X",
                                            "207VF0040X"
                                          )
) {
  base::cat("Processing table", table_name, "...\n")

  npi_table <- dplyr::tbl(con, table_name)

  processed_data <- npi_table %>%
    duckplyr::select(
      NPI,
      `Entity Type Code`,
      `Provider First Name`,
      `Provider Middle Name`,
      `Provider Last Name (Legal Name)`,
      `Provider Other First Name`,
      `Provider Other Last Name`,
      `Provider Other Middle Name`,
      `Provider First Line Business Practice Location Address`,
      `Provider Business Practice Location Address City Name`,
      `Provider Business Practice Location Address State Name`,
      `Provider Business Practice Location Address Postal Code`,
      `Provider Business Practice Location Address Country Code (If outside U.S.)`,
      `Provider Business Practice Location Address Telephone Number`,
      `Provider First Line Business Mailing Address`,
      `Provider Business Mailing Address City Name`,
      `Provider Business Mailing Address State Name`,
      `Provider Business Mailing Address Country Code (If outside U.S.)`,
      `Provider Business Mailing Address Postal Code`,
      `Provider Gender Code`,
      `Provider Credential Text`,
      `Provider Organization Name (Legal Business Name)`,
      `Healthcare Provider Taxonomy Code_1`,
      `Healthcare Provider Taxonomy Code_2`,
      `Is Sole Proprietor`,
      `Certification Date`
    ) %>%
    duckplyr::filter(
      `Entity Type Code` == 1,
      `Provider Business Mailing Address Country Code (If outside U.S.)` == "US",
      `Provider Business Practice Location Address Country Code (If outside U.S.)` == "US",
      `Healthcare Provider Taxonomy Code_1` %in% taxonomy_codes_1 |
        `Healthcare Provider Taxonomy Code_2` %in% taxonomy_codes_2
    ) %>%
    duckplyr::select(-`Entity Type Code`, -`Provider Other First Name`, -`Provider Business Practice Location Address Country Code (If outside U.S.)`, -`Provider Business Mailing Address Country Code (If outside U.S.)`) %>%
    duckplyr::mutate(
      dplyr::across(
        c(
          `Provider First Name`,
          `Provider Last Name (Legal Name)`,
          `Provider First Line Business Practice Location Address`,
          `Provider Business Practice Location Address City Name`,
          `Provider First Line Business Mailing Address`,
          `Provider Business Mailing Address City Name`
        ),
        stringr::str_to_upper
      )
    ) %>%
    duckplyr::distinct(
      NPI,
      `Provider Business Practice Location Address City Name`,
      `Provider Business Practice Location Address State Name`,
      .keep_all = TRUE
    )

  base::cat("Table", table_name, "processed successfully.\n")
  base::cat("Next step: nppes_collect_and_clean_data()\n")
  return(processed_data)
}
