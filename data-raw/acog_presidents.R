#' Data: ACOG Presidents
#' @description A dataset containing information about ACOG presidents.
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#'   \item{President}{Name of the president.}
#'   \item{Year}{Year they served.}
#' }
acog_presidents <- exploratory::scrape_html_table("https://www.acog.org/about/leadership-and-governance/board-of-directors/past-presidents", 1, "TRUE", encoding = "UTF-8") %>%
  exploratory::clean_data_frame() %>%
  dplyr::distinct(President, .keep_all = TRUE) %>%
  tidyr::separate(President, into = c("President", "honorrific"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  dplyr::mutate(first = humaniformat::first_name(President)) %>%
  dplyr::mutate(last = humaniformat::last_name(President)) %>%
  dplyr::mutate(middle = humaniformat::middle_name(President)) %>%
  dplyr::mutate(middle = stringr::str_remove_all(middle, "[[\\p{P}][\\p{S}]]")) %>%
  exploratory::reorder_cols(first, last, middle, President, honorrific, Presidency) %>%
  dplyr::mutate(Presidency = stringr::word(Presidency, 1, sep = "\\s*\\-\\s*")) %>%
  dplyr::mutate(Presidency = readr::parse_number(Presidency)) %>%
  dplyr::arrange(desc(Presidency)) %>%
  dplyr::mutate(first = str_remove_all(first, "[[\\p{P}][\\p{S}]]"))

readr::write_csv(acog_presidents, "data-raw/acog_presidents.csv")

usethis::use_data(acog_presidents, overwrite = TRUE)
