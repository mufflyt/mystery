#' Utility to unwrap a list column to its first value
#'
#' @param x A list or vector to unwrap
#' @return The first element as a character string, or NA if the list is empty
#' @export
clean_column_unwrap <- function(x) {
  if (length(x) > 0) {
    return(as.character(x[[1]]))
  }
  return(NA)
}
