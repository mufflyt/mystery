#' Write Arsenal Table Object to Word Document with Error Handling and Logging
#'
#' This function exports an Arsenal table object to a Word document for ease of review and sharing, logging each step and handling errors.
#'
#' @param object An Arsenal table object to export, typically created using `arsenal::tableby`.
#' @param filename A string representing the filename (without extension) for the output Word document.
#' @return None. Outputs a Word document to the specified location.
#'
#' @importFrom arsenal write2word
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' # Example 1: Export a table to Word
#' arsenal_tables_write2word(my_table, "physician_summary")
#'
#' # Example 2: Saving with a custom filename
#' arsenal_tables_write2word(my_table, "custom_output")
#'
#' # Example 3: Exporting a different Arsenal table object
#' another_table <- arsenal::tableby(~ var1 + var2, data = sample_data)
#' arsenal_tables_write2word(another_table, "analysis_output")
#' }
#' @export
arsenal_tables_write2word <- function(object, filename) {
  # Validate input parameters
  if (!is.data.frame(object)) {
    stop("Error: 'object' must be a data frame object.")
  }
  if (!is.character(filename)) {
    stop("Error: 'filename' must be a character string.")
  }

  # Check if the "tables" directory exists; if not, create it
  if (!dir.exists("tables")) {
    dir.create("tables")
  }

  # Create full path for the Word file
  word_path <- file.path("tables", paste0(filename, ".docx"))

  message("Creating Arsenal table as a Word document...")
  tryCatch({
    arsenal::write2word(
      object,
      file = word_path,
      keep.md = FALSE,
      quiet = TRUE
    )
  }, error = function(e) {
    stop("Error occurred while writing to Word document:", e$message)
  })

  # Print the full path to the saved file
  cat("Word file saved successfully at", word_path, "\n")
}
