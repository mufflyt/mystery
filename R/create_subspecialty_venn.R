#' Create a Subspecialty Venn Diagram
#'
#' This function generates a Venn diagram for a given subspecialty with detailed
#' logging of inputs, outputs, and transformations. It uses tidyverse-style
#' syntax for better clarity and the logger package for console-based logging.
#'
#' @param counts A named list containing counts for the Venn diagram. It must
#'   include the following elements: `businessDays`, `ableContact`,
#'   `acceptsPediatric`, `bd_ac`, `bd_ap`, `ac_ap`, `all_three`, and `total`.
#' @param title A character string specifying the title of the diagram.
#' @param verbose A logical value. If \code{TRUE}, logs detailed information
#'   about the process.
#'
#' @return The Venn diagram object.
#'
#' @importFrom VennDiagram draw.triple.venn
#' @importFrom grid grid.text grid.newpage gpar
#' @importFrom logger log_info
#' @importFrom assertthat assert_that
#'
#' @examples
#' # Example Venn Diagram for General Dermatology
#' counts <- list(
#'   businessDays = 92,
#'   ableContact = 137,
#'   acceptsPediatric = 131,
#'   bd_ac = 91,
#'   bd_ap = 92,
#'   ac_ap = 93,
#'   all_three = 91,
#'   total = 269
#' )
#' create_subspecialty_venn(counts, "General Dermatology", verbose = TRUE)
#'
#' @export
create_subspecialty_venn <- function(counts, title, verbose = FALSE) {
  # Validate inputs
  assertthat::assert_that(is.list(counts), msg = "Counts must be a named list.")
  required_keys <- c(
    "businessDays", "ableContact", "acceptsPediatric",
    "bd_ac", "bd_ap", "ac_ap", "all_three", "total"
  )
  missing_keys <- setdiff(required_keys, names(counts))
  assertthat::assert_that(
    length(missing_keys) == 0,
    msg = paste("The following keys are missing in counts:",
                paste(missing_keys, collapse = ", "))
  )
  assertthat::assert_that(
    is.character(title), msg = "Title must be a character string."
  )

  # Log inputs
  if (verbose) {
    logger::log_info("Inputs validated successfully.")
    logger::log_info("Counts input:")
    logger::log_info(counts)
    logger::log_info(glue::glue("Title: {title}"))
  }

  # Start new page for plot
  grid::grid.newpage()
  if (verbose) logger::log_info("Started new page for the Venn diagram.")

  # Generate the Venn diagram
  venn.plot <- VennDiagram::draw.triple.venn(
    area1 = counts$businessDays,
    area2 = counts$ableContact,
    area3 = counts$acceptsPediatric,
    n12 = counts$bd_ac,
    n23 = counts$ac_ap,
    n13 = counts$bd_ap,
    n123 = counts$all_three,
    category = c("Business Days > 0", "Able to Contact", "Accepts Pediatric"),
    fill = c("#85C1E9", "#F1948A", "#82E0AA"),
    alpha = 0.5,
    lty = "solid",
    lwd = 2,
    cex = 1.5,
    cat.cex = 1.5,
    cat.fontface = "bold",
    cat.col = c("#2874A6", "#C0392B", "#1D8348"),
    margin = 0.1,
    euler.d = TRUE,
    scaled = TRUE
  )
  if (verbose) logger::log_info("Venn diagram generated successfully.")

  # Add title
  grid::grid.text(
    label = paste0(title, " (n=", counts$total, ")"),
    x = 0.5,
    y = 0.9,
    gp = grid::gpar(fontsize = 18, fontface = "bold")
  )
  if (verbose) logger::log_info("Title added to the Venn diagram.")

  # Add legend
  legend_labels <- c(
    "Business Days > 0: Physicians with an appointment available",
    "Able to Contact: Successfully contacted physicians",
    "Accepts Pediatric: Physicians who accept pediatric patients"
  )
  grid::grid.text(
    label = paste(legend_labels, collapse = "\n"),
    x = 0.5,
    y = 0.1,
    just = "center",
    gp = grid::gpar(fontsize = 12, lineheight = 1.5)
  )
  if (verbose) logger::log_info("Legend added to the Venn diagram.")

  # Log outputs
  if (verbose) logger::log_info("Venn diagram generation complete.")

  return(venn.plot)
}
