#' Convert State Abbreviations to Full State Names with Logging
#'
#' This function takes a vector of state abbreviations and returns the corresponding
#' full state names, including District of Columbia (DC) and Puerto Rico (PR). The
#' function logs the input, output, and all transformations via the console. The
#' function can handle standard state abbreviations as well as special cases such as
#' DC and PR.
#'
#' @param state_abbr A character vector containing state abbreviations. These can
#' include two-letter state codes like "NY" for New York, "CA" for California, and
#' special cases such as "DC" for District of Columbia and "PR" for Puerto Rico.
#' Abbreviations must be valid state abbreviations.
#'
#' @return A character vector with full state names corresponding to the input
#' abbreviations. Invalid abbreviations will be returned as `NA`.
#'
#' @examples
#' # Example 1: A simple case with a mix of state abbreviations
#' state_abbr <- c("NY", "CA", "TX", "DC", "PR")
#' convert_state_abbreviations(state_abbr)
#' # Expected output:
#' # "New York" "California" "Texas" "District of Columbia" "Puerto Rico"
#'
#' # Example 2: A vector of state abbreviations including only valid ones
#' state_abbr <- c("TX", "FL", "WI", "VA")
#' convert_state_abbreviations(state_abbr)
#' # Expected output:
#' # "Texas" "Florida" "Wisconsin" "Virginia"
#'
#' # Example 3: A vector of abbreviations with mixed cases and handling unknown abbreviations
#' state_abbr <- c("ny", "ca", "xx", "DC", "pr")
#' convert_state_abbreviations(state_abbr)
#' # Expected output:
#' # "New York" "California" NA "District of Columbia" "Puerto Rico"
#'
#' @importFrom dplyr recode
#' @importFrom logger log_info
#' @export
phase0_convert_state_abbreviations <- function(state_abbr) {

  # Log the input
  logger::log_info("Received input vector: %s", paste(state_abbr, collapse = ", "))

  # Get the state map and log the creation
  state_map <- create_state_map()
  logger::log_info("State map created with abbreviations and full names.")

  # Recode the state abbreviations using the state map
  recoded_states <- recode_state_abbr(state_abbr, state_map)

  # Log the output after transformation
  logger::log_info("Returning output vector: %s", paste(recoded_states, collapse = ", "))

  return(recoded_states)
}

#' Create a Mapping of State Abbreviations to Full Names
#'
#' This helper function creates a named vector mapping state abbreviations to
#' their corresponding full state names, including special cases like DC and
#' Puerto Rico.
#'
#' @noRd
create_state_map <- function() {
  state_map <- c(
    "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
    "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware",
    "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii", "ID" = "Idaho",
    "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa", "KS" = "Kansas",
    "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
    "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota",
    "MS" = "Mississippi", "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska",
    "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
    "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina",
    "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma", "OR" = "Oregon",
    "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
    "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah",
    "VT" = "Vermont", "VA" = "Virginia", "WA" = "Washington",
    "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming",
    "DC" = "District of Columbia", "PR" = "Puerto Rico"
  )

  return(state_map)
}

#' Recode State Abbreviations Using the Provided Mapping
#'
#' This helper function uses a vector of state abbreviations and recodes them
#' to their full names using a provided state map.
#'
#' @noRd
recode_state_abbr <- function(state_abbr, state_map) {
  # Recode state abbreviations to full names using the state_map
  recoded_states <- dplyr::recode(state_abbr, !!!state_map)

  # Leave invalid abbreviations as NA
  recoded_states[is.na(recoded_states)] <- NA

  return(recoded_states)
}
