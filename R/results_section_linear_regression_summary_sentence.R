#' Generate Summary Sentence for Linear Regression on Race and Drive Time with Raw Proportions
#'
#' This function generates a summary sentence using linear regression to analyze the trend in the proportion of women without access to a gynecologic oncologist over time for a specified race and driving time. It includes the raw proportions for the first and last years in the dataset.
#'
#' @param tabulated_data A data frame containing the data to analyze. Must include columns `Driving Time (minutes)`, `Year`, and columns for race proportions like `White_prop`, `Black_prop`, etc.
#' @param driving_time_minutes A numeric value specifying the driving time in minutes to filter the data. Default is 180.
#' @param race A character string specifying the race for which to generate the summary sentence. Supported values are "White", "Black", "American Indian/Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", or "all" to generate sentences for all supported races. Default is "American Indian/Alaska Native".
#'
#' @return A character string containing the summary sentence, or a list of summary sentences if `race = "all"`.
#' @examples
#' # Example usage
#' summary_sentence <- linear_regression_race_drive_time_generate_summary_sentence(
#'   tabulated_data = tabulated_all_years_numeric,
#'   driving_time_minutes = 180,
#'   race = "White"
#' )
#' print(summary_sentence)
#' @import dplyr
#' @importFrom stats lm na.omit poisson sd setNames
#' @export
linear_regression_race_drive_time_generate_summary_sentence <- function(tabulated_data, driving_time_minutes = 180, race = "American Indian/Alaska Native") {
  # Log the function call with input arguments
  message("Function linear_regression_race_drive_time_generate_summary_sentence called with inputs:")
  message("Driving Time (minutes): ", driving_time_minutes)
  message("Race: ", race)

  # Define a mapping of race names to their corresponding columns in the data
  race_column_map <- list(
    "White" = "White_prop",
    "Black" = "Black_prop",
    "American Indian/Alaska Native" = "AIAN_prop",
    "Asian" = "Asian_prop",
    "Native Hawaiian or Pacific Islander" = "NHPI_prop"
  )

  # If race is "all", loop through all supported races
  if (race == "all") {
    all_summaries <- lapply(names(race_column_map), function(current_race) {
      message("Processing race: ", current_race)
      linear_regression_race_drive_time_generate_summary_sentence(
        tabulated_data = tabulated_data,
        driving_time_minutes = driving_time_minutes,
        race = current_race
      )
    })
    names(all_summaries) <- names(race_column_map)
    return(all_summaries)
  }

  # Check if the race is supported
  if (!race %in% names(race_column_map)) {
    stop("Unsupported race specified. Supported values are: ", paste(names(race_column_map), collapse = ", "), " or 'all'.")
  }

  # Determine the correct column for the race proportion
  race_column <- race_column_map[[race]]

  # Log the race column being used
  message("Race column used for analysis: ", race_column)

  # Validate input data
  if (!"Driving Time (minutes)" %in% names(tabulated_data)) {
    stop("The input data must contain the 'Driving Time (minutes)' column.")
  }
  if (!race_column %in% names(tabulated_data)) {
    stop("The input data must contain the column: ", race_column)
  }
  if (!"Year" %in% names(tabulated_data)) {
    stop("The input data must contain the 'Year' column.")
  }

  # Filter the dataframe to include only rows where Driving Time is as specified
  filtered_data <- tabulated_data %>%
    dplyr::filter(`Driving Time (minutes)` == driving_time_minutes)

  # Log the number of rows after filtering
  message("Number of rows after filtering for Driving Time (minutes) = ", driving_time_minutes, ": ", nrow(filtered_data))

  # Check if the filtered data is empty
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified 'Driving Time (minutes)'.")
  }

  # Ensure the Year column is numeric
  filtered_data <- filtered_data %>%
    dplyr::mutate(Year = as.numeric(as.character(Year)))

  # Log the transformation of the Year column
  message("Year column converted to numeric.")

  # Fit a linear model to analyze the trend over time
  tryCatch({
    linear_model <- lm(as.formula(paste(race_column, "~ Year")), data = filtered_data)
    model_summary <- summary(linear_model)
  }, error = function(e) {
    stop("Error in linear regression model: ", e$message)
  })

  # Extract slope and p-value for Year
  slope <- model_summary$coefficients["Year", "Estimate"]
  p_value_lm <- model_summary$coefficients["Year", "Pr(>|t|)"]

  # Format the p-value
  formatted_p_value_lm <- ifelse(p_value_lm < 0.01, "<0.01", format(p_value_lm, digits = 2))

  # Log the results
  message("Linear model p-value for Year: ", formatted_p_value_lm)
  message("Slope of the trend for Year: ", slope)

  # Determine trend direction and significance
  trend_direction <- ifelse(slope > 0, "upward", "downward")
  trend_significance <- ifelse(p_value_lm < 0.05, "significant", "not significant")
  trend_impact <- ifelse(slope > 0, "worsening", "improvement")

  # Extract the raw proportions for the initial and final years
  initial_year <- min(filtered_data$Year, na.rm = TRUE)
  final_year <- max(filtered_data$Year, na.rm = TRUE)
  initial_proportion <- filtered_data %>% dplyr::filter(Year == initial_year) %>% dplyr::pull(!!sym(race_column))
  final_proportion <- filtered_data %>% dplyr::filter(Year == final_year) %>% dplyr::pull(!!sym(race_column))

  # Log the raw proportions
  message("Initial proportion (", initial_year, "): ", initial_proportion)
  message("Final proportion (", final_year, "): ", final_proportion)

  # Generate the summary sentence
  summary_sentence <- paste(
    "The p-value for the Year variable is", formatted_p_value_lm,
    "indicating a", trend_significance, trend_direction, "trend in the proportion of", race,
    "women without access to a gynecologic oncologist over time.",
    "The", ifelse(slope > 0, "positive", "negative"), "slope (", round(slope, 5), ") shows that this proportion has been",
    ifelse(slope > 0, "increasing", "decreasing"), "each year, suggesting a", trend_impact, "in access to care for", race, "women over the study period.",
    "The proportion of", race, "women without access changed from", round(initial_proportion * 100, 2), "% in", initial_year,
    "to", round(final_proportion * 100, 2), "% in", final_year, "."
  )

  # Log the generated summary sentence
  message("Generated summary sentence: ", summary_sentence)

  # Return the summary sentence
  return(summary_sentence)
}
