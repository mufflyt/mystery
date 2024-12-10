#' Analyze Pairwise Trends Programmatically
#'
#' This function performs pairwise Kruskal-Wallis tests on a numeric variable across specified x-axis groups,
#' analyzes directionality trends, identifies significance trends, and generates interpretive sentences.
#'
#' @param data A tibble containing at least the columns specified in `x_var` and `numeric_var`.
#' @param numeric_var A string specifying the name of the numeric variable to analyze.
#' @param x_var A string specifying the name of the grouping (x-axis) variable to analyze trends across.
#' @return A list containing `pairwise_trends`, `trend_summary`, and `interpretation_sentences`.
#' @importFrom logger log_info
#' @importFrom dplyr filter group_by summarise mutate left_join arrange bind_rows select
#' @importFrom stats kruskal.test
#' @importFrom tibble tibble
#' @importFrom stringr str_c
#' @importFrom glue glue
#' @examples
#' # Example 1: Basic analysis with three scenarios
#' example_data <- tibble::tibble(
#'   scenario = rep(c("Group_A", "Group_B", "Group_C"), each = 30),
#'   measurement = c(rnorm(30, mean = 10), rnorm(30, mean = 15), rnorm(30, mean = 20))
#' )
#' result <- result_section_pairwise_difference(
#'   data = example_data, numeric_var = "measurement", x_var = "scenario"
#' )
#' print(result$pairwise_trends)
#' print(result$trend_summary)
#' print(result$interpretation_sentences)
#' @export
result_section_pairwise_difference <- function(data, numeric_var, x_var) {
  # Log inputs
  logger::log_info("Starting result_section_pairwise_difference...")
  logger::log_info("Input tibble dimensions: {nrow(data)} rows, {ncol(data)} columns.")
  logger::log_info("Analyzing numeric variable: {numeric_var}, grouped by: {x_var}.")

  # Validation for unique groups
  if (length(unique(data[[x_var]])) < 2) {
    stop(glue::glue("The variable '{x_var}' must contain at least two unique groups for pairwise comparisons."))
  }

  # Step 1: Perform pairwise comparisons
  pairwise_trends <- perform_pairwise_tests(data, numeric_var, x_var)

  # Step 2: Analyze trends
  direction_summary <- analyze_directionality(pairwise_trends, x_var)
  significance_summary <- analyze_significance(pairwise_trends, x_var)

  # Step 3: Combine trends
  trend_summary <- combine_trends(direction_summary, significance_summary)

  # Step 4: Calculate medians and percentiles for each group
  logger::log_info("Calculating medians and percentiles for each group in {x_var}...")
  summary_stats <- data %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::summarise(
      Median = formatC(median(.data[[numeric_var]], na.rm = TRUE), big.mark = ",", format = "f"),
      Q1 = formatC(quantile(.data[[numeric_var]], 0.25, na.rm = TRUE), big.mark = ",", format = "f"),
      Q3 = formatC(quantile(.data[[numeric_var]], 0.75, na.rm = TRUE), big.mark = ",", format = "f"),
      .groups = "drop"
    )

  logger::log_info("Medians and percentiles calculated successfully.")

  # Step 5: Generate interpretive sentences
  logger::log_info("Generating interpretive sentences...")
  interpretation_sentences <- generate_interpretations(pairwise_trends, summary_stats, x_var)

  # Log outputs
  logger::log_info("Pairwise trend analysis completed successfully.")
  logger::log_info("Returning pairwise trends, trend summary, and interpretation sentences.")

  return(list(pairwise_trends = pairwise_trends, trend_summary = trend_summary, interpretation_sentences = interpretation_sentences))
}

# Helper function: Perform pairwise Kruskal-Wallis tests
perform_pairwise_tests <- function(data, numeric_var, x_var) {
  logger::log_info("Step 1: Performing pairwise Kruskal-Wallis tests...")
  groups <- unique(data[[x_var]])
  logger::log_info("Unique groups identified in {x_var}: {length(groups)}")

  results <- combn(groups, 2, simplify = FALSE, FUN = function(pair) {
    logger::log_info("Comparing groups: {pair[1]} vs {pair[2]}")

    group1 <- dplyr::filter(data, .data[[x_var]] == pair[1])[[numeric_var]]
    group2 <- dplyr::filter(data, .data[[x_var]] == pair[2])[[numeric_var]]

    test_result <- stats::kruskal.test(list(group1, group2))
    coef_direction <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
    direction <- ifelse(coef_direction > 0, "Higher", "Lower")

    tibble::tibble(
      Group1 = pair[1],
      Group2 = pair[2],
      Direction = direction,
      p_value = test_result$p.value,
      p_value_formatted = ifelse(
        test_result$p.value < 0.01, "p<0.01",
        stringr::str_c("p=", round(test_result$p.value, 3))
      )
    )
  }) %>%
    dplyr::bind_rows()

  logger::log_info("Pairwise comparisons completed. Total comparisons: {nrow(results)}.")
  return(results)
}

# Helper function: Analyze directionality trends
analyze_directionality <- function(pairwise_trends, x_var) {
  logger::log_info("Step 2: Analyzing directionality trends...")

  direction_summary <- pairwise_trends %>%
    dplyr::group_by(Group1, Group2) %>%
    dplyr::summarise(
      Higher = sum(Direction == "Higher"),
      Lower = sum(Direction == "Lower"),
      .groups = "drop"
    )

  logger::log_info("Directionality analysis completed successfully.")
  return(direction_summary)
}

# Helper function: Analyze significance trends
analyze_significance <- function(pairwise_trends, x_var) {
  logger::log_info("Step 3: Analyzing significance trends...")

  significance_summary <- pairwise_trends %>%
    dplyr::filter(p_value < 0.05) %>%
    dplyr::group_by(Group1, Group2) %>%
    dplyr::summarise(Significant = n(), .groups = "drop")

  logger::log_info("Significance analysis completed successfully.")
  return(significance_summary)
}

# Helper function: Combine trends
combine_trends <- function(direction_summary, significance_summary) {
  logger::log_info("Step 4: Combining directionality and significance trends...")

  trend_summary <- dplyr::left_join(
    direction_summary, significance_summary, by = c("Group1", "Group2")
  ) %>%
    dplyr::mutate(Significant = ifelse(is.na(Significant), 0, Significant))

  logger::log_info("Trends combined successfully.")
  return(trend_summary)
}

# Helper function: Generate interpretive sentences
generate_interpretations <- function(pairwise_trends, summary_stats, x_var) {
  sentences <- pairwise_trends %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Sentence = {
        group1_stats <- summary_stats %>% dplyr::filter(.data[[x_var]] == Group1)
        group2_stats <- summary_stats %>% dplyr::filter(.data[[x_var]] == Group2)

        glue::glue(
          "Payments for {Group1} (median: ${group1_stats$Median}; IQR [${group1_stats$Q1} - ${group1_stats$Q3}]) are ",
          "{tolower(Direction)} than payments for {Group2} (median: ${group2_stats$Median}; IQR [${group2_stats$Q1} - ${group2_stats$Q3}], {p_value_formatted})."
        )
      }
    ) %>%
    dplyr::pull(Sentence)

  logger::log_info("Interpretive sentences generated successfully.")
  return(sentences)
}
