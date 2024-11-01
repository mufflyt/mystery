% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_section_fit_mixed_model_with_logging.R
\name{fit_mixed_model_with_logging}
\alias{fit_mixed_model_with_logging}
\title{Fit a Mixed-Effects Model with Logging and Robust Error Handling}
\usage{
fit_mixed_model_with_logging(
  data,
  response_var = "log_business_days_until_appointment",
  random_effect = "(1 | NPI)",
  exclude_vars = c(response_var, "last", "business_days_until_appointment",
    "cleaned_does_the_physician_accept_medicaid", "record_id", "ID", "middle",
    "physician_information", "address", "offered_a_clinic_appointment_to_be_seen",
    "reason_for_exclusions", "state", "Grd_yr", "age_category", "notes", "first",
    "does_the_physician_accept_medicaid", "insurance_type", "zip", "Subspecialty", "NPI",
    "lng", "lat", "including_this_physician_in_the_study",
    "told_to_go_to_the_emergency_department"),
  model_type = "lmer",
  significance_cutoff = 0.2,
  save_path = NULL
)
}
\arguments{
\item{data}{A data frame containing the dataset.}

\item{response_var}{A character string specifying the response variable. Default is \code{"log_business_days_until_appointment"}.}

\item{random_effect}{A character string specifying the random effect. Default is \code{"(1 | NPI)"}.}

\item{exclude_vars}{A character vector specifying the columns to exclude from the predictor variables. Default is a list of variables typically excluded in the analysis.}

\item{model_type}{A character string indicating the model type: \code{"lmer"} for linear mixed-effects or \code{"rlmer"} for robust linear mixed-effects. Default is \code{"lmer"}.}

\item{significance_cutoff}{A numeric value for the p-value threshold for filtering significant predictors. Default is \code{0.2}.}

\item{save_path}{Optional. A character string specifying the file path where the results should be saved as a CSV. If not provided, results will not be saved.}
}
\value{
A tibble with significant predictors, p-values, IRR (Incident Rate Ratios), confidence intervals, and the associated wait time effects.
}
\description{
This function fits either a linear mixed-effects model (\code{lmer}) or a robust linear mixed-effects model (\code{rlmer})
and logs every step. It is designed to handle errors robustly, provide progress and completion signals through system beeps, and
optionally save results to a file. The function is highly configurable but works out of the box with default settings.
}
\examples{
# Example 1: Basic usage with default settings
df <- my_data_frame
result <- fit_mixed_model_with_logging(data = df)

# Example 2: Using a robust linear mixed-effects model (rlmer) and saving the results
result <- fit_mixed_model_with_logging(data = df, model_type = "rlmer", save_path = "results_rlmer.csv")

# Example 3: Custom response variable and random effect, with significance level 0.05
result <- fit_mixed_model_with_logging(data = df,
                                       response_var = "some_other_response",
                                       random_effect = "(1 | group_id)",
                                       significance_cutoff = 0.05)
}
