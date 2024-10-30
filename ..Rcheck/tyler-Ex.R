pkgname <- "tyler"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tyler')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ACOG_Districts")
### * ACOG_Districts

flush(stderr()); flush(stdout())

### Name: ACOG_Districts
### Title: ACOG Districts Data
### Aliases: ACOG_Districts
### Keywords: dataset

### ** Examples

# Load the ACOG Districts Data
data(ACOG_Districts)

# View the first few rows of the dataset
head(ACOG_Districts)

# Get a summary of the dataset
summary(ACOG_Districts)

# Perform data analysis and exploration




cleanEx()
nameEx("MaxTable")
### * MaxTable

flush(stderr()); flush(stdout())

### Name: MaxTable
### Title: Calculate the Maximum Value(s) and Corresponding Level(s) of a
###   Factor Variable
### Aliases: MaxTable

### ** Examples

vec <- factor(c("A", "B", "A", "C", "B", "B"))
MaxTable(vec) # Returns "A"
MaxTable(vec, mult = TRUE) # Returns c("A", "B")



cleanEx()
nameEx("MinTable")
### * MinTable

flush(stderr()); flush(stdout())

### Name: MinTable
### Title: Calculate the Minimum Value(s) and Corresponding Level(s) of a
###   Factor Variable
### Aliases: MinTable

### ** Examples

vec <- factor(c("A", "B", "A", "C", "B", "B"))
MinTable(vec) # Returns "C"
MinTable(vec, mult = TRUE) # Returns "C"



cleanEx()
nameEx("acgme")
### * acgme

flush(stderr()); flush(stdout())

### Name: acgme
### Title: ACGME OBGYN Residency Data
### Aliases: acgme
### Keywords: dataset

### ** Examples

# Load the ACGME OBGYN Residency Data
data(acgme)

# View the first few rows of the dataset
head(acgme)

# Get a summary of the dataset
summary(acgme)

# Perform data analysis and exploration




cleanEx()
nameEx("arsenal_tables_write2word")
### * arsenal_tables_write2word

flush(stderr()); flush(stdout())

### Name: arsenal_tables_write2word
### Title: Writes an Arsenal table object to a Word document.
### Aliases: arsenal_tables_write2word

### ** Examples

## Not run: 
##D arsenal_tables_write2word(my_table, "output_table")
## End(Not run)



cleanEx()
nameEx("calcpercentages")
### * calcpercentages

flush(stderr()); flush(stdout())

### Name: calcpercentages
### Title: Calculate the Percentage of the Most Common Value in a
###   Categorical Variable
### Aliases: calcpercentages

### ** Examples

# Example 1: Basic usage with a simple dataset
df <- data.frame(category = c("A", "B", "A", "C", "A", "B", "B", "A"))
result <- calcpercentages(df, "category")
print(result)

# Example 2: Using a dataset with multiple most common values
df_tie <- data.frame(category = c("A", "B", "A", "B", "C", "C", "C", "A", "B"))
result <- calcpercentages(df_tie, "category")
print(result)

# Example 3: Handling a dataset with missing values
df_na <- data.frame(category = c("A", NA, "A", "C", "A", "B", "B", NA))
result <- calcpercentages(df_na, "category")
print(result)




cleanEx()
nameEx("calculate_descriptive_stats")
### * calculate_descriptive_stats

flush(stderr()); flush(stdout())

### Name: calculate_descriptive_stats
### Title: Calculate Descriptive Statistics with Robust Logging
### Aliases: calculate_descriptive_stats

### ** Examples

# Example: Calculate descriptive statistics for a column with logging
stats <- calculate_descriptive_stats(df, "business_days_until_appointment", verbose = TRUE)




cleanEx()
nameEx("calculate_distribution")
### * calculate_distribution

flush(stderr()); flush(stdout())

### Name: calculate_distribution
### Title: Calculate Demographic Distribution with Logging
### Aliases: calculate_distribution

### ** Examples

df <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female", NA))
result <- calculate_distribution(df, "gender")
print(result)



cleanEx()
nameEx("calculate_intersection_overlap_and_save")
### * calculate_intersection_overlap_and_save

flush(stderr()); flush(stdout())

### Name: calculate_intersection_overlap_and_save
### Title: Calculate intersection overlap and save results to shapefiles.
### Aliases: calculate_intersection_overlap_and_save

### ** Examples

calculate_intersection_overlap_and_save(block_groups, isochrones_joined, 30L, "data/shp/")




cleanEx()
nameEx("calculate_proportion")
### * calculate_proportion

flush(stderr()); flush(stdout())

### Name: calculate_proportion
### Title: Calculate the Proportion of Each Level in a Categorical Variable
### Aliases: calculate_proportion

### ** Examples

# Example 1: Basic usage with a simple dataset
df <- data.frame(gender = c("Male", "Female", "Female", "Male", "Male", "Female"))
result <- calculate_proportion(df, gender)
print(result)

# Example 2: Handling a dataset with missing values
df_na <- data.frame(gender = c("Male", NA, "Female", "Female", "Male", "Female", NA))
result <- calculate_proportion(df_na, gender)
print(result)

# Example 3: Using a variable with multiple levels
df_multi <- data.frame(grade = c("A", "B", "A", "C", "B", "A", "C", "B"))
result <- calculate_proportion(df_multi, grade)
print(result)




cleanEx()
nameEx("check_normality")
### * check_normality

flush(stderr()); flush(stdout())

### Name: check_normality
### Title: Check Normality and Summarize Data
### Aliases: check_normality

### ** Examples

# Example usage with a dataframe 'df' and outcome variable 'business_days_until_appointment'
check_normality(df, "business_days_until_appointment")



cleanEx()
nameEx("city_state_assign_scenarios")
### * city_state_assign_scenarios

flush(stderr()); flush(stdout())

### Name: city_state_assign_scenarios
### Title: Assign Cases to Professionals by City and State
### Aliases: city_state_assign_scenarios

### ** Examples

# Example 1: Using default parameters
data <- data.frame(
  city = c("CityA", "CityA", "CityB", "CityB"),
  state_code = c("State1", "State1", "State2", "State2"),
  specialty_primary = c("Generalist", "Specialist", "Generalist", "Specialist"),
  stringsAsFactors = FALSE
)
result <- city_state_assign_scenarios(data)
print(result)

# Example 2: Specifying custom specialties and cases
data <- data.frame(
  city = c("CityA", "CityA", "CityB", "CityB", "CityC"),
  state_code = c("State1", "State1", "State2", "State2", "State3"),
  specialty_primary = c("General Worker", "Specialist Worker", "General Worker", "Specialist Worker", "Other Specialty"),
  stringsAsFactors = FALSE
)
result <- city_state_assign_scenarios(
  data = data,
  generalist = "General Worker",
  specialty = "Specialist Worker",
  case_names = c("Case1", "Case2", "Case3"),
  output_csv_path = "output/custom_assignments.csv",
  seed = 1234
)
print(result)

# Example 3: Testing with more generalists than cases and a set seed
data <- data.frame(
  city = rep("CityA", 6),
  state_code = rep("State1", 6),
  specialty_primary = c("Generalist", "Generalist", "Generalist", "Generalist", "Generalist", "Specialist"),
  stringsAsFactors = FALSE
)
result <- city_state_assign_scenarios(
  data = data,
  case_names = c("Alpha", "Beta"),
  output_csv_path = "output/more_generalists.csv",
  seed = 42
)
print(result)




cleanEx()
nameEx("city_state_sample_specialists")
### * city_state_sample_specialists

flush(stderr()); flush(stdout())

### Name: city_state_sample_specialists
### Title: Sample Generalists and Specialists by City-State Combination
### Aliases: city_state_sample_specialists

### ** Examples

# Example 1: Basic usage with default generalist and specialist
data <- data.frame(
  city = rep(c("New York", "Los Angeles"), each = 6),
  state_code = rep(c("NY", "CA"), each = 6),
  specialty_primary = c(
    "General Dermatology", "Pediatric Dermatology", "General Dermatology",
    "General Dermatology", "Pediatric Dermatology", "General Dermatology",
    "General Dermatology", "General Dermatology", "Pediatric Dermatology",
    "General Dermatology", "General Dermatology", "Pediatric Dermatology"
  )
)
result <- city_state_sample_specialists(data)

# Example 2: Custom sampling with different generalist and specialist, saving to a CSV
result <- city_state_sample_specialists(
  data,
  generalist = "General Dermatology",
  specialist1 = "Pediatric Dermatology",
  general_sample_size = 3,
  specialist1_sample_size = 2,
  output_csv_path = "sampled_data.csv"
)

# Example 3: Sampling multiple specialists with custom sample sizes
result <- city_state_sample_specialists(
  data,
  generalist = "General Dermatology",
  specialist1 = "Pediatric Dermatology",
  specialist2 = "Cosmetic Dermatology",
  general_sample_size = 2,
  specialist1_sample_size = 1,
  specialist2_sample_size = 1
)




cleanEx()
nameEx("clean_phase_1_results")
### * clean_phase_1_results

flush(stderr()); flush(stdout())

### Name: clean_phase_1_results
### Title: Clean Phase 1 Results Data
### Aliases: clean_phase_1_results

### ** Examples

## Not run: 
##D library(tyler)
##D file_path <- "/path/to/your/input/file.xls"
##D df <- readxl::read_excel(file_path)  # Assuming use of readxl for Excel files
##D clean_phase_1_results(df)
## End(Not run)




cleanEx()
nameEx("clean_phase_2_data")
### * clean_phase_2_data

flush(stderr()); flush(stdout())

### Name: clean_phase_2_data
### Title: Clean and process Phase 2 data
### Aliases: clean_phase_2_data

### ** Examples

# Assuming an input path to a CSV file
input_path <- "path_to_your_data.csv"
required_strings <- c("physician_information", "able_to_contact_office")
standard_names <- c("physician_info", "contact_office")
cleaned_data <- clean_phase_2_data(input_path, required_strings, standard_names)

# Directly using a data frame
df <- data.frame(
  doc_info = 1:5,
  contact_data = 6:10
)
required_strings <- c("doc_info", "contact_data")
standard_names <- c("doctor_info", "patient_contact_info")
cleaned_df <- clean_phase_2_data(df, required_strings, standard_names)
print(cleaned_df)



cleanEx()
nameEx("convert_list_to_df_expanded")
### * convert_list_to_df_expanded

flush(stderr()); flush(stdout())

### Name: convert_list_to_df_expanded
### Title: Convert a List of Column Names to an Expanded Data Frame
### Aliases: convert_list_to_df_expanded

### ** Examples

# Example 1: Convert a list of column names to an expanded data frame
test_list <- list(table1 = c("col1", "col2"), table2 = c("col1", "col2", "col3"))
convert_list_to_df_expanded(test_list)

# Example 2: Handling missing columns in some tables
test_list <- list(table1 = c("col1", "col2"), table2 = c("col1"))
convert_list_to_df_expanded(test_list)



cleanEx()
nameEx("count_unique_physicians")
### * count_unique_physicians

flush(stderr()); flush(stdout())

### Name: count_unique_physicians
### Title: Count Unique Physicians Based on Insurance Type and Exclusion
###   Reason
### Aliases: count_unique_physicians

### ** Examples

# Example usage:
count_unique_physicians(df, insurance_type = "Medicaid", reason_for_exclusion = "Able to contact")
count_unique_physicians(df, insurance_type = "Blue Cross/Blue Shield", verbose = TRUE)




cleanEx()
nameEx("create_and_plot_interaction")
### * create_and_plot_interaction

flush(stderr()); flush(stdout())

### Name: create_and_plot_interaction
### Title: Create and Plot Interaction Effects in GLMM
### Aliases: create_and_plot_interaction

### ** Examples

## Not run: 
##D # Example 1: Analyzing the effect of gender and appointment center on wait times
##D result <- create_and_plot_interaction(
##D   data_path = "Ari/data/Phase2/late_phase_2_ENT_analysis_3.rds",
##D   response_variable = "business_days_until_appointment",
##D   variable_of_interest = "central_number_e_g_appointment_center",
##D   interaction_variable = "gender",
##D   random_intercept = "city",
##D   output_path = "Ari/data/figures",
##D   resolution = 100
##D )
##D 
##D # Example 2: Examining the interaction between insurance type and scenario on appointment delays
##D result <- create_and_plot_interaction(
##D   data_path = "data/healthcare_calls.rds",
##D   response_variable = "business_days_until_appointment",
##D   variable_of_interest = "insurance_type",
##D   interaction_variable = "scenario",
##D   random_intercept = "state",
##D   output_path = "figures/insurance_scenario_interaction",
##D   resolution = 150
##D )
##D 
##D # Example 3: Studying the interaction between gender and subspecialty in wait times
##D result <- create_and_plot_interaction(
##D   data_path = "data/mystery_caller_study.rds",
##D   response_variable = "waiting_time_days",
##D   variable_of_interest = "subspecialty",
##D   interaction_variable = "gender",
##D   random_intercept = "clinic_id",
##D   output_path = "results/waiting_times",
##D   resolution = 300
##D )
## End(Not run)




cleanEx()
nameEx("create_and_save_physician_dot_map")
### * create_and_save_physician_dot_map

flush(stderr()); flush(stdout())

### Name: create_and_save_physician_dot_map
### Title: Create and Save a Leaflet Dot Map of Physicians
### Aliases: create_and_save_physician_dot_map

### ** Examples

## Not run: 
##D # Load required libraries
##D library(viridis)
##D library(leaflet)
##D 
##D # Generate physician data (replace with your own data)
##D physician_data <- data.frame(
##D   long = c(-95.363271, -97.743061, -98.493628, -96.900115, -95.369803),
##D   lat = c(29.763283, 30.267153, 29.424349, 32.779167, 29.751808),
##D   name = c("Physician 1", "Physician 2", "Physician 3", "Physician 4", "Physician 5"),
##D   ACOG_District = c("District I", "District II", "District III", "District IV", "District V")
##D )
##D 
##D # Create and save the dot map
##D create_and_save_physician_dot_map(physician_data)
## End(Not run)




cleanEx()
nameEx("create_block_group_overlap_map")
### * create_block_group_overlap_map

flush(stderr()); flush(stdout())

### Name: create_block_group_overlap_map
### Title: Function to create and export a map showing block group overlap
###   with isochrones
### Aliases: create_block_group_overlap_map

### ** Examples

## Not run: 
##D # Create and export the map with the default output directory
##D create_block_group_overlap_map(block_groups, isochrones_joined_map)
##D 
##D # Create and export the map with a custom output directory
##D create_block_group_overlap_map(block_groups, isochrones_joined_map, "custom_output/")
## End(Not run)




cleanEx()
nameEx("create_density_plot")
### * create_density_plot

flush(stderr()); flush(stdout())

### Name: create_density_plot
### Title: Create a Density Plot for Mystery Caller Studies with Optional
###   Transformations
### Aliases: create_density_plot

### ** Examples

# Example 1: Basic density plot with log transformation
create_density_plot(
    data = df3,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_transform = "log",  # Log transformation
    dpi = 100,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_density",
    x_label = "Log (Waiting Times in Days)",
    y_label = "Density",
    plot_title = "Density Plot of Waiting Times by Insurance"
)

# Example 2: Density plot with square root transformation
create_density_plot(
    data = df3,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_transform = "sqrt",  # Square root transformation
    dpi = 150,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_density_sqrt",
    x_label = "Sqrt (Waiting Times in Days)",
    y_label = "Density",
    plot_title = "Square Root Transformed Density Plot"
)

# Example 3: Density plot without any transformation
create_density_plot(
    data = df3,
    x_var = "business_days_until_appointment",
    fill_var = "insurance",
    x_transform = "none",  # No transformation
    dpi = 200,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_density_none",
    x_label = "Waiting Times in Days",
    y_label = "Density",
    plot_title = "Density Plot Without Transformation"
)



cleanEx()
nameEx("create_formula")
### * create_formula

flush(stderr()); flush(stdout())

### Name: create_formula
### Title: Create a Formula for Poisson Model
### Aliases: create_formula

### ** Examples

# Example usage:
response_variable <- "days"
random_effect_term <- "name"  # Change this to the desired random effect variable
df3_filtered <- data.frame(days = c(5, 10, 15), age = c(30, 40, 50), name = c("A", "B", "C"))
formula <- create_formula(df3_filtered, response_variable, random_effect_term)
formula




cleanEx()
nameEx("create_individual_isochrone_plots")
### * create_individual_isochrone_plots

flush(stderr()); flush(stdout())

### Name: create_individual_isochrone_plots
### Title: Create Individual Isochrone Maps and Shapefiles
### Aliases: create_individual_isochrone_plots

### ** Examples

## Not run: 
##D # Load required libraries
##D library(sf)
##D library(leaflet)
##D library(tyler)
##D 
##D # Load isochrone data
##D isochrones <- readRDS("path_to_isochrones.rds")
##D 
##D # List of unique drive times for which you want to create plots and shapefiles
##D drive_times <- unique(isochrones$drive_time)
##D 
##D # Create individual isochrone maps and shapefiles
##D create_individual_isochrone_plots(isochrones, drive_times)
## End(Not run)




cleanEx()
nameEx("create_insurance_by_insurance_scatter_plot")
### * create_insurance_by_insurance_scatter_plot

flush(stderr()); flush(stdout())

### Name: create_insurance_by_insurance_scatter_plot
### Title: Create a Scatter Plot Comparing Waiting Times Between Two
###   Insurance Types
### Aliases: create_insurance_by_insurance_scatter_plot

### ** Examples

## Not run: 
##D # Create and save a scatter plot with all arguments filled in
##D scatterplot <- create_insurance_by_insurance_scatter_plot(
##D   df = df3,  # The data frame to be used
##D   unique_variable = "phone",  # The unique identifier variable
##D   insurance1 = "medicaid",  # The first insurance type
##D   insurance2 = "blue cross/blue shield",  # The second insurance type
##D   output_directory = "ortho_sports_med/figures/custom_directory/",  # Custom output directory
##D   dpi = 300,  # Higher DPI for better resolution
##D   height = 10,  # Custom height for the plot
##D   width = 15,  # Custom width for the plot
##D   x_label = "Appointment Time (days) - Blue Cross Blue Shield (Log Scale)",  # Custom x-axis label
##D   y_label = "Appointment Time (days) - Medicaid (Log Scale)",  # Custom y-axis label
##D   plot_title = "Custom Plot Title: Waiting Times Comparison",  # Custom plot title
##D   point_size = 4,  # Larger points
##D   point_alpha = 0.8,  # Less transparent points
##D   add_confidence_interval = FALSE  # Do not add a confidence interval around the fit line
##D )
## End(Not run)




cleanEx()
nameEx("create_isochrones")
### * create_isochrones

flush(stderr()); flush(stdout())

### Name: create_isochrones
### Title: Memoized function to try a location with isoline calculations
### Aliases: create_isochrones

### ** Examples

## Not run: 
##D # Set your HERE API key in your Renviron file using the following steps:
##D # 1. Add key to .Renviron
##D Sys.setenv(HERE_API_KEY = "your_api_key_here")
##D # 2. Reload .Renviron
##D readRenviron("~/.Renviron")
##D 
##D # Define a sf object for the location
##D location <- sf::st_point(c(-73.987, 40.757))
##D 
##D # Calculate isolines for the location with a 30-minute, 60-minute, 120-minute, and 180-minute range
##D isolines <- create_isochrones(location = location, range = c(1800, 3600, 7200, 10800))
##D 
##D # Print the isolines
##D print(isolines)
##D 
## End(Not run)




cleanEx()
nameEx("create_line_plot")
### * create_line_plot

flush(stderr()); flush(stdout())

### Name: create_line_plot
### Title: Create a Line Plot with Optional Transformations and Grouping
### Aliases: create_line_plot

### ** Examples

# Example 1: Basic plot with log transformation
create_line_plot(
    data = df3,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "log",  # Log transformation
    dpi = 100,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance"
)

# Example 2: Plot with square root transformation and lines grouped by 'last'
create_line_plot(
    data = df3,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "sqrt",  # Square root transformation
    dpi = 150,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_sqrt",
    use_geom_line = TRUE,  # Include lines
    geom_line_group = "last"  # Group lines by 'last' column
)

# Example 3: Plot without any transformation and without lines
create_line_plot(
    data = df3,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "none",  # No transformation
    dpi = 200,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_none"
)



cleanEx()
nameEx("create_scatter_plot")
### * create_scatter_plot

flush(stderr()); flush(stdout())

### Name: create_scatter_plot
### Title: Create a Scatter Plot for Mystery Caller Studies with Optional
###   Transformations, Jitter, and Custom Labels
### Aliases: create_scatter_plot

### ** Examples

# Example 1: Basic scatter plot with log transformation
create_scatter_plot(
    data = df3,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "log",  # Log transformation
    dpi = 100,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance",
    x_label = "Insurance",
    y_label = "Log (Waiting Times in Days)",
    plot_title = "Scatter Plot of Waiting Times by Insurance"
)

# Example 2: Scatter plot with square root transformation and custom jitter
create_scatter_plot(
    data = df3,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "sqrt",  # Square root transformation
    dpi = 150,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_sqrt",
    jitter_width = 0.3,
    jitter_height = 0.1,
    x_label = "Insurance",
    y_label = "Square Root (Waiting Times in Days)",
    plot_title = "Square Root Transformed Scatter Plot"
)

# Example 3: Scatter plot without any transformation and increased transparency
create_scatter_plot(
    data = df3,
    x_var = "insurance",
    y_var = "business_days_until_appointment",
    y_transform = "none",  # No transformation
    dpi = 200,
    output_dir = "ortho_sports_med/Figures",
    file_prefix = "ortho_sports_vs_insurance_none",
    point_alpha = 0.8,
    x_label = "Insurance",
    y_label = "Waiting Times in Days",
    plot_title = "Scatter Plot Without Transformation"
)



cleanEx()
nameEx("find_common_columns_in_years_of_open_payments_data")
### * find_common_columns_in_years_of_open_payments_data

flush(stderr()); flush(stdout())

### Name: find_common_columns_in_years_of_open_payments_data
### Title: Find Common Columns in Years of Open Payments Data
### Aliases: find_common_columns_in_years_of_open_payments_data

### ** Examples

# Example with default table names:
con <- DBI::dbConnect(duckdb::duckdb(), "path_to_duckdb.duckdb")
find_common_columns_in_years_of_open_payments_data(con, "output.xlsx")

# Example with custom table names:
custom_tables <- c("OP_DTL_GNRL_PGYR2014_P06302021", "OP_DTL_GNRL_PGYR2015_P06302021")
find_common_columns_in_years_of_open_payments_data(con, "output_custom.xlsx", custom_tables)



cleanEx()
nameEx("fit_mixed_model_with_logging")
### * fit_mixed_model_with_logging

flush(stderr()); flush(stdout())

### Name: fit_mixed_model_with_logging
### Title: Fit a Mixed-Effects Model with Logging and Robust Error Handling
### Aliases: fit_mixed_model_with_logging

### ** Examples

# Example 1: Basic usage with default settings
df <- my_data_frame
result <- fit_mixed_model_with_logging(data = df)

# Example 2: Using a robust linear mixed-effects model (rlmer) and saving the results
result <- fit_mixed_model_with_logging(data = df, model_type = "rlmer", output_path = "results_rlmer.csv")

# Example 3: Custom response variable and random effect, with significance level 0.05
result <- fit_mixed_model_with_logging(data = df,
                                       response_variable = "some_other_response",
                                       random_effect = "(1 | group_id)",
                                       significance_level = 0.05)




cleanEx()
nameEx("format_pct")
### * format_pct

flush(stderr()); flush(stdout())

### Name: format_pct
### Title: Format a Numeric Value as a Percentage
### Aliases: format_pct

### ** Examples

# Example 1: Format a single numeric value
result <- format_pct(0.12345)
print(result)  # Output: "12.3%"

# Example 2: Format a vector of numeric values with 2 decimal places
values <- c(0.12345, 0.6789, 0.54321)
formatted_values <- format_pct(values, my_digits = 2)
print(formatted_values)  # Output: "12.35%", "67.89%", "54.32%"

# Example 3: Format a value with no decimal places
no_decimal <- format_pct(0.5, my_digits = 0)
print(no_decimal)  # Output: "50%"




cleanEx()
nameEx("genderize_physicians")
### * genderize_physicians

flush(stderr()); flush(stdout())

### Name: genderize_physicians
### Title: Genderize Physicians Data
### Aliases: genderize_physicians

### ** Examples

## Not run: 
##D result <- genderize_physicians("sample.csv")
## End(Not run)




cleanEx()
nameEx("generate_latex_equation")
### * generate_latex_equation

flush(stderr()); flush(stdout())

### Name: generate_latex_equation
### Title: Generate LaTeX Equation with Logging
### Aliases: generate_latex_equation

### ** Examples

# Example 1: Basic usage with logging to the console
## Not run: 
##D generate_latex_equation("Patient Scenario")
## End(Not run)

# Example 2: Handle underscores in the patient_scenario_label
## Not run: 
##D generate_latex_equation("Patient_Scenario_With_Underscores")
## End(Not run)

# Example 3: Logging the process to a file
## Not run: 
##D log_file_path <- "latex_generation_log.txt"
##D generate_latex_equation("Patient Scenario", log_file = log_file_path)
## End(Not run)

# Example 4: Using default patient_scenario_label when none is provided
## Not run: 
##D latex_code <- generate_latex_equation()
##D cat(latex_code)
## End(Not run)

# Example 5: Input validation error - non-character input
## Not run: 
##D generate_latex_equation(12345)  # This will throw an error
## End(Not run)




cleanEx()
nameEx("generate_leaflet_base_map")
### * generate_leaflet_base_map

flush(stderr()); flush(stdout())

### Name: generate_leaflet_base_map
### Title: Generate Leaflet Base Map
### Aliases: generate_leaflet_base_map

### ** Examples

map <- generate_leaflet_base_map()



cleanEx()
nameEx("geocode_unique_addresses")
### * geocode_unique_addresses

flush(stderr()); flush(stdout())

### Name: geocode_unique_addresses
### Title: Geocode Unique Addresses
### Aliases: geocode_unique_addresses

### ** Examples

## Not run: 
##D # Define the input file path, Google Maps API key, and output file path (optional)
##D file_path <- "input_data.csv"
##D google_maps_api_key <- "your_api_key"
##D output_file_path <- "output_data.csv"  # Optional
##D 
##D # Call the geocode_unique_addresses function with or without specifying output_file_path
##D geocoded_data <- geocode_unique_addresses(file_path, google_maps_api_key)
##D # or
##D geocoded_data <- geocode_unique_addresses(file_path, google_maps_api_key, output_file_path)
## End(Not run)




cleanEx()
nameEx("install_missing_packages")
### * install_missing_packages

flush(stderr()); flush(stdout())

### Name: install_missing_packages
### Title: Install Missing CRAN Packages
### Aliases: install_missing_packages

### ** Examples

# Example 1: Install ggplot2 and dplyr if missing
install_missing_packages(c("ggplot2", "dplyr"))

# Example 2: Install multiple packages including data.table and tidyr
install_missing_packages(c("data.table", "tidyr", "readr"))

# Example 3: Install a large set of packages commonly used in data science
install_missing_packages(c("ggplot2", "dplyr", "tidyverse", "caret", "randomForest"))




cleanEx()
nameEx("linear_regression_race_drive_time_generate_summary_sentence")
### * linear_regression_race_drive_time_generate_summary_sentence

flush(stderr()); flush(stdout())

### Name: linear_regression_race_drive_time_generate_summary_sentence
### Title: Generate Summary Sentence for Linear Regression on Race and
###   Drive Time with Raw Proportions
### Aliases: linear_regression_race_drive_time_generate_summary_sentence

### ** Examples

# Example usage
summary_sentence <- linear_regression_race_drive_time_generate_summary_sentence(
  tabulated_data = tabulated_all_years_numeric,
  driving_time_minutes = 180,
  race = "White"
)
print(summary_sentence)



cleanEx()
nameEx("load_data")
### * load_data

flush(stderr()); flush(stdout())

### Name: load_data
### Title: Load and Process Data from an RDS File with Robust Logging
### Aliases: load_data

### ** Examples

# Example: Load data from a specified directory with logging
df <- load_data(data_dir = "data", file_name = "Phase_2.rds", verbose = TRUE)




cleanEx()
nameEx("logistic_regression")
### * logistic_regression

flush(stderr()); flush(stdout())

### Name: logistic_regression
### Title: Perform Logistic Regression on Multiple Predictors with Logging
### Aliases: logistic_regression

### ** Examples

# Assuming df is your data frame
target_variable <- "cleaned_does_the_physician_accept_medicaid_numeric"
predictor_vars <- setdiff(names(df), c(target_variable, "does_the_physician_accept_medicaid", "cleaned_does_the_physician_accept_medicaid"))
significance_logistic_regression <- 0.2

significant_vars <- logistic_regression(df, target_variable, predictor_vars, significance_level = significance_logistic_regression)
print(significant_vars)



cleanEx()
nameEx("most_common_gender_training_academic")
### * most_common_gender_training_academic

flush(stderr()); flush(stdout())

### Name: most_common_gender_training_academic
### Title: Generate a Summary Sentence for the Most Common Gender,
###   Specialty, Training, and Academic Affiliation
### Aliases: most_common_gender_training_academic

### ** Examples

# Example 1: Basic usage with a small dataset
df <- data.frame(
  gender = c("Male", "Female", "Female", "Male", "Male"),
  specialty = c("Cardiology", "Cardiology", "Neurology", "Cardiology", "Neurology"),
  Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
  academic_affiliation = c("Yes", "No", "Yes", "No", "Yes")
)
result <- most_common_gender_training_academic(df)
print(result)

# Example 2: Handling missing data
df_with_na <- data.frame(
  gender = c("Male", NA, "Female", "Male", "Male"),
  specialty = c("Cardiology", "Cardiology", "Neurology", NA, "Neurology"),
  Provider.Credential.Text = c("MD", "MD", "DO", "MD", "DO"),
  academic_affiliation = c("Yes", "No", "Yes", "No", NA)
)
result <- most_common_gender_training_academic(df_with_na)
print(result)

# Example 3: Different proportions with a larger dataset
df_large <- data.frame(
  gender = c(rep("Male", 70), rep("Female", 30)),
  specialty = c(rep("Cardiology", 50), rep("Neurology", 30), rep("Orthopedics", 20)),
  Provider.Credential.Text = c(rep("MD", 60), rep("DO", 40)),
  academic_affiliation = c(rep("Yes", 40), rep("No", 60))
)
result <- most_common_gender_training_academic(df_large)
print(result)




cleanEx()
nameEx("nppes_collect_and_clean_data")
### * nppes_collect_and_clean_data

flush(stderr()); flush(stdout())

### Name: nppes_collect_and_clean_data
### Title: Collect and clean the processed data
### Aliases: nppes_collect_and_clean_data

### ** Examples

cleaned_data <- nppes_collect_and_clean_data(processed_data)
# Next step: nppes_save_data_to_csv()



cleanEx()
nameEx("nppes_connect_to_duckdb")
### * nppes_connect_to_duckdb

flush(stderr()); flush(stdout())

### Name: nppes_connect_to_duckdb
### Title: Connect to DuckDB
### Aliases: nppes_connect_to_duckdb

### ** Examples

con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")



cleanEx()
nameEx("nppes_create_table_from_csv")
### * nppes_create_table_from_csv

flush(stderr()); flush(stdout())

### Name: nppes_create_table_from_csv
### Title: Create a table in DuckDB from a CSV file
### Aliases: nppes_create_table_from_csv

### ** Examples

con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
nppes_create_table_from_csv(con, "/path/to/your/csv_file.csv", "npi_2024")
# Next step: nppes_query_sample_data()

con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
nppes_create_table_from_csv(con, "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/NPPES_Data.csv", "npi_2024")
# Next step: nppes_query_sample_data()



cleanEx()
nameEx("nppes_disconnect_from_duckdb")
### * nppes_disconnect_from_duckdb

flush(stderr()); flush(stdout())

### Name: nppes_disconnect_from_duckdb
### Title: Disconnect from DuckDB
### Aliases: nppes_disconnect_from_duckdb

### ** Examples

con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
# After all operations, disconnect from the database
nppes_disconnect_from_duckdb(con)

con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
# After all operations, disconnect from the database
nppes_disconnect_from_duckdb(con)



cleanEx()
nameEx("nppes_get_data_for_one_year")
### * nppes_get_data_for_one_year

flush(stderr()); flush(stdout())

### Name: nppes_get_data_for_one_year
### Title: Get NPPES Data for One Year with Chunked Processing
### Aliases: nppes_get_data_for_one_year

### ** Examples

# Example 1: Basic usage with default taxonomy codes
result <- nppes_get_data_for_one_year(
  npi_file_path = "/path/to/npi_file.csv",
  output_csv_path = "/path/to/output.csv"
)

# Example 2: Using custom taxonomy codes for filtering
result <- nppes_get_data_for_one_year(
  npi_file_path = "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/NPPES_Data_Disseminat_September_2024/npidata_pfile_20050523-20240811.csv",
  output_csv_path = "/path/to/output.csv",
  taxonomy_codes_1 = c("207X00000X", "207Y00000X"),
  taxonomy_codes_2 = c("207X00000X", "207Y00000X")
)

# Example 3: Saving sample data to an Excel file
result <- nppes_get_data_for_one_year(
  npi_file_path = "/path/to/npi_file.csv",
  output_csv_path = "/path/to/output.csv",
  save_column_in_each_nppes_year = TRUE,
  excel_file_path = "/path/to/sample_data.xlsx"
)




cleanEx()
nameEx("nppes_initialize_environment")
### * nppes_initialize_environment

flush(stderr()); flush(stdout())

### Name: nppes_initialize_environment
### Title: Initialize the environment by loading necessary libraries and
###   resolving conflicts
### Aliases: nppes_initialize_environment

### ** Examples

nppes_initialize_environment()
# Next step: tyler::connect_to_duckdb()



cleanEx()
nameEx("nppes_process_npi_table_chunk")
### * nppes_process_npi_table_chunk

flush(stderr()); flush(stdout())

### Name: nppes_process_npi_table_chunk
### Title: Process an NPI table in DuckDB
### Aliases: nppes_process_npi_table_chunk

### ** Examples

con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
processed_data <- nppes_process_npi_table_chunk(con, "npi_2024", c("207V00000X"), c("207V00000X"))
# Next step: nppes_collect_and_clean_data()

con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
processed_data <- nppes_process_npi_table_chunk(con)
# Next step: nppes_collect_and_clean_data()



cleanEx()
nameEx("nppes_query_sample_data")
### * nppes_query_sample_data

flush(stderr()); flush(stdout())

### Name: nppes_query_sample_data
### Title: Query sample data from a DuckDB table
### Aliases: nppes_query_sample_data

### ** Examples

con <- nppes_connect_to_duckdb("/path/to/your/duckdb_file.duckdb")
nppes_query_sample_data(con, "npi_2024", 5)
# Next step: nppes_process_npi_table_chunk()

con <- nppes_connect_to_duckdb("/Volumes/Video Projects Muffly 1/nppes_historical_downloads/my_duckdb.duckdb")
nppes_query_sample_data(con, "npi_2024", 10, TRUE, "/path/to/excel_file.xlsx")
# Next step: nppes_process_npi_table_chunk()



cleanEx()
nameEx("nppes_save_data_to_csv")
### * nppes_save_data_to_csv

flush(stderr()); flush(stdout())

### Name: nppes_save_data_to_csv
### Title: Save the final data to CSV
### Aliases: nppes_save_data_to_csv

### ** Examples

nppes_save_data_to_csv(cleaned_data, "/path/to/your/file.csv")



cleanEx()
nameEx("open_payments_collect_and_convert")
### * open_payments_collect_and_convert

flush(stderr()); flush(stdout())

### Name: open_payments_collect_and_convert
### Title: Collect and Convert Open Payments Data with Crosswalk Merging
### Aliases: open_payments_collect_and_convert

### ** Examples

# Example 1: Using default output path and logging to a file
## Not run: 
##D open_payments_collect_and_convert(
##D   duckdb_file_path = "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/nber/nber_my_duckdb.duckdb",
##D   specialty_csv_path = "/Volumes/Video Projects Muffly 1/openpayments/unzipped_files/open_payments_merged/clean_open_payments_specialty.csv",
##D   crosswalk_rds_path = "/Users/tylermuffly/Dropbox (Personal)/Lo_Fellowship_Directors/academic_hand_search/data/merged_into_a_crosswalk.rds",
##D   log_path = "/path/to/log_file.txt"
##D )
## End(Not run)

# Example 2: Custom output path for CSV and detailed logging
## Not run: 
##D open_payments_collect_and_convert(
##D   duckdb_file_path = "/path/to/duckdb_file.duckdb",
##D   specialty_csv_path = "/path/to/specialty_data.csv",
##D   crosswalk_rds_path = "/path/to/crosswalk_data.rds",
##D   log_path = "/path/to/detailed_log.txt",
##D   output_csv = "/path/to/merged_output_data.csv"
##D )
## End(Not run)

# Example 3: Using different file locations for each argument
## Not run: 
##D open_payments_collect_and_convert(
##D   duckdb_file_path = "/my_data/nppes_historical_downloads/duckdb_file.duckdb",
##D   specialty_csv_path = "/my_data/open_payments_data/filtered_data.csv",
##D   crosswalk_rds_path = "/my_data/crosswalk_data/crosswalk_file.rds",
##D   log_path = "/my_data/logs/processing_log.txt",
##D   output_csv = "/my_data/output/merged_data.csv"
##D )
## End(Not run)




cleanEx()
nameEx("open_payments_processed_per_npi")
### * open_payments_processed_per_npi

flush(stderr()); flush(stdout())

### Name: open_payments_processed_per_npi
### Title: Process Payments Per NPI from CSV in Chunks and Write to CSV
###   with Date in Filename
### Aliases: open_payments_processed_per_npi

### ** Examples

# Example 1: Process payments per NPI from CSV and write output with date
result <- open_payments_processed_per_npi(
  input_csv_path = "path/to/input.csv",
  output_csv_path = "path/to/output.csv"
)

# Example 2: Custom logging function and different output path
result <- open_payments_processed_per_npi(
  input_csv_path = "path/to/input.csv",
  output_csv_path = "custom/output/path.csv",
  log_function = print
)



cleanEx()
nameEx("open_payments_specialty_cleaning")
### * open_payments_specialty_cleaning

flush(stderr()); flush(stdout())

### Name: open_payments_specialty_cleaning
### Title: Clean and Filter Open Payments Data by Specialty
### Aliases: open_payments_specialty_cleaning

### ** Examples

# Example 1: Process two Open Payments tables for specific specialties and save to a CSV
open_payments_specialty_cleaning(
  con = DBI::dbConnect(duckdb::duckdb(), "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/nber/nber_my_duckdb.duckdb"),
  table_names = c("OP_DTL_GNRL_PGYR2020_P01182024", "OP_DTL_GNRL_PGYR2021_P01182024"),
  output_csv_path = "/Volumes/Video Projects Muffly 1/openpayments/clean_open_payments_specialty.csv",
  specialties = c("Allopathic & Osteopathic Physicians|Obstetrics & Gynecology",
                  "Student in an Organized Health Care Education/Training Program"),
  chunk_size = 50000
)

# Example 2: Process multiple years of Open Payments data and filter by multiple OB-GYN specialties
open_payments_specialty_cleaning(
  con = DBI::dbConnect(duckdb::duckdb(), "/Volumes/Video Projects Muffly 1/nppes_historical_downloads/nber/nber_my_duckdb.duckdb"),
  table_names = c("OP_DTL_GNRL_PGYR2022_P01182024", "OP_DTL_GNRL_PGYR2023_P06282024_06122024"),
  output_csv_path = "/Volumes/Video Projects Muffly 1/openpayments/unzipped_files/open_payments_filtered.csv",
  specialties = c("Allopathic & Osteopathic Physicians|Obstetrics & Gynecology",
                  "Allopathic & Osteopathic Physicians|Obstetrics & Gynecology|Maternal & Fetal Medicine"),
  chunk_size = 100000
)

# Example 3: Process data with default connection and specialty filtering
open_payments_specialty_cleaning(
  table_names = c("OP_DTL_GNRL_PGYR2021_P01182024"),
  output_csv_path = "/Volumes/Video Projects Muffly 1/openpayments/unzipped_files/cleaned_open_payments_specialty.csv",
  specialties = c("Allopathic & Osteopathic Physicians|Gynecologic Oncology",
                  "Allopathic & Osteopathic Physicians|Obstetrics & Gynecology|Reproductive Endocrinology")
)




cleanEx()
nameEx("physician_age")
### * physician_age

flush(stderr()); flush(stdout())

### Name: physician_age
### Title: Calculate and Summarize Physician Age
### Aliases: physician_age

### ** Examples

# Example 1: Basic usage with a small dataset
df <- data.frame(age = c(30, 40, 50, 60, 35, 45, 55, 65))
summary_sentence <- physician_age(df, "age")
print(summary_sentence)

# Example 2: Handling missing data
df_with_na <- data.frame(age = c(30, 40, NA, 60, 35, NA, 55, 65))
summary_sentence <- physician_age(df_with_na, "age")
print(summary_sentence)

# Example 3: Different age distribution
df_large <- data.frame(age = c(rep(30, 70), rep(40, 30), rep(50, 20), rep(60, 10)))
summary_sentence <- physician_age(df_large, "age")
print(summary_sentence)




cleanEx()
nameEx("plot_and_save_emmeans")
### * plot_and_save_emmeans

flush(stderr()); flush(stdout())

### Name: plot_and_save_emmeans
### Title: Plot and Save Estimated Marginal Means (EMMs)
### Aliases: plot_and_save_emmeans

### ** Examples

## Not run: 
##D # Example 1: Comparing Appointment Wait Times Between Scenarios
##D # Assume 'model' is a fitted GLM object with appointment wait time as the outcome
##D result <- plot_and_save_emmeans(
##D   model_object = model,
##D   specs = "scenario",
##D   variable_of_interest = "scenario",
##D   color_by = "insurance",
##D   output_dir = "Figures/ScenarioComparison"
##D )
##D 
##D # Example 2: Comparing EMMs Across Different Insurance Types
##D # Assume 'model' is a fitted GLM object
##D result <- plot_and_save_emmeans(
##D   model_object = model,
##D   specs = "insurance",
##D   variable_of_interest = "insurance",
##D   color_by = "academic",
##D   output_dir = "Figures/InsuranceComparison"
##D )
##D 
##D # Example 3: Visualizing EMMs for Different Age Groups in a Mystery Caller Study
##D # Assume 'model' is a fitted GLM object with age groups as a factor
##D result <- plot_and_save_emmeans(
##D   model_object = model,
##D   specs = "age_group",
##D   variable_of_interest = "age_group",
##D   color_by = "gender",
##D   output_dir = "Figures/AgeGroupComparison"
##D )
##D 
##D # Example 4: Interaction Effects Between Gender and Insurance Type
##D # Assume 'model' is a fitted GLM object
##D result <- plot_and_save_emmeans(
##D   model_object = model,
##D   specs = c("gender", "insurance"),
##D   variable_of_interest = "gender",
##D   color_by = "insurance",
##D   output_dir = "Figures/InteractionEffects"
##D )
## End(Not run)




cleanEx()
nameEx("poisson_wait_time_stats")
### * poisson_wait_time_stats

flush(stderr()); flush(stdout())

### Name: poisson_wait_time_stats
### Title: Fit a Poisson Regression Model for Waiting Times by Group
### Aliases: poisson_wait_time_stats

### ** Examples

## Not run: 
##D # Example usage:
##D poisson_model <- poisson_wait_time_stats(data_dir = "your_data_directory",
##D                                          file_name = "Phase_2.rds",
##D                                          group_var = "scenario")
## End(Not run)




cleanEx()
nameEx("process_and_save_isochrones")
### * process_and_save_isochrones

flush(stderr()); flush(stdout())

### Name: process_and_save_isochrones
### Title: Process and Save Isochrones
### Aliases: process_and_save_isochrones

### ** Examples

# Load the input file (e.g., from a CSV)
input_file <- read_csv("data/locations.csv")

# Process and save isochrones for the input file (chunk size set to 25)
isochrones_data <- process_and_save_isochrones(input_file, chunk_size = 25)

# Optionally, write the combined isochrones to a shapefile
sf::st_write(isochrones_data, dsn = "data/isochrones/isochrones_all_combined",
             layer = "isochrones", driver = "ESRI Shapefile", quiet = FALSE)




cleanEx()
nameEx("race_drive_time_generate_summary_sentence")
### * race_drive_time_generate_summary_sentence

flush(stderr()); flush(stdout())

### Name: race_drive_time_generate_summary_sentence
### Title: Generate Summary Sentence for Race and Drive Time
### Aliases: race_drive_time_generate_summary_sentence

### ** Examples

# Example usage
summary_sentence <- race_drive_time_generate_summary_sentence(
  tabulated_data = tabulated_all_years_numeric,
  driving_time_minutes = 180,
  race = "American Indian/Alaska Native"
)
print(summary_sentence)



cleanEx()
nameEx("remove_constant_vars")
### * remove_constant_vars

flush(stderr()); flush(stdout())

### Name: remove_constant_vars
### Title: Remove Constant Variables from a Data Frame
### Aliases: remove_constant_vars

### ** Examples

## Not run: 
##D new_data <- remove_constant_vars(data_frame)
## End(Not run)




cleanEx()
nameEx("remove_near_zero_var")
### * remove_near_zero_var

flush(stderr()); flush(stdout())

### Name: remove_near_zero_var
### Title: Remove Near-Zero Variance Variables from a Data Frame
### Aliases: remove_near_zero_var

### ** Examples

## Not run: 
##D new_data <- remove_near_zero_var(data_frame)
## End(Not run)




cleanEx()
nameEx("rename_columns_by_substring")
### * rename_columns_by_substring

flush(stderr()); flush(stdout())

### Name: rename_columns_by_substring
### Title: Rename columns based on substring matches
### Aliases: rename_columns_by_substring

### ** Examples

df <- data.frame(
  doctor_info = 1:5,
  patient_contact_data = 6:10
)
# Renaming 'doctor_info' to 'physician_info'
df <- rename_columns_by_substring(df,
                                  target_strings = c("doctor"),
                                  new_names = c("physician_info"))
print(df)

# More complex example with multiple renamings
df <- data.frame(
  doc_information = 1:5,
  patient_contact = 6:10,
  doctor_notes = 11:15
)
# Renaming 'doc_information' to 'doctor_info' and 'doctor_notes' to 'notes'
df <- rename_columns_by_substring(df,
                                  target_strings = c("doc_information", "doctor_notes"),
                                  new_names = c("doctor_info", "notes"))
print(df)



cleanEx()
nameEx("scrape_physicians_data_with_tor")
### * scrape_physicians_data_with_tor

flush(stderr()); flush(stdout())

### Name: scrape_physicians_data_with_tor
### Title: Scrape Physicians' Data with Tor
### Aliases: scrape_physicians_data_with_tor

### ** Examples

# Call the function
scrape_result <- scrape_physicians_data_with_tor(startID = 9045999, endID = 9046000, torPort = 9150)




cleanEx()
nameEx("search_by_taxonomy")
### * search_by_taxonomy

flush(stderr()); flush(stdout())

### Name: search_by_taxonomy
### Title: Search NPI Database by Taxonomy
### Aliases: search_by_taxonomy

### ** Examples

# Example usage with multiple taxonomy descriptions:
go_data <- search_by_taxonomy("Gynecologic Oncology")
fpmrs_data <- search_by_taxonomy("Female Pelvic Medicine and Reconstructive Surgery")
rei_data <- search_by_taxonomy("Reproductive Endocrinology")
mfm_data <- search_by_taxonomy("Maternal & Fetal Medicine")




cleanEx()
nameEx("search_npi")
### * search_npi

flush(stderr()); flush(stdout())

### Name: search_npi
### Title: Search NPI Numbers for Given Names
### Aliases: search_npi

### ** Examples

# Input as a dataframe
input_df <- data.frame(
  first = c("John", "Jane", "Alice"),
  last = c("Doe", "Smith", "Johnson")
)
npi_results <- search_npi(input_df)

# Input as a CSV file
input_csv <- "path/to/input.csv"
npi_results <- search_npi(input_csv)




cleanEx()
nameEx("split_and_save")
### * split_and_save

flush(stderr()); flush(stdout())

### Name: split_and_save
### Title: Split data into multiple parts and save each part as separate
###   Excel files
### Aliases: split_and_save

### ** Examples

## Not run: 
##D library(tyler)
##D input_data <- readr::read_csv("/path/to/your/input/file.csv")
##D output_directory <- "/path/to/your/output/directory"
##D lab_assistant_names <- c("Label1", "Label2", "Label3")
##D insurance_order <- c("Medicaid", "Blue Cross/Blue Shield")
##D split_and_save(data_or_path = input_data, output_directory, lab_assistant_names, insurance_order = insurance_order)
## End(Not run)



cleanEx()
nameEx("states_where_physicians_were_NOT_contacted")
### * states_where_physicians_were_NOT_contacted

flush(stderr()); flush(stdout())

### Name: states_where_physicians_were_NOT_contacted
### Title: Summarize States Where Physicians Were NOT Contacted
### Aliases: states_where_physicians_were_NOT_contacted

### ** Examples

# Example with provided all_states
filtered_data <- data.frame(state = c("California", "New York", "Texas"))
all_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                 "New Hampshire", "New Jersey", "New Mexico", "New York",
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                 "Washington", "West Virginia", "Wisconsin", "Wyoming",
                 "District of Columbia")
states_where_physicians_were_NOT_contacted(filtered_data, all_states)

# Example with default all_states
filtered_data <- data.frame(state = c("California", "New York", "Texas", "Nevada"))
states_where_physicians_were_NOT_contacted(filtered_data)




cleanEx()
nameEx("taxonomy")
### * taxonomy

flush(stderr()); flush(stdout())

### Name: taxonomy
### Title: Taxonomy Codes for Obstetricians and Gynecologists
### Aliases: taxonomy
### Keywords: datasets

### ** Examples

## Not run: 
##D # Load the taxonomy dataset
##D data(taxonomy)
##D 
##D # Explore the dataset
##D head(taxonomy)
## End(Not run)



cleanEx()
nameEx("test_and_process_isochrones")
### * test_and_process_isochrones

flush(stderr()); flush(stdout())

### Name: test_and_process_isochrones
### Title: Test and Process Isochrones
### Aliases: test_and_process_isochrones

### ** Examples

# Validate the file of geocoded data.
input_file <- readr::read_csv("data/isochrones/inner_join_postmastr_clinician_data.csv") %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::filter(postmastr.name.x != "Hye In Park, MD")

test_and_process_isochrones(input_file = input_file)

# Filter out the rows that are going to error out after using the test_and_process_isochrones function.
# error_rows <- c(265, 431, 816, 922, 1605, 2049, 2212, 2284, 2308, 2409, 2482, 2735, 2875, 2880, 3150, 3552, 3718)
# input_file_no_error_rows <- input_file %>%
#   dplyr::filter(!id %in% error_rows)




cleanEx()
nameEx("tm_write2pdf")
### * tm_write2pdf

flush(stderr()); flush(stdout())

### Name: tm_write2pdf
### Title: Generate overall table
### Aliases: tm_write2pdf

### ** Examples

## Not run: 
##D # Generate the overall table
##D generate_overall_table("data/Table1.rds", "output_tables")
## End(Not run)



cleanEx()
nameEx("validate_and_remove_invalid_npi")
### * validate_and_remove_invalid_npi

flush(stderr()); flush(stdout())

### Name: validate_and_remove_invalid_npi
### Title: Validate and Remove Invalid NPI Numbers
### Aliases: validate_and_remove_invalid_npi

### ** Examples

# Example usage:
# input_data <- "~/path/to/your/NPI/file.csv"
# valid_df <- validate_and_remove_invalid_npi(input_data)




cleanEx()
nameEx("write_output_csv")
### * write_output_csv

flush(stderr()); flush(stdout())

### Name: write_output_csv
### Title: Write a Data Frame to CSV with Robust Logging and Error Handling
### Aliases: write_output_csv

### ** Examples

# Example 1: Save a data frame to the default directory with detailed logging
write_output_csv(df, "output.csv")

# Example 2: Save a data frame to a custom directory without logging
write_output_csv(df, "output.csv", output_dir = "custom/directory", verbose = FALSE)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
