library(forecast)
library(tsibble)
library(dplyr)
library(lubridate)


# Load the data (adjust path as needed)
df <- read_excel("C:/Users/slakshminarayanan/Desktop/Reckitt_shiny_app/code/V1/www/Burden_estimate_v1.xlsx")
# Assuming df is already loaded from the provided document

# Convert 'Calendar Year' to Date
df$calendar.year <- ymd(paste(df$`Calendar Year`, "01", "01", sep = "-"))
if (any(is.na(df$calendar.year))) {
  warning("Some 'Calendar Year' values failed to convert. Check your data:")
  print(df[is.na(df$calendar.year), "Calendar Year"])
}

# Filter for ibuprofen-containing molecules, France, and strength == 100, and calculate Total_MG
df <- df %>%
  filter(grepl("ibuprofen", `Molecule Combination`, ignore.case = TRUE))  %>%
  mutate(Total_MG = strength * SU)

# Debug: Check for valid Total_MG values
print("Summary of Total_MG:")
print(summary(df$Total_MG))
print("Rows with invalid Total_MG:")
print(nrow(df[is.na(df$Total_MG) | df$Total_MG <= 0, ]))

# Aggregate to yearly data (in case there are multiple entries per year)
df_yearly <- df %>%
  mutate(year = year(calendar.year)) %>%
  group_by(Country, `CHC 3`, `Molecule Combination`, `Pack Strength`, year) %>%
  summarise(Total_MG = sum(Total_MG, na.rm = TRUE),
            calendar.year = min(calendar.year),
            .groups = "drop") %>%
  ungroup()

# Group the data
grouped <- df_yearly %>%
  group_by(Country, `CHC 3`, `Molecule Combination`, `Pack Strength`)

# Forecast function using Linear Regression with a fallback to Naive
forecast_lm <- function(group) {
  # Filter out NA and non-positive values for Total_MG
  valid_data <- group %>%
    filter(!is.na(Total_MG) & Total_MG > 0)
  
  # Check for sufficient data (need at least 3 points for linear regression)
  if (nrow(valid_data) < 3) {
    warning(paste("Skipping group with insufficient valid Total_MG data (rows =", nrow(valid_data), "):",
                  paste(unique(group$Country), unique(group$`CHC 3`),
                        unique(group$`Molecule Combination`), unique(group$`Pack Strength`),
                        collapse = ", ")))
    return(data.frame(calendar.year = as.Date(character()), Total_MG = numeric()))
  }
  
  # Convert to tsibble with yearly frequency
  valid_data_ts <- valid_data %>%
    as_tsibble(index = calendar.year, regular = TRUE) %>%
    fill_gaps(.full = TRUE) %>%  # Fill missing years
    mutate(Total_MG = ifelse(is.na(Total_MG), mean(Total_MG, na.rm = TRUE), Total_MG))  # Impute NA with mean
  
  # If imputation results in NA (e.g., all values NA), skip
  if (any(is.na(valid_data_ts$Total_MG))) {
    warning(paste("Skipping group due to NA after imputation:",
                  paste(unique(group$Country), unique(group$`CHC 3`),
                        unique(group$`Molecule Combination`), unique(group$`Pack Strength`),
                        collapse = ", ")))
    return(data.frame(calendar.year = as.Date(character()), Total_MG = numeric()))
  }
  
  # Extract year as numeric for regression
  valid_data_ts$year_num <- as.numeric(format(valid_data_ts$calendar.year, "%Y"))
  
  # Try fitting linear regression model (Total_MG ~ year_num)
  fit <- tryCatch({
    lm(Total_MG ~ year_num, data = valid_data_ts)
  }, error = function(e) {
    warning(paste("Linear regression failed for group:",
                  paste(unique(group$Country), unique(group$`CHC 3`),
                        unique(group$`Molecule Combination`), unique(group$`Pack Strength`),
                        collapse = ", "), "Error:", e$message))
    return(NULL)
  })
  
  # If linear regression fails, fall back to Naive
  if (is.null(fit)) {
    cat("Falling back to NAIVE model for group:",
        paste(unique(group$Country), unique(group$`CHC 3`),
              unique(group$`Molecule Combination`), unique(group$`Pack Strength`),
              collapse = ", "), "\n")
    fit_naive <- valid_data_ts %>%
      model(NAIVE(Total_MG))
    forecasted <- fit_naive %>% forecast(h = 5)
    forecast_df <- data.frame(
      calendar.year = seq.Date(from = max(valid_data$calendar.year) + years(1), by = "year", length.out = 5),
      Total_MG = as.numeric(forecasted$.mean)
    )
    return(forecast_df)
  }
  
  # Create future years for prediction
  max_year <- max(valid_data_ts$year_num)
  future_years <- (max_year + 1):(max_year + 5)
  future_df <- data.frame(year_num = future_years)
  
  # Predict using the linear regression model
  predicted <- predict(fit, newdata = future_df)
  
  # Debugging output
  cat("Fitting model for group:",
      paste(unique(group$Country), unique(group$`CHC 3`),
            unique(group$`Molecule Combination`), unique(group$`Pack Strength`),
            collapse = ", "), "\n")
  print("Historical data:")
  print(valid_data_ts)
  print("Model summary:")
  print(summary(fit))
  print("Forecasted values:")
  print(data.frame(calendar.year = future_years, Total_MG = predicted))
  
  # Get max date
  max_date <- max(valid_data$calendar.year, na.rm = TRUE)
  if (!is.finite(as.numeric(max_date))) {
    warning("Invalid max_date for group:",
            paste(unique(group$Country), unique(group$`CHC 3`),
                  unique(group$`Molecule Combination`), unique(group$`Pack Strength`),
                  collapse = ", "))
    return(data.frame(calendar.year = as.Date(character()), Total_MG = numeric()))
  }
  
  # Create forecast data frame
  forecast_df <- data.frame(
    calendar.year = seq.Date(from = max_date + years(1), by = "year", length.out = 5),
    Total_MG = as.numeric(predicted)
  )
  
  return(forecast_df)
}

# Apply forecast to groups
forecasted_groups <- grouped %>%
  group_modify(~ forecast_lm(.x)) %>%
  filter(!is.na(calendar.year))

# Debug: Check if forecasted_groups is empty
if (nrow(forecasted_groups) == 0) {
  warning("No forecasts generated. Check warnings above for skipped groups or model failures.")
} else {
  print("Forecasted groups preview:")
  print(head(forecasted_groups, 10))
}

# Append forecasted values to original dataframe
df_subset <- df %>%
  select(Country, `CHC 3`, `Molecule Combination`, `Pack Strength`, calendar.year, Total_MG)
result_df <- bind_rows(df_subset, forecasted_groups)

# View results
print("Final result preview:")
print(head(result_df, 20))

library(writexl)

file_name <- paste0("C:/Users/slakshminarayanan/Desktop/Reckitt_shiny_app/code/V1/www/burden_estimate_data_v1", ".xlsx")
writexl::write_xlsx(result_df, path = file_name)