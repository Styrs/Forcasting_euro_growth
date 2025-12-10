
library(forecast)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(zoo)    



#################################################################################¨
#Import the data
#################################################################################


url_data_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Euro_Countries_GDP_Growth_Log.xlsx"
download.file(url_data_countries, destfile = "data.xlsx", mode = "wb")
Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx")



url_data_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/annual_growth_observed.xlsx"
download.file(url_data_countries, destfile = "data.xlsx", mode = "wb")
annual_growth_observed <- read_excel("data.xlsx")



###################################################################################
#The forecast
###################################################################################

#-------------------------------------------------------------------------------
# CHAPTER 1 – Compute forecast growth
#-------------------------------------------------------------------------------



# List of countries to process
countries <- c("Eurozone",
               "France",
               "Germany",
               "Italy",
               "Netherlands",
               "Spain")

# To store results for each country
all_country_deflator <- list()


for (country_name in countries) {
  
  start_quarter <- 2000.25
  end_quarter   <- 2025.25
  
  # Prepare data and fit ARMA model
  
  country_data  <- prepare_country_ts(
    data          = Euro_Countries_GDP_Growth_Log,
    country_name  = country_name,
    start_quarter = start_quarter,
    end_quarter   = end_quarter,
    gdp_growth_to_train_on = "deflator_logdiff"
  )
  
  arma_fit <- select_best_arma(country_data$ts_data,criterion = "BIC")
  
  # Generate forecasts
  fc_deflator <- forecast_arma_model(
    model         = arma_fit$model,
    start_quarter = start_quarter,
    end_quarter   = end_quarter,
    horizons      = 1:10
  )
  
  # store results for this country
  all_country_deflator[[country_name]] <- list(
    arma_fit      = arma_fit,
    forecast      = fc_deflator,
    train_data    = country_data$train_data
  )
  
  
}


#-------------------------------------------------------------------------------
# CHAPTER 2 – Make a table of the results
#-------------------------------------------------------------------------------

deflator_forecast  <- bind_rows(
  lapply(names(all_country_deflator), function(country_name) {
    
    res <- all_country_deflator[[country_name]]
    
    tibble(
      Country        = country_name,
      Horizon        = 1:10,
      Forecasted_Quarter  = res$forecast$forecast_dates,
      Forecast_deflator = res$forecast$forecast_values,
      ARMA           = paste0("ARMA(",
                              res$arma_fit$order["p"], ",",
                              res$arma_fit$order["q"], ")"),
    )
  })
)



compute_annual_deflator_forecast <- function(deflator_forecast,
                                             Euro_Countries_GDP_Growth_Log) {
  ## --- 1. Get last observed deflator level for each country ----
  last_deflator <- Euro_Countries_GDP_Growth_Log %>%
    group_by(Country) %>%
    filter(Quarter == max(Quarter, na.rm = TRUE)) %>%
    summarise(
      start_level = deflator_2005_index_seas[1],
      .groups = "drop"
    )
  
  ## --- 2. Build forecast deflator LEVELS from log-diff forecasts ----
  fc_levels <- deflator_forecast %>%
    left_join(last_deflator, by = "Country") %>%
    group_by(Country) %>%
    arrange(Horizon, .by_group = TRUE) %>%
    mutate(
      # Forecast_deflator is a log-diff in %, so use exp(x/100)
      growth_factor = exp(Forecast_deflator / 100),
      deflator_fcst_level = start_level * cumprod(growth_factor)
    ) %>%
    ungroup() %>%
    select(Country, Forecasted_Quarter, deflator_fcst_level)
  
  ## --- 3. Prepare forecast quarters by year and quarter index ----
  fc_q <- fc_levels %>%
    mutate(
      year    = floor(Forecasted_Quarter),
      quarter = as.integer(round((Forecasted_Quarter - year) * 4)) + 1L
    ) %>%
    select(Country, year, quarter, deflator_fcst_level)
  
  years_forecast     <- sort(unique(fc_q$year))
  countries_forecast <- unique(fc_q$Country)
  
  ## --- 4. Prepare observed deflator quarters for those years ----
  obs_q <- Euro_Countries_GDP_Growth_Log %>%
    filter(
      Country %in% countries_forecast,
      floor(Quarter) %in% years_forecast
    ) %>%
    mutate(
      year    = floor(Quarter),
      quarter = as.integer(round((Quarter - year) * 4)) + 1L
    ) %>%
    select(Country, year, quarter,
           deflator_obs_level = deflator_2005_index_seas)
  
  ## --- 5. Combine: forecast overrides observed where available ----
  combined_q <- obs_q %>%
    full_join(fc_q,
              by = c("Country", "year", "quarter")) %>%
    mutate(
      deflator = ifelse(!is.na(deflator_fcst_level),
                        deflator_fcst_level,
                        deflator_obs_level)
    )
  
  ## --- 6. Annual mean deflator per Country-Year ----
  annual_deflator <- combined_q %>%
    group_by(Country, year) %>%
    summarise(
      annual_deflator = mean(deflator, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## --- 7. Wide table: Country x Year ----
  annual_deflator_wide <- annual_deflator %>%
    mutate(year = as.character(year)) %>%
    pivot_wider(
      names_from  = year,
      values_from = annual_deflator
    ) %>%
    arrange(Country)
  
  return(annual_deflator_wide)
}


annual_deflator_forecast <- compute_annual_deflator_forecast(deflator_forecast,Euro_Countries_GDP_Growth_Log)


#copy the Eurozone's forecast for the sum of small countries. 
year_cols <- grep("^[0-9]{4}$", names(annual_deflator_forecast), value = TRUE)
euro_row <- annual_deflator_forecast %>%
  filter(Country == "Eurozone")
sum_small_row <- euro_row %>%
  mutate(Country = "Sum Small euro countries")
annual_deflator_forecast <- annual_deflator_forecast %>%
  bind_rows(sum_small_row) %>%
  arrange(Country)
