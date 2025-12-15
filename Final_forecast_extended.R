####libraries

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
  
  run_the_forecast <- function(Euro_Countries_GDP_Growth_Log,annual_growth_observed,annual_deflator_forecast){
  
  ###################################################################################
  #The forecast
  ###################################################################################
  
    #-------------------------------------------------------------------------------
    # CHAPTER 1 – Compute forecast growth
    #-------------------------------------------------------------------------------
    
  
  
    # List of countries to process
    countries <- c("France",
                   "Germany",
                   "Italy",
                   "Netherlands",
                   "Spain",
                   "Sum Small euro countries")
    
    # To store results for each country
    all_country_results <- list()
  
  
    for (country_name in countries) {
      
      start_quarter <- 2000.25
      end_quarter   <- 2025.25
      
      # Prepare data and fit ARMA model
      
      country_data  <- prepare_country_ts(
        data          = Euro_Countries_GDP_Growth_Log,
        country_name  = country_name,
        start_quarter = start_quarter,
        end_quarter   = end_quarter,
        gdp_growth_to_train_on = "gdp_growth_log_wins_001"
      )
      
      arma_fit <- select_best_arma(country_data$ts_data,criterion = "AIC")
      
      # Generate forecasts
      fc_log_growth <- forecast_arma_model(
        model         = arma_fit$model,
        start_quarter = start_quarter,
        end_quarter   = end_quarter,
        horizons      = 1:10
      )
      
      # store results for this country
      all_country_results[[country_name]] <- list(
        arma_fit      = arma_fit,
        forecast      = fc_log_growth,
        train_data    = country_data$train_data
      )
    
      
    }
    
    
    #-------------------------------------------------------------------------------
    # CHAPTER 2 – Make a table of the results
    #-------------------------------------------------------------------------------
    
    forecast_table_growth_countries <- bind_rows(
      lapply(names(all_country_results), function(country_name) {
        
        res <- all_country_results[[country_name]]
        
        tibble(
          Country        = country_name,
          Horizon        = 1:10,
          Forecasted_Quarter  = res$forecast$forecast_dates,
          Forecast_Growth_logdiff = res$forecast$forecast_values,
          ARMA           = paste0("ARMA(",
                                  res$arma_fit$order["p"], ",",
                                  res$arma_fit$order["q"], ")"),
        )
      })
    )
    
    forecast_table_growth_countries <- add_forecast_nominal(forecast_table_growth_countries,Euro_Countries_GDP_Growth_Log, MultiHorizonSets = FALSE)
    
    
    
    
    #-------------------------------------------------------------------------------
    # CHAPTER 3 – Compute the forecasted Nominal and Growth for the Eurozone
    #-------------------------------------------------------------------------------
    
    
    #1 prepare the Eurozone countries in the data set
    eurozone_final <- forecast_table_growth_countries %>%
      group_by(Forecasted_Quarter, Horizon) %>%
      summarise(
        Forecast_Nominal_GDP = sum(Forecast_Nominal_GDP, na.rm = TRUE),
        last_observed_quarter = first(last_observed_quarter),  # take the common value
        .groups = "drop"
      ) %>%
      mutate(
        Country = "Eurozone",
        Forecast_Growth_logdiff = NA_real_,
        Forecast_Growth_rate = NA_real_,
        ARMA = NA_character_
      )
    
    forecast_table_growth_countries <- bind_rows(
      forecast_table_growth_countries,
      eurozone_final
    )
    
    #2 compute the Eurozone's log-diff
    forecast_table_growth_countries <- compute_eurozone_logdiff(forecast_table_growth_countries,Euro_Countries_GDP_Growth_Log,MultiHorizonSets = FALSE)
    
    
    #-------------------------------------------------------------------------------
    # CHAPTER 4 – Compute the quarterly and annual growth rate
    #-------------------------------------------------------------------------------
    
    #3 compute the growth rate from the log-diff
    forecast_table_growth_countries <- forecast_table_growth_countries %>%
      mutate(
        Forecast_Growth_rate = 100 * (exp(Forecast_Growth_logdiff / 100) - 1),
      )
    
    #compute the yearly nominal GDP
    annual_growth_forecast <- compute_annual_nominal(forecast_table_growth_countries,Euro_Countries_GDP_Growth_Log)
  
    #compute the yearly nominal growth 
    annual_growth_forecast <- compute_growth(annual_growth_forecast,annual_growth_observed, which = "nominal")
    
    #Add the forecasted deflator 
    annual_growth_forecast <- add_deflator_forecast_to_annual(annual_growth_forecast,annual_deflator_forecast)
  
    #compute the yearly real GDP
    annual_growth_forecast <- add_real_gdp(annual_growth_forecast)
    
    #compute the yearly real growth
    annual_growth_forecast <- compute_growth(annual_growth_forecast,annual_growth_observed, which = "real")
    
    annual_growth_forecast_display <- annual_growth_forecast %>%
      mutate(across(matches("^[0-9]{4}$"), ~ format(., scientific = FALSE)))
    
    #-------------------------------------------------------------------------------
    # CHAPTER 5 – Compute the countries share of growth
    #-------------------------------------------------------------------------------
    
    ################### Prepare the annual data (forecast+observed) ##############
    
    annual_growth_all <- full_join(
      annual_growth_observed,
      annual_growth_forecast,
      by = c("Country", "type")
    )
    annual_growth_all_display <- annual_growth_all %>%
      mutate(across(matches("^[0-9]{4}$"), ~ format(., scientific = FALSE)))
    
    ################### Compute the weight of each country
    
    matrix_country_weitgh <- compute_country_weight(annual_growth_all)
    
    ################### Compute the countrie's nominal contribution ##############
    
    countries_gowth_contributions <- compute_country_weighted_growth(annual_growth_all,matrix_country_weitgh)
    
    countries_gowth_contributions <- add_relative_contribution_nominal(countries_gowth_contributions)
    
    ################## Compute the countrie's reel contribution ##################
    
    matrix_country_real_nominal_diff <- compute_growth_nominal_real_diff(annual_growth_all)
    
    countries_gowth_contributions <- add_real_growth_pure_contribution(
      countries_gowth_contributions,
      matrix_country_weitgh,
      matrix_country_real_nominal_diff
    )
    countries_gowth_contributions <- correct_purreel_small_countries(countries_gowth_contributions)
    
    countries_gowth_contributions <- add_relative_contribution_real(countries_gowth_contributions)
    
    return(list(
      countries_gowth_contributions = countries_gowth_contributions,
      annual_growth_all             = annual_growth_all,
      annual_growth_all_display     = annual_growth_all_display
    ))
    
}
