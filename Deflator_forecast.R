
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

run_deflator_forecast <- function(Euro_Countries_GDP_Growth_Log,annual_growth_observed,confidence_quantiles){
  
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
    
    # compute the quantiles (Gaussian assumption)
    quantiles_forecast <- compute_forecast_quantiles(fc_deflator, probs = c(confidence_quantiles, 1-confidence_quantiles))
    
    
    # store results for this country
    all_country_deflator[[country_name]] <- list(
      arma_fit      = arma_fit,
      forecast      = fc_deflator,
      forecast_q    = quantiles_forecast,
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
        Forecast_deflator_q10  = res$forecast_q$q10,
        Forecast_deflator_q90  = res$forecast_q$q90,
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
      dplyr::group_by(Country) %>%
      dplyr::filter(Quarter == max(Quarter, na.rm = TRUE)) %>%
      dplyr::summarise(
        start_level = deflator_2005_index_seas[1],
        .groups = "drop"
      )
    
    ## --- 2. Build forecast deflator LEVELS from log-diff forecasts (mean + q10 + q90) ----
    fc_levels <- deflator_forecast %>%
      dplyr::left_join(last_deflator, by = "Country") %>%
      dplyr::group_by(Country) %>%
      dplyr::arrange(Horizon, .by_group = TRUE) %>%
      dplyr::mutate(
        # mean path
        growth_factor      = exp(Forecast_deflator / 100),
        deflator_fcst_level = start_level * cumprod(growth_factor),
        
        # q10 path
        growth_factor_q10      = exp(Forecast_deflator_q10 / 100),
        deflator_fcst_level_q10 = start_level * cumprod(growth_factor_q10),
        
        # q90 path
        growth_factor_q90      = exp(Forecast_deflator_q90 / 100),
        deflator_fcst_level_q90 = start_level * cumprod(growth_factor_q90)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        Country, Forecasted_Quarter,
        deflator_fcst_level,
        deflator_fcst_level_q10,
        deflator_fcst_level_q90
      )
    
    ## --- 3. Prepare forecast quarters by year and quarter index ----
    fc_q <- fc_levels %>%
      dplyr::mutate(
        year    = floor(Forecasted_Quarter),
        quarter = as.integer(round((Forecasted_Quarter - year) * 4)) + 1L
      ) %>%
      dplyr::select(
        Country, year, quarter,
        deflator_fcst_level,
        deflator_fcst_level_q10,
        deflator_fcst_level_q90
      )
    
    years_forecast     <- sort(unique(fc_q$year))
    countries_forecast <- unique(fc_q$Country)
    
    ## --- 4. Prepare observed deflator quarters for those years ----
    obs_q <- Euro_Countries_GDP_Growth_Log %>%
      dplyr::filter(
        Country %in% countries_forecast,
        floor(Quarter) %in% years_forecast
      ) %>%
      dplyr::mutate(
        year    = floor(Quarter),
        quarter = as.integer(round((Quarter - year) * 4)) + 1L
      ) %>%
      dplyr::select(
        Country, year, quarter,
        deflator_obs_level = deflator_2005_index_seas
      )
    
    ## --- 5. Combine: forecast overrides observed where available ----
    combined_q <- obs_q %>%
      dplyr::full_join(fc_q, by = c("Country", "year", "quarter")) %>%
      dplyr::mutate(
        # For quarters without forecast, fallback to observed for ALL paths
        deflator_mean = dplyr::if_else(!is.na(deflator_fcst_level),
                                       deflator_fcst_level,
                                       deflator_obs_level),
        deflator_q10  = dplyr::if_else(!is.na(deflator_fcst_level_q10),
                                       deflator_fcst_level_q10,
                                       deflator_obs_level),
        deflator_q90  = dplyr::if_else(!is.na(deflator_fcst_level_q90),
                                       deflator_fcst_level_q90,
                                       deflator_obs_level)
      )
    
    ## --- 6. Annual mean deflator per Country-Year (mean of quarters) ----
    annual_deflator <- combined_q %>%
      dplyr::group_by(Country, year) %>%
      dplyr::summarise(
        annual_deflator     = mean(deflator_mean, na.rm = TRUE),
        annual_deflator_q10 = mean(deflator_q10,  na.rm = TRUE),
        annual_deflator_q90 = mean(deflator_q90,  na.rm = TRUE),
        .groups = "drop"
      )
    
    ## --- 7. Wide table: Country x Year (three blocks of columns) ----
    annual_mean_wide <- annual_deflator %>%
      dplyr::select(Country, year, annual_deflator) %>%
      dplyr::mutate(year = as.character(year)) %>%
      tidyr::pivot_wider(names_from = year, values_from = annual_deflator)
    
    annual_q10_wide <- annual_deflator %>%
      dplyr::select(Country, year, annual_deflator_q10) %>%
      dplyr::mutate(year = paste0(as.character(year), "_q10")) %>%
      tidyr::pivot_wider(names_from = year, values_from = annual_deflator_q10)
    
    annual_q90_wide <- annual_deflator %>%
      dplyr::select(Country, year, annual_deflator_q90) %>%
      dplyr::mutate(year = paste0(as.character(year), "_q90")) %>%
      tidyr::pivot_wider(names_from = year, values_from = annual_deflator_q90)
    
    annual_deflator_wide <- annual_mean_wide %>%
      dplyr::left_join(annual_q10_wide, by = "Country") %>%
      dplyr::left_join(annual_q90_wide, by = "Country") %>%
      dplyr::arrange(Country)
    
    return(annual_deflator_wide)
  }
  
  
  annual_deflator_forecast <- compute_annual_deflator_forecast(deflator_forecast,Euro_Countries_GDP_Growth_Log)
  
  
  # Copy the Eurozone annual deflator (mean + quantiles) for Sum Small euro countries
  euro_row <- annual_deflator_forecast %>%
    dplyr::filter(Country == "Eurozone")
  
  sum_small_row <- euro_row %>%
    dplyr::mutate(Country = "Sum Small euro countries")
  
  annual_deflator_forecast <- annual_deflator_forecast %>%
    dplyr::bind_rows(sum_small_row) %>%
    dplyr::arrange(Country)
  
  return(annual_deflator_forecast)
}
