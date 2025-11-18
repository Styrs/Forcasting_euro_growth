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


url_data_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/data_countries_euro_growth_gdp_seas.xlsx"
download.file(url_data_countries, destfile = "data.xlsx", mode = "wb")
data_countries_euro_growth_gdp_seas <- read_excel("data.xlsx")

url_Eurozone_GDPgrowth_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_GDPgrowth_seas.xlsx"
download.file(url_Eurozone_GDPgrowth_seas, destfile = "data.xlsx", mode = "wb")
Eurozone_GDPgrowth_seas <- read_excel("data.xlsx")

################################################################################
#The functions
################################################################################
  
  #-------------------------------------------------------------------------------
  # CHAPTER 1 – Function to prepare the data (same that for the extended )
  #-------------------------------------------------------------------------------
  
  prepare_country_ts <- function(data, country_name, start_quarter, end_quarter) {
    
    country_data <- data[data$Country == country_name, ]
    
    # Keep only the estimation window
    train_data <- country_data[country_data$Quarter >= start_quarter &
                                 country_data$Quarter <= end_quarter, ]
    
    # Build quarterly time series object
    # start = c(year, quarter_index) with quarter_index in {1,2,3,4}
    start_year    <- floor(start_quarter)
    start_q_index <- as.integer((start_quarter - start_year) * 4 + 1)
    
    ts_data <- ts(
      train_data$seasonal_adjusted_wins,
      start = c(start_year, start_q_index),
      frequency = 4
    )
    
    return(list(
      train_data = train_data,
      ts_data    = ts_data
    ))
  }
  
  #-------------------------------------------------------------------------------
  # CHAPTER 2 – ARMA MODEL SELECTION (AIC) AND ESTIMATION
  #-------------------------------------------------------------------------------
  
  select_best_arma <- function(ts_data, max_p = 5, max_q = 5) {
    
    best_aic   <- Inf
    best_model <- NULL
    
    for (p in 0:max_p) {
      for (q in 0:max_q) {
        
        # Skip ARMA(0,0)
        if (p == 0 && q == 0) next
        
        tryCatch({
          candidate_model <- Arima(ts_data, order = c(p, 0, q))
          candidate_aic   <- AIC(candidate_model)
          
          if (candidate_aic < best_aic) {
            best_aic   <- candidate_aic
            best_model <- candidate_model
          }
        }, error = function(e) {
          # silently skip failed models
        })
        
      }
    }
    
    if (is.null(best_model)) {
      stop("No ARMA model could be estimated for this time series window.")
    }
    
    best_order <- arimaorder(best_model)
    
    return(list(
      model = best_model,
      order = best_order,
      aic   = best_aic
    ))
  }
  
  
  #-------------------------------------------------------------------------------
  # CHAPTER 3 – ARMA FORECASTS (CORRECTED DATES)
  #-------------------------------------------------------------------------------
  
  forecast_arma_model <- function(model, start_quarter, end_quarter, horizons) {
    
    # 1) Forecast growth with the ARMA model
    fc <- forecast(model, h = max(horizons))
    fc_values <- fc$mean[horizons]
    
    # 2) Convert horizons into year-quarter labels based on end_quarter
    date_labels <- sapply(horizons, function(h) {
      
      # Quarter value of the target (e.g. 2010.00 + 0.25*1 = 2010.25)
      q_val <- end_quarter + 0.25 * h
      
      year    <- floor(q_val)
      q_index <- as.integer((q_val - year) * 4 + 1)  # 1..4
      
      paste0(year, "-Q", q_index)
    })
    
    # 3) Return a structured list
    return(list(
      forecast_values = as.numeric(fc_values),
      forecast_dates  = date_labels,
      forecast_object = fc
    ))
  }



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
    
    country_data  <- prepare_country_ts(data_countries_euro_growth_gdp_seas,
                                       country_name, start_quarter, end_quarter)
    arma_fit <- select_best_arma(country_data$ts_data)
    
    # Generate forecasts
    fc_growth <- forecast_arma_model(
      model         = arma_fit$model,
      start_quarter = start_quarter,
      end_quarter   = end_quarter,
      horizons      = 1:10
    )
    
    # store results for this country
    all_country_results[[country_name]] <- list(
      arma_fit      = arma_fit,
      forecast      = fc_growth,
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
        Forecast_Date  = res$forecast$forecast_dates,
        Forecast_Value = res$forecast$forecast_values,
        AR_Order       = res$arma_fit$order["p"],
        MA_Order       = res$arma_fit$order["q"]
      )
    })
  )
  
  # 1) Add ARMA model column
  forecast_table_growth_countries <- forecast_table_growth_countries %>%
    mutate(Model = paste0("ARMA(", AR_Order, ",", MA_Order, ")"))
  
  # 2) Go from long (one row per country × horizon) 
  #    to wide (one row per country, one column per forecast date)
  forecast_table_growth_countries <- forecast_table_growth_countries %>%
    select(-Horizon) %>%                 # we don't need Horizon anymore
    pivot_wider(
      id_cols   = c(Country, Model),
      names_from  = Forecast_Date,       # each date becomes a column
      values_from = Forecast_Value       # cell content = forecast value
      # if you prefer safer column names: 
      # names_glue = "fc_{Forecast_Date}"
    ) %>%
    arrange(Country)
  
  
  #-------------------------------------------------------------------------------
  # CHAPTER 3 – Compute the forecasted Nominal 
  #-------------------------------------------------------------------------------
  
  
  
  
  # 3.1 For each country: start from last observed Nominal_GDP and apply growth
  forecast_table_nominal_countries <- bind_rows(
    lapply(names(all_country_results), function(country_name) {
      
      res <- all_country_results[[country_name]]
      
      # Last observed nominal GDP in the estimation sample for this country
      train_df <- res$train_data %>%
        filter(!is.na(Nominal_GDP)) %>%
        arrange(Quarter)
      
      last_nominal_gdp <- tail(train_df$Nominal_GDP, 1)
      
      # Corresponding growth forecasts and dates
      g      <- res$forecast$forecast_values   # in %
      dates  <- res$forecast$forecast_dates
      model  <- paste0("ARMA(", res$arma_fit$order["p"], ",", res$arma_fit$order["q"], ")")
      
      # Apply growth recursively: GDP_{t+1} = GDP_t * (1 + g/100)
      nominal_fc <- numeric(length(g))
      prev <- last_nominal_gdp
      
      for (i in seq_along(g)) {
        prev <- prev * (1 + g[i] / 100)
        nominal_fc[i] <- prev
      }
      
      tibble(
        Country              = country_name,
        Model                = model,
        Forecast_Date        = dates,
        Forecast_Nominal_GDP = nominal_fc
      )
    })
  )
  
  # 3.2 Put nominal forecasts in wide format (one row per country)
  forecast_table_nominal_countries <- forecast_table_nominal_countries %>%
    pivot_wider(
      id_cols    = c(Country, Model),
      names_from = Forecast_Date,
      values_from = Forecast_Nominal_GDP
    ) %>%
    arrange(Country)
  
  
  
  #-------------------------------------------------------------------------------
  # CHAPTER 4 – Compute the forecasted Nominal and Growth for the Eurozone
  #-------------------------------------------------------------------------------
  
  # 4.1 Go back to long format for country nominal forecasts
  Forecasted_eurozone_nominal <- forecast_table_nominal_countries %>%
    pivot_longer(
      cols = -c(Country, Model),
      names_to  = "Forecast_Date",
      values_to = "Forecast_Nominal_GDP"
    )
  
  # 4.2 Sum across countries to get Eurozone nominal GDP for each forecast date
  Forecasted_eurozone_nominal <- Forecasted_eurozone_nominal %>%
    group_by(Forecast_Date) %>%
    summarise(
      Eurozone_Nominal_GDP = sum(Forecast_Nominal_GDP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Forecast_Date)
  
  # 4.3 Compute Eurozone growth between periods (QoQ growth in %)
  
    # last observed nominal GDP for the Eurozone (e.g. Quarter = 2025.25)
    last_euro_nominal <- Eurozone_GDPgrowth_seas %>%
      filter(!is.na(Eurozone_Nominal_GDP)) %>%
      arrange(Quarter) %>%
      slice_tail(n = 1) %>%
      pull(Eurozone_Nominal_GDP)
    
    Forecasted_eurozone_nominal <- Forecasted_eurozone_nominal %>%
      arrange(Forecast_Date) %>%
      mutate(
        lag_nominal = dplyr::lag(Eurozone_Nominal_GDP),
        lag_nominal = if_else(
          is.na(lag_nominal),
          last_euro_nominal,
          lag_nominal
        ),
        Eurozone_Growth = 100 * (Eurozone_Nominal_GDP / lag_nominal - 1)
      ) %>%
      select(-lag_nominal)
  
  # 4.4 Wide table: forecasted Eurozone growth
  Forecasted_eurozone_growth <- Forecasted_eurozone_nominal %>%
    mutate(Region = "Eurozone") %>%
    select(Region, Forecast_Date, Eurozone_Growth) %>%
    pivot_wider(
      id_cols    = Region,
      names_from = Forecast_Date,
      values_from = Eurozone_Growth
    )
  
  # 4.5 Wide table: forecasted Eurozone nominal GDP
  Forecasted_eurozone_nominal <- Forecasted_eurozone_nominal %>%
    mutate(Region = "Eurozone") %>%
    select(Region, Forecast_Date, Eurozone_Nominal_GDP) %>%
    pivot_wider(
      id_cols    = Region,
      names_from = Forecast_Date,
      values_from = Eurozone_Nominal_GDP
    )
  
  
  
  
  
  
###################################################################################
#Plots
###################################################################################
  
  
  
  #------------Prepare the data-------------------
  
  hist <- Eurozone_GDPgrowth_seas %>%
    select(Quarter, seasonal_adjusted_wins) %>%
    mutate(Date = as.yearqtr(Quarter))
  
  forecast <- Forecasted_eurozone_growth %>%
    pivot_longer(
      cols = -Region,
      names_to = "Quarter",
      values_to = "Forecast"
    ) %>%
    mutate(Date = as.yearqtr(Quarter, format = "%Y-Q%q"))
  
  # Keep only from 2015 onwards
  hist_recent     <- hist     %>% filter(Date >= as.yearqtr("2015 Q1"))
  forecast_recent <- forecast %>% filter(Date >= as.yearqtr("2015 Q1"))
  
  # Start of forecast (for the red vertical line)
  start_forecast_date <- min(forecast_recent$Date)
  
  
  #------------The growth plot-------------------
  
  ggplot() +
    geom_line(data = hist_recent,
              aes(x = Date, y = seasonal_adjusted_wins,
                  color = "Past GDP Growth"),
              size = 1) +
    geom_line(data = forecast_recent,
              aes(x = Date, y = Forecast/100,
                  color = "Forecasted GDP Growth"),
              size = 1) +
    # red line at the start of the forecast
    geom_vline(xintercept = start_forecast_date,
               color = "red", linetype = "dashed") +
    scale_color_manual(values = c("Past GDP Growth" = "blue",
                                  "Forecasted GDP Growth" = "red")) +
    labs(
      title = "Eurozone GDP Growth (Actual vs Forecast)",
      x = "Quarter",
      y = "GDP Growth (QoQ, %)",
      color = ""
    ) +
    theme_minimal(base_size = 12)
  
  
