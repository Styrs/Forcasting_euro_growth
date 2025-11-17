## Load the libraries
library(readxl)
library(dplyr)
library(tseries)
library(dplyr)
library(forecast)
library(writexl)







## ======================================================================
## 1. Import the data
## ======================================================================



  url_euro_growth_gdp <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Euro_Countries_GDP_Growth_Log.xlsx"
  
  download.file(url_euro_growth_gdp, destfile = "data.xlsx", mode = "wb")
  
  Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx")

  Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx") %>%
    filter(Country != "Poland")   # <-- remove Poland here
  
  
  
  
  url_eurozone_growth_gdp <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_agregated_GDPgrowth.xlsx"
  
  download.file(url_eurozone_growth_gdp, destfile = "data.xlsx", mode = "wb")
  
  Eurozone_agregated_GDPgrowth <- read_excel("data.xlsx")
  


## ======================================================================
## 2. The ADF test function 
## ======================================================================


  ###### 2.1 Defining the function of ADF test######################################3
  
  run_adf_test <- function(data, value_col, group_col = NULL, time_col = NULL) {
    library(dplyr)
    library(tseries)
    
    if (!is.null(group_col)) {
      # Case with groups (e.g. countries)
      results <- data %>%
        group_by(.data[[group_col]]) %>%
        arrange(.data[[time_col]], .by_group = TRUE) %>%
        reframe({
          x <- na.omit(.data[[value_col]])
          test <- tseries::adf.test(x, k = trunc(length(x)^(1/3)))
          tibble(
            n = length(x),
            adf_stat = unname(test$statistic),
            p_value = test$p.value
          )
        })
    } else {
      # Case without groups (e.g. Eurozone aggregate)
      x <- na.omit(data[[value_col]])
      test <- tseries::adf.test(x, k = trunc(length(x)^(1/3)))
      results <- tibble(
        n = length(x),
        adf_stat = unname(test$statistic),
        p_value = test$p.value
      )
    }
    
    # Print summary automatically
    cat("=========================================\n")
    cat(" Augmented Dickey-Fuller (ADF) Test\n")
    cat("=========================================\n")
    print(results)
    cat("\nInterpretation:\n")
    cat(" - Null hypothesis: the series has a unit root (non-stationary)\n")
    cat(" - Small p-value (< 0.05) → reject H0 → series is stationary\n")
    cat("=========================================\n\n")
    
    return(results)
  }
  
  
  
  #### 2.2 Calling the function with our datasets########################
  
  
  adf_results_countries <- run_adf_test(
    data = Euro_Countries_GDP_Growth_Log,
    value_col = "gdp_growth_qoq_log",
    group_col = "Country",
    time_col = "Quarter"
  )
  
  
  
  adf_results_eurozone <- run_adf_test(
    data = Eurozone_agregated_GDPgrowth,
    value_col = "log_GDP_growth"
  )
  
  
  
  
  
  
  
  

## ======================================================================
## 3. Do the calendar and seasonnality test
## ======================================================================

  ###### 3.1 Defining the function of deseasonnalizing######################################
  
  
  run_seasonal_adjustment <- function(data, value_col, group_col = NULL, time_col = NULL) {
    library(dplyr)
    library(forecast)
    
    if (!is.null(group_col)) {
      # Case with multiple groups (e.g., countries)
      data_adj <- data %>%
        group_by(.data[[group_col]]) %>%
        arrange(.data[[time_col]], .by_group = TRUE) %>%
        mutate(
          seasonal_adjusted = {
            x <- na.omit(.data[[value_col]])
            ts_x <- ts(x, frequency = 4)       # quarterly data
            fit <- stl(ts_x, s.window = "periodic")
            adj <- seasadj(fit)                # remove seasonal component
            # pad NAs for alignment
            c(rep(NA, length(.data[[value_col]]) - length(adj)), as.numeric(adj))
          }
        ) %>%
        ungroup()
      
    } else {
      # Single time series (e.g., Eurozone aggregate)
      data <- data %>% arrange(.data[[time_col]])
      x <- na.omit(data[[value_col]])
      ts_x <- ts(x, frequency = 4)
      fit <- stl(ts_x, s.window = "periodic")
      adj <- seasadj(fit)
      data$seasonal_adjusted <- c(rep(NA, nrow(data) - length(adj)), as.numeric(adj))
      data_adj <- data
    }
    
    # Print confirmation
    cat("=========================================\n")
    cat(" Seasonal Adjustment Completed\n")
    cat("=========================================\n\n")
    
    return(data_adj)
  }
  
  ###### 3.2  deseasonnalizing the data sets ######################################
  
  data_countries_euro_growth_gdp_seas <- run_seasonal_adjustment(
    data = Euro_Countries_GDP_Growth_Log,
    value_col = "gdp_growth_qoq_log",
    group_col = "Country",
    time_col = "Quarter"
  )
  
  
  Eurozone_GDPgrowth_seas <- run_seasonal_adjustment(
    data = Eurozone_agregated_GDPgrowth,
    value_col = "log_GDP_growth",
    time_col = "Quarter"
  )
  
  
  ## ======================================================================
  ## 4. Winsorisation
  ## ======================================================================
  
  
  
  
  
  
  ###### 4.1 winsorisation of Eurizone data ######################################################3
  
  x <- Eurozone_GDPgrowth_seas$seasonal_adjusted
  q_low  <- quantile(x, 0.01, na.rm = TRUE)
  q_high <- quantile(x, 0.99, na.rm = TRUE)
  
  Eurozone_GDPgrowth_seas$seasonal_adjusted_wins <-
    pmin(pmax(x, q_low), q_high)
  
  
  ####### 4.2 winsoristation of countries data ##############################################33
  
  data_countries_euro_growth_gdp_seas <-
    data_countries_euro_growth_gdp_seas %>%
    group_by(Country) %>%
    mutate(
      seasonal_adjusted_wins = {
        x <- seasonal_adjusted
        
        q_low  <- quantile(x, 0.01, na.rm = TRUE)
        q_high <- quantile(x, 0.99, na.rm = TRUE)
        
        pmin(pmax(x, q_low), q_high)
      }
    ) %>%
    ungroup()
  
  
  
  
  
  
  write_xlsx(Eurozone_GDPgrowth_seas, "Eurozone_GDPgrowth_seas.xlsx")
  
  write_xlsx(data_countries_euro_growth_gdp_seas, "data_countries_euro_growth_gdp_seas.xlsx")
  
