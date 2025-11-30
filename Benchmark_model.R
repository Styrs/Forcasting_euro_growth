#### Libraries

library(forecast)
library(readxl)
library(lmtest)
library(ggplot2)



# ----------------------------------------------------------------------------
# CHAPTER 1 – GLOBAL PARAMETERS & PREPARATION
# ----------------------------------------------------------------------------


first_quarter       <- 2000.25      
initial_end_quarter <- 2010.00

last_end_quarter    <- 2022.75    # 2022.75 for full data set 
max_data_quarter    <- 2025.25    

forecast_horizons <- 1:10          


# --- Import de la série Eurozone désaisonnalisée & winsorisée ---

url_data_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Euro_Countries_GDP_Growth_Log.xlsx"
download.file(url_data_countries, destfile = "data.xlsx", mode = "wb")
Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx")



# ----------------------------------------------------------------------------
# CHAPTER 2 – COUNTRY LOOP & AGGREGATED NOMINAL GDP FORECASTS
# ----------------------------------------------------------------------------


#Same code as the other model but there we force an ARMA specification
#with the function fit_fixed_arma that will replace select_best_arma

imposed_p_ARMA = 1
imposed_q_ARMA = 0 



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
  
  message("===============================================")
  message(paste("Processing country:", country_name))
  
  # Expanding-window initialization (per country)
  start_quarter <- first_quarter
  end_quarter   <- initial_end_quarter
  iteration     <- 1
  
  country_results <- list()
  
  while (TRUE) {
    
    last_forecast_quarter <- end_quarter + max(forecast_horizons) * 0.25        # Compute the last forecast quarter for this iteration
    
    
    if (end_quarter > last_end_quarter || last_forecast_quarter > max_data_quarter) {       # Stopping rule (
      message("Stopping loop for country: reached last allowed estimation or forecast date.")
      break
    }
    message(paste("  Iteration", iteration,
                  "- estimation window:", start_quarter, "to", end_quarter))
    
    # 1) Prepare data for this window 
    window_data <- prepare_country_ts(
      data          = Euro_Countries_GDP_Growth_Log,
      country_name  = country_name,
      start_quarter = start_quarter,
      end_quarter   = end_quarter
    )
    
    ts_data <- window_data$ts_data
    
    # 2) Select ARMA model 
    arma_fit <- fit_fixed_arma(ts_data,imposed_p_ARMA,imposed_q_ARMA,"AIC")   #<- we impose the ARMA specification 
    
    # 3) Forecast growth 
    fc_growth <- forecast_arma_model(
      model         = arma_fit$model,
      start_quarter = start_quarter,
      end_quarter   = end_quarter,
      horizons      = forecast_horizons
    )
    
    
    
    growth_table <- data.frame(
      Forecasted_Quarter = end_quarter + forecast_horizons * 0.25,
      Horizon          = forecast_horizons,
      Forecast_Growth_logdiff  = as.numeric(fc_growth$forecast_values)
    )
    
    # 5) Add metadata: country, estimation end, ARMA order, AIC
    growth_table$Country            <- country_name
    growth_table$End_Estimation_Set <- end_quarter
    growth_table$ARMA <- paste0("ARMA(",
                                arma_fit$order["p"], ",",
                                arma_fit$order["q"], ")")
    growth_table$ARMA_p <- NULL
    growth_table$ARMA_q <- NULL
    growth_table$AIC                <- arma_fit$aic
    
    # Store this iteration's table
    country_results[[iteration]] <- growth_table
    
    # Expand the window by one quarter
    end_quarter <- end_quarter + 0.25
    iteration   <- iteration + 1
  }
  
  # Bind all iterations for this country into one data.frame
  if (length(country_results) > 0) {
    all_country_results[[country_name]] <- do.call(rbind, country_results)
  } else {
    all_country_results[[country_name]] <- NULL
  }
}

# Optional: single big data.frame with *all* countries
all_growth_results_bench <- do.call(rbind, all_country_results)
rownames(all_growth_results_bench) <- NULL


#compute forecasted nominals
all_growth_results_bench <- add_forecast_nominal(all_growth_results_bench,Euro_Countries_GDP_Growth_Log, MultiHorizonSets = TRUE)


# ----------------------------------------------------------------------------
# CHAPTER 3 – EURO AREA NOMINAL GDP & GROWTH 
# ----------------------------------------------------------------------------

## 3.1 Add the Eurozone rows and Nominals 


eurozone_iterated_bench <- all_growth_results_bench %>%
  group_by(Forecasted_Quarter, Horizon, End_Estimation_Set) %>%
  summarise(
    Forecast_Nominal_GDP = sum(Forecast_Nominal_GDP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Country = "Eurozone",
    Forecast_Growth_logdiff = NA_real_,
    ARMA = NA
  )

# Bind Eurozone to the main dataset
all_growth_results_bench <- bind_rows(all_growth_results_bench, eurozone_iterated_bench)


## 3.2 Compute the Eurozone log-diff forecast


all_growth_results_bench <- compute_eurozone_logdiff(all_growth_results_bench,Euro_Countries_GDP_Growth_Log,MultiHorizonSets = TRUE)

#3.3 Spagetti charts
for (quarter_step in seq(from = initial_end_quarter, to = last_end_quarter, by = 0.25)) {
  
  
  horizon_to_display = 10 
  p<-plot_spaghetti_growth(Euro_Countries_GDP_Growth_Log,all_growth_results_bench,quarter_step,horizon_to_display,country = "Eurozone")
  print(p)
  
}


# ----------------------------------------------------------------------------
# CHAPTER 4 – CREATE THE ERRORS AND TESTING THE MODEL
# ----------------------------------------------------------------------------

# 4.1 get if needed the pure error and forecast matrix per horizon 
one_country_forecast_matrix_bench <-make_forecast_matrix(all_growth_results_bench, country ="Eurozone")

one_country_pure_error_matrix_bench <-make_pure_error_matrix(one_country_forecast_matrix_bench,Euro_Countries_GDP_Growth_Log, country ="Eurozone")




# 4.2 Create the results_table_bench 
horizons <- sort(unique(all_growth_results_bench$Horizon))

results_table_bench <- data.frame(row.names = horizons)


# 4.3 compute and create error_matrix_bench
error_matrix_bench <- make_error_matrix(all_growth_results_bench,Euro_Countries_GDP_Growth_Log,country = "Eurozone") 

results_table_bench <- add_msfe(results_table_bench,error_matrix_bench)  

# 4.4 compute the MZ and LB tests
mz_output_bench <- mz_test_country(all_growth_results_bench,Euro_Countries_GDP_Growth_Log,results_table_bench,country = "Eurozone")

MZ_table_bench <- mz_output_bench$MZ_table
results_table_bench <- mz_output_bench$results_table


lb_results_bench <- ljung_box_by_horizon(error_matrix_bench, K = 8,results_table_bench)

lb_table_bench <- lb_results_bench$lb_table
results_table_bench <- lb_results_bench$results_table
