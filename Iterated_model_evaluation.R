####libraries

library(forecast)
library(readxl)
library(lmtest)
library(ggplot2)
library(xtable)

url_data_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Euro_Countries_GDP_Growth_Log.xlsx"
download.file(url_data_countries, destfile = "data.xlsx", mode = "wb")
Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx")


evaluate_model <- function (Euro_Countries_GDP_Growth_Log,first_quarter,initial_end_quarter,
                            last_end_quarter,max_data_quarter,forecast_horizons,results_table_bench,error_matrix_bench,logdiff_to_train_on) {

  
  # ----------------------------------------------------------------------------
  # CHAPTER 1 – GLOBAL PARAMETERS & PREPARATION
  # ----------------------------------------------------------------------------
  
  ################ Define the parameters #############################################
  
  
  first_quarter <- first_quarter 
  initial_end_quarter <- initial_end_quarter #the end date of the training set
  
  last_end_quarter <- last_end_quarter   # This is the last in-sample date you want to reach with the expanding window
  max_data_quarter <- max_data_quarter  
  
  forecast_horizons <- forecast_horizons    # h = 1,...,10
  
  
  logdiff_to_train_on <- logdiff_to_train_on
  




  
  # ----------------------------------------------------------------------------
  # CHAPTER 2 – COUNTRY LOOP & AGGREGATED NOMINAL GDP FORECASTS
  # ----------------------------------------------------------------------------
  
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
        end_quarter   = end_quarter,
        gdp_growth_to_train_on = logdiff_to_train_on
      )
      
      ts_data <- window_data$ts_data
      
      # 2) Select ARMA model 
      arma_fit <- select_best_arma(ts_data)
      
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
  all_growth_results <- do.call(rbind, all_country_results)
  rownames(all_growth_results) <- NULL
  
  
  #compute forecasted nominals
  all_growth_results <- add_forecast_nominal(all_growth_results,Euro_Countries_GDP_Growth_Log, MultiHorizonSets = TRUE)
  
  # ----------------------------------------------------------------------------
  # CHAPTER 3 – EURO AREA NOMINAL GDP & GROWTH 
  # ----------------------------------------------------------------------------
  
  ## 3.1 Add the Eurozone rows and Nominals 
  
  
  eurozone_iterated <- all_growth_results %>%
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
  all_growth_results <- bind_rows(all_growth_results, eurozone_iterated)
  
  
  ## 3.2 Compute the Eurozone log-diff forecast
  
  
  all_growth_results <- compute_eurozone_logdiff(all_growth_results,Euro_Countries_GDP_Growth_Log,MultiHorizonSets = TRUE)
  
  #3.3 Spagetti charts
  for (quarter_step in seq(from = initial_end_quarter, to = last_end_quarter, by = 0.25)) {
    
    
    horizon_to_display = 10 
    p<-plot_spaghetti_growth(Euro_Countries_GDP_Growth_Log,all_growth_results,quarter_step,horizon_to_display,country = "Eurozone")
    print(p)
    
  }
  
  
  # ----------------------------------------------------------------------------
  # CHAPTER 4 – CREATE THE ERRORS AND TESTING THE MODEL
  # ----------------------------------------------------------------------------
  
  # 4.1 get if needed the pure error and forecast matrix per horizon 
  one_country_forecast_matrix <-make_forecast_matrix(all_growth_results, country ="Eurozone")
  
  one_country_pure_error_matrix <-make_pure_error_matrix(one_country_forecast_matrix,Euro_Countries_GDP_Growth_Log, country ="Eurozone")
  
  
  
  
  # 4.2 Create the results_table 
  horizons <- sort(unique(all_growth_results$Horizon))
  
  results_table <- data.frame(row.names = horizons)
  
  
  # 4.3 compute and create error_matrix
  error_matrix <- make_error_matrix(all_growth_results,Euro_Countries_GDP_Growth_Log,country = "Eurozone") 
  
  results_table <- add_msfe(results_table,error_matrix)  
  
  # 4.4 compute the MZ and LB tests
  mz_output <- mz_test_country(all_growth_results,Euro_Countries_GDP_Growth_Log,results_table,country = "Eurozone")
  
  MZ_table <- mz_output$MZ_table
  results_table <- mz_output$results_table
  
  
  lb_results <- ljung_box_by_horizon(error_matrix, K = 8,results_table)
  
  lb_table <- lb_results$lb_table
  results_table <- lb_results$results_table
  
  
  
  
  
  
  
  results_table <- mean_variance_forecasts(one_country_forecast_matrix,results_table)
  
  
  # ----------------------------------------------------------------------------
  # CHAPTER 5  – Comparison to the benchmark
  # ----------------------------------------------------------------------------
  
  
  
  
  results_table <- compute_ratio_MSFE(results_table,results_table_bench)
  
  
  results_table <- dm_test_msfe(
    results          = results_table,
    results_bench    = results_table_bench,
    error_mat        = error_matrix,
    error_mat_bench  = error_matrix_bench,
    two_sided        = TRUE
  )
  
  
  
  xtable(results_table)
  xtable(error_matrix)
  xtable(MZ_table)
  xtable(lb_table)

  
  return(list(
    results_table        = results_table,
    error_matrix         = error_matrix,
    MZ_table             = MZ_table,
    lb_table             = lb_table,
    all_growth_results   = all_growth_results
  ))
}

