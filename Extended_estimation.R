####libraries

library(forecast)
library(readxl)
library(lmtest)




# ----------------------------------------------------------------------------
# CHAPTER 1 – GLOBAL PARAMETERS & PREPARATION
# ----------------------------------------------------------------------------

  ################ Define the parameters #############################################

    
    first_quarter <- 2000.25   
    initial_end_quarter <- 2010.00 #the end date of the training set
  
    last_end_quarter <- 2022.75   # This is the last in-sample date you want to reach with the expanding window
    max_data_quarter <- 2025.25   
  
    forecast_horizons <- 1:10     # h = 1,...,10
  
    
    url_data_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/data_countries_euro_growth_gdp_seas.xlsx"
    download.file(url_data_countries, destfile = "data.xlsx", mode = "wb")
    data_countries_euro_growth_gdp_seas <- read_excel("data.xlsx")
    
    url_Eurozone_GDPgrowth_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_GDPgrowth_seas.xlsx"
    download.file(url_Eurozone_GDPgrowth_seas, destfile = "data.xlsx", mode = "wb")
    Eurozone_GDPgrowth_seas <- read_excel("data.xlsx")
    
  
  ################  function: prepare country-specific time series for a given window ################################
  
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
    
    
    
  

  
  
# ----------------------------------------------------------------------------
# CHAPTER 2 – ARMA MODEL SELECTION (AIC) AND ESTIMATION
# ----------------------------------------------------------------------------
  
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
  
  
# ----------------------------------------------------------------------------
# CHAPTER 3 – ARMA FORECASTS (CORRECTED DATES)
# ----------------------------------------------------------------------------
  
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
  
# ----------------------------------------------------------------------------
# CHAPTER 4 – FROM GROWTH FORECASTS TO NOMINAL GDP FORECASTS 
# ----------------------------------------------------------------------------
  
  forecast_nominal_gdp_from_growth <- function(data,
                                               country_name,
                                               start_quarter,
                                               end_quarter,
                                               horizons,
                                               forecast_growth,
                                               growth_in_percent = TRUE) {
    
    # Basic checks
    if (length(horizons) != length(forecast_growth)) {
      stop("horizons and forecast_growth must have the same length.")
    }
    
    # 1. Get the last observed nominal GDP at end_quarter
    idx_last <- data$Country == country_name & data$Quarter == end_quarter
    if (!any(idx_last)) {
      stop(paste("No Nominal_GDP found for", country_name,
                 "at quarter", end_quarter))
    }
    last_nominal_gdp <- data$Nominal_GDP[idx_last][1]
    
    # 2. Build date labels (year-quarter) and numeric quarter values
    #    using end_quarter + 0.25 * h
    n_h <- length(horizons)
    date_labels     <- character(n_h)
    quarter_numeric <- numeric(n_h)
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      q_val <- end_quarter + 0.25 * h
      year  <- floor(q_val)
      q_idx <- as.integer((q_val - year) * 4 + 1)  # 1..4
      
      date_labels[i]     <- paste0(year, "-Q", q_idx)
      quarter_numeric[i] <- q_val
    }
    
    # 3. Convert log-diff forecasts into multiplicative factors on levels
      
    
    if (growth_in_percent) {
      # forecast_growth is 100 * log-diff
      growth_factors <- exp(forecast_growth / 100)
    } else {
      # forecast_growth is raw log-diff
      growth_factors <- exp(forecast_growth)
    }
    
    # 4. Build the nominal GDP forecast path
    nominal_gdp_forecast <- numeric(length(horizons))
    current_gdp <- last_nominal_gdp
    
    for (i in seq_along(horizons)) {
      current_gdp <- current_gdp * growth_factors[i]
      nominal_gdp_forecast[i] <- current_gdp
    }
    
    
    # 5. Return a table with growth and nominal GDP forecasts
    results_table <- data.frame(
      Date                 = date_labels,
      Quarter_Value        = quarter_numeric,
      Horizon              = horizons,
      Growth_Forecast      = as.numeric(forecast_growth),
      Growth_Factor        = growth_factors,
      Nominal_GDP_Last_Obs = last_nominal_gdp,
      Nominal_GDP_Forecast = nominal_gdp_forecast
    )
    
    return(results_table)
  }
  
  
  
# ----------------------------------------------------------------------------
# CHAPTER 5 – COUNTRY LOOP & AGGREGATED NOMINAL GDP FORECASTS
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
  
        
        if (end_quarter > last_end_quarter || last_forecast_quarter > max_data_quarter) {       # Stopping rule (same logic as Chapter 2)
          message("Stopping loop for country: reached last allowed estimation or forecast date.")
          break
        }
        message(paste("  Iteration", iteration,
                      "- estimation window:", start_quarter, "to", end_quarter))
        
        # 1) Prepare data for this window (Chapter 1 / prepare_country_ts)
        window_data <- prepare_country_ts(
          data          = data_countries_euro_growth_gdp_seas,
          country_name  = country_name,
          start_quarter = start_quarter,
          end_quarter   = end_quarter
        )
        
        ts_data <- window_data$ts_data
        
        # 2) Select ARMA model (Chapter 3)
        arma_fit <- select_best_arma(ts_data)
        
        # 3) Forecast growth (Chapter 4)
        fc_growth <- forecast_arma_model(
          model         = arma_fit$model,
          start_quarter = start_quarter,
          end_quarter   = end_quarter,
          horizons      = forecast_horizons
        )
        
        # 4) Convert growth forecasts into nominal GDP forecasts (Chapter 5)
        gdp_table <- forecast_nominal_gdp_from_growth(
          data            = data_countries_euro_growth_gdp_seas,
          country_name    = country_name,
          start_quarter   = start_quarter,
          end_quarter     = end_quarter,
          horizons        = forecast_horizons,
          forecast_growth = fc_growth$forecast_values
        )
        
        # 5) Add metadata: country, estimation end, ARMA order, AIC
        gdp_table$Country            <- country_name
        gdp_table$Estimation_End_Qtr <- end_quarter
        gdp_table$ARMA_p             <- arma_fit$order["p"]
        gdp_table$ARMA_q             <- arma_fit$order["q"]
        gdp_table$AIC                <- arma_fit$aic
        
        # Store this iteration's table
        country_results[[iteration]] <- gdp_table
        
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
  
    
# ----------------------------------------------------------------------------
# CHAPTER 6 – EURO AREA NOMINAL GDP & GROWTH 
# ----------------------------------------------------------------------------
    
  ## 6.1 Aggregate country forecasts to euro area nominal GDP
    
    all_results_df <- do.call(rbind, all_country_results)
    
    Agregated_euro_all_results <- aggregate(
      Nominal_GDP_Forecast ~ Date + Quarter_Value + Horizon + Estimation_End_Qtr,
      data = all_results_df,
      FUN  = sum
    )
    
    names(Agregated_euro_all_results)[
      names(Agregated_euro_all_results) == "Nominal_GDP_Forecast"
    ] <- "EA_Nominal_GDP_Forecast"
    
    
  ## 6.2 Observed nominal GDP (euro area) and log
    
    ea_obs <- Eurozone_GDPgrowth_seas
    ea_obs$Quarter_Value <- ea_obs$Quarter
    ea_obs$log_NGDP      <- log(ea_obs$Eurozone_Nominal_GDP)
    
    # Observed log-diff growth (you can skip this if you only need levels)
    ea_obs <- ea_obs[order(ea_obs$Quarter_Value), ]
    ea_obs$Observed_Growth <- c(NA, diff(ea_obs$log_NGDP))
    
   ##### Build forecast_eval (used in Chapter 7)
    forecast_eval <- merge(
      Agregated_euro_all_results[, c("Quarter_Value",
                                     "Estimation_End_Qtr",
                                     "Horizon",
                                     "Euro_Nominal_Growth")],
      ea_obs[, c("Quarter_Value", "Observed_Growth")],
      by = "Quarter_Value",
      all.x = TRUE
    )
    
    # Keep a clean ordering (not strictly necessary but nice)
    forecast_eval <- forecast_eval[order(forecast_eval$Quarter_Value,
                                         forecast_eval$Estimation_End_Qtr,
                                         forecast_eval$Horizon), ]
    
  ## 6.3 Forecasted growth: log(NGDP_forecast at horizon h) - log(NGDP at origin)
    
    # Merge observed log GDP at the ORIGIN (Estimation_End_Qtr)
    origin_log <- ea_obs[, c("Quarter_Value", "log_NGDP")]
    names(origin_log) <- c("Estimation_End_Qtr", "log_NGDP_origin")
    
    Agregated_euro_all_results <- merge(
      Agregated_euro_all_results,
      origin_log,
      by = "Estimation_End_Qtr",
      all.x = TRUE
    )
    
    # Log of forecasted level
    Agregated_euro_all_results$log_NGDP_fc <-
      log(Agregated_euro_all_results$EA_Nominal_GDP_Forecast)
    
    # Forecasted growth from origin to horizon h
    Agregated_euro_all_results$Euro_Nominal_Growth <-
      Agregated_euro_all_results$log_NGDP_fc -
      Agregated_euro_all_results$log_NGDP_origin
    
    
    ## 6.4 Final table: rows = horizons (Observed, 1..10), columns = periods
    
    tmp <- merge(
      Agregated_euro_all_results[, c("Quarter_Value", "Horizon", "Euro_Nominal_Growth")],
      ea_obs[, c("Quarter_Value", "Observed_Growth")],
      by   = "Quarter_Value",
      all.x = TRUE
    )
    
    # Observed row (one value per period)
    obs_long <- unique(tmp[, c("Quarter_Value", "Observed_Growth")])
    obs_long$Horizon <- "Observed"
    names(obs_long)[2] <- "Value"
    
    # Forecast rows
    fc_long <- tmp[, c("Quarter_Value", "Horizon", "Euro_Nominal_Growth")]
    names(fc_long)[3] <- "Value"
    
    full_long <- rbind(obs_long, fc_long)
    
    final_forecast_table <- reshape(
      full_long,
      idvar   = "Horizon",
      timevar = "Quarter_Value",
      direction = "wide"
    )
    
    # Clean column names
    colnames(final_forecast_table) <- gsub("Value\\.", "", colnames(final_forecast_table))
    
    # Order rows: Observed, 1..H
    max_h <- max(Agregated_euro_all_results$Horizon, na.rm = TRUE)
    final_forecast_table$Horizon <- factor(
      final_forecast_table$Horizon,
      levels = c("Observed", as.character(1:max_h))
    )
    
    final_forecast_table <- final_forecast_table[order(final_forecast_table$Horizon), ]
    rownames(final_forecast_table) <- as.character(final_forecast_table$Horizon)
    
# ----------------------------------------------------------------------------
# CHAPTER 7 – FORECAST ERROR MATRIX FOR EUROZONE GROWTH
# ----------------------------------------------------------------------------
    
  ##### 1) Compute forecast errors #############################################
    
    
    
    forecast_eval$Forecast_Error <- forecast_eval$Euro_Nominal_Growth -
      forecast_eval$Observed_Growth
    
    
  ##### 2) Create a loop (boucle) index for each estimation window #############
    
    origins <- sort(unique(forecast_eval$Estimation_End_Qtr))
    origin_map <- data.frame(
      Estimation_End_Qtr = origins,
      Loop = seq_along(origins)   # 1, 2, 3, ...
    )
    
    forecast_eval <- merge(
      forecast_eval,
      origin_map,
      by = "Estimation_End_Qtr"
    )
    
    
  ##### 3) Build a wide “forecast error matrix” ################################
    
    # We want:
    # - rows: forecasted period (Quarter_Value)
    # - columns: loop number (Loop)
    # - entries: Forecast_Error
    
    error_matrix_long <- forecast_eval[, c("Quarter_Value", "Loop", "Forecast_Error")]
    
    error_matrix_wide <- reshape(
      error_matrix_long,
      idvar   = "Quarter_Value",
      timevar = "Loop",
      direction = "wide"
    )
    
    # Sort rows by time
    error_matrix_wide <- error_matrix_wide[order(error_matrix_wide$Quarter_Value), ]
    
    # Rename columns to Error_L1, Error_L2, ...
    colnames(error_matrix_wide) <-
      gsub("Forecast_Error\\.", "Error_L", colnames(error_matrix_wide))
    
    
  ##### 4) Add the observed growth as the leftmost column ######################
    
    # Take observed growth directly from forecast_eval (no need to re-merge from the raw data)
    ea_obs <- unique(forecast_eval[, c("Quarter_Value", "Observed_Growth")])
    
    final_error_table <- merge(
      ea_obs,
      error_matrix_wide,
      by = "Quarter_Value",
      all.y = TRUE   # keep only quarters that appear in forecasts
    )
    
    final_error_table <- final_error_table[order(final_error_table$Quarter_Value), ]
    
# ----------------------------------------------------------------------------
# CHAPTER 8 – POINT FORECAST ABSOLUTE EVALUATION
# ----------------------------------------------------------------------------
    
    
  # 1) Compute MSFE by horizon -----------------------------------------------
    
    msfe_results <- aggregate(
      Forecast_Error ~ Horizon,
      data = forecast_eval,
      FUN = function(x) mean(x^2, na.rm = TRUE)
    )
    
    colnames(msfe_results)[2] <- "MSFE"
    
    message("MSFE by horizon:")
    print(msfe_results)
    
    
    
  # 2) Mincer–Zarnowitz regression --------------------------------------------
    
    # y_t = Observed growth
    # f_t = Forecast
    
    mz_data <- forecast_eval[!is.na(forecast_eval$Observed_Growth), ]
    
    mz_data$Forecast <- mz_data$Euro_Nominal_Growth
    mz_data$Actual   <- mz_data$Observed_Growth
    
    mincer_model <- lm(Actual ~ Forecast, data = mz_data)
    
    message("Mincer–Zarnowitz regression results:")
    print(summary(mincer_model))
    
    
    
  # 3) Ljung–Box test on forecast errors --------------------------------------
    
    # Use h=1 errors for autocorrelation test
    errors_h1 <- forecast_eval$Forecast_Error[forecast_eval$Horizon == 1]
    
    lb_test <- Box.test(errors_h1, lag = 8, type = "Ljung-Box")
    
    message("Ljung–Box test on forecast errors (h=1):")
    print(lb_test)
    
    
    
  # 4) Efficiency regression --------------------------------------------------
    
    # e_t = alpha + beta * z_t + u_t
    # Let z_t = lagged observed growth
    
    eff_data <- forecast_eval
    eff_data <- eff_data[order(eff_data$Quarter_Value), ]
    
    # Create lag of observed growth
    eff_data$Lag_Observed <- c(NA, head(eff_data$Observed_Growth, -1))
    
    # Keep h=1 forecasts (standard in efficiency tests)
    eff_h1 <- eff_data[eff_data$Horizon == 1, ]
    eff_h1 <- eff_h1[!is.na(eff_h1$Lag_Observed), ]
    
    eff_model <- lm(Forecast_Error ~ Lag_Observed, data = eff_h1)
    
    message("Efficiency regression results (errors ~ lagged observed growth):")
    print(summary(eff_model))
    
    
    
    
    
