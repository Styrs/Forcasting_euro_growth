library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(writexl)
library(dplyr)
library(ggplot2)
library(rlang)



################################################################################
# Function to test and treat the data
################################################################################
  

  
  ################### Function to deseasonalize the Nominal GDP ##################
  
  deseasonalize_series <- function(nominal_gdp, quarters) {

    tq           <- min(quarters, na.rm = TRUE)
    start_year   <- floor(tq)
    start_quarter <- as.integer(round((tq - start_year) * 4 + 1))
    
    ts_data <- ts(
      nominal_gdp,
      start     = c(start_year, start_quarter),
      frequency = 4
    )
    
    # Apply X-13-ARIMA-SEATS
    tryCatch({
      seas_result    <- seas(ts_data)
      deseasonalized <- as.numeric(final(seas_result))
      return(deseasonalized)
    }, error = function(e) {
      warning("X-13 failed, returning original data")
      return(nominal_gdp)
    })
  }
  
  
  ######################## Function to winsorize #################################
  
  winsorize <- function(x, probs = c(0.01, 0.99)) {
    q <- quantile(x, probs, na.rm = TRUE)
    x <- pmin(pmax(x, q[1]), q[2])
    return(x)
  }
  
  
  
  
  ######################## Defining the function of ADF test######################
  
  run_adf_test <- function(data,
                           value_col,
                           group_col = NULL,
                           time_col  = NULL,
                           min_n     = 10) {
    
    adf_on_vector <- function(x, min_n) {
      # remove NA
      x <- na.omit(x)
      n <- length(x)
      
      # not enough observations
      if (n < min_n) {
        return(tibble(
          n        = n,
          adf_stat = NA_real_,
          p_value  = NA_real_,
          note     = "Too few non-NA observations"
        ))
      }
      
      # lag length (ensure < n)
      k <- min(trunc(n^(1/3)), n - 1)
      
      # safe ADF
      res <- tryCatch(
        tseries::adf.test(x, k = k),
        error = function(e) e
      )
      
      if (inherits(res, "error")) {
        tibble(
          n        = n,
          adf_stat = NA_real_,
          p_value  = NA_real_,
          note     = paste("Error in adf.test:", res$message)
        )
      } else {
        tibble(
          n        = n,
          adf_stat = unname(res$statistic),
          p_value  = res$p.value,
          note     = NA_character_
        )
      }
    }
    
    if (!is.null(group_col)) {
      # ---- grouped version (e.g. by Country) ----
      results <- data %>%
        group_by(.data[[group_col]]) %>%
        {
          if (!is.null(time_col)) arrange(., .data[[time_col]], .by_group = TRUE) else .
        } %>%
        reframe(
          adf_on_vector(.data[[value_col]], min_n)
        ) %>%
        ungroup()
    } else {
      # ---- ungrouped version (single series) ----
      results <- adf_on_vector(data[[value_col]], min_n)
    }
    
    # Pretty print
    cat("=========================================\n")
    cat(" Augmented Dickey-Fuller (ADF) Test\n")
    cat("=========================================\n")
    print(results)
    cat("\nInterpretation:\n")
    cat(" - Null hypothesis: the series has a unit root (non-stationary)\n")
    cat(" - Small p-value (< 0.05) → reject H0 → series is stationary\n")
    cat(" - 'note' column explains NA results (too few obs / test error)\n")
    cat("=========================================\n\n")
    
    invisible(results)
  }
  ######################## Adding the real gdp values #################
  
  add_real_gdp <- function(annual_growth_observed,
                           add_quantiles = FALSE) {
    
    # ---- 1. Long format ----
    annual_long <- annual_growth_observed %>%
      tidyr::pivot_longer(
        cols      = -c(Country, type),
        names_to  = "year",
        values_to = "value"
      )
    
    # ---- 2. Mean real GDP: nominal_gdp * 100 / deflator ----
    real_gdp_df <- annual_long %>%
      dplyr::filter(type %in% c("nominal_gdp", "deflator")) %>%
      tidyr::pivot_wider(
        id_cols     = c(Country, year),
        names_from  = type,
        values_from = value
      ) %>%
      dplyr::mutate(
        real_gdp = dplyr::if_else(
          is.na(nominal_gdp) | is.na(deflator),
          NA_real_,
          nominal_gdp * 100 / deflator
        )
      )
    
    real_gdp_rows <- real_gdp_df %>%
      dplyr::select(Country, year, real_gdp) %>%
      dplyr::mutate(type = "real_gdp") %>%
      tidyr::pivot_wider(
        id_cols     = c(Country, type),
        names_from  = year,
        values_from = real_gdp
      )
    
    # ---- 3. Optional: quantile real GDP rows ----
    quantile_rows <- NULL
    if (isTRUE(add_quantiles)) {
      
      # q10
      real_gdp_q10_df <- annual_long %>%
        dplyr::filter(type %in% c("nominal_gdp_q10", "deflator_q10")) %>%
        tidyr::pivot_wider(
          id_cols     = c(Country, year),
          names_from  = type,
          values_from = value
        ) %>%
        dplyr::mutate(
          real_gdp_q10 = dplyr::if_else(
            is.na(nominal_gdp_q10) | is.na(deflator_q10),
            NA_real_,
            nominal_gdp_q10 * 100 / deflator_q10
          )
        )
      
      real_gdp_q10_rows <- real_gdp_q10_df %>%
        dplyr::select(Country, year, real_gdp_q10) %>%
        dplyr::mutate(type = "real_gdp_q10") %>%
        tidyr::pivot_wider(
          id_cols     = c(Country, type),
          names_from  = year,
          values_from = real_gdp_q10
        )
      
      # q90
      real_gdp_q90_df <- annual_long %>%
        dplyr::filter(type %in% c("nominal_gdp_q90", "deflator_q90")) %>%
        tidyr::pivot_wider(
          id_cols     = c(Country, year),
          names_from  = type,
          values_from = value
        ) %>%
        dplyr::mutate(
          real_gdp_q90 = dplyr::if_else(
            is.na(nominal_gdp_q90) | is.na(deflator_q90),
            NA_real_,
            nominal_gdp_q90 * 100 / deflator_q90
          )
        )
      
      real_gdp_q90_rows <- real_gdp_q90_df %>%
        dplyr::select(Country, year, real_gdp_q90) %>%
        dplyr::mutate(type = "real_gdp_q90") %>%
        tidyr::pivot_wider(
          id_cols     = c(Country, type),
          names_from  = year,
          values_from = real_gdp_q90
        )
      
      quantile_rows <- dplyr::bind_rows(real_gdp_q10_rows, real_gdp_q90_rows)
    }
    
    # ---- 4. Bind original data + real GDP rows (+ optional quantiles) ----
    out <- annual_growth_observed %>%
      dplyr::bind_rows(real_gdp_rows) %>%
      dplyr::bind_rows(quantile_rows) %>%
      dplyr::arrange(Country, type)
    
    return(out)
  }
  
  
  


  ######################## Adding the real gdp growth ##########################
  
  compute_real_gdp_growth <- function(df) {
    # ---- 1. detect year columns ----
    year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
    
    # ---- 2. make sure year columns are numeric ----
    df[year_cols] <- lapply(df[year_cols], function(x) {
      as.numeric(as.character(x))
    })
    
    # ---- 3. drop old real_growth rows if any ----
    df <- df[df$type != "real_growth", ]
    
    # ---- 4. keep only real_gdp rows ----
    real_gdp <- df[df$type == "real_gdp", ]
    real_growth <- real_gdp
    
    # ---- 5. compute YoY % growth ----
    for (j in seq_along(year_cols)) {
      col <- year_cols[j]
      if (j == 1) {
        real_growth[[col]] <- NA_real_
      } else {
        prev_col <- year_cols[j - 1]
        real_growth[[col]] <- (real_gdp[[col]] / real_gdp[[prev_col]] - 1) * 100
      }
    }
    
    # set type
    real_growth$type <- "real_growth"
    
    # ---- 6. bind and arrange ----
    df_out <- rbind(df, real_growth)
    
    # order by country (and type if useful)
    df_out <- df_out[order(df_out$Country, df_out$type), ]
    
    rownames(df_out) <- NULL
    return(df_out)
  }
  ######################## Adding the nominal growth ###########################
  
  
  compute_nominal_gdp_growth<- function(df) {

    # 1) detect year columns (2000, 2001, …)
    year_cols <- grep("^[0-9]{4}$", names(df), value = TRUE)
    
    # 2) make sure these columns are numeric
    df[year_cols] <- lapply(df[year_cols], function(x) as.numeric(as.character(x)))
    
    # 3) keep only nominal_gdp rows and make a copy for growth
    nominal      <- df %>% filter(type == "nominal_gdp")
    nominal_grow <- nominal
    
    # 4) compute YoY nominal growth for each year
    for (j in seq_along(year_cols)) {
      col <- year_cols[j]
      if (j == 1) {
        # first year has no previous year ⇒ NA
        nominal_grow[[col]] <- NA_real_
      } else {
        prev_col <- year_cols[j - 1]
        nominal_grow[[col]] <- (nominal[[col]] / nominal[[prev_col]] - 1) * 100
      }
    }
    
    # 5) label these rows as nominal_growth
    nominal_grow$type <- "nominal_growth"
    
    # 6) bind back to original table and order nicely
    df_out <- bind_rows(df, nominal_grow)
    
    # optional: control order of 'type' column
    df_out$type <- factor(
      df_out$type,
      levels = c("deflator", "nominal_gdp", "nominal_growth", "real_gdp", "real_growth")
    )
    
    df_out <- df_out %>% arrange(Country, type)
    rownames(df_out) <- NULL
    
    return(df_out)
  }
  
  
  
  
  
  
  
  
  
  
  
  
################################################################################
# Function to run the models
################################################################################
  
    
  
  
  ######################## Prepare the data for the model ######################
  
  prepare_country_ts <- function(data, country_name, start_quarter, end_quarter, gdp_growth_to_train_on) {
    
    country_data <- data[data$Country == country_name, ]
    
    # Keep only the estimation window
    train_data <- country_data[country_data$Quarter >= start_quarter &
                                 country_data$Quarter <= end_quarter, ]
    
    # Build quarterly time series object
    # start = c(year, quarter_index) with quarter_index in {1,2,3,4}
    start_year    <- floor(start_quarter)
    start_q_index <- as.integer((start_quarter - start_year) * 4 + 1)
    
    ts_data <- ts(
      train_data[[gdp_growth_to_train_on]],  # <- dynamic column selection
      start     = c(start_year, start_q_index),
      frequency = 4
    )
    
    return(list(
      train_data = train_data,
      ts_data    = ts_data
    ))
  }
  
  ######################## Select the best arma model for the given data #########
  
  
  
  select_best_arma <- function(ts_data,
                               max_p    = 5,
                               max_q    = 5,
                               criterion = c("AIC", "BIC")) {
    criterion <- match.arg(criterion)
    
    best_score <- Inf
    best_model <- NULL
    best_p     <- NA_integer_
    best_q     <- NA_integer_
    
    for (p in 0:max_p) {
      for (q in 0:max_q) {
        
        # Skip ARMA(0,0)
        if (p == 0 && q == 0) next
        
        tryCatch({
          # ARMA(p,q) with d = 0 via base R
          candidate_model <- stats::arima(ts_data, order = c(p, 0, q))
          
          candidate_score <- if (criterion == "AIC") {
            stats::AIC(candidate_model)
          } else {
            stats::BIC(candidate_model)
          }
          
          if (candidate_score < best_score) {
            best_score <- candidate_score
            best_model <- candidate_model
            best_p     <- p
            best_q     <- q
          }
        }, error = function(e) {
          # silently skip models that fail to converge
          NULL
        })
      }
    }
    
    if (is.null(best_model)) {
      stop("No ARMA model could be estimated for this time series.")
    }
    
    # Mimic the structure you use elsewhere: $model, $order, $aic, etc.
    out <- list(
      model     = best_model,
      order     = c(p = best_p, q = best_q),
      criterion = criterion,
      score     = best_score,
      aic       = stats::AIC(best_model),
      bic       = stats::BIC(best_model)
    )
    
    return(out)
  }
  
  ######################## Choose manual Arma specification ####################
  
  fit_fixed_arma <- function(ts_data, p, q,
                             criterion = c("AIC", "BIC")) {
    
    criterion <- match.arg(criterion)
    
    # Estimate the forced ARMA(p, q) model with base R
    model <- stats::arima(ts_data, order = c(p, 0, q))
    
    # Compute the requested information criterion
    score <- if (criterion == "AIC") {
      stats::AIC(model)
    } else {
      stats::BIC(model)
    }
    
    # Return in the same format as select_best_arma()
    return(list(
      model     = model,
      order     = c(p = p, q = q),
      criterion = criterion,
      score     = score,
      aic       = stats::AIC(model),
      bic       = stats::BIC(model)
    ))
  }
  
  
  
  ######################## Compute forecast growth ###############################
  
  
  forecast_arma_model <- function(model, start_quarter, end_quarter, horizons) {
    
    # Ensure horizons is an integer vector (e.g. 1:10)
    horizons <- sort(unique(as.integer(horizons)))
    h_max    <- max(horizons)
    
    # 1) Forecast growth with the ARMA model using base R
    # stats::predict() for "arima" objects
    fc <- stats::predict(model, n.ahead = h_max)
    
    # fc$pred = point forecasts, fc$se = standard errors
    fc_values <- fc$pred[horizons]
    fc_standard_error <- fc$se[horizons]
    
    # 2) Compute numeric quarter values (same format as your input data)
    # Example: 2025.25, 2025.50, 2025.75, ...
    forecast_quarters <- end_quarter + 0.25 * horizons
    
    # 3) Return a structured list
    return(list(
      forecast_values = as.numeric(fc_values),
      forecast_SD = as.numeric(fc_standard_error),
      forecast_dates  = forecast_quarters,  # numeric, not "YYYY-Qx"
      forecast_object = fc                  # now the list from stats::predict()
    ))
  }
  
  
  ######################## Transform log-diff forecast into nominal forecast #####
  
  
  
  add_forecast_nominal <- function(forecast_data,
                                   observed_data,
                                   MultiHorizonSets = TRUE,
                                   add_quantiles = FALSE,
                                   q_cols = c("Forecast_Growth_logdiff_q10",
                                              "Forecast_Growth_logdiff_q90")) {
    
    # Helper to compute nominal level from a given logdiff column
    add_one_nominal_path <- function(df, start_col, group_cols, logdiff_col, out_col) {
      df %>%
        group_by(across(all_of(group_cols))) %>%
        arrange(Horizon, .by_group = TRUE) %>%
        mutate(
          growth_factor_tmp = exp(.data[[logdiff_col]] / 100),
          !!out_col := .data[[start_col]] * cumprod(growth_factor_tmp)
        ) %>%
        ungroup() %>%
        select(-growth_factor_tmp)
    }
    
    if (MultiHorizonSets) {
      # ----- Case 1: End_Estimation_Set exists -----
      start_levels <- observed_data %>%
        transmute(
          Country,
          End_Estimation_Set = Quarter,
          start_level = Nominal_GDP_seas
        )
      
      modified_data <- forecast_data %>%
        left_join(start_levels, by = c("Country", "End_Estimation_Set"))
      
      # Mean path (existing behavior)
      modified_data <- add_one_nominal_path(
        df         = modified_data,
        start_col  = "start_level",
        group_cols = c("Country", "End_Estimation_Set"),
        logdiff_col= "Forecast_Growth_logdiff",
        out_col    = "Forecast_Nominal_GDP"
      )
      
      # Optional quantile paths
      if (isTRUE(add_quantiles)) {
        # only compute those that exist
        if (q_cols[1] %in% names(modified_data)) {
          modified_data <- add_one_nominal_path(
            df          = modified_data,
            start_col   = "start_level",
            group_cols  = c("Country", "End_Estimation_Set"),
            logdiff_col = q_cols[1],
            out_col     = "Forecast_Nominal_GDP_q10"
          )
        }
        if (q_cols[2] %in% names(modified_data)) {
          modified_data <- add_one_nominal_path(
            df          = modified_data,
            start_col   = "start_level",
            group_cols  = c("Country", "End_Estimation_Set"),
            logdiff_col = q_cols[2],
            out_col     = "Forecast_Nominal_GDP_q90"
          )
        }
      }
      
      modified_data <- modified_data %>%
        select(-start_level)
      
    } else {
      # ----- Case 2: no End_Estimation_Set -----
      start_levels <- observed_data %>%
        transmute(
          Country,
          last_observed_quarter = Quarter,
          start_level = Nominal_GDP_seas
        )
      
      modified_data <- forecast_data %>%
        mutate(last_observed_quarter = Forecasted_Quarter - Horizon * 0.25) %>%
        left_join(start_levels, by = c("Country", "last_observed_quarter"))
      
      # Mean path (existing behavior)
      modified_data <- add_one_nominal_path(
        df         = modified_data,
        start_col  = "start_level",
        group_cols = c("Country", "last_observed_quarter"),
        logdiff_col= "Forecast_Growth_logdiff",
        out_col    = "Forecast_Nominal_GDP"
      )
      
      # Optional quantile paths
      if (isTRUE(add_quantiles)) {
        if (q_cols[1] %in% names(modified_data)) {
          modified_data <- add_one_nominal_path(
            df          = modified_data,
            start_col   = "start_level",
            group_cols  = c("Country", "last_observed_quarter"),
            logdiff_col = q_cols[1],
            out_col     = "Forecast_Nominal_GDP_q10"
          )
        }
        if (q_cols[2] %in% names(modified_data)) {
          modified_data <- add_one_nominal_path(
            df          = modified_data,
            start_col   = "start_level",
            group_cols  = c("Country", "last_observed_quarter"),
            logdiff_col = q_cols[2],
            out_col     = "Forecast_Nominal_GDP_q90"
          )
        }
      }
      
      modified_data <- modified_data %>%
        select(-start_level)
    }
    
    return(modified_data)
  }
  
  
  ######################## Compute eurozone log-diff #############################
  
  
  compute_eurozone_logdiff <- function(forecast_df,
                                       observed_df,
                                       MultiHorizonSets = TRUE,
                                       euro_name = "Eurozone",
                                       add_quantiles = FALSE,
                                       q_nominal_cols = c("Forecast_Nominal_GDP_q10",
                                                          "Forecast_Nominal_GDP_q90")) {
    
    # Observed data for Eurozone only
    obs_euro <- observed_df %>%
      dplyr::filter(.data$Country == euro_name) %>%
      dplyr::select(.data$Country, .data$Quarter, .data$Nominal_GDP_seas)
    
    has_q10_nom <- q_nominal_cols[1] %in% names(forecast_df)
    has_q90_nom <- q_nominal_cols[2] %in% names(forecast_df)
    
    if (isTRUE(MultiHorizonSets)) {
      
      euro_forecast <- forecast_df %>%
        dplyr::filter(.data$Country == euro_name) %>%
        dplyr::left_join(
          obs_euro,
          by = c("Country", "End_Estimation_Set" = "Quarter")
        ) %>%
        dplyr::arrange(.data$End_Estimation_Set, .data$Horizon) %>%
        dplyr::group_by(.data$Country, .data$End_Estimation_Set) %>%
        dplyr::mutate(
          base_nominal = dplyr::case_when(
            .data$Horizon == 1L ~ .data$Nominal_GDP_seas,
            TRUE                ~ dplyr::lag(.data$Forecast_Nominal_GDP)
          ),
          Forecast_Growth_logdiff =
            (log(.data$Forecast_Nominal_GDP) - log(.data$base_nominal)) * 100
        )
      
      if (isTRUE(add_quantiles) && isTRUE(has_q10_nom)) {
        euro_forecast <- euro_forecast %>%
          dplyr::mutate(
            base_nominal_q10 = dplyr::case_when(
              .data$Horizon == 1L ~ .data$Nominal_GDP_seas,
              TRUE ~ dplyr::lag(.data[[q_nominal_cols[1]]])
            ),
            Forecast_Growth_logdiff_q10 =
              (log(.data[[q_nominal_cols[1]]]) - log(.data$base_nominal_q10)) * 100
          )
      }
      
      if (isTRUE(add_quantiles) && isTRUE(has_q90_nom)) {
        euro_forecast <- euro_forecast %>%
          dplyr::mutate(
            base_nominal_q90 = dplyr::case_when(
              .data$Horizon == 1L ~ .data$Nominal_GDP_seas,
              TRUE ~ dplyr::lag(.data[[q_nominal_cols[2]]])
            ),
            Forecast_Growth_logdiff_q90 =
              (log(.data[[q_nominal_cols[2]]]) - log(.data$base_nominal_q90)) * 100
          )
      }
      
      euro_forecast <- euro_forecast %>%
        dplyr::ungroup() %>%
        dplyr::select(
          -dplyr::any_of(c("Nominal_GDP_seas", "base_nominal",
                           "base_nominal_q10", "base_nominal_q90"))
        )
      
    } else {
      
      euro_forecast <- forecast_df %>%
        dplyr::filter(.data$Country == euro_name) %>%
        dplyr::left_join(
          obs_euro,
          by = c("Country", "last_observed_quarter" = "Quarter")
        ) %>%
        dplyr::arrange(.data$last_observed_quarter, .data$Horizon) %>%
        dplyr::group_by(.data$Country, .data$last_observed_quarter) %>%
        dplyr::mutate(
          base_nominal = dplyr::case_when(
            .data$Horizon == 1L ~ .data$Nominal_GDP_seas,
            TRUE                ~ dplyr::lag(.data$Forecast_Nominal_GDP)
          ),
          Forecast_Growth_logdiff =
            (log(.data$Forecast_Nominal_GDP) - log(.data$base_nominal)) * 100
        )
      
      if (isTRUE(add_quantiles) && isTRUE(has_q10_nom)) {
        euro_forecast <- euro_forecast %>%
          dplyr::mutate(
            base_nominal_q10 = dplyr::case_when(
              .data$Horizon == 1L ~ .data$Nominal_GDP_seas,
              TRUE ~ dplyr::lag(.data[[q_nominal_cols[1]]])
            ),
            Forecast_Growth_logdiff_q10 =
              (log(.data[[q_nominal_cols[1]]]) - log(.data$base_nominal_q10)) * 100
          )
      }
      
      if (isTRUE(add_quantiles) && isTRUE(has_q90_nom)) {
        euro_forecast <- euro_forecast %>%
          dplyr::mutate(
            base_nominal_q90 = dplyr::case_when(
              .data$Horizon == 1L ~ .data$Nominal_GDP_seas,
              TRUE ~ dplyr::lag(.data[[q_nominal_cols[2]]])
            ),
            Forecast_Growth_logdiff_q90 =
              (log(.data[[q_nominal_cols[2]]]) - log(.data$base_nominal_q90)) * 100
          )
      }
      
      euro_forecast <- euro_forecast %>%
        dplyr::ungroup() %>%
        dplyr::select(
          -dplyr::any_of(c("Nominal_GDP_seas", "base_nominal",
                           "base_nominal_q10", "base_nominal_q90"))
        )
    }
    
    # Put Eurozone back with the other countries and keep original column order
    forecast_updated <- forecast_df %>%
      dplyr::filter(.data$Country != euro_name) %>%
      dplyr::bind_rows(euro_forecast) %>%
      dplyr::select(dplyr::all_of(names(forecast_df)))
    
    return(forecast_updated)
  }
  
  
  
  
  

  ######################## compute_annual_nominal GDP #################
  
  compute_annual_nominal <- function(forecast_table_growth_countries,
                                     Euro_Countries_GDP_Growth_Log) {
    
    # Helper to build annual nominal table from one quarterly nominal column
    build_one_annual <- function(fc_col, out_type) {
      
      # 1) Forecast quarters: Country–Quarter–Year–Q_index–Nominal_Forecast
      forecast_q <- forecast_table_growth_countries %>%
        transmute(
          Country,
          Quarter = Forecasted_Quarter,
          year    = floor(Forecasted_Quarter),
          quarter = as.integer(round((Forecasted_Quarter - year) * 4)) + 1L,
          nominal_fc = .data[[fc_col]]
        )
      
      # Years that have at least one forecast (per country)
      forecast_years <- forecast_q %>%
        distinct(Country, year)
      
      # 2) Observed quarters for the same country–years
      observed_q <- Euro_Countries_GDP_Growth_Log %>%
        transmute(
          Country,
          Quarter,
          year    = floor(Quarter),
          quarter = as.integer(round((Quarter - year) * 4)) + 1L,
          nominal_obs = Nominal_GDP_seas
        ) %>%
        semi_join(forecast_years, by = c("Country", "year"))
      
      # 3) Combine forecast + observed and choose nominal per quarter
      all_quarters <- observed_q %>%
        full_join(
          forecast_q,
          by = c("Country", "Quarter", "year", "quarter")
        ) %>%
        mutate(
          chosen_nominal = dplyr::case_when(
            !is.na(nominal_fc) ~ nominal_fc,  # use forecast if available
            TRUE               ~ nominal_obs  # otherwise fallback to observed
          )
        )
      
      # 4) Compute annual nominal GDP (only if 4 quarters available)
      annual_nominal <- all_quarters %>%
        group_by(Country, year) %>%
        summarise(
          n_quarters     = sum(!is.na(chosen_nominal)),
          annual_nominal = ifelse(
            n_quarters < 4,
            NA_real_,
            sum(chosen_nominal, na.rm = TRUE)
          ),
          .groups = "drop"
        )
      
      # 5) Pivot wide + add type
      annual_nominal %>%
        select(-n_quarters) %>%
        mutate(year = as.character(year)) %>%
        pivot_wider(
          id_cols     = Country,
          names_from  = year,
          values_from = annual_nominal
        ) %>%
        mutate(type = out_type) %>%
        relocate(type, .after = Country) %>%
        arrange(Country)
    }
    
    # Mean + quantiles (assumed present)
    annual_mean <- build_one_annual("Forecast_Nominal_GDP", "nominal_gdp")
    annual_q10  <- build_one_annual("Forecast_Nominal_GDP_q10", "nominal_gdp_q10")
    annual_q90  <- build_one_annual("Forecast_Nominal_GDP_q90", "nominal_gdp_q90")
    
    # Stack them (same format as your annual tables)
    annual_nominal_all <- bind_rows(annual_mean, annual_q10, annual_q90) %>%
      arrange(Country, type)
    
    return(annual_nominal_all)
  }
  
  ######################## Compute annual forecast growth ##############
  
  
  compute_growth <- function(annual_growth_forecast,
                             annual_growth_observed,
                             which = c("nominal", "real")) {
    
    which <- match.arg(which)
    
    if (which == "nominal") {
      level_types  <- c("nominal_gdp", "nominal_gdp_q10", "nominal_gdp_q90")
      growth_types <- c("nominal_growth", "nominal_growth_q10", "nominal_growth_q90")
    } else {
      level_types  <- c("real_gdp", "real_gdp_q10", "real_gdp_q90")
      growth_types <- c("real_growth", "real_growth_q10", "real_growth_q90")
    }
    
    # Identify year columns in the forecast table
    year_cols_fc <- grep("^[0-9]{4}$", names(annual_growth_forecast), value = TRUE)
    year_cols_obs <- grep("^[0-9]{4}$", names(annual_growth_observed), value = TRUE)
    
    # Helper: compute growth for one (level_type -> growth_type)
    compute_one <- function(level_type, growth_type) {
      
      # Forecast levels (long) - these are the levels you want to use for growth
      fc_long <- annual_growth_forecast %>%
        dplyr::filter(type == level_type) %>%
        tidyr::pivot_longer(
          cols      = dplyr::all_of(year_cols_fc),
          names_to  = "year",
          values_to = "level_fc"
        )
      
      if (nrow(fc_long) == 0) return(NULL)
      
      years_forecast <- sort(unique(as.integer(fc_long$year)))
      base_years     <- unique(years_forecast - 1L)
      
      # Observed MEAN levels (nominal_gdp or real_gdp) used as base for year-1
      obs_mean_long <- annual_growth_observed %>%
        dplyr::filter(type == if (which == "nominal") "nominal_gdp" else "real_gdp") %>%
        tidyr::pivot_longer(
          cols      = dplyr::all_of(year_cols_obs),
          names_to  = "year",
          values_to = "level_obs_mean"
        ) %>%
        dplyr::mutate(year_int = as.integer(year)) %>%
        dplyr::filter(year_int %in% base_years) %>%
        dplyr::select(Country, year = year_int, level = level_obs_mean)
      
      # Build the level series used for growth:
      # - include base year from observed mean (needed for 2025 growth)
      # - include forecast years from the chosen level_type (mean or quantile)
      combined_levels <- fc_long %>%
        dplyr::mutate(year = as.integer(year)) %>%
        dplyr::transmute(Country, year, level = level_fc) %>%
        dplyr::bind_rows(obs_mean_long) %>%
        dplyr::arrange(Country, year) %>%
        dplyr::distinct(Country, year, .keep_all = TRUE)
      
      # Compute YoY growth using levels (rate between nominals)
      growth_long <- combined_levels %>%
        dplyr::group_by(Country) %>%
        dplyr::mutate(
          level_prev = dplyr::lag(level),
          growth = (level / level_prev - 1) * 100
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(year %in% years_forecast) %>%   # only keep forecast-year growth
        dplyr::mutate(year = as.character(year)) %>%
        dplyr::select(Country, year, growth)
      
      growth_wide <- growth_long %>%
        dplyr::mutate(type = growth_type) %>%
        tidyr::pivot_wider(
          id_cols     = c(Country, type),
          names_from  = year,
          values_from = growth
        )
      
      return(growth_wide)
    }
    
    growth_rows <- dplyr::bind_rows(
      compute_one(level_types[1], growth_types[1]),
      compute_one(level_types[2], growth_types[2]),
      compute_one(level_types[3], growth_types[3])
    )
    
    # Drop any old versions of these growth types to avoid duplicates, then bind back
    out <- annual_growth_forecast %>%
      dplyr::filter(!(type %in% growth_types)) %>%
      dplyr::bind_rows(growth_rows) %>%
      dplyr::arrange(Country, type)
    
    return(out)
  }
  
  
  
  
  
  
  ######################## Add deflator yearly forecast to annual ##############
  
  add_deflator_forecast_to_annual <- function(annual_growth_forecast,
                                              annual_deflator_forecast) {
    
    df_long <- annual_deflator_forecast %>%
      tidyr::pivot_longer(
        cols      = -Country,
        names_to  = "year_raw",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        type = dplyr::case_when(
          grepl("_q10$", year_raw) ~ "deflator_q10",
          grepl("_q90$", year_raw) ~ "deflator_q90",
          TRUE                     ~ "deflator"
        ),
        # IMPORTANT: put quantiles under the same year columns (2025, 2026, 2027)
        year = gsub("_(q10|q90)$", "", year_raw)
      ) %>%
      dplyr::select(Country, type, year, value)
    
    deflator_rows <- df_long %>%
      tidyr::pivot_wider(
        id_cols     = c(Country, type),
        names_from  = year,
        values_from = value
      )
    
    out <- annual_growth_forecast %>%
      dplyr::bind_rows(deflator_rows) %>%
      dplyr::arrange(Country, type)
    
    return(out)
  }
  
  
  
  
  ######################## Compute the matrix of country's weight ##############
  
  compute_country_weight <- function(annual_growth_all) {
    
    # 1. Identify year columns (e.g. 2000, 2001, 2002…)
    year_cols <- grep("^[0-9]{4}$", names(annual_growth_all), value = TRUE)
    
    # 2. Keep only nominal GDP rows
    nominal_df <- annual_growth_all %>%
      filter(type == "nominal_gdp")
    
    # 3. Extract Eurozone nominal GDP row
    euro_row <- nominal_df %>%
      filter(Country == "Eurozone") %>%
      select(all_of(year_cols))
    
    # 4. Compute weights for each country
    country_weight <- nominal_df %>%
      mutate(across(
        all_of(year_cols),
        ~ .x / as.numeric(euro_row[[cur_column()]]),   # divide by Eurozone
        .names = "{.col}"
      )) %>%
      select(Country, all_of(year_cols)) %>%
      arrange(Country)
    
    return(country_weight)
  }
  
  ######################## Compute pure growth contribution to euro growth #####
  
  
  
  compute_country_weighted_growth <- function(annual_growth_all,
                                              matrix_country_weight) {
    
    
    # 1. Year columns present in both data frames
    year_cols <- intersect(
      grep("^[0-9]{4}$", names(annual_growth_all), value = TRUE),
      grep("^[0-9]{4}$", names(matrix_country_weight), value = TRUE)
    )
    
    # 2. Keep only nominal_growth rows
    nominal_growth <- annual_growth_all %>%
      filter(type == "nominal_growth") %>%
      select(Country, type, all_of(year_cols))
    
    # 3. Long format for growth
    nominal_growth_long <- nominal_growth %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "growth"
      )
    
    # 4. Long format for weights
    weight_long <- matrix_country_weight %>%
      select(Country, all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "weight"
      )
    
    # 5. Merge and compute weighted growth
    combined_long <- nominal_growth_long %>%
      left_join(weight_long, by = c("Country", "year")) %>%
      mutate(value = growth * weight) %>%
      select(Country, year, value)
    
    # 6. Wide format & final structure
    Country_weighted_growth <- combined_long %>%
      mutate(type = "nominal_growth_pure_contribution") %>%
      select(Country, type, year, value) %>%
      pivot_wider(
        names_from  = year,
        values_from = value
      ) %>%
      arrange(Country, type)
    
    return(Country_weighted_growth)
  }
  
  
  
  ######################## Compute relative growth contrib. to euro growth #####
  
  add_relative_contribution_nominal <- function(countries_gowth_contributions) {
   
    
    # 1. Identify year columns
    year_cols <- grep("^[0-9]{4}$", names(countries_gowth_contributions), value = TRUE)
    
    # 2. Extract Eurozone nominal growth (= its pure contribution)
    euro_growth <- countries_gowth_contributions %>%
      filter(Country == "Eurozone",
             type == "nominal_growth_pure_contribution") %>%
      select(all_of(year_cols))
    
    # 3. Compute relative contributions for each country
    relative_contrib <- countries_gowth_contributions %>%
      filter(type == "nominal_growth_pure_contribution") %>%
      mutate(
        type = "nominal_pourcent",
        across(
          all_of(year_cols),
          ~ .x / as.numeric(euro_growth[[cur_column()]])*100,   # ratio; add *100 if % desired
          .names = "{.col}"
        )
      )
    
    # 4. Bind back to the original table
    updated_table <- countries_gowth_contributions %>%
      bind_rows(relative_contrib) %>%
      arrange(Country, type)
    
    return(updated_table)
  }
  ######################## Compute diff between real and nominal growth ########
  
  compute_growth_nominal_real_diff <- function(annual_growth_all) {
    
    # 1. Identify year columns
    year_cols <- grep("^[0-9]{4}$", names(annual_growth_all), value = TRUE)
    
    # 2. Real growth in long format
    real_long <- annual_growth_all %>%
      filter(type == "real_growth") %>%
      select(Country, all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "real_growth"
      )
    
    # 3. Nominal growth in long format
    nominal_long <- annual_growth_all %>%
      filter(type == "nominal_growth") %>%
      select(Country, all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "nominal_growth"
      )
    
    # 4. Join and compute diff = real - nominal
    diff_long <- real_long %>%
      inner_join(nominal_long, by = c("Country", "year")) %>%
      mutate(diff = real_growth - nominal_growth)
    
    # 5. Back to wide: one row per country, one column per year
    growth_nominal_real_diff <- diff_long %>%
      select(Country, year, diff) %>%
      pivot_wider(
        names_from  = "year",
        values_from = "diff"
      ) %>%
      arrange(Country)
    
    return(growth_nominal_real_diff)
  }
  ######################## Compute pure real gowth contrib. to euro growth #####
  
  
  add_real_growth_pure_contribution <- function(countries_gowth_contributions,
                                                matrix_country_weitgh,
                                                matric_country_real_nominal_diff) {
    library(dplyr)
    library(tidyr)
    
    # Year columns common to all three tables
    year_cols <- Reduce(
      intersect,
      list(
        grep("^[0-9]{4}$", names(countries_gowth_contributions), value = TRUE),
        grep("^[0-9]{4}$", names(matrix_country_weitgh), value = TRUE),
        grep("^[0-9]{4}$", names(matric_country_real_nominal_diff), value = TRUE)
      )
    )
    
    ## 1. Nominal growth pure contribution (long)
    nominal_contrib_long <- countries_gowth_contributions %>%
      filter(type == "nominal_growth_pure_contribution") %>%
      select(Country, all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "nominal_contrib"
      )
    
    ## 2. Real–nominal growth difference (long)
    diff_long <- matric_country_real_nominal_diff %>%
      select(Country, all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "real_nominal_diff"
      )
    
    ## 3. Country weights (long)
    weight_long <- matrix_country_weitgh %>%
      select(Country, all_of(year_cols)) %>%
      pivot_longer(
        cols      = all_of(year_cols),
        names_to  = "year",
        values_to = "weight"
      )
    
    ## 4. Join everything and compute real contribution
    real_contrib_long <- nominal_contrib_long %>%
      left_join(diff_long,   by = c("Country", "year")) %>%
      left_join(weight_long, by = c("Country", "year")) %>%
      mutate(
        value = nominal_contrib + real_nominal_diff * weight,
        type  = "real_growth_pure_contribution"
      ) %>%
      select(Country, type, year, value)
    
    ## 5. Back to wide and bind to original table
    real_contrib_wide <- real_contrib_long %>%
      pivot_wider(
        names_from  = "year",
        values_from = "value"
      )
    
    updated_countries_contributions <- countries_gowth_contributions %>%
      bind_rows(real_contrib_wide) %>%
      arrange(Country, type)
    
    return(updated_countries_contributions)
  }
  ######################## Compute relative real growth contrib. ###############
  
  add_relative_contribution_real <- function(countries_gowth_contributions) {
    
    
    # 1. Identify year columns
    year_cols <- grep("^[0-9]{4}$", names(countries_gowth_contributions), value = TRUE)
    
    # 2. Extract Eurozone nominal growth (= its pure contribution)
    euro_growth <- countries_gowth_contributions %>%
      filter(Country == "Eurozone",
             type == "real_growth_pure_contribution") %>%
      select(all_of(year_cols))
    
    # 3. Compute relative contributions for each country
    relative_contrib <- countries_gowth_contributions %>%
      filter(type == "real_growth_pure_contribution") %>%
      mutate(
        type = "reel_pourcent",
        across(
          all_of(year_cols),
          ~ .x / as.numeric(euro_growth[[cur_column()]])*100,   # ratio; add *100 if % desired
          .names = "{.col}"
        )
      )
    
    # 4. Bind back to the original table
    updated_table <- countries_gowth_contributions %>%
      bind_rows(relative_contrib) %>%
      arrange(Country, type)
    
    return(updated_table)
  }
  ######################## Correct pure real of the sum of small countries #####
  correct_purreel_small_countries <- function(data,
                                    target_country = "Sum Small euro countries",
                                    target_type = "real_growth_pure_contribution",
                                    eurozone_country = "Eurozone",
                                    big_countries = c("France", "Germany", "Italy", "Netherlands", "Spain"),
                                    years = 2001:2027,
                                    verbose = TRUE,
                                    print_years = c(2001, 2005, 2010, 2015, 2020, 2025, 2027)) {
    
    # 1) Find the target row to update
    row_index <- which(data$Country == target_country & data$type == target_type)
    
    if (length(row_index) != 1) {
      stop(sprintf(
        "Error: target row not found or not unique (%d found).",
        length(row_index)
      ))
    }
    
    if (verbose) {
      cat("Row index to update:", row_index, "\n")
      cat("Row found:", data$Country[row_index], data$type[row_index], "\n\n")
    }
    
    # 2) Keep only rows of the relevant type
    real_data <- subset(data, type == target_type)
    
    # Helper: safely extract a single value (or NA if missing)
    get_val <- function(country, year_col) {
      v <- real_data[real_data$Country == country, year_col]
      if (nrow(v) == 0) return(NA_real_)
      if (nrow(v) > 1 && verbose) {
        warning(sprintf(
          "Duplicate rows detected for Country='%s' and type='%s'. Using the first one.",
          country, target_type
        ))
      }
      as.numeric(v[1, 1])
    }
    
    # 3) Loop over years
    year_cols <- as.character(years)
    
    for (year in year_cols) {
      if (!year %in% names(data)) {
        stop(sprintf("Missing year column in data: %s", year))
      }
      
      eurozone_val <- get_val(eurozone_country, year)
      big_vals <- sapply(big_countries, get_val, year_col = year)
      
      # Eurozone minus the sum of large countries
      new_value <- eurozone_val - sum(big_vals)
      
      data[row_index, year] <- new_value
      
      if (verbose && as.integer(year) %in% print_years) {
        cat(sprintf(
          "%s: %.4f = %.4f - (%s)\n",
          year, new_value, eurozone_val,
          paste(sprintf("%.4f", big_vals), collapse = " + ")
        ))
      }
    }
    
    if (verbose) {
      cat("\n=== SUMMARY OF CHANGES ===\n")
      years_show <- intersect(
        as.character(c(2001, 2005, 2010, 2015, 2020, 2021:2027)),
        names(data)
      )
      print(data[row_index, c("Country", "type", years_show), drop = FALSE])
    }
    
    return(data)
  }
  
  ######################## Compute the forecast quantiles ######################
  
  compute_forecast_quantiles <- function(fc_object,
                                         probs = c(0.05, 0.95)) {
    

    mu <- fc_object$forecast_values
    sd <- fc_object$forecast_SD
    

    
    # Gaussian quantiles
    z <- stats::qnorm(probs)
    
    quantiles <- sapply(z, function(z_i) {
      mu + z_i * sd
    })
    
    colnames(quantiles) <- paste0("q", probs * 100)
    
    return(as.data.frame(quantiles))
  }
  
  
  
################################################################################
# Function test the models
################################################################################
  
  
  
  ######################## Make a spaghetti chart #################################
  
  
  
  
  plot_spaghetti_growth <- function(
      obs_df,
      forecast_df,
      forecast_year,
      horizon_forecast,
      country      = "Eurozone",
      first_obs    = 2000.25,
      # column names (customisable if needed)
      obs_quarter_col      = "Quarter",
      obs_growth_col       = "gdp_growth_log",
      fcst_quarter_col     = "Forecasted_Quarter",
      fcst_growth_col      = "Forecast_Growth_logdiff",
      end_set_col          = "End_Estimation_Set",
      country_col          = "Country"
  ) {
    # last quarter to display
    last_quarter <- forecast_year + 0.25 * horizon_forecast
    
    # --- Observed series (blue) ---
    obs_df_plot <- obs_df %>%
      filter(
        .data[[country_col]] == country,
        .data[[obs_quarter_col]] >= first_obs,
        .data[[obs_quarter_col]] <= last_quarter
      ) %>%
      transmute(
        Country  = .data[[country_col]],
        Quarter  = .data[[obs_quarter_col]],
        Growth   = .data[[obs_growth_col]],
        series   = "Observed"
      )
    
    # --- Forecast series (red) ---
    fcst_df_plot <- forecast_df %>%
      filter(
        .data[[country_col]] == country,
        .data[[end_set_col]] == forecast_year,
        Horizon <= horizon_forecast
      ) %>%
      transmute(
        Country  = .data[[country_col]],
        Quarter  = .data[[fcst_quarter_col]],
        Growth   = .data[[fcst_growth_col]],
        series   = "Forecast"
      )
    
    # Combine for plotting
    df_plot <- bind_rows(obs_df_plot, fcst_df_plot)
    
    # --- Plot ---
    p <- ggplot(df_plot, aes(x = Quarter, y = Growth, colour = series)) +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = forecast_year, linetype = "dashed") +
      scale_colour_manual(values = c("Observed" = "blue", "Forecast" = "red")) +
      labs(
        title    = paste0("Observed vs Forecast log-diff – ", country),
        subtitle = paste0(
          "Forecast starting at ", forecast_year,
          " (horizon = ", horizon_forecast, ")"
        ),
        x = "Quarter",
        y = "Log-diff growth",
        colour = NULL
      ) +
      theme_minimal()
    class(p)
    
    return(p)
  }
  
  ######################## Make error matrix #################################
  
  
  
  
  make_error_matrix <- function(all_growth_results,
                            Euro_Countries_GDP_Growth_Log,
                            country = "Eurozone") {
    # packages
    library(dplyr)
    library(tidyr)
    
    # 1. Keep only the chosen country and compute the quarter
    #    where the forecast is realized
    fc <- all_growth_results %>%
      filter(Country == country) %>%
      mutate(
        target_quarter = End_Estimation_Set + 0.25 * Horizon,
        target_quarter = round(target_quarter, 2),       # guard vs float issues
        Forecasted_Quarter = round(Forecasted_Quarter, 2)
      )
    
    # 2. Keep only chosen country in the observed data
    obs <- Euro_Countries_GDP_Growth_Log %>%
      filter(Country == country) %>%
      mutate(Quarter = round(Quarter, 2))
    
    # 3. Match forecast with realized outcome using End_Estimation_Set + h
    merged <- fc %>%
      left_join(obs, by = c("Country", "target_quarter" = "Quarter"))
    
    # 4. Compute squared error
    merged <- merged %>%
      mutate(sq_error = (Forecast_Growth_logdiff - gdp_growth_log)^2)
    
    # 5. Reshape: columns = Forecasted_Quarter, rows = Horizon
    se_wide <- merged %>%
      select(Forecasted_Quarter, Horizon, sq_error) %>%
      pivot_wider(
        names_from  = Forecasted_Quarter,
        values_from = sq_error
      ) %>%
      arrange(Horizon)
    
    # 6. Put Horizon into row names and return a matrix / data frame
    se_df <- as.data.frame(se_wide)
    rownames(se_df) <- se_df$Horizon
    se_df$Horizon <- NULL
    
    # (optional) order columns by quarter
    se_df <- se_df[, order(as.numeric(colnames(se_df)))]
    
    return(se_df)   # a data frame whose rownames = horizons, cols = Forecasted_Quarter
  }
  
  
  ######################## add MSerror matrix to results_table #####################
  
  
  add_msfe <- function(results_table, se_df) {
    
    # se_df has horizons as row names and squared errors as columns
    # MSFE = mean of squared errors across all forecast origins (columns)
    
    msfe_vec <- rowMeans(se_df, na.rm = TRUE)
    
    # Add as a new column to results_table
    results_table$MSFE <- msfe_vec
    
    return(results_table)
  }
  
  ######################## make forecast matrix ##################################
  
  
  make_forecast_matrix <- function(all_growth_results,
                                   country = "Eurozone") {
    
    # 1. Filter for the chosen country
    fc <- all_growth_results %>%
      filter(Country == country) %>%
      mutate(Forecasted_Quarter = round(Forecasted_Quarter, 2))
    
    # 2. Reshape: columns = Forecasted_Quarter, rows = Horizon
    forecast_wide <- fc %>%
      select(Forecasted_Quarter, Horizon, Forecast_Growth_logdiff) %>%
      pivot_wider(
        names_from  = Forecasted_Quarter,
        values_from = Forecast_Growth_logdiff
      ) %>%
      arrange(Horizon)
    
    # 3. Put Horizon as row names and clean
    out <- as.data.frame(forecast_wide)
    rownames(out) <- out$Horizon
    out$Horizon <- NULL
    
    # optional: sort columns by quarter
    out <- out[, order(as.numeric(colnames(out)))]
    
    return(out)
  }
  
  
  ######################## make pure forecast error matrix #######################
  
  
  
  make_pure_error_matrix <- function(forecast_matrix,
                                     Euro_Countries_GDP_Growth_Log,
                                     country = "Eurozone") {
    
  
    # -------------------------------------------------------
    # 1. Extract horizons and forecasted quarters
    # -------------------------------------------------------
    horizons <- as.numeric(rownames(forecast_matrix))
    forecast_quarters <- as.numeric(colnames(forecast_matrix))
    
    # -------------------------------------------------------
    # 2. Filter observations for the country
    # -------------------------------------------------------
    obs <- Euro_Countries_GDP_Growth_Log %>%
      filter(Country == country) %>%
      mutate(Quarter = round(Quarter, 2))
    
    # -------------------------------------------------------
    # 3. Build the pure error matrix
    # -------------------------------------------------------
    pure_error_mat <- matrix(NA,
                             nrow = length(horizons),
                             ncol = length(forecast_quarters),
                             dimnames = list(horizons, forecast_quarters))
    
    # -------------------------------------------------------
    # 4. Fill each cell: forecast - observed
    # -------------------------------------------------------
    for (fq in forecast_quarters) {
      # observed value for that quarter
      g_obs <- obs$gdp_growth_log[obs$Quarter == fq]
      
      if (length(g_obs) == 1) {
        # column exists in forecast_matrix
        pure_error_mat[, as.character(fq)] <-
          forecast_matrix[, as.character(fq)] - g_obs
      }
    }
    
    # -------------------------------------------------------
    # 5. Convert to data frame
    # -------------------------------------------------------
    pure_error_df <- as.data.frame(pure_error_mat)
    
    # -------------------------------------------------------
    # 6. Add the average pure error column
    # -------------------------------------------------------
    pure_error_df$pure_error_avg <- rowMeans(pure_error_df, na.rm = TRUE)
    
    # Move the average column to first position
    pure_error_df <- pure_error_df %>%
      relocate(pure_error_avg)
    
    return(pure_error_df)
  }
  
  
  
  ######################## The MZ test with HAC and F-test #######################
  
  
  mz_test_country <- function(all_growth_results,
                              Euro_Countries_GDP_Growth_Log,
                              results_table,
                              country = "Eurozone") {
    # Required packages
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Package 'dplyr' is required.")
    }
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package 'sandwich' is required.")
    }
    
    # 1. Filter to the chosen country ----------------------------------------
    df_fore <- dplyr::filter(all_growth_results, Country == country)
    df_real <- Euro_Countries_GDP_Growth_Log |>
      dplyr::filter(Country == country) |>
      dplyr::select(Quarter, gdp_growth_log)
    
    # 2. Match forecasts with realizations by quarter ------------------------
    df_merged <- dplyr::inner_join(
      df_fore,
      df_real,
      by = c("Forecasted_Quarter" = "Quarter")
    )
    
    # 3. Loop over horizons and run MZ regressions ---------------------------
    horizons <- sort(unique(df_merged$Horizon))
    
    mz_list <- lapply(horizons, function(h) {
      dat_h <- df_merged[df_merged$Horizon == h, ]
      dat_h <- dat_h[complete.cases(dat_h$Forecast_Growth_logdiff,
                                    dat_h$gdp_growth_log), ]
      
      if (nrow(dat_h) < 5) {
        # Not enough obs; return NA row
        return(data.frame(
          Horizon = h,
          alpha_hat = NA_real_,
          beta_hat  = NA_real_,
          p_alpha_0 = NA_real_,
          p_beta_1  = NA_real_,
          joint_p   = NA_real_
        ))
      }
      
      # MZ regression
      fit <- lm(gdp_growth_log ~ Forecast_Growth_logdiff, data = dat_h)
      
      # HAC / Newey-West covariance, lag = h - 1 (at least 0)
      lag_hac <- max(h - 1, 0)
      Vhac <- sandwich::NeweyWest(fit, lag = lag_hac, prewhite = FALSE, adjust = TRUE)
      
      coefs <- coef(fit)
      alpha_hat <- coefs[1]
      beta_hat  <- coefs[2]
      
      # Robust standard errors
      se_alpha <- sqrt(Vhac[1, 1])
      se_beta  <- sqrt(Vhac[2, 2])
      
      df_resid <- fit$df.residual
      
      ## Test H0: alpha = 0  -----------------------------------------------
      t_alpha <- (alpha_hat - 0) / se_alpha
      p_alpha <- 2 * pt(abs(t_alpha), df = df_resid, lower.tail = FALSE)
      
      ## Test H0: beta = 1  -------------------------------------------------
      t_beta <- (beta_hat - 1) / se_beta
      p_beta <- 2 * pt(abs(t_beta), df = df_resid, lower.tail = FALSE)
      
      ## Joint robust Wald F-test: H0: [alpha = 0, beta = 1] ---------------
      R <- rbind(
        c(1, 0),  # alpha
        c(0, 1)   # beta
      )
      r_vec <- c(0, 1)
      b_vec <- c(alpha_hat, beta_hat)
      
      diff <- R %*% b_vec - r_vec        # (R*b - r)
      RVRT_inv <- solve(R %*% Vhac %*% t(R))
      
      W <- as.numeric(t(diff) %*% RVRT_inv %*% diff)  # Wald statistic
      q <- 2  # number of restrictions
      
      F_stat <- W / q
      joint_p <- 1 - pf(F_stat, df1 = q, df2 = df_resid)
      
      data.frame(
        Horizon   = h,
        alpha_hat = alpha_hat,
        beta_hat  = beta_hat,
        p_alpha_0 = p_alpha,
        p_beta_1  = p_beta,
        joint_p   = joint_p
      )
    })
    
    mz_table <- do.call(rbind, mz_list)
    
    # 4. Add MZ joint p-values to results_table ------------------------------
    # Assume rownames of results_table are "1", "2", ..., horizons
    results_table$MZ_joint_test <- NA_real_
    
    for (i in seq_len(nrow(mz_table))) {
      h <- mz_table$Horizon[i]
      rn <- as.character(h)
      if (rn %in% rownames(results_table)) {
        results_table[rn, "MZ_joint_test"] <- mz_table$joint_p[i]
      }
    }
    
    
    mz_table <- mz_table |>
      dplyr::mutate(
        p_alpha_0 = sprintf("%.3f", p_alpha_0),
        p_beta_1  = sprintf("%.3f", p_beta_1),
        joint_p   = sprintf("%.3f", joint_p)
      )
    
    
    results_table <- results_table |>
      dplyr::mutate(
        MZ_joint_test   = sprintf("%.3f", MZ_joint_test)
      )
    
    # 5. Return both: the MZ table and the updated results_table ------------
    list(
      MZ_table      = mz_table,
      results_table = results_table
    )
  }
  
  ######################## ljung_box test: error auto correlations ###############
  
  
  ljung_box_by_horizon <- function(error_matrix, K = 8, results_table) {
    # Make sure we have matrices / data.frames
    error_matrix  <- as.matrix(error_matrix)
    results_table <- as.data.frame(results_table)
    
    # Use rownames of error_matrix as horizons; if none, create 1..n
    horizons <- rownames(error_matrix)
    if (is.null(horizons)) {
      horizons <- seq_len(nrow(error_matrix))
      rownames(error_matrix) <- horizons
    }
    
    # Compute Ljung–Box test p-values for each horizon (row)
    lb_pvalues <- sapply(seq_len(nrow(error_matrix)), function(i) {
      x <- as.numeric(error_matrix[i, ])
      x <- x[!is.na(x)]           # drop NAs
      
      # If not enough obs for K lags, return NA
      if (length(x) <= K) return(NA_real_)
      
      stats::Box.test(x, lag = K, type = "Ljung-Box")$p.value
    })
    
    # Small table: one column, horizons as rownames
    lb_table <- data.frame(
      LjungBox_pvalue = lb_pvalues,
      row.names = horizons
    )
    
    # Add column to results_table
    # (assumes rownames of results_table are the horizons)
    if (is.null(rownames(results_table))) {
      rownames(results_table) <- horizons
    }
    
    # Align horizons just in case
    match_idx <- match(rownames(results_table), horizons)
    results_table$LjungBox_pvalue <- lb_pvalues[match_idx]
    
    # Return both
    list(
      lb_table      = lb_table,
      results_table = results_table
    )
  }
  ######################## Compute the MSFE ratio ##########################
  
  compute_ratio_MSFE <- function(results_table,
                              results_table_bench,
                              col_model  = "MSFE",   # name of MSFE column in results_table
                              col_bench  = "MSFE",   # name of MSFE column in results_table_bench
                              new_col    = "ratio_MSFE") {
    # predictive R^2 for each horizon (row)
    ratio_M_onB <- results_table[[col_model]] / results_table_bench[[col_bench]]
    
    # add as new column
    results_table[[new_col]] <- ratio_M_onB
    
    return(results_table)
  }
  
  
  
  
  ######################## Compute the long-run Variance #######################
  
  # Custom Newey-West long-run variance (HAC) for a series x
  nw_lrv <- function(x, lag) {
    T <- length(x)
    if (T <= 1L) return(NA_real_)
    
    x_c <- x - mean(x)
    # gamma_0
    gamma0 <- sum(x_c^2) / T
    
    if (lag <= 0L) {
      return(gamma0)
    }
    
    lrv <- gamma0
    for (k in 1:lag) {
      gamma_k <- sum(x_c[(k + 1):T] * x_c[1:(T - k)]) / T
      weight  <- 1 - k / (lag + 1)  # Bartlett weight
      lrv     <- lrv + 2 * weight * gamma_k
    }
    lrv
  }
  
  
  ######################## Compute Diebold-Mariano test ########################
  
  
  # Diebold-Mariano test based on MSFE for each horizon
  dm_test_msfe <- function(results,
                           results_bench,
                           error_mat,
                           error_mat_bench,
                           two_sided = TRUE) {
    
    # Basic checks
    if (nrow(results) != nrow(results_bench)) {
      stop("results and results_bench must have the same number of rows (horizons).")
    }
    if (nrow(error_mat) != nrow(error_mat_bench)) {
      stop("error_mat and error_mat_bench must have the same number of rows (horizons).")
    }
    
    H <- nrow(results)  # number of horizons
    
    DM_stat    <- rep(NA_real_, H)
    DM_pvalue  <- rep(NA_real_, H)
    
    for (h in 1:H) {
      e1 <- as.numeric(error_mat[h, ])
      e2 <- as.numeric(error_mat_bench[h, ])
      
      # keep only columns where both are observed
      ok <- !is.na(e1) & !is.na(e2)
      e1 <- e1[ok]
      e2 <- e2[ok]
      
      T_h <- length(e1)
      if (T_h <= 1L) next  # not enough data at this horizon
      
      # Loss differential series for this horizon
      d_t <- e1^2 - e2^2
      
      # Mean loss differential (should be MSFE_diff)
      d_bar <- mean(d_t)
      
      # HAC lag: standard choice for h-step DM: h-1, but cannot exceed T_h-1
      lag_h <- min(h - 1L, T_h - 1L)
      if (lag_h < 0L) lag_h <- 0L
      
      lrv_h <- nw_lrv(d_t, lag_h)
      
      # If lrv_h is NA or zero, skip
      if (is.na(lrv_h) || lrv_h <= 0) next
      
      se_dbar <- sqrt(lrv_h / T_h)
      
      DM_h <- d_bar / se_dbar
      DM_stat[h] <- DM_h
      
      if (two_sided) {
        DM_pvalue[h] <- 2 * (1 - pnorm(abs(DM_h)))
      } else {
        # one-sided (alternative: model better than benchmark)
        DM_pvalue[h] <- 1 - pnorm(DM_h)
      }
    }
    
    # Add columns to the main result table
    results$DM_stat   <- DM_stat
    results$DM_pvalue <- DM_pvalue
    
    return(results)
  }
  ######################## Compute the mean and variance of the forecasts ######
  
  
  mean_variance_forecasts <- function(one_country_forecast_matrix, results_table) {
    
    # Compute mean and variance per horizon (each row)
    mean_forecast <- apply(one_country_forecast_matrix, 1, function(x) mean(x, na.rm = TRUE))
    variance_forecast <- apply(one_country_forecast_matrix, 1, function(x) var(x, na.rm = TRUE))
    
    # Add to results_table
    results_table$mean_forecast <- mean_forecast
    results_table$variance_forecast <- variance_forecast
    
    return(results_table)
  }
  
  
  
  
  
  ######################## compute the forecast path at each period ############
  
  simulate_forecast_paths_tagged <- function(forecast_values,
                                             forecast_SD,
                                             loop = 10,
                                             end_quarter,
                                             country_name) {
    
    
    
    horizons <- seq_along(forecast_values)
    H <- length(horizons)
    
    # Sim matrix: rows=draws, cols=horizons
    sim_matrix <- sapply(horizons, function(h) {
      stats::rnorm(
        n    = loop,
        mean = forecast_values[h],
        sd   = forecast_SD[h]
      )
    })
    
    # Build tidy df with metadata
    sim_df <- data.frame(
      Country            = country_name,
      End_Estimation_Set = end_quarter,
      Draw               = rep(seq_len(loop), times = H),
      Horizon            = rep(horizons, each = loop),
      Forecasted_Quarter = end_quarter + 0.25 * rep(horizons, each = loop),
      Forecast           = as.vector(sim_matrix)
    )
    
    return(sim_df)
  }
  ######################## Transform the log-diff paths into nominal ###########
  
  logdiff_sim_to_nominal <- function(sim_df, Euro_Countries_GDP_Growth_Log,
                                     country_col = "Country",
                                     end_col     = "End_Estimation_Set",
                                     horizon_col = "Horizon",
                                     draw_col    = "Draw",
                                     forecast_col= "Forecast",
                                     obs_level_col = "Nominal_GDP_seas",
                                     obs_quarter_col = "Quarter") {
    
    
    # --- 1) Build start levels: observed nominal GDP at the forecast origin ---
    start_levels <- Euro_Countries_GDP_Growth_Log %>%
      transmute(
        Country = .data[[country_col]],
        End_Estimation_Set = round(.data[[obs_quarter_col]], 2),
        start_level = .data[[obs_level_col]]
      )
    
    # --- 2) Prepare sim_df (round End_Estimation_Set to avoid float mismatches) ---
    sim_df2 <- sim_df %>%
      mutate(
        End_Estimation_Set = round(.data[[end_col]], 2)
      ) %>%
      left_join(start_levels, by = c("Country", "End_Estimation_Set"))
    
    # Check: if start_level missing, you cannot build nominal levels
    if (any(is.na(sim_df2$start_level))) {
      bad <- sim_df2 %>%
        filter(is.na(start_level)) %>%
        distinct(Country, End_Estimation_Set)
      stop("Missing start_level for some Country/End_Estimation_Set pairs. Example:\n",
           paste0(capture.output(print(bad)), collapse = "\n"))
    }
    
    # --- 3) Convert log-diff to growth factor and cumulate to get nominal levels ---
    out <- sim_df2 %>%
      arrange(.data[[country_col]], .data[[end_col]], .data[[draw_col]], .data[[horizon_col]]) %>%
      group_by(.data[[country_col]], .data[[end_col]], .data[[draw_col]]) %>%
      mutate(
        growth_factor = exp(.data[[forecast_col]] / 100),            # from log-diff(%)
        Forecast_Nominal_GDP = start_level * cumprod(growth_factor)  # level path
      ) %>%
      ungroup() %>%
      # return "same structure" but forecast is nominal now:
      mutate(!!forecast_col := Forecast_Nominal_GDP) %>%
      select(-start_level, -growth_factor, -Forecast_Nominal_GDP)
    
    return(out)
  }
  ######################## Transform the paths forecast into eurozone paths ####
  
  add_eurozone_logdiff_to_simulated_path_all <- function(simulated_path_all,
                                                         simulated_path_all_nominal,
                                                         Euro_Countries_GDP_Growth_Log,
                                                         countries_to_sum = c("France","Germany","Italy","Netherlands","Spain","Sum Small euro countries"),
                                                         euro_name = "Eurozone") {
    
    # ----------------------------
    # 0) Round to avoid float issues
    # ----------------------------
    sim_nom <- simulated_path_all_nominal %>%
      mutate(
        End_Estimation_Set = round(End_Estimation_Set, 2),
        Forecasted_Quarter = round(Forecasted_Quarter, 2)
      )
    
    sim_ld <- simulated_path_all %>%
      mutate(
        End_Estimation_Set = round(End_Estimation_Set, 2),
        Forecasted_Quarter = round(Forecasted_Quarter, 2)
      )
    
    # -----------------------------------------
    # 1) Aggregate simulated Eurozone nominal
    # -----------------------------------------
    euro_nom <- sim_nom %>%
      filter(Country %in% countries_to_sum) %>%
      group_by(End_Estimation_Set, Forecasted_Quarter, Draw, Horizon) %>%
      summarise(
        euro_nominal = sum(Forecast, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Country = euro_name) %>%
      relocate(Country)
    
    # -------------------------------------------------------
    # 2) Get observed Eurozone nominal at End_Estimation_Set
    # -------------------------------------------------------
    euro_start_levels <- Euro_Countries_GDP_Growth_Log %>%
      filter(Country == euro_name) %>%
      transmute(
        Country = euro_name,
        End_Estimation_Set = round(Quarter, 2),
        start_level = Nominal_GDP_seas
      )
    
    euro_nom <- euro_nom %>%
      left_join(euro_start_levels, by = c("Country", "End_Estimation_Set"))
    
    if (any(is.na(euro_nom$start_level))) {
      bad <- euro_nom %>% filter(is.na(start_level)) %>% distinct(End_Estimation_Set)
      stop(
        "Missing observed Eurozone Nominal_GDP_seas at some End_Estimation_Set. Example:\n",
        paste0(capture.output(print(bad)), collapse = "\n")
      )
    }
    
    # -------------------------------------------------------
    # 3) Compute Eurozone simulated log-diff
    #     Horizon==1: base = observed start_level
    #     Horizon>1: base = lag(euro_nominal) within (End, Draw)
    # -------------------------------------------------------
    euro_logdiff <- euro_nom %>%
      arrange(End_Estimation_Set, Draw, Horizon) %>%
      group_by(End_Estimation_Set, Draw) %>%
      mutate(
        base_nominal = dplyr::case_when(
          Horizon == 1L ~ start_level,
          TRUE          ~ dplyr::lag(euro_nominal)
        ),
        Forecast = 100 * (log(euro_nominal) - log(base_nominal))  # <-- log-diff in %
      ) %>%
      ungroup() %>%
      select(Country, End_Estimation_Set, Forecasted_Quarter, Draw, Horizon, Forecast)
    
    # -------------------------------------------------------
    # 4) Append Eurozone rows to simulated_path_all
    #     Ensure same columns (add missing columns as NA)
    # -------------------------------------------------------
    # Add any columns present in simulated_path_all but missing in euro_logdiff
    missing_in_euro <- setdiff(names(sim_ld), names(euro_logdiff))
    if (length(missing_in_euro) > 0) euro_logdiff[missing_in_euro] <- NA
    
    # Add any columns present in euro_logdiff but missing in simulated_path_all
    missing_in_sim <- setdiff(names(euro_logdiff), names(sim_ld))
    if (length(missing_in_sim) > 0) sim_ld[missing_in_sim] <- NA
    
    # Reorder euro columns to match simulated_path_all
    euro_logdiff <- euro_logdiff[, names(sim_ld), drop = FALSE]
    
    out <- dplyr::bind_rows(sim_ld, euro_logdiff)
    
    return(out)
  }
  
  
  
  ######################## Compuation if predictive variance of countries ######
  
  compute_predictive_variance_from_draws <- function(simulated_path_all,
                                                     country_col  = "Country",
                                                     end_col      = "End_Estimation_Set",
                                                     horizon_col  = "Horizon",
                                                     draw_col     = "Draw",
                                                     forecast_col = "Forecast") {
    
    out <- simulated_path_all %>%
      group_by(
        .data[[country_col]],
        .data[[end_col]],
        .data[[horizon_col]]
      ) %>%
      summarise(
        n_draws_used = sum(!is.na(.data[[forecast_col]])),
        
        # Empirical predictive moments from draws
        forecast_mean = ifelse(n_draws_used >= 1,
                               mean(.data[[forecast_col]], na.rm = TRUE),
                               NA_real_),
        forecast_variance = ifelse(n_draws_used >= 2,
                                   var(.data[[forecast_col]], na.rm = TRUE),
                                   NA_real_),
        forecast_sd = ifelse(n_draws_used >= 2,
                             sd(.data[[forecast_col]], na.rm = TRUE),
                             NA_real_),
        
        
        .groups = "drop"
      ) %>%
      arrange(.data[[country_col]], .data[[end_col]], .data[[horizon_col]])
    
    return(out)
  }
  
  ######################## Compute the PIT and Berkowitz joint test ############
  
  add_berkowitz_test <- function(results_table,
                                 pred_var_table,
                                 Euro_Countries_GDP_Growth_Log,
                                 logdiff_to_train_on = "gdp_growth_log_wins_001",
                                 country = "Eurozone",
                                 pit_clip = 1e-6) {
  
    
    # --- 1) Build the (t,h) dataset: predictive moments + realized outcome ----
    pv <- pred_var_table %>%
      dplyr::filter(.data$Country == country) %>%
      dplyr::mutate(
        End_Estimation_Set = round(.data$End_Estimation_Set, 2),
        Forecasted_Quarter = round(.data$End_Estimation_Set + 0.25 * .data$Horizon, 2)
      )
    
    obs <- Euro_Countries_GDP_Growth_Log %>%
      dplyr::filter(.data$Country == country) %>%
      dplyr::transmute(
        Country = .data$Country,
        Forecasted_Quarter = round(.data$Quarter, 2),
        realized = .data[[logdiff_to_train_on]]
      )
    
    df <- pv %>%
      dplyr::left_join(obs, by = c("Country", "Forecasted_Quarter"))
    
    # Basic sanity checks (you said no NA, but better to fail loudly if mismatch)
    if (any(is.na(df$realized))) {
      bad <- df %>% dplyr::filter(is.na(realized)) %>%
        dplyr::distinct(End_Estimation_Set, Horizon, Forecasted_Quarter)
      stop("Some realized values could not be matched in Euro_Countries_GDP_Growth_Log.\n",
           paste0(capture.output(print(bad)), collapse = "\n"))
    }
    if (any(df$forecast_sd <= 0 | is.na(df$forecast_sd))) {
      stop("forecast_sd must be > 0 for Berkowitz (needed for PIT and qnorm).")
    }
    
    # --- 2) Compute PIT and z = qnorm(PIT) ----
    df <- df %>%
      dplyr::mutate(
        pit_raw = stats::pnorm((realized - forecast_mean) / forecast_sd),
        pit     = pmin(pmax(pit_raw, pit_clip), 1 - pit_clip),
        z       = stats::qnorm(pit)
      )
    
    
    # --- Diagnostics (after PIT, before Berkowitz LR) ----
    cat("[Berkowitz] PIT raw:  min=", min(df$pit_raw, na.rm=TRUE),
        " max=", max(df$pit_raw, na.rm=TRUE), "\n")
    
    cat("[Berkowitz] PIT raw extremes:  <=0:", sum(df$pit_raw <= 0, na.rm=TRUE),
        " >=1:", sum(df$pit_raw >= 1, na.rm=TRUE), "\n")
    
    cat("[Berkowitz] PIT near extremes (raw):  <1e-6:", sum(df$pit_raw < 1e-6, na.rm=TRUE),
        " >1-1e-6:", sum(df$pit_raw > 1 - 1e-6, na.rm=TRUE), "\n")
    
    cat("[Berkowitz] PIT clipped extremes:  ==clip:", sum(df$pit <= pit_clip, na.rm=TRUE),
        " ==1-clip:", sum(df$pit >= 1 - pit_clip, na.rm=TRUE), "\n")
    
    cat("[Berkowitz] z infinities:", sum(!is.finite(df$z)), "\n")
    
    # --- 3) Berkowitz LR test per horizon ----
    berkowitz_pval_by_h <- df %>%
      dplyr::arrange(End_Estimation_Set) %>%
      dplyr::group_by(Horizon) %>%
      dplyr::summarise(
        Berkowitz_pvalue = {
          zt <- z
          n  <- length(zt)
          
          if (n < 5) {
            NA_real_
          } else {
            # Restricted likelihood
            ll_restricted <- sum(stats::dnorm(zt, mean = 0, sd = 1, log = TRUE))
            
            # Unrestricted AR(1)
            y <- zt[-1]
            x <- zt[-n]
            fit <- stats::lm(y ~ x)
            
            resid <- stats::residuals(fit)
            sigma2_hat <- mean(resid^2)
            
            ll_unrestricted <- sum(
              stats::dnorm(resid, mean = 0, sd = sqrt(sigma2_hat), log = TRUE)
            )
            
            LR   <- 2 * (ll_unrestricted - ll_restricted)
            pval <- stats::pchisq(LR, df = 3, lower.tail = FALSE)
            p_log <- stats::pchisq(LR, df = 3, lower.tail = FALSE, log.p = TRUE)
            
            cat("[Berkowitz] Horizon ", dplyr::first(Horizon),
                " LR=", LR, " log(p)=", p_log, "\n")
            
            pval   
          }
        },
        .groups = "drop"
      )
    
    
    # --- 4) Write into results_table ----
    results_table <- as.data.frame(results_table)
    
    for (i in seq_len(nrow(berkowitz_pval_by_h))) {
      h <- berkowitz_pval_by_h$Horizon[i]
      rn <- as.character(h)
      if (rn %in% rownames(results_table)) {
        pval <- berkowitz_pval_by_h$Berkowitz_pvalue[i]
        pval <- as.numeric(pval)
        results_table[rn, "Berkowitz_pvalue"] <- round(pval, 8)
      }
    }
    
    return(results_table)
  }
  
  ######################## Test de difference of the predictive density ########
  
  add_ag_density_test <- function(results_table,
                                  pred_var_table,
                                  pred_var_table_bench,
                                  Euro_Countries_GDP_Growth_Log,
                                  country      = "Eurozone",
                                  observed_col = "gdp_growth_log_wins_001",
                                  # column names
                                  country_col  = "Country",
                                  end_col      = "End_Estimation_Set",
                                  horizon_col  = "Horizon",
                                  mean_col     = "forecast_mean",
                                  var_col      = "forecast_variance",
                                  obs_q_col    = "Quarter",
                                  # behavior
                                  two_sided    = TRUE,
                                  verbose      = TRUE,
                                  # treat "near zero variance" as zero
                                  sd_epsilon   = 1e-10) {
    
    # ----------------------------
    # Helper: Newey–West long-run variance (Bartlett)
    # ----------------------------
    nw_lrv <- function(x, lag) {
      x <- as.numeric(x)
      x <- x[is.finite(x)]
      T <- length(x)
      if (T <= 1L) return(NA_real_)
      
      x_c <- x - mean(x)
      gamma0 <- sum(x_c^2) / T
      if (lag <= 0L) return(gamma0)
      
      lrv <- gamma0
      for (k in 1:lag) {
        gamma_k <- sum(x_c[(k + 1):T] * x_c[1:(T - k)]) / T
        w <- 1 - k / (lag + 1)
        lrv <- lrv + 2 * w * gamma_k
      }
      lrv
    }
    
    msg <- function(...) if (isTRUE(verbose)) message(...)
    
    # ----------------------------
    # 0) Build an integer quarter index everywhere to avoid float join problems
    #     q_index = round(Quarter * 4)
    #     target_index = end_index + Horizon
    # ----------------------------
    obs <- Euro_Countries_GDP_Growth_Log |>
      dplyr::transmute(
        Country  = .data[[country_col]],
        q_index  = as.integer(round(.data[[obs_q_col]] * 4)),
        y_obs    = as.numeric(.data[[observed_col]])
      ) |>
      dplyr::filter(Country == country)
    
    if (nrow(obs) == 0) {
      stop("No observed data found for country='", country, "' in Euro_Countries_GDP_Growth_Log.")
    }
    
    m <- pred_var_table |>
      dplyr::filter(.data[[country_col]] == country) |>
      dplyr::transmute(
        Country    = .data[[country_col]],
        end_index  = as.integer(round(.data[[end_col]] * 4)),
        Horizon    = as.integer(.data[[horizon_col]]),
        mu_m       = as.numeric(.data[[mean_col]]),
        var_m      = as.numeric(.data[[var_col]])
      )
    
    b <- pred_var_table_bench |>
      dplyr::filter(.data[[country_col]] == country) |>
      dplyr::transmute(
        Country    = .data[[country_col]],
        end_index  = as.integer(round(.data[[end_col]] * 4)),
        Horizon    = as.integer(.data[[horizon_col]]),
        mu_b       = as.numeric(.data[[mean_col]]),
        var_b      = as.numeric(.data[[var_col]])
      )
    
    if (nrow(m) == 0) stop("No rows found in pred_var_table for country='", country, "'.")
    if (nrow(b) == 0) stop("No rows found in pred_var_table_bench for country='", country, "'.")
    
    # ----------------------------
    # 1) Merge model and benchmark, attach realized y_{t+h}
    # ----------------------------
    df <- dplyr::inner_join(m, b, by = c("Country", "end_index", "Horizon")) |>
      dplyr::mutate(
        target_index = end_index + Horizon
      ) |>
      dplyr::left_join(obs, by = c("Country" = "Country", "target_index" = "q_index"))
    
    if (nrow(df) == 0) {
      stop("After merging pred_var_table and pred_var_table_bench, there are 0 matching rows. ",
           "Check keys (Country, End_Estimation_Set, Horizon).")
    }
    
    # quick global diagnostics
    if (isTRUE(verbose)) {
      total_rows <- nrow(df)
      n_yobs     <- sum(is.finite(df$y_obs))
      msg("[AG] Merged rows: ", total_rows, " | with observed y: ", n_yobs,
          " | missing y: ", total_rows - n_yobs)
    }
    
    # ----------------------------
    # 2) Compute log predictive densities (Gaussian) and differential d_t
    # ----------------------------
    df <- df |>
      dplyr::mutate(
        sd_m = dplyr::if_else(is.finite(var_m) & var_m > 0, sqrt(var_m), NA_real_),
        sd_b = dplyr::if_else(is.finite(var_b) & var_b > 0, sqrt(var_b), NA_real_),
        
        logf_m = dplyr::if_else(
          is.finite(y_obs) & is.finite(mu_m) & is.finite(sd_m) & sd_m > 0,
          stats::dnorm(y_obs, mean = mu_m, sd = sd_m, log = TRUE),
          NA_real_
        ),
        logf_b = dplyr::if_else(
          is.finite(y_obs) & is.finite(mu_b) & is.finite(sd_b) & sd_b > 0,
          stats::dnorm(y_obs, mean = mu_b, sd = sd_b, log = TRUE),
          NA_real_
        ),
        
        d_t = logf_m - logf_b
      )
    
    # ----------------------------
    # 3) Horizon-by-horizon: mean diff + HAC test (two-sided)
    # ----------------------------
    horizons <- sort(unique(df$Horizon))
    ag_out <- data.frame(
      Horizon         = horizons,
      AG_mean_logdiff = NA_real_,
      AG_pvalue       = NA_real_,
      stringsAsFactors = FALSE
    )
    
    for (h in horizons) {
      d_vec_all <- df$d_t[df$Horizon == h]
      d_vec <- d_vec_all[is.finite(d_vec_all)]
      T_h <- length(d_vec)
      
      # mean (if any)
      if (T_h >= 1) ag_out$AG_mean_logdiff[ag_out$Horizon == h] <- mean(d_vec)
      
      # detailed reasons for NA p-values
      if (T_h < 2) {
        msg("[AG] Horizon ", h, ": AG_pvalue = NA (valid d_t count < 2). ",
            "Valid=", T_h, " | Total=", sum(df$Horizon == h))
        next
      }
      
      sd_dt <- stats::sd(d_vec)
      if (!is.finite(sd_dt) || sd_dt <= sd_epsilon) {
        msg("[AG] Horizon ", h, ": AG_pvalue = NA (d_t variance ~ 0). ",
            "sd(d_t)=", signif(sd_dt, 6), " | mean(d_t)=", signif(mean(d_vec), 6),
            " | Valid=", T_h)
        next
      }
      
      # HAC lag: h-1 but cannot exceed T_h-1
      lag_h <- min(max(h - 1L, 0L), T_h - 1L)
      lrv_h <- nw_lrv(d_vec, lag = lag_h)
      
      if (!is.finite(lrv_h) || lrv_h <= 0) {
        msg("[AG] Horizon ", h, ": AG_pvalue = NA (HAC long-run variance not computable). ",
            "lag=", lag_h, " | lrv=", signif(lrv_h, 6), " | Valid=", T_h)
        next
      }
      
      se_mean <- sqrt(lrv_h / T_h)
      if (!is.finite(se_mean) || se_mean <= 0) {
        msg("[AG] Horizon ", h, ": AG_pvalue = NA (standard error not computable). ",
            "se_mean=", signif(se_mean, 6), " | Valid=", T_h)
        next
      }
      
      stat <- mean(d_vec) / se_mean
      
      pval <- if (isTRUE(two_sided)) {
        2 * (1 - stats::pnorm(abs(stat)))
      } else {
        # one-sided: alternative mean(d_vec) > 0 (model better)
        1 - stats::pnorm(stat)
      }
      
      ag_out$AG_pvalue[ag_out$Horizon == h] <- pval
      
      msg("[AG] Horizon ", h, ": mean=", signif(mean(d_vec), 6),
          " | stat=", signif(stat, 6),
          " | p=", signif(pval, 6),
          " | lag=", lag_h,
          " | Valid=", T_h)
    }
    
    # ----------------------------
    # 4) Write into results_table (rownames are horizons)
    # ----------------------------
    results_table$AG_mean_logdiff <- NA_real_
    results_table$AG_pvalue       <- NA_real_
    
    for (i in seq_len(nrow(ag_out))) {
      h <- ag_out$Horizon[i]
      rn <- as.character(h)
      if (rn %in% rownames(results_table)) {
        results_table[rn, "AG_mean_logdiff"] <- ag_out$AG_mean_logdiff[i]
        results_table[rn, "AG_pvalue"]       <- ag_out$AG_pvalue[i]
      } else {
        msg("[AG] Note: Horizon ", h, " not found in results_table rownames. Skipping write.")
      }
    }
    
    # ----------------------------
    # 5) Extra global warnings to help debugging
    # ----------------------------
    if (isTRUE(verbose)) {
      # missing y
      miss_y <- df |> dplyr::summarise(miss = sum(!is.finite(y_obs)), tot = dplyr::n())
      if (miss_y$miss > 0) {
        msg("[AG] Warning: ", miss_y$miss, "/", miss_y$tot,
            " merged rows have missing observed y (likely outside sample or join mismatch).")
      }
      
      # invalid variances
      bad_vm <- sum(!is.finite(df$var_m) | df$var_m <= 0)
      bad_vb <- sum(!is.finite(df$var_b) | df$var_b <= 0)
      if (bad_vm > 0 || bad_vb > 0) {
        msg("[AG] Warning: invalid predictive variances found. ",
            "bad var_m=", bad_vm, " | bad var_b=", bad_vb,
            ". These rows cannot produce log densities.")
      }
      
      # how many d_t are usable overall
      n_dt <- sum(is.finite(df$d_t))
      msg("[AG] Overall usable d_t: ", n_dt, " / ", nrow(df))
    }
    
    return(results_table)
  }
  
  
  
  ################################################################################
  # Function to do a plots
  ################################################################################
  
  ######################## Plot the forecast and the intervals #################
  
  plot_real_growth_forecast_ci <- function(annual_growth_all,
                                           annual_growth_observed,
                                           country = "Eurozone",
                                           year_from = 2022) {
    stopifnot(is.data.frame(annual_growth_all), is.data.frame(annual_growth_observed))
    stopifnot(all(c("Country", "type") %in% names(annual_growth_all)))
    stopifnot(all(c("Country", "type") %in% names(annual_growth_observed)))
    
    # Helper: detect year columns like "2000", "2025", etc.
    get_year_cols <- function(df) grep("^[0-9]{4}$", names(df), value = TRUE)
    
    obs_year_cols <- get_year_cols(annual_growth_observed)
    fc_year_cols  <- get_year_cols(annual_growth_all)
    
    if (length(obs_year_cols) == 0) stop("annual_growth_observed has no year columns like '2024'.")
    if (length(fc_year_cols)  == 0) stop("annual_growth_all has no year columns like '2025'.")
    
    # ---------- Observed series (blue): real_growth up to 2024 ----------
    obs_long <- annual_growth_observed %>%
      dplyr::filter(Country == country, type == "real_growth") %>%
      tidyr::pivot_longer(cols = dplyr::all_of(obs_year_cols),
                          names_to = "year", values_to = "value") %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::filter(year >= year_from, year <= 2024) %>%
      dplyr::arrange(year)
    
    if (nrow(obs_long) == 0) {
      stop("No observed real_growth data found for the requested period/year_from.")
    }
    
    last_obs_year <- max(obs_long$year, na.rm = TRUE)
    
    # ---------- Forecast series (red): real_growth + q10/q90 from annual_growth_all ----------
    fc_mean <- annual_growth_all %>%
      dplyr::filter(Country == country, type == "real_growth") %>%
      tidyr::pivot_longer(cols = dplyr::all_of(fc_year_cols),
                          names_to = "year", values_to = "mean") %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::arrange(year)
    
    fc_q10 <- annual_growth_all %>%
      dplyr::filter(Country == country, type == "real_growth_q10") %>%
      tidyr::pivot_longer(cols = dplyr::all_of(fc_year_cols),
                          names_to = "year", values_to = "q10") %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::arrange(year)
    
    fc_q90 <- annual_growth_all %>%
      dplyr::filter(Country == country, type == "real_growth_q90") %>%
      tidyr::pivot_longer(cols = dplyr::all_of(fc_year_cols),
                          names_to = "year", values_to = "q90") %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::arrange(year)
    
    if (nrow(fc_mean) == 0) stop("No forecast real_growth row found in annual_growth_all.")
    if (nrow(fc_q10)  == 0) stop("No forecast real_growth_q10 row found in annual_growth_all.")
    if (nrow(fc_q90)  == 0) stop("No forecast real_growth_q90 row found in annual_growth_all.")
    
    # Merge forecast mean + quantiles
    fc_long <- fc_mean %>%
      dplyr::left_join(fc_q10, by = "year") %>%
      dplyr::left_join(fc_q90, by = "year")
    
    # Keep only forecast years >= (last_obs_year + 1)
    fc_only <- fc_long %>%
      dplyr::filter(year >= last_obs_year + 1L)
    
    # Link point at 2024: set q10=q90=observed value so ribbon starts in 2024
    link_point <- obs_long %>%
      dplyr::filter(year == last_obs_year) %>%
      dplyr::transmute(
        year,
        mean = value,
        q10  = value,
        q90  = value
      )
    
    # Forecast line includes the 2024 link point
    fc_for_line <- dplyr::bind_rows(link_point, fc_only) %>%
      dplyr::arrange(year)
    
    # Ribbon also includes the 2024 link point
    fc_for_ribbon <- dplyr::bind_rows(link_point, fc_only) %>%
      dplyr::arrange(year)
    
    # ---------- Plot ----------
    ggplot2::ggplot() +
      # Observed (blue)
      ggplot2::geom_line(
        data = obs_long,
        ggplot2::aes(x = year, y = value),
        color = "blue",
        linewidth = 1
      ) +
      # Forecast CI (red ribbon)
      ggplot2::geom_ribbon(
        data = fc_for_ribbon,
        ggplot2::aes(x = year, ymin = q10, ymax = q90),
        fill = "red",
        alpha = 0.15
      ) +
      # Forecast mean (red), including link from 2024 to 2025
      ggplot2::geom_line(
        data = fc_for_line,
        ggplot2::aes(x = year, y = mean),
        color = "red",
        linewidth = 1
      ) +
      ggplot2::labs(
        title = paste0(country, " – Annual Real GDP Growth: Observed vs Forecast"),
        x = "Year",
        y = "Real GDP growth (%)"
      ) +
      ggplot2::theme_minimal()
  }
  
