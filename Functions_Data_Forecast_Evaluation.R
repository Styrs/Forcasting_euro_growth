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
  
  add_real_gdp <- function(annual_growth_observed) {
    
    # ---- 1. Long format ----
    annual_long <- annual_growth_observed %>%
      pivot_longer(
        cols      = -c(Country, type),
        names_to  = "year",
        values_to = "value"
      )
    
    # ---- 2. Compute annual real GDP: nominal_gdp * 100 / deflator ----
    real_gdp_df <- annual_long %>%
      filter(type %in% c("nominal_gdp", "deflator")) %>%
      pivot_wider(
        id_cols    = c(Country, year),
        names_from = type,
        values_from = value
      ) %>%
      mutate(
        real_gdp = ifelse(
          is.na(nominal_gdp) | is.na(deflator),
          NA_real_,
          nominal_gdp * 100 / deflator
        )
      )
    
    # ---- 3. Turn real GDP into wide "type = real_gdp" rows ----
    real_gdp_rows <- real_gdp_df %>%
      select(Country, year, real_gdp) %>%
      mutate(type = "real_gdp") %>%
      pivot_wider(
        id_cols    = c(Country, type),
        names_from = year,
        values_from = real_gdp
      )
    
    # ---- 4. Bind original data + real GDP rows ----
    out <- annual_growth_observed %>%
      bind_rows(real_gdp_rows) %>%
      arrange(Country, type)
    
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
  
  
  
  select_best_arma <- function(ts_data, max_p = 5, max_q = 5,
                               criterion = c("AIC", "BIC")) {
    
    # Match the criterion argument and standardize
    criterion <- match.arg(criterion)
    
    best_score <- Inf
    best_model <- NULL
    
    for (p in 0:max_p) {
      for (q in 0:max_q) {
        
        # Skip ARMA(0,0)
        if (p == 0 && q == 0) next
        
        tryCatch({
          candidate_model <- Arima(ts_data, order = c(p, 0, q))
          
          # Compute the chosen information criterion
          candidate_score <- if (criterion == "AIC") {
            AIC(candidate_model)
          } else {
            BIC(candidate_model)
          }
          
          # Keep the model if it is better
          if (candidate_score < best_score) {
            best_score <- candidate_score
            best_model <- candidate_model
          }
          
        }, error = function(e) {
          # silently skip problematic models
        })
        
      }
    }
    
    if (is.null(best_model)) {
      stop("No ARMA model could be estimated for this time series.")
    }
    
    return(list(
      model = best_model,
      order = arimaorder(best_model),
      criterion = criterion,
      score = best_score
    ))
  }
  
  ######################## Choose manual Arma specification ####################
  
  fit_fixed_arma <- function(ts_data, p, q, 
                             criterion = c("AIC", "BIC")) {
    
    criterion <- match.arg(criterion)
    
    # Estimate the forced ARMA(p, q) model
    best_model <- Arima(ts_data, order = c(p, 0, q))
    
    # Compute the requested information criterion
    best_score <- if (criterion == "AIC") {
      AIC(best_model)
    } else {
      BIC(best_model)
    }
    
    # Return in the same format as select_best_arma()
    return(list(
      model     = best_model,
      order     = arimaorder(best_model),
      criterion = criterion,
      score     = best_score
    ))
  }
  
  
  
  ######################## Compute forecast growth ###############################
  
  
  forecast_arma_model <- function(model, start_quarter, end_quarter, horizons) {
    
    # 1) Forecast growth with the ARMA model
    fc <- forecast(model, h = max(horizons))
    fc_values <- fc$mean[horizons]
    
    # 2) Compute numeric quarter values (same format as your input data)
    # Example: 2025.25, 2025.50, 2025.75, ...
    forecast_quarters <- end_quarter + 0.25 * horizons
    
    # 3) Return a structured list
    return(list(
      forecast_values = as.numeric(fc_values),
      forecast_dates  = forecast_quarters,   # numeric, not "YYYY-Qx"
      forecast_object = fc
    ))
  }
  
  
  ######################## Transform log-diff forecast into nominal forecast #####
  
  
  
  add_forecast_nominal <- function(forecast_data,
                                   observed_data,
                                   MultiHorizonSets = TRUE) {
    
    if (MultiHorizonSets) {
      ## ----- Case 1: End_Estimation_Set exists -----
      # Match End_Estimation_Set with the base GDP to get starting level
      
      start_levels <- observed_data %>%
        transmute(
          Country,
          End_Estimation_Set = Quarter,
          start_level = Nominal_GDP_seas
        )
      
      modified_data <- forecast_data %>%
        left_join(start_levels,
                  by = c("Country", "End_Estimation_Set")) %>%
        group_by(Country, End_Estimation_Set) %>%
        arrange(Horizon, .by_group = TRUE) %>%
        mutate(
          growth_factor = exp(Forecast_Growth_logdiff / 100),
          Forecast_Nominal_GDP = start_level * cumprod(growth_factor)
        ) %>%
        ungroup() %>%
        select(-start_level, -growth_factor)
      
    } else {
      ## ----- Case 2: no End_Estimation_Set -----
      # Horizon == 1 starts at quarter Q1, origin quarter is Q0 = Q1 - 0.25.
      # In general: Q0 = Forecasted_Quarter - Horizon * 0.25
      
      start_levels <- observed_data %>%
        transmute(
          Country,
          last_observed_quarter = Quarter,
          start_level = Nominal_GDP_seas
        )
      
      modified_data <- forecast_data %>%
        mutate(
          last_observed_quarter = Forecasted_Quarter - Horizon * 0.25
        ) %>%
        left_join(start_levels,
                  by = c("Country", "last_observed_quarter")) %>%
        group_by(Country, last_observed_quarter) %>%
        arrange(Horizon, .by_group = TRUE) %>%
        mutate(
          growth_factor = exp(Forecast_Growth_logdiff / 100),
          Forecast_Nominal_GDP = start_level * cumprod(growth_factor)
        ) %>%
        ungroup() %>%
        select(-start_level, -growth_factor)
    }
    
    return(modified_data)
  }
  
  
  ######################## Compute eurozone log-diff #############################
  
  
  compute_eurozone_logdiff <- function(forecast_df,
                                       observed_df,
                                       MultiHorizonSets = TRUE,
                                       euro_name = "Eurozone") {
    
    # Observed data for Eurozone only
    obs_euro <- observed_df %>%
      filter(Country == euro_name) %>%
      select(Country, Quarter, Nominal_GDP_seas)
    
    if (MultiHorizonSets) {
      # ---- Case 1: dataset has End_Estimation_Set ----
      
      euro_forecast <- forecast_df %>%
        filter(Country == euro_name) %>%
        # attach observed nominal GDP at End_Estimation_Set quarter
        left_join(
          obs_euro,
          by = c("Country", "End_Estimation_Set" = "Quarter"),
          suffix = c("", ".obs")   # keep only forecast version
        ) %>%
        arrange(End_Estimation_Set, Horizon) %>%
        group_by(Country, End_Estimation_Set) %>%
        mutate(
          base_nominal = dplyr::case_when(
            Horizon == 1L ~ Nominal_GDP_seas,          # use observed value
            TRUE         ~ lag(Forecast_Nominal_GDP)   # use previous forecast
          ),
          Forecast_Growth_logdiff =
            (log(Forecast_Nominal_GDP) - log(base_nominal))*100
        ) %>%
        select(-Nominal_GDP_seas, -base_nominal) %>%
        ungroup()
      
    } else {
      # ---- Case 2: dataset has last_observed_quarter ----
      
      euro_forecast <- forecast_df %>%
        filter(Country == euro_name) %>%
        # attach observed nominal GDP at last_observed_quarter
        left_join(
          obs_euro,
          by = c("Country", "last_observed_quarter" = "Quarter"),
          suffix = c("", ".obs")   # keep only forecast version
        ) %>%
        arrange(last_observed_quarter, Horizon) %>%
        group_by(Country, last_observed_quarter) %>%
        mutate(
          base_nominal = dplyr::case_when(
            Horizon == 1L ~ Nominal_GDP_seas,
            TRUE         ~ lag(Forecast_Nominal_GDP)
          ),
          Forecast_Growth_logdiff =
            (log(Forecast_Nominal_GDP) - log(base_nominal))*100
        ) %>%
        select(-Nominal_GDP_seas, -base_nominal) %>%
        ungroup()
    }
    
    # Put Eurozone back with the other countries and return
    forecast_updated <- forecast_df %>%
      filter(Country != euro_name) %>%
      bind_rows(euro_forecast) %>%
      # optional: keep original column order
      select(all_of(names(forecast_df)))
    
    return(forecast_updated)
  }
  
  
  
  ######################## Compute annual growth################################
  
  compute_annual_growth <- function(forecast_table_growth_countries,
                                    Euro_Countries_GDP_Growth_Log,
                                    rate_in_pourcent = TRUE) {
    
    # --- 1. Prepare forecast data ----
    forecast_q <- forecast_table_growth_countries %>%
      transmute(
        Country,
        quarter_num = Forecasted_Quarter,
        year = floor(Forecasted_Quarter),
        # Q1–Q4 from decimal part (.00, .25, .50, .75)
        quarter = as.integer(round((Forecasted_Quarter - year) * 4)) + 1L,
        growth = Forecast_Growth_rate,       # already in %
        source = "forecast"
      )
    
    # --- 2. Prepare observed data (only where forecast missing) ----
    # First, same year/quarter breakdown
    observed_q_all <- Euro_Countries_GDP_Growth_Log %>%
      transmute(
        Country,
        quarter_num = Quarter,
        year = floor(Quarter),
        quarter = as.integer(round((Quarter - year) * 4)) + 1L,
        growth = true_growth,
        source = "observed"
      )
    
    # We only care about years & countries that appear in the forecast table
    years_of_interest <- unique(forecast_q$year)
    countries_of_interest <- unique(forecast_q$Country)
    
    observed_q <- observed_q_all %>%
      filter(year %in% years_of_interest,
             Country %in% countries_of_interest)
    
    # Remove observed quarters that are already in the forecast data
    # (forecast should override observed for the same country-quarter)
    observed_q_missing <- observed_q %>%
      anti_join(
        forecast_q %>%
          select(Country, year, quarter),
        by = c("Country", "year", "quarter")
      )
    
    # --- 3. Combine forecast and observed quarters ----
    all_quarters <- bind_rows(forecast_q, observed_q_missing)
    
    # --- 4. Compute annual growth per Country-Year ----
    annual_growth <- all_quarters %>%
      group_by(Country, year) %>%
      # we expect 4 quarters; if not, result will be NA
      summarise(
        n_quarters = n(),
        annual_growth = {
          if (n_quarters < 4) {
            NA_real_
          } else {
            if (rate_in_pourcent) {
              # inputs in %, output in %
              factors <- 1 + growth / 100
              (prod(factors, na.rm = TRUE) - 1) * 100
            } else {
              # inputs in rates, output in rate
              factors <- 1 + growth
              prod(factors, na.rm = TRUE) - 1
            }
          }
        },
        .groups = "drop"
      )
    
    # --- 5. Pivot to wide format: one column per year ----
    result_wide <- annual_growth %>%
      select(-n_quarters) %>%
      # year as character so column names are "2025", "2026", ...
      mutate(year = as.character(year)) %>%
      pivot_wider(
        names_from = year,
        values_from = annual_growth
      ) %>%
      arrange(Country)
    
    return(result_wide)
  }
  

  
  
  
  ######################## compute_annual_nominal GDP #################
  
  compute_annual_nominal <- function(forecast_table_growth_countries,
                                          Euro_Countries_GDP_Growth_Log) {
    # 1) Forecast quarters: Country–Quarter–Year–Q_index–Nominal_Forecast
    forecast_q <- forecast_table_growth_countries %>%
      transmute(
        Country,
        Quarter = Forecasted_Quarter,
        year    = floor(Forecasted_Quarter),
        quarter = as.integer(round((Forecasted_Quarter - year) * 4)) + 1L,
        nominal_fc = Forecast_Nominal_GDP
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
    
    # 3) Combine forecast + observed and choose "best" nominal per quarter
    all_quarters <- observed_q %>%
      full_join(
        forecast_q,
        by = c("Country", "Quarter", "year", "quarter")
      ) %>%
      mutate(
        chosen_nominal = dplyr::case_when(
          !is.na(nominal_fc) ~ nominal_fc,       # use forecast if available
          TRUE               ~ nominal_obs       # otherwise fallback to observed
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
    
    # 5) Keep only forecast years (already ensured by semi_join), pivot wide
    annual_nominal_wide <- annual_nominal %>%
      select(-n_quarters) %>%
      mutate(year = as.character(year)) %>%
      pivot_wider(
        id_cols    = Country,
        names_from = year,
        values_from = annual_nominal
      ) %>%
      mutate(type = "nominal_gdp") %>%
      relocate(type, .after = Country) %>%
      arrange(Country)
    
    return(annual_nominal_wide)
  }
  
  ######################## Compute annual forecast nominal growth ##############
  
  
  compute_growth <- function(annual_growth_forecast,
                             annual_growth_observed,
                             which = c("nominal", "real")) {
    
    which <- match.arg(which)
    
    # choose the level & growth type depending on the mode
    if (which == "nominal") {
      level_type   <- "nominal_gdp"
      growth_type  <- "nominal_growth"
    } else { # "real"
      level_type   <- "real_gdp"
      growth_type  <- "real_growth"
    }
    
    # 1) Identify year columns in the forecast table
    year_cols_fc <- grep("^[0-9]{4}$", names(annual_growth_forecast), value = TRUE)
    
    # 2) Long format: forecast levels (nominal_gdp or real_gdp)
    fc_long <- annual_growth_forecast %>%
      dplyr::filter(type == level_type) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(year_cols_fc),
        names_to  = "year",
        values_to = "level_fc"
      )
    
    # 3) Long format: observed levels (all years available)
    year_cols_obs <- grep("^[0-9]{4}$", names(annual_growth_observed), value = TRUE)
    
    obs_long <- annual_growth_observed %>%
      dplyr::filter(type == level_type) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(year_cols_obs),
        names_to  = "year",
        values_to = "level_obs"
      )
    
    # 4) Combine observed + forecast levels (forecast overrides observed)
    combined_levels <- obs_long %>%
      dplyr::select(Country, year, level_obs) %>%
      dplyr::full_join(
        fc_long %>% dplyr::select(Country, year, level_fc),
        by = c("Country", "year")
      ) %>%
      dplyr::mutate(
        level = dplyr::if_else(!is.na(level_fc), level_fc, level_obs)
      ) %>%
      dplyr::select(Country, year, level)
    
    # 5) Compute growth from combined levels, then keep only forecast years
    years_forecast <- unique(fc_long$year)
    
    growth_long <- combined_levels %>%
      dplyr::mutate(year_num = as.integer(year)) %>%
      dplyr::arrange(Country, year_num) %>%
      dplyr::group_by(Country) %>%
      dplyr::mutate(
        level_prev = dplyr::lag(level),
        growth = (level / level_prev - 1) * 100
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year %in% years_forecast) %>%
      dplyr::select(Country, year, growth)
    
    # 6) Put growth in wide format, one row per country
    growth_wide <- growth_long %>%
      dplyr::mutate(type = growth_type) %>%
      tidyr::pivot_wider(
        id_cols    = c(Country, type),
        names_from = year,
        values_from = growth
      )
    
    # 7) Optional: drop old rows of that growth type to avoid duplicates
    out <- annual_growth_forecast %>%
      dplyr::filter(type != growth_type) %>%
      dplyr::bind_rows(growth_wide) %>%
      dplyr::arrange(Country, type)
    
    return(out)
  }
  
  
  
  
  
  
  ######################## Add deflator yearly forecast to annual ##############
  
  add_deflator_forecast_to_annual <- function(annual_growth_forecast,
                                              annual_deflator_forecast) {
    
    # 1) Convert annual_deflator_forecast to long form
    df_long <- annual_deflator_forecast %>%
      pivot_longer(
        cols      = -Country,
        names_to  = "year",
        values_to = "value"
      ) %>%
      mutate(
        type = "deflator"
      )
    
    # 2) Convert back to wide with type + years (same structure as annual_growth_forecast)
    deflator_rows <- df_long %>%
      pivot_wider(
        id_cols    = c(Country, type),
        names_from = year,
        values_from = value
      )
    
    # 3) Bind the new rows to the forecast table
    out <- annual_growth_forecast %>%
      bind_rows(deflator_rows) %>%
      arrange(Country, type)
    
    return(out)
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
                              new_col    = "pred_R2") {
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
  
  
  
  
  
