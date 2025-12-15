

library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(seasonal)
library(writexl)
library(scales)

Create_the_data_sets <- function (){

  #######################################################################
  ## 1. We import the data
  #######################################################################
  
  url_euro_bigcountries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Countries_Excel_euro_GDP.xlsx"
  download.file(url_euro_bigcountries, destfile = "data.xlsx", mode = "wb")
  data_countries_bigeuro <- read_excel("data.xlsx")
  
  # Remove Poland
  data_countries_bigeuro <- data_countries_bigeuro %>%
    filter(TIME != "Poland")
  
  url_euro_smallcountries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Data_GDP_SmallEuroCountries.xlsx"
  download.file(url_euro_smallcountries, destfile = "data.xlsx", mode = "wb")
  data_countries_Smalleuro <- read_excel("data.xlsx")
  
  
  url_euro_deflator <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/2005_linked_deflator.xlsx"
  download.file(url_euro_deflator, destfile = "data.xlsx", mode = "wb")
  delfator_2005_linked <- read_excel("data.xlsx")
  
  
  #######################################################################
  ## 2. Organizing the data, columns
  #######################################################################
  
  ############## 2.1 Transformation for big countries raw data ##########
  
  data_countries_bigeuro <- data_countries_bigeuro %>%
    rename(Country = TIME) %>%            # rename first column
    pivot_longer(
      cols      = -Country,               # all other columns are quarters
      names_to  = "Quarter",
      values_to = "Nominal_GDP"
    ) %>%
    mutate(
      Quarter = as.yearqtr(Quarter, format = "%Y-Q%q")
    )
  
  ############## 2.2 Transformation for small countries raw data ########
  
  data_smalleuro_prepared <- data_countries_Smalleuro %>%
    # Remove the unwanted period first
    filter(TIME_PERIOD != "2025-Q3") %>%
    rename(
      Country     = geo,
      Quarter     = TIME_PERIOD,
      Nominal_GDP = OBS_VALUE
    ) %>%
    # Normalize quarter strings then convert to yearqtr
    mutate(
      Quarter = gsub("-", " ", Quarter),          # "2000-Q1" → "2000 Q1"
      Quarter = as.yearqtr(Quarter, "%Y Q%q")
    )
  
  # Aggregate small countries into one "Sum Small euro countries"
  data_smalleuro_sum <- data_smalleuro_prepared %>%
    group_by(Quarter) %>%
    summarise(
      Nominal_GDP = sum(Nominal_GDP, na.rm = TRUE)
    ) %>%
    mutate(Country = "Sum Small euro countries") %>%  # label this aggregate
    select(Country, Quarter, Nominal_GDP)
  
  ######### 2.3 Combine big + small countries ###########################
  
  Euro_Countries_GDP_Growth_Log <- bind_rows(
    data_countries_bigeuro,
    data_smalleuro_sum
  ) %>%
    arrange(Country, Quarter)
  
  ######### 2.4 Transform the quarters in the right format ##############
  
  
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    arrange(Country, Quarter) %>%
    mutate(
      Quarter = {
        year    <- as.numeric(format(Quarter, "%Y"))
        q_index <- as.numeric(format(Quarter, "%q"))
        year + (q_index - 1) * 0.25
      }
    )
  
  ######### 2.5 Prepare the deflator table to bo added ####################
  
  delfator_2005_linked <- delfator_2005_linked %>%
    pivot_longer(
      cols      = -Country,
      names_to  = "Quarter",
      values_to = "deflator_2005_index"
    ) %>%
    mutate(
      Quarter = gsub("-", " ", Quarter),
      Quarter = as.yearqtr(Quarter, "%Y Q%q"),
      # convert to decimal format to match Euro_Countries_GDP_Growth_Log
      Quarter = {
        year    <- as.numeric(format(Quarter, "%Y"))
        q_index <- as.numeric(format(Quarter, "%q"))
        year + (q_index - 1) * 0.25
      }
    )
  
  
  
  #######################################################################
  ## 3 DÉSAISONNALISATION DES NOMINAL_GDP AVEC X-13-ARIMA-SEATS
  #######################################################################
  
  
  # désaisonaliser le deflateur
  delfator_2005_linked <- delfator_2005_linked %>%
    group_by(Country) %>%
    arrange(Country, Quarter) %>%
    mutate(
      deflator_2005_index_seas = deseasonalize_series(deflator_2005_index, Quarter)
    ) %>%
    ungroup()
  
  # Appliquer la désaisonnalisation à chaque "pays" (grands + Sum Small)
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    group_by(Country) %>%
    arrange(Country, Quarter) %>%
    mutate(
      Nominal_GDP_seas = deseasonalize_series(Nominal_GDP, Quarter)
    ) %>%
    ungroup()
  
  #######################################################################
  ## 4 Creation of the observed aggregated eurozone nominal GDP
  ##     (sum of national GDP)
  #######################################################################
  
  Eurozone_rows <- Euro_Countries_GDP_Growth_Log %>%
    group_by(Quarter) %>%
    summarise(
      Country               = "Eurozone",
      Nominal_GDP           = sum(Nominal_GDP, na.rm = TRUE),
      Nominal_GDP_seas      = sum(Nominal_GDP_seas, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Quarter)
  
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    bind_rows(Eurozone_rows) %>%
    arrange(Country, Quarter)
  
  
  ################### Add the deflator to the table #######################
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    left_join(delfator_2005_linked, by = c("Country", "Quarter"))
  
  
  #######################################################################
  ## 5. Computing the log-diff to have the GDP growth
  #######################################################################
  
  ################### 5.1 LogGDP for euro countries #####################
  
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    arrange(Country, Quarter) %>%
    group_by(Country) %>%
    mutate(
      # Safely take logs: NA if GDP <= 0
      log_gdp = ifelse(Nominal_GDP_seas > 0, log(Nominal_GDP_seas), NA_real_),
      
      # Quarter-over-Quarter log growth: 100 * Δlog
      gdp_growth_log = 100 * (log_gdp - lag(log_gdp))
      
    ) %>%
    ungroup()
  
  ################### 5.2 LogGDP for deflator #####################
  
  
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    arrange(Country, Quarter) %>%
    group_by(Country) %>%
    mutate(
      # Safely take logs: NA if deflator <= 0
      deflator_log_seas = ifelse(deflator_2005_index_seas > 0, log(deflator_2005_index_seas), NA_real_),
      
      # Quarter-over-Quarter deflator: 100 * Δlog
      deflator_logdiff = 100 * (deflator_log_seas - lag(deflator_log_seas))
      
    ) %>%
    ungroup()
  
  #######################################################################
  ## 5. winsorizing
  #######################################################################
  
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
    mutate(
      gdp_growth_log_wins_001 = winsorize(gdp_growth_log, probs = c(0.01, 0.99)),
      gdp_growth_log_wins_005 = winsorize(gdp_growth_log, probs = c(0.05, 0.95))
    )
  
  
  
  
  #######################################################################
  ## 6. To ADF tests
  #######################################################################
  
  
  adf_results_countries <- run_adf_test(
    data = Euro_Countries_GDP_Growth_Log,
    value_col = "gdp_growth_log",
    group_col = "Country",
    time_col = "Quarter"
  )
  
  adf_results_countries_deflator <- run_adf_test(
    data = Euro_Countries_GDP_Growth_Log,
    value_col = "deflator_2005_index_seas",
    group_col = "Country",
    time_col = "Quarter"
  )
  
  
  #######################################################################
  ## 7. Add true growth
  #######################################################################
  
  
  Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log |>
    dplyr::mutate(nominal_growth = (exp(gdp_growth_log/100) - 1)*100 )
  
  
  #######################################################################
  ## 8. Create a annual obsreved growth rate table 
  #######################################################################
  
  ############# 8.1 Add the annual nominal  to the table #########
  
  annual_growth_observed <- Euro_Countries_GDP_Growth_Log %>%
    mutate(
      Quarter = as.numeric(as.character(Quarter)),
      year    = floor(Quarter)
    ) %>%
    group_by(Country, year) %>%
    # only keep full years with 4 quarters and non-NA Nominal_GDP
    filter(n() == 4, all(!is.na(Nominal_GDP))) %>%
    summarise(
      annual_nominal = sum(Nominal_GDP),
      .groups = "drop"
    ) %>%
    mutate(
      year = as.character(year),
      type = "nominal_gdp"      # label for this kind of series
    ) %>%
    pivot_wider(
      id_cols    = c(Country, type),
      names_from = year,
      values_from = annual_nominal
    )
  
  ############# 8.2 Add the annual deflator to the table #################
  
  
  annual_deflator <- Euro_Countries_GDP_Growth_Log %>%
    mutate(
      Quarter = as.numeric(as.character(Quarter)),
      year    = floor(Quarter)
    ) %>%
    group_by(Country, year) %>%
    # only full years with 4 quarters and non-missing deflators
    filter(n() == 4, all(!is.na(deflator_2005_index))) %>%
    summarise(
      annual_deflator = mean(deflator_2005_index),
      .groups = "drop"
    ) %>%
    mutate(
      year = as.character(year),
      type = "deflator"
    ) %>%
    pivot_wider(
      id_cols    = c(Country, type),
      names_from = year,
      values_from = annual_deflator
    )
  
  annual_growth_observed <- annual_growth_observed %>%
    bind_rows(annual_deflator) %>%
    arrange(Country, type) %>%
    relocate(`2000`, .after = type)
    
  # --- copy Eurozone deflator to "Sum Small euro countries" ---
  
  # identify year columns (2000, 2001, 2002, ...)
  year_cols <- grep("^[0-9]{4}$", names(annual_growth_observed), value = TRUE)
  
  # take the Eurozone deflator row
  euro_deflator_row <- annual_growth_observed %>%
    filter(Country == "Eurozone", type == "deflator")
  
  # create the "Sum Small euro countries" deflator row by copying Eurozone
  sum_small_deflator_row <- euro_deflator_row %>%
    mutate(Country = "Sum Small euro countries")
  
  # add it to the table (if not already there) and keep things ordered
  annual_growth_observed <- annual_growth_observed %>%
    bind_rows(sum_small_deflator_row) %>%
    arrange(Country, type)
  
  ############# 8.3 Add the annual nominal growths to the table ################
  
  annual_growth_observed <- compute_nominal_gdp_growth(annual_growth_observed)
  
  
  
  ############# Add the real GDP and growth value to the table ##########
  
  annual_growth_observed <- add_real_gdp(annual_growth_observed)
  
  annual_growth_observed <- compute_real_gdp_growth(annual_growth_observed)
  
  annual_growth_observed_display <- annual_growth_observed %>%
    mutate(across(matches("^[0-9]{4}$"), ~ format(., scientific = FALSE)))

  
  
  return(list(
    Euro_Countries_GDP_Growth_Log  = Euro_Countries_GDP_Growth_Log,
    annual_growth_observed         = annual_growth_observed,
    annual_growth_observed_display = annual_growth_observed_display
  ))
  
}

           
