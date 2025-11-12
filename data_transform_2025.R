
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(writexl)


#######################################################################






##---------------------------------------------------------------------
## We import the data
##---------------------------------------------------------------------
  
  url_euro_bigcountries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Countries_Excel_euro_GDP.xlsx"
  download.file(url_euro_bigcountries, destfile = "data.xlsx", mode = "wb")
  data_countries_bigeuro <- read_excel("data.xlsx")
  
  url_euro_smallcountries <-"https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Data_GDP_SmallEuroCountries.xlsx"
  download.file(url_euro_smallcountries, destfile = "data.xlsx", mode = "wb")
  data_countries_Smalleuro <- read_excel("data.xlsx")

##---------------------------------------------------------------------
## Organizing the data, column
##---------------------------------------------------------------------


##############Transformation for big countries raw data ####################################
  data_countries_euro_prepared <- data_countries_bigeuro %>%
    rename(Country = TIME) %>%            # rename first column
    pivot_longer(
      cols = -Country,                    # all other columns are quarters
      names_to = "Quarter",
      values_to = "Nominal_GDP"
    )
  
  
  
  data_countries_euro_prepared <- data_countries_euro_prepared %>%
    mutate(Quarter = as.yearqtr(Quarter, format = "%Y-Q%q"))





##############Transformation for small countries raw data ####################################

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


  data_smalleuro_sum <- data_smalleuro_prepared %>%
    group_by(Quarter) %>%
    summarise(
      Nominal_GDP = sum(Nominal_GDP, na.rm = TRUE)
    ) %>%
    mutate(Country = "Sum Small euro countries") %>%  # optional: label this aggregate
    select(Country, Quarter, Nominal_GDP)
  
  
  
  
  

######### Combine big + small countries ##################################################################

  data_Allcountries_euro_prepared <- bind_rows(
    data_countries_euro_prepared,
    data_smalleuro_sum
  ) %>%
    arrange(Country, Quarter)


##############Creation of the observed aggregated eurozone nominal GDP (sum of nationnal GDP) ####################################

  
  Eurozone_agregated_GDPgrowth <- data_Allcountries_euro_prepared %>%
    group_by(Quarter) %>%
    summarise(Eurozone_Nominal_GDP = sum(Nominal_GDP, na.rm = TRUE), .groups = "drop") %>%
    arrange(Quarter)
  
  Eurozone_agregated_GDPgrowth



##---------------------------------------------------------------------
## Computing the log-diff to have the GDP growth
##---------------------------------------------------------------------
  
###################LogGDP for euro coutries#####################################
  
  Euro_Countries_GDP_Growth_Log <- data_Allcountries_euro_prepared %>%
    arrange(Country, Quarter) %>%
    group_by(Country) %>%
    # Safely take logs: NA if GDP <= 0
    mutate(
      log_gdp = ifelse(Nominal_GDP > 0, log(Nominal_GDP), NA_real_),
      # Quarter-over-Quarter log growth (≈ % change): 100 * Δlog
      gdp_growth_qoq_log = 100 * (log_gdp - lag(log_gdp)),
  
    ) %>%
    ungroup()
  
##################LogGDP for aggregates Eurozone################################  

  Eurozone_agregated_GDPgrowth <- Eurozone_agregated_GDPgrowth %>%
    arrange(Quarter) %>%
    mutate(
      log_GDP_growth = log(Eurozone_Nominal_GDP) - log(lag(Eurozone_Nominal_GDP))
    )
  
  Eurozone_agregated_GDPgrowth
  
  
  




