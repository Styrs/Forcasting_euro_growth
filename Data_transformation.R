

library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(seasonal)
library(writexl)

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

#######################################################################
## 3 DÉSAISONNALISATION DES NOMINAL_GDP AVEC X-13-ARIMA-SEATS
#######################################################################



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


#######################################################################
## 5. winsorizing
#######################################################################

Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log %>%
  mutate(
    gdp_growth_log_wins = winsorize(gdp_growth_log)
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



#######################################################################
## 7. Add true growth
#######################################################################


Euro_Countries_GDP_Growth_Log <- Euro_Countries_GDP_Growth_Log |>
  dplyr::mutate(true_growth = (exp(gdp_growth_log_wins/100) - 1)*100 )


#######################################################################
## 8. Create a annual obsreved growth rate table 
#######################################################################


annual_growth_observed <- Euro_Countries_GDP_Growth_Log %>%
  # Make sure Quarter and true_growth are numeric
  mutate(
    Quarter = as.numeric(as.character(Quarter)),
    true_growth = as.numeric(as.character(true_growth)),
    year = floor(Quarter)
  ) %>%
  
  # Group by country and year
  group_by(Country, year) %>%
  
  # Keep only full years
  filter(n() == 4, all(!is.na(true_growth))) %>%
  
  # Annual compounded growth
  summarise(
    annual_growth = (prod(1 + true_growth / 100) - 1) * 100,
    .groups = "drop"
  ) %>%
  
  # Pivot to wide format
  mutate(year = as.character(year)) %>%
  pivot_wider(
    names_from = year,
    values_from = annual_growth
  )





#######################################################################
## 9 Save the transformed datasets
#######################################################################


write_xlsx(Euro_Countries_GDP_Growth_Log,
           "Euro_Countries_GDP_Growth_Log.xlsx")

write_xlsx(annual_growth_observed,"annual_growth_observed.xlsx")
           