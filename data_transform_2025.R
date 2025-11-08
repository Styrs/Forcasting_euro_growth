
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(writexl)

##---------------------------------------------------------------------
## We import the data
##---------------------------------------------------------------------

url_euro_countries <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Countries_Excel_euro_GDP.xlsx"

download.file(url_euro_countries, destfile = "data.xlsx", mode = "wb")

data_countries_euro <- read_excel("data.xlsx")

head(data_countries_euro)


##---------------------------------------------------------------------
## Organizing the data, column
##---------------------------------------------------------------------

data_countries_euro_prepared <- data_countries_euro %>%
  rename(Country = TIME) %>%            # rename first column
  pivot_longer(
    cols = -Country,                    # all other columns are quarters
    names_to = "Quarter",
    values_to = "Nominal_GDP"
  )



data_countries_euro_prepared <- data_countries_euro_prepared %>%
  mutate(Quarter = as.yearqtr(Quarter, format = "%Y-Q%q"))

##---------------------------------------------------------------------
## Computing the log-diff to have the GDP growth
##---------------------------------------------------------------------

Euro_Countries_GDP_Growth_Log <- data_countries_euro_prepared %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  # Safely take logs: NA if GDP <= 0
  mutate(
    log_gdp = ifelse(Nominal_GDP > 0, log(Nominal_GDP), NA_real_),
    # Quarter-over-Quarter log growth (≈ % change): 100 * Δlog
    gdp_growth_qoq_log = 100 * (log_gdp - lag(log_gdp)),

  ) %>%
  ungroup()


