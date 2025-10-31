
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
## Organizing the data, fist arrange the form and then havind the gdp growth
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


#Have it into gdp growth

data_countries_euro_Growth_GDP_unseasonnalized <- data_countries_euro_prepared 

data_countries_euro_Growth_GDP_unseasonnalized <- data_countries_euro_Growth_GDP_unseasonnalized %>%
  group_by(Country) %>%
  arrange(Quarter) %>%
  mutate(gdp_growth = (Nominal_GDP / lag(Nominal_GDP) - 1) * 100)

data_countries_euro_Growth_GDP_unseasonnalized <- data_countries_euro_Growth_GDP_unseasonnalized %>%
  arrange(Country, Quarter)

