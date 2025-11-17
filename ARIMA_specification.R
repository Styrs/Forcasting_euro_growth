library(dplyr)
library(forecast)
library(purrr)
library(tidyr)
library(readxl)


## ======================================================================
## 1. Import the data
## ======================================================================



url_euro_growth_gdp_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/data_countries_euro_growth_gdp_seas.xlsx"

download.file(url_euro_growth_gdp_seas, destfile = "data.xlsx", mode = "wb")

data_countries_euro_growth_gdp_seas <- read_excel("data.xlsx")



# keep only non-missing growth, and ensure time order within each country
#Normally the data should be clear before that but we never know, security check 
data_clean <- data_countries_euro_growth_gdp_seas %>%
  filter(!is.na(seasonal_adjusted)) %>%
  arrange(Country, Quarter)



## ======================================================================
## 2. find the best ARMA q and p for each country using BIC:
## ======================================================================


# Find best ARMA(p,q) per country using BIC (seasonal = FALSE because already adjusted)
arma_orders_BIC <- data_clean %>%
  group_by(Country) %>%
  summarise({
    fit <- auto.arima(seasonal_adjusted,
                      seasonal = FALSE,
                      ic = "bic",
                      stepwise = FALSE,
                      approximation = FALSE)
    ord <- arimaorder(fit)               # names are "p","d","q",...
    tibble(p = unname(ord["p"]),
           q = unname(ord["q"]))
  }, .groups = "drop")

arma_orders_BIC


## ======================================================================
## 3. Find the best ARMA q and p for each country using AIC:
## ======================================================================

arma_orders_AIC <- data_clean %>%
  group_by(Country) %>%
  summarise({
    fit <- auto.arima(seasonal_adjusted,
                      seasonal = FALSE,  # data already seasonally adjusted
                      ic = "aic",        # use AIC criterion this time
                      stepwise = FALSE,
                      approximation = FALSE)
    ord <- arimaorder(fit)
    tibble(p = unname(ord["p"]),
           q = unname(ord["q"]))
  }, .groups = "drop")

arma_orders_AIC
