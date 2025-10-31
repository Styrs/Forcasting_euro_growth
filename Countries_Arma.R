## Load the libraries
library(readxl)
library(dplyr)
library(tseries)
library(dplyr)
library(forecast)
library(writexl)







## ======================================================================
## Import the data
## ======================================================================



  url_euro_growth_gdp <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/data_countries_euro_Growth_GDP_unseasonnalized.xlsx"
  
  download.file(url_euro_growth_gdp, destfile = "data.xlsx", mode = "wb")
  
  data_countries_euro_growth_gdp <- read_excel("data.xlsx")






## ======================================================================
## Do the Stationarity test on growth: ADF
## ======================================================================



  adf_results <- data_countries_euro_growth_gdp %>%
    group_by(Country) %>%
    arrange(Quarter, .by_group = TRUE) %>%
    reframe({
      x <- na.omit(gdp_growth)
      # automatic lag length (k ≈ n^(1/3)); you can set k manually if you prefer
      test <- tseries::adf.test(x, k = trunc(length(x)^(1/3)))
      tibble(
        n = length(x),
        adf_stat = unname(test$statistic),
        p_value = test$p.value
      )
    })
  
  adf_results


## ======================================================================
## Do the calendar and seasonnality test
## ======================================================================


#No calendar effect tested as the data are quaterly and seasonnal tests caputres calendar effect 


  data_countries_euro_growth_gdp_seas <- data_countries_euro_growth_gdp %>%
    group_by(Country) %>%
    arrange(Quarter, .by_group = TRUE) %>%
    mutate(
      gdp_growth_seas = {
        x <- na.omit(gdp_growth)
        ts_x <- ts(x, frequency = 4)
        # STL decomposition and seasonal adjustment
        fit <- stl(ts_x, s.window = "periodic")
        adj <- seasadj(fit)  # remove the seasonal component
        # match adjusted values back to original data length
        c(rep(NA, length(gdp_growth) - length(adj)), as.numeric(adj))
      }
    )






