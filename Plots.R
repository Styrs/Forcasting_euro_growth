## Load the libraries
library(readxl)
library(ggplot2)







## ----------------------------------------------------------------------¨
## Import the data
## ----------------------------------------------------------------------


########## Unseazonalized data ########################
url_euro_growth_gdp <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Euro_Countries_GDP_Growth_Log.xlsx"
download.file(url_euro_growth_gdp, destfile = "data.xlsx", mode = "wb")
Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx")
Euro_Countries_GDP_Growth_Log <- read_excel("data.xlsx") %>%
  filter(Country != "Poland")   # <-- remove Poland here


url_eurozone_growth_gdp <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_agregated_GDPgrowth.xlsx"
download.file(url_eurozone_growth_gdp, destfile = "data.xlsx", mode = "wb")
Eurozone_agregated_GDPgrowth <- read_excel("data.xlsx")



########## Seazonalized data ##########################3
url_data_countries_euro_growth_gdp_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/data_countries_euro_growth_gdp_seas.xlsx"
download.file(url_data_countries_euro_growth_gdp_seas, destfile = "data.xlsx", mode = "wb")
data_countries_euro_growth_gdp_seas <- read_excel("data.xlsx")

url_Eurozone_GDPgrowth_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_GDPgrowth_seas.xlsx"
download.file(url_Eurozone_GDPgrowth_seas, destfile = "data.xlsx", mode = "wb")
Eurozone_GDPgrowth_seas <- read_excel("data.xlsx")

## -----------------------------------------------------------------------
## do the plots 
## -----------------------------------------------------------------------


########## Unseazonalized data ########################


ggplot(Euro_Countries_GDP_Growth_Log, 
       aes(x = Quarter, y = gdp_growth_qoq_log, color = Country, group = Country)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Quarterly GDP Growth by Country",
    x = "Quarter",
    y = "GDP growth (%)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



ggplot(Eurozone_agregated_GDPgrowth, 
       aes(x = Quarter, y = log_GDP_growth)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Quarterly GDP Growth for Eurozone",
    x = "Quarter",
    y = "GDP growth (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



########## Seazonalized data ##########################


ggplot(data_countries_euro_growth_gdp_seas, 
       aes(x = Quarter, y = seasonal_adjusted, color = Country, group = Country)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Quarterly seas GDP Growth by Country",
    x = "Quarter",
    y = "GDP growth (%)",
    color = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



ggplot(Eurozone_GDPgrowth_seas, 
       aes(x = Quarter, y = seasonal_adjusted)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Quarterly GDP Growth for Eurozone",
    x = "Quarter",
    y = "GDP growth (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



