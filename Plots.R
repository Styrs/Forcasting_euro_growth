## Load the libraries
library(readxl)
library(ggplot2)







## ----------------------------------------------------------------------¨
## Import the data
## ----------------------------------------------------------------------



url_euro_growth_gdp <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/data_countries_euro_Growth_GDP_unseasonnalized.xlsx"

download.file(url_euro_growth_gdp, destfile = "data.xlsx", mode = "wb")

data_countries_euro_growth_gdp <- read_excel("data.xlsx")





## -----------------------------------------------------------------------
## do the plots 
## -----------------------------------------------------------------------


#plots of the growth of all the countries:


ggplot(data_countries_euro_growth_gdp, 
       aes(x = Quarter, y = gdp_growth, color = Country, group = Country)) +
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



