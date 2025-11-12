# scripts/pipeline_github.R
# ============================================================
# Pipeline GitHub-friendly : BIG+SMALL → Δlog×100 → Visu → ADF → STL
# + Rolling window 2010 (2000–2009) et 2011→2027 (fenêtre 10 ans)
# - Chemins relatifs (data/, output/, figures/)
# - Télécharge BIG/SMALL depuis GitHub si absents en local
# ============================================================

suppressPackageStartupMessages({
  library(readxl); library(readr); library(dplyr); library(tidyr); library(stringr)
  library(zoo); library(ggplot2); library(tools); library(writexl)
  library(tseries); library(forecast); library(purrr)
})

# ─────────────────────────────────────────────────────────────
# Dossiers + horodatage
# ─────────────────────────────────────────────────────────────
dir.create("data",    showWarnings = FALSE, recursive = TRUE)
dir.create("output",  showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ─────────────────────────────────────────────────────────────
# Helper: download si besoin + lecteur universel
# ─────────────────────────────────────────────────────────────
safe_download <- function(url, dest){
  if (!file.exists(dest)) {
    ok <- TRUE
    tryCatch({ download.file(url, destfile = dest, mode = "wb", quiet = TRUE) },
             error = function(e){ ok <<- FALSE })
    if (!ok || !file.exists(dest) || file.size(dest) == 0) {
      stop("⚠️ Impossible de télécharger: ", url, "\nPlace le fichier localement à: ", dest)
    }
  }
  dest
}

read_any <- function(path){
  ext <- tolower(file_ext(path))
  if (!file.exists(path)) stop("Fichier introuvable: ", path)
  if (ext %in% c("xlsx","xls")) {
    readxl::read_excel(path)
  } else if (ext %in% c("csv","txt")) {
    df <- suppressWarnings(readr::read_delim(path, delim = ","))
    if (ncol(df) == 1) df <- suppressWarnings(readr::read_delim(path, delim = ";"))
    df
  } else if (ext == "rds") {
    readRDS(path)
  } else stop("Extension non gérée: ", ext)
}

# ============================================================
# 2) Chemins d’accès (version GitHub-friendly)
# ============================================================
url_big   <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Countries_Excel_euro_GDP.xlsx"
url_small <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Data_GDP_SmallEuroCountries.xlsx"
big_path   <- safe_download(url_big,   "data/Countries_Excel_euro_GDP.xlsx")
small_path <- safe_download(url_small, "data/Data_GDP_SmallEuroCountries.xlsx")

# ============================================================
# 3) Importation
# ============================================================
data_countries_bigeuro   <- read_any(big_path)
data_countries_Smalleuro <- read_any(small_path)

# ============================================================
# 4) Transformation BIG
# ============================================================
if (!"TIME" %in% names(data_countries_bigeuro)) names(data_countries_bigeuro)[1] <- "TIME"
data_countries_euro_prepared <- data_countries_bigeuro %>%
  rename(Country = TIME) %>%
  pivot_longer(cols = -Country, names_to = "Quarter", values_to = "Nominal_GDP") %>%
  mutate(
    Quarter     = as.character(Quarter) |> gsub("[ _]", "-", x = _),
    Quarter     = zoo::as.yearqtr(Quarter, format = "%Y-Q%q"),
    Nominal_GDP = readr::parse_number(as.character(Nominal_GDP))
  )

# ============================================================
# 5) Transformation SMALL
# ============================================================
data_smalleuro_prepared <- data_countries_Smalleuro %>%
  filter(is.na(TIME_PERIOD) | TIME_PERIOD != "2025-Q3") %>%
  rename(
    Country     = geo,
    Quarter     = TIME_PERIOD,
    Nominal_GDP = OBS_VALUE
  ) %>%
  mutate(
    Quarter     = gsub("-", " ", Quarter),
    Quarter     = zoo::as.yearqtr(Quarter, "%Y Q%q"),
    Nominal_GDP = readr::parse_number(as.character(Nominal_GDP))
  )

data_smalleuro_sum <- data_smalleuro_prepared %>%
  group_by(Quarter) %>%
  summarise(Nominal_GDP = sum(Nominal_GDP, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = "Sum Small euro countries") %>%
  select(Country, Quarter, Nominal_GDP)

# ============================================================
# 6) Fusion BIG + SMALL
# ============================================================
data_Allcountries_euro_prepared <- bind_rows(
  data_countries_euro_prepared,
  data_smalleuro_sum
) %>% arrange(Country, Quarter)

# ============================================================
# 7) Δlog×100 (QoQ growth)
# ============================================================
Euro_Countries_GDP_Growth_Log <- data_Allcountries_euro_prepared %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(
    log_gdp            = if_else(Nominal_GDP > 0, log(Nominal_GDP), NA_real_),
    gdp_growth_qoq_log = 100 * (log_gdp - lag(log_gdp))
  ) %>% ungroup()

writexl::write_xlsx(Euro_Countries_GDP_Growth_Log,
                    file.path("output", paste0("Euro_Countries_GDP_Growth_Log_", stamp, ".xlsx")))
cat("✅ Growth table exportée.\n")

# ============================================================
# 8) Visualisation (interpolation sans extrapolation)
# ============================================================
df_long <- Euro_Countries_GDP_Growth_Log %>% rename(gdp_growth = gdp_growth_qoq_log)

df_interp <- df_long %>%
  group_by(Country) %>%
  arrange(Quarter, .by_group = TRUE) %>%
  mutate(gdp_growth_interp = zoo::na.approx(gdp_growth, x = as.numeric(Quarter), na.rm = FALSE)) %>%
  ungroup() %>%
  filter(!is.na(gdp_growth_interp))

p <- ggplot(df_interp, aes(Quarter, gdp_growth_interp, color = Country, group = Country)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Quarterly GDP Growth by Country (Big + Small euro area)",
       x = "Quarter", y = "GDP growth (%)", color = "Country") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave(file.path("figures", paste0("Euro_GDP_Growth_BigSmall_", stamp, ".png")),
       p, width = 10, height = 6, dpi = 300)

# ============================================================
# 9) ADF + STL (robuste)
# ============================================================
df_long <- df_long %>%
  mutate(
    gdp_growth = suppressWarnings(readr::parse_number(as.character(gdp_growth))),
    gdp_growth = if_else(is.infinite(gdp_growth), NA_real_, gdp_growth)
  ) %>% arrange(Country, Quarter)

adf_per_country <- function(x){
  x <- stats::na.omit(x)
  if (length(x) < 8) return(tibble(n = length(x), adf_stat = NA_real_, p_value = NA_real_))
  out <- tryCatch({
    test <- suppressWarnings(tseries::adf.test(x, k = trunc(length(x)^(1/3))))
    tibble(n = length(x), adf_stat = unname(test$statistic), p_value = as.numeric(test$p.value))
  }, error = function(e) tibble(n = length(x), adf_stat = NA_real_, p_value = NA_real_))
  out
}

seas_adjust_qtr <- function(v){
  idx <- which(!is.na(v)); if (length(idx) < 8) return(rep(NA_real_, length(v)))
  i1 <- min(idx); i2 <- max(idx); sub <- v[i1:i2]
  fit_adj <- function(x){ fit <- stats::stl(stats::ts(x, frequency = 4), s.window = "periodic"); as.numeric(forecast::seasadj(fit)) }
  if (any(is.na(sub))){
    sub_filled <- zoo::na.approx(sub, na.rm = FALSE)
    if (any(is.na(sub_filled))) return(rep(NA_real_, length(v)))
    adj <- fit_adj(sub_filled); out <- rep(NA_real_, length(v)); out[i1:i2] <- adj
    out[(i1:i2)[is.na(sub)]] <- NA_real_; out
  } else {
    adj <- fit_adj(sub); out <- rep(NA_real_, length(v)); out[i1:i2] <- adj; out
  }
}

adf_results <- df_long %>% group_by(Country) %>% reframe(adf_per_country(gdp_growth)) %>% arrange(Country)

df_seas <- df_long %>%
  group_by(Country) %>%
  arrange(Quarter, .by_group = TRUE) %>%
  mutate(gdp_growth_seas = seas_adjust_qtr(gdp_growth)) %>%
  ungroup()

writexl::write_xlsx(list(ADF_results = adf_results,
                         Growth_with_seasonal_adjustment = df_seas),
                    file.path("output", paste0("ADF_and_SeasonalAdj_BigSmall_", stamp, ".xlsx")))
readr::write_csv(adf_results, file.path("output", paste0("ADF_results_BigSmall_", stamp, ".csv")))
readr::write_csv(df_seas,    file.path("output", paste0("Growth_seasonal_BigSmall_", stamp, ".csv")))
cat("✅ ADF + STL exportés.\n")

# ============================================================
# 13) Rolling Window — Prévoir 2010Q1–Q4 (fenêtre 10 ans)
#     (train 2000Q1–2009Q4 pour chaque cible 2010Q1..Q4)
# ============================================================
stopifnot(all(c("Country","Quarter","gdp_growth_seas") %in% names(df_seas)))
if (!inherits(df_seas$Quarter, "yearqtr")) {
  df_seas <- df_seas %>%
    mutate(Quarter = zoo::as.yearqtr(gsub("[ _]", "-", as.character(Quarter)), "%Y-Q%q"))
}

roll_fc_one <- function(dat_country, target_q){
  start_window <- target_q - 10
  end_window   <- target_q - 0.25
  dd <- dat_country %>%
    arrange(Quarter) %>%
    filter(Quarter >= start_window, Quarter <= end_window)
  y_act <- dat_country$gdp_growth_seas[dat_country$Quarter == target_q]
  y_act <- if (length(y_act) == 0) NA_real_ else y_act
  if (nrow(dd) < 40 || all(is.na(dd$gdp_growth_seas))) {
    return(tibble(
      Country = unique(dat_country$Country), forecast_quarter = target_q,
      train_start = start_window, train_end = end_window, n_train = nrow(dd),
      model_type = "insufficient data", y_forecast = NA_real_, y_actual = y_act
    ))
  }
  y <- dd$gdp_growth_seas
  fit <- tryCatch(forecast::auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE),
                  error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble(
      Country = unique(dat_country$Country), forecast_quarter = target_q,
      train_start = start_window, train_end = end_window, n_train = nrow(dd),
      model_type = "fit error", y_forecast = NA_real_, y_actual = y_act
    ))
  }
  fc   <- forecast::forecast(fit, h = 1)
  y_fc <- as.numeric(fc$mean[1])
  ord  <- forecast::arimaorder(fit)
  tibble(
    Country = unique(dat_country$Country), forecast_quarter = target_q,
    train_start = start_window, train_end = end_window, n_train = nrow(dd),
    model_type = paste0("ARIMA(", paste(ord[c("p","d","q")], collapse=","), ")"),
    y_forecast = y_fc, y_actual = y_act
  )
}

targets_2010 <- zoo::as.yearqtr(paste("2010 Q", 1:4), format = "%Y Q%q")
rolling_2010 <- df_seas %>%
  select(Country, Quarter, gdp_growth_seas) %>%
  group_split(Country) %>%
  map_dfr(function(dat_ct){
    map_dfr(targets_2010, ~ roll_fc_one(dat_ct, .x))
  }) %>%
  mutate(
    forecast_error = y_forecast - y_actual,
    abs_error      = abs(forecast_error),
    ape            = if_else(is.na(y_actual) | y_actual == 0, NA_real_, 100 * abs_error / abs(y_actual))
  )

readr::write_csv(rolling_2010, file.path("output", paste0("RollingForecast_2010_full_", stamp, ".csv")))
cat("✅ Rolling 2010 exporté.\n")

# ============================================================
# Rolling annuel 2011 → 2027 (fenêtre fixe 10 ans, h=4 par année)
# (2001–2010 → 2011 ; … ; 2017–2026 → 2027)
# ============================================================
roll_fc_year <- function(dat_country, year_target){
  start_yq <- zoo::as.yearqtr(paste(year_target - 10, "Q1"))
  end_yq   <- zoo::as.yearqtr(paste(year_target - 1,  "Q4"))
  train <- dat_country %>%
    arrange(Quarter) %>%
    filter(Quarter >= start_yq, Quarter <= end_yq)
  targets <- zoo::as.yearqtr(paste(year_target, paste0("Q", 1:4)))
  y_act <- dat_country %>%
    filter(Quarter %in% targets) %>%
    arrange(Quarter) %>%
    pull(gdp_growth_seas)
  if (nrow(train) < 40 || all(is.na(train$gdp_growth_seas))) {
    return(tibble(
      Country          = unique(dat_country$Country),
      target_year      = year_target,
      forecast_quarter = targets,
      train_start      = start_yq, train_end = end_yq, n_train = nrow(train),
      model_type       = "insufficient data",
      y_forecast       = NA_real_,
      y_actual         = if (length(y_act)==4) y_act else rep(NA_real_, 4)
    ))
  }
  y <- train$gdp_growth_seas
  fit <- tryCatch(auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE),
                  error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble(
      Country          = unique(dat_country$Country),
      target_year      = year_target,
      forecast_quarter = targets,
      train_start      = start_yq, train_end = end_yq, n_train = nrow(train),
      model_type       = "fit error",
      y_forecast       = NA_real_,
      y_actual         = if (length(y_act)==4) y_act else rep(NA_real_, 4)
    ))
  }
  fc   <- forecast::forecast(fit, h = 4)
  y_fc <- as.numeric(fc$mean)
  ord  <- forecast::arimaorder(fit)
  
  # réalignement observés Q1..Q4
  y_act_full <- rep(NA_real_, 4)
  if (length(y_act) > 0) {
    m <- match(dat_country$Quarter[dat_country$Quarter %in% targets], targets)
    y_act_full[m] <- dat_country$gdp_growth_seas[dat_country$Quarter %in% targets]
  }
  
  tibble(
    Country          = unique(dat_country$Country),
    target_year      = year_target,
    forecast_quarter = targets,
    train_start      = start_yq, train_end = end_yq, n_train = nrow(train),
    model_type       = paste0("ARIMA(", paste(ord[c("p","d","q")], collapse=","), ")"),
    y_forecast       = y_fc,
    y_actual         = y_act_full
  )
}

years_targets <- 2011:2027
countries     <- sort(unique(df_seas$Country))

rolling_2011_2027 <- map_dfr(
  countries,
  function(ct){
    dat_ct <- df_seas %>% select(Country, Quarter, gdp_growth_seas) %>% filter(Country == ct)
    map_dfr(years_targets, ~ roll_fc_year(dat_ct, .x))
  }
) %>%
  mutate(
    forecast_error = y_forecast - y_actual,
    abs_error      = abs(forecast_error),
    ape            = if_else(is.na(y_actual) | y_actual == 0, NA_real_, 100 * abs_error / abs(y_actual))
  )

readr::write_csv(rolling_2011_2027, file.path("output", paste0("RollingForecast_YbyY_2011_2027_", stamp, ".csv")))
cat("✅ Rolling 2011→2027 exporté.\n")
