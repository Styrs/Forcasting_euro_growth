# ============================================================
# Projet complet : BIG + SMALL → Fusion → Visualisation + ADF + STL
# Dossier :/Users/dominiquezielecomiati/Desktop/R.File/nouvelle base de donnée 
# ============================================================

# 0) Préambule --------------------------------------------------------------
base_dir <- "/Users/dominiquezielecomiati/Desktop/R.File/nouvelle base de donnée "
base_dir <- normalizePath(base_dir, mustWork = TRUE)
setwd(base_dir)

dir.create(file.path(base_dir, "figures"), showWarnings = FALSE)
dir.create(file.path(base_dir, "output"),  showWarnings = FALSE)

# 1) Librairies -------------------------------------------------------------
safe_lib <- function(pkgs){
  miss <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(miss)) install.packages(miss, dependencies = TRUE)
  invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
}
safe_lib(c(
  "readxl","readr","dplyr","tidyr","stringr","zoo","ggplot2","tools",
  "writexl","tseries","forecast"
))

# 2) Chemins d’accès fichiers locaux -----------------------------------------
big_path   <- file.path(base_dir, "Countries_Excel_euro_GDP.xlsx")
small_path <- file.path(base_dir, "Data_GDP_SmallEuroCountries.xlsx")

if (!file.exists(big_path)) {
  message("Sélectionne le fichier BIG countries (.xlsx)…")
  big_path <- file.choose()
}
if (!file.exists(small_path)) {
  message("Sélectionne le fichier SMALL countries (.xlsx)…")
  small_path <- file.choose()
}

# 3) Importation -------------------------------------------------------------
data_countries_bigeuro   <- readxl::read_excel(big_path)
data_countries_Smalleuro <- readxl::read_excel(small_path)

# 4) Transformation BIG ------------------------------------------------------
if (!"TIME" %in% names(data_countries_bigeuro)) names(data_countries_bigeuro)[1] <- "TIME"
data_countries_euro_prepared <- data_countries_bigeuro %>%
  rename(Country = TIME) %>%
  pivot_longer(cols = -Country, names_to = "Quarter", values_to = "Nominal_GDP") %>%
  mutate(
    Quarter = as.character(Quarter),
    Quarter = gsub("[ _]", "-", Quarter),
    Quarter = as.yearqtr(Quarter, format = "%Y-Q%q"),
    Nominal_GDP = readr::parse_number(as.character(Nominal_GDP))
  )

# 5) Transformation SMALL ----------------------------------------------------
data_smalleuro_prepared <- data_countries_Smalleuro %>%
  filter(is.na(TIME_PERIOD) | TIME_PERIOD != "2025-Q3") %>%
  rename(
    Country     = geo,
    Quarter     = TIME_PERIOD,
    Nominal_GDP = OBS_VALUE
  ) %>%
  mutate(
    Quarter = gsub("-", " ", Quarter),
    Quarter = as.yearqtr(Quarter, "%Y Q%q"),
    Nominal_GDP = readr::parse_number(as.character(Nominal_GDP))
  )

data_smalleuro_sum <- data_smalleuro_prepared %>%
  group_by(Quarter) %>%
  summarise(Nominal_GDP = sum(Nominal_GDP, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = "Sum Small euro countries") %>%
  select(Country, Quarter, Nominal_GDP)

# 6) Fusion BIG + SMALL ------------------------------------------------------
data_Allcountries_euro_prepared <- bind_rows(
  data_countries_euro_prepared,
  data_smalleuro_sum
) %>%
  arrange(Country, Quarter)

# 7) Calcul croissance log QoQ -----------------------------------------------
Euro_Countries_GDP_Growth_Log <- data_Allcountries_euro_prepared %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(
    log_gdp = ifelse(Nominal_GDP > 0, log(Nominal_GDP), NA_real_),
    gdp_growth_qoq_log = 100 * (log_gdp - lag(log_gdp))
  ) %>%
  ungroup()

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
xlsx_growth <- file.path(base_dir, "output", paste0("Euro_Countries_GDP_Growth_Log_", stamp, ".xlsx"))
writexl::write_xlsx(Euro_Countries_GDP_Growth_Log, xlsx_growth)

cat("✅ Données fusionnées et croissance calculée.\nExport :", xlsx_growth, "\n")

# 8) Visualisation -----------------------------------------------------------
df_long <- Euro_Countries_GDP_Growth_Log %>%
  rename(gdp_growth = gdp_growth_qoq_log)

df_interp <- df_long %>%
  group_by(Country) %>%
  arrange(Quarter, .by_group = TRUE) %>%
  mutate(gdp_growth_interp = na.approx(gdp_growth, x = as.numeric(Quarter), na.rm = FALSE)) %>%
  ungroup() %>%
  filter(!is.na(gdp_growth_interp))

p <- ggplot(df_interp, aes(Quarter, gdp_growth_interp, color = Country, group = Country)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Quarterly GDP Growth by Country (Big + Small euro area)",
       x = "Quarter", y = "GDP growth (%)", color = "Country") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
print(p)

fig_path <- file.path(base_dir, "figures", paste0("Euro_GDP_Growth_BigSmall_", stamp, ".png"))
ggsave(fig_path, p, width = 10, height = 6, dpi = 300)



# =========================
# 9) ADF + STL (robuste)
# =========================

# Packages requis (au cas où)
suppressPackageStartupMessages({
  library(dplyr); library(zoo); library(forecast); library(tseries); library(readr)
})

# --- (sécurité) s'assurer que gdp_growth est bien numérique
df_long <- df_long %>%
  mutate(
    gdp_growth = suppressWarnings(readr::parse_number(as.character(gdp_growth))),
    gdp_growth = ifelse(is.infinite(gdp_growth), NA_real_, gdp_growth)
  ) %>%
  arrange(Country, Quarter)

# --- Fonction ADF par pays (gère séries courtes + erreurs)
adf_per_country <- function(x){
  x <- stats::na.omit(x)
  if (length(x) < 8) {
    return(tibble::tibble(n = length(x), adf_stat = NA_real_, p_value = NA_real_))
  }
  out <- tryCatch({
    test <- tseries::adf.test(x, k = trunc(length(x)^(1/3)))
    tibble::tibble(n = length(x), adf_stat = unname(test$statistic), p_value = test$p.value)
  }, error = function(e){
    tibble::tibble(n = length(x), adf_stat = NA_real_, p_value = NA_real_)
  })
  out
}

# --- Fonction d'ajustement saisonnier trimestriel (STL, sans extrapolation)
seas_adjust_qtr <- function(v){
  # v = vecteur numérique avec éventuels NA
  idx <- which(!is.na(v))
  if (length(idx) < 8) return(rep(NA_real_, length(v)))  # série trop courte pour STL
  i1 <- min(idx); i2 <- max(idx)
  sub <- v[i1:i2]
  
  fit_and_adj <- function(x){
    fit <- stats::stl(stats::ts(x, frequency = 4), s.window = "periodic")
    as.numeric(forecast::seasadj(fit))
  }
  
  if (any(is.na(sub))) {
    # trous internes -> on interpole juste pour faire tourner STL
    sub_filled <- zoo::na.approx(sub, na.rm = FALSE)
    if (any(is.na(sub_filled))) return(rep(NA_real_, length(v)))  # encore des NA -> abandon
    adj <- fit_and_adj(sub_filled)
    out <- rep(NA_real_, length(v))
    out[i1:i2] <- adj
    # remettre NA aux positions qui étaient NA à l'origine dans 'sub'
    na_in_sub <- is.na(sub)
    out[(i1:i2)[na_in_sub]] <- NA_real_
    return(out)
  } else {
    adj <- fit_and_adj(sub)
    out <- rep(NA_real_, length(v))
    out[i1:i2] <- adj
    return(out)
  }
}

# ---- ADF (avec df_long renommé) ----
adf_results <- df_long %>%
  group_by(Country) %>%
  reframe(adf_per_country(gdp_growth)) %>%
  arrange(Country)

# ---- STL (avec df_long renommé) ----
df_seas <- df_long %>%
  group_by(Country) %>%
  arrange(Quarter, .by_group = TRUE) %>%
  mutate(gdp_growth_seas = seas_adjust_qtr(gdp_growth)) %>%
  ungroup()

# (optionnel) aperçu console
print(head(adf_results, 10))
print(df_seas %>% filter(Country %in% unique(df_seas$Country)[1]) %>% head(8))




# --- Point 10 : Ajustement saisonnier STL (Option A : colonne = gdp_growth) ---

# Assure-toi d'avoir ces packages chargés
library(forecast)
library(zoo)
library(dplyr)

# Fonction STL (si pas déjà définie)
seas_adjust_qtr <- function(v){
  idx <- which(!is.na(v))
  if (length(idx) < 8) return(rep(NA_real_, length(v)))  # série trop courte
  i1 <- min(idx); i2 <- max(idx); sub <- v[i1:i2]
  
  fit_and_adj <- function(x){
    fit <- stats::stl(stats::ts(x, frequency = 4), s.window = "periodic")
    as.numeric(forecast::seasadj(fit))
  }
  
  if (any(is.na(sub))) {
    sub_filled <- zoo::na.approx(sub, na.rm = FALSE)
    if (any(is.na(sub_filled))) return(rep(NA_real_, length(v)))
    adj <- fit_and_adj(sub_filled)
    out <- rep(NA_real_, length(v)); out[i1:i2] <- adj
    # remettre NA aux positions manquantes d'origine
    out[(i1:i2)[is.na(sub)]] <- NA_real_
    out
  } else {
    adj <- fit_and_adj(sub)
    out <- rep(NA_real_, length(v)); out[i1:i2] <- adj
    out
  }
}

# IMPORTANT : utiliser gdp_growth (pas gdp_growth_qoq_log)
df_seas <- df_long %>%
  group_by(Country) %>%
  arrange(Quarter, .by_group = TRUE) %>%
  mutate(gdp_growth_seas = seas_adjust_qtr(gdp_growth)) %>%
  ungroup()


# 11) Exports finaux ----------------------------------------------------------
xlsx_out <- file.path(base_dir, "output", paste0("ADF_and_SeasonalAdj_BigSmall_", stamp, ".xlsx"))
csv_adf  <- file.path(base_dir, "output", paste0("ADF_results_BigSmall_", stamp, ".csv"))
csv_seas <- file.path(base_dir, "output", paste0("Growth_seasonal_BigSmall_", stamp, ".csv"))

writexl::write_xlsx(list(ADF_results = adf_results,
                         Growth_with_seasonal_adjustment = df_seas),
                    xlsx_out)
readr::write_csv(adf_results, csv_adf)
readr::write_csv(df_seas,    csv_seas)

cat("\n✅ Terminé.\n",
    "Fichiers créés dans : ", file.path(base_dir, "output"), "\n",
    "• Croissance fusionnée : ", basename(xlsx_growth), "\n",
    "• ADF + STL : ", basename(xlsx_out), "\n", sep = "")




### ATTENTION JE VAIS PEUT ETRE ME REPETER MAIS J'ESSAYE QUELQUE CHOSE ===============
# ============================================================
# PIPELINE COMPLET — Big+Small → Δlog×100 → Visu → ADF → STL →
#                     ARMA (BIC/AIC) → Diagnostics
# Dossier : /Users/…/nouvelle base de donnée
# ============================================================

# 0) Préambule --------------------------------------------------------------
base_dir <- "/Users/dominiquezielecomiati/Desktop/R.File/nouvelle base de donnée "
base_dir <- normalizePath(base_dir, mustWork = TRUE)
setwd(base_dir)

dir.create(file.path(base_dir, "figures"), showWarnings = FALSE)
dir.create(file.path(base_dir, "output"),  showWarnings = FALSE)
dir.create(file.path(base_dir, "figures", "diagnostics"), showWarnings = FALSE)

# 1) Librairies -------------------------------------------------------------
safe_lib <- function(pkgs){
  miss <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(miss)) install.packages(miss, dependencies = TRUE)
  invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
}
safe_lib(c(
  "readxl","readr","dplyr","tidyr","stringr","zoo","ggplot2","tools",
  "writexl","tseries","forecast","purrr"
))

# 2) Fichiers d’entrée (Big & Small) -----------------------------------------
big_path   <- file.path(base_dir, "Countries_Excel_euro_GDP.xlsx")
small_path <- file.path(base_dir, "Data_GDP_SmallEuroCountries.xlsx")
if (!file.exists(big_path))  { message("Sélectionne le fichier BIG countries (.xlsx)…");   big_path   <- file.choose() }
if (!file.exists(small_path)){ message("Sélectionne le fichier SMALL countries (.xlsx)…"); small_path <- file.choose() }

# 3) Import & préparation Big/Small -----------------------------------------
data_countries_bigeuro   <- readxl::read_excel(big_path)
data_countries_Smalleuro <- readxl::read_excel(small_path)

# --- BIG (TIME + colonnes trimestrielles) -> long
if (!"TIME" %in% names(data_countries_bigeuro)) names(data_countries_bigeuro)[1] <- "TIME"
data_countries_euro_prepared <- data_countries_bigeuro %>%
  dplyr::rename(Country = TIME) %>%
  tidyr::pivot_longer(cols = -Country, names_to = "Quarter", values_to = "Nominal_GDP") %>%
  dplyr::mutate(
    Quarter = as.character(Quarter),
    Quarter = gsub("[ _]", "-", Quarter),
    Quarter = zoo::as.yearqtr(Quarter, format = "%Y-Q%q"),
    Nominal_GDP = readr::parse_number(as.character(Nominal_GDP))
  )

# --- SMALL (geo, TIME_PERIOD, OBS_VALUE) -> long + somme par trimestre
needed_small <- c("geo","TIME_PERIOD","OBS_VALUE")
stopifnot(all(needed_small %in% names(data_countries_Smalleuro)))

data_smalleuro_prepared <- data_countries_Smalleuro %>%
  dplyr::filter(is.na(TIME_PERIOD) | TIME_PERIOD != "2025-Q3") %>%
  dplyr::rename(
    Country     = geo,
    Quarter     = TIME_PERIOD,
    Nominal_GDP = OBS_VALUE
  ) %>%
  dplyr::mutate(
    Quarter = gsub("-", " ", Quarter),
    Quarter = zoo::as.yearqtr(Quarter, "%Y Q%q"),
    Nominal_GDP = readr::parse_number(as.character(Nominal_GDP))
  )

data_smalleuro_sum <- data_smalleuro_prepared %>%
  dplyr::group_by(Quarter) %>%
  dplyr::summarise(Nominal_GDP = sum(Nominal_GDP, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(Country = "Sum Small euro countries") %>%
  dplyr::select(Country, Quarter, Nominal_GDP)

# 4) Fusion Big + Small ------------------------------------------------------
data_all <- dplyr::bind_rows(
  data_countries_euro_prepared,
  data_smalleuro_sum
) %>% dplyr::arrange(Country, Quarter)

# 5) Croissance Δlog×100 (QoQ) ----------------------------------------------
Euro_Countries_GDP_Growth_Log <- data_all %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Quarter, .by_group = TRUE) %>%
  dplyr::mutate(
    log_gdp = dplyr::if_else(Nominal_GDP > 0, log(Nominal_GDP), NA_real_),
    gdp_growth_qoq_log = 100 * (log_gdp - dplyr::lag(log_gdp))
  ) %>% dplyr::ungroup()

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
writexl::write_xlsx(Euro_Countries_GDP_Growth_Log,
                    file.path(base_dir, "output", paste0("Euro_Countries_GDP_Growth_Log_", stamp, ".xlsx")))

# 6) Visualisation (interpolation sans extrapolation) ------------------------
df_long <- Euro_Countries_GDP_Growth_Log %>% dplyr::rename(gdp_growth = gdp_growth_qoq_log)
df_interp <- df_long %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Quarter, .by_group = TRUE) %>%
  dplyr::mutate(gdp_growth_interp = zoo::na.approx(gdp_growth, x = as.numeric(Quarter), na.rm = FALSE)) %>%
  dplyr::ungroup() %>% dplyr::filter(!is.na(gdp_growth_interp))

# --- Nettoyage des valeurs avant le tracé ---
to_plot <- df_interp %>%
  dplyr::filter(
    !is.na(Quarter),
    !is.na(gdp_growth_interp),
    is.finite(gdp_growth_interp)
  )

p_main <- ggplot2::ggplot(
  to_plot,
  ggplot2::aes(Quarter, gdp_growth_interp, color = Country, group = Country)
) +
  ggplot2::geom_line(linewidth = 0.9, alpha = 0.9, na.rm = TRUE) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(title = "Quarterly GDP Growth by Country (interpolated)",
                x = "Quarter", y = "GDP growth (%)", color = "Country") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom",
                 plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
ggplot2::ggsave(file.path(base_dir, "figures", paste0("Euro_GDP_Growth_BigSmall_", stamp, ".png")),
                p_main, width = 10, height = 6, dpi = 300)


# 7) ADF par pays -------------------------------------------------------------
adf_per_country <- function(x){
  x <- stats::na.omit(x)
  if (length(x) < 8) {
    return(tibble::tibble(n = length(x), adf_stat = NA_real_, p_value = NA_real_))
  }
  out <- tryCatch({
    # on supprime les warnings "p-value smaller than printed p-value"
    test <- suppressWarnings(tseries::adf.test(x, k = trunc(length(x)^(1/3))))
    tibble::tibble(
      n        = length(x),
      adf_stat = unname(test$statistic),
      p_value  = as.numeric(test$p.value)
    )
  }, error = function(e){
    tibble::tibble(n = length(x), adf_stat = NA_real_, p_value = NA_real_)
  })
  out
}

adf_results <- df_long %>%
  dplyr::group_by(Country) %>%
  dplyr::reframe(adf_per_country(gdp_growth)) %>%
  dplyr::arrange(Country)


# 8) STL trimestriel (sans extrapolation) ------------------------------------
seas_adjust_qtr <- function(v){
  idx <- which(!is.na(v))
  if (length(idx) < 8) return(rep(NA_real_, length(v)))
  i1 <- min(idx); i2 <- max(idx); sub <- v[i1:i2]
  fit_and_adj <- function(x){
    fit <- stats::stl(stats::ts(x, frequency = 4), s.window = "periodic")
    as.numeric(forecast::seasadj(fit))
  }
  if (any(is.na(sub))) {
    sub_filled <- zoo::na.approx(sub, na.rm = FALSE)
    if (any(is.na(sub_filled))) return(rep(NA_real_, length(v)))
    adj <- fit_and_adj(sub_filled)
    out <- rep(NA_real_, length(v)); out[i1:i2] <- adj
    # restituer NA aux trous d'origine
    out[(i1:i2)[is.na(sub)]] <- NA_real_
    out
  } else {
    adj <- fit_and_adj(sub)
    out <- rep(NA_real_, length(v)); out[i1:i2] <- adj
    out
  }
}

df_seas <- df_long %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Quarter, .by_group = TRUE) %>%
  dplyr::mutate(gdp_growth_seas = seas_adjust_qtr(gdp_growth)) %>%
  dplyr::ungroup()

# 9) Graphiques brut vs ajusté (3 pays) --------------------------------------
countries_to_plot <- c("France","Germany","Italy")
plot_ba <- function(ctry){
  dd <- df_seas %>%
    dplyr::filter(Country == ctry) %>%
    dplyr::transmute(Quarter, raw = gdp_growth, seas = gdp_growth_seas) %>%
    tidyr::pivot_longer(c(raw, seas), names_to = "series", values_to = "value") %>%
    dplyr::filter(!is.na(Quarter), is.finite(value))
  
  if (nrow(dd) == 0) return(NULL)
  
  ggplot2::ggplot(dd, ggplot2::aes(x = Quarter, y = value, linetype = series)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_linetype_manual(values = c(raw = "solid", seas = "dashed"),
                                   labels = c(raw = "Raw", seas = "Seasonally adjusted")) +
    ggplot2::labs(title = paste0("GDP Growth — ", ctry, " (raw vs STL)"),
                  y = "Growth (%)", linetype = "") +
    ggplot2::theme_minimal(base_size = 12)
}
ba_plots <- purrr::map(countries_to_plot, plot_ba)
for (i in seq_along(countries_to_plot)){
  if (!is.null(ba_plots[[i]])){
    ggplot2::ggsave(file.path(base_dir, "figures",
                              paste0("BA_", countries_to_plot[i], "_", stamp, ".png")),
                    ba_plots[[i]], width = 8, height = 5, dpi = 300)
  }
}

# 10) ARMA(p,q) — BIC & AIC par pays -----------------------------------------
data_clean <- df_seas %>%
  dplyr::filter(!is.na(gdp_growth_seas)) %>%
  dplyr::arrange(Country, Quarter) %>%
  dplyr::group_by(Country) %>%
  dplyr::filter(dplyr::n() >= 8) %>%   # séries trop courtes -> exclues
  dplyr::ungroup()

fit_best_arma <- function(y, ic = c("bic","aic")){
  ic <- match.arg(ic)
  out <- tryCatch({
    fit <- forecast::auto.arima(y, seasonal = FALSE, ic = ic,
                                stepwise = FALSE, approximation = FALSE)
    ord <- forecast::arimaorder(fit)
    tibble::tibble(p = unname(ord["p"]), q = unname(ord["q"]))
  }, error = function(e) tibble::tibble(p = NA_integer_, q = NA_integer_))
  out
}

arma_orders_BIC <- data_clean %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(fit_best_arma(gdp_growth_seas, ic = "bic"), .groups = "drop")

arma_orders_AIC <- data_clean %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(fit_best_arma(gdp_growth_seas, ic = "aic"), .groups = "drop")

readr::write_csv(arma_orders_BIC, file.path(base_dir, "output", paste0("ARMA_orders_BIC_", stamp, ".csv")))
readr::write_csv(arma_orders_AIC, file.path(base_dir, "output", paste0("ARMA_orders_AIC_", stamp, ".csv")))

# 11) Diagnostics résidus (ACF/PACF + Ljung-Box) -----------------------------
diag_pdf <- file.path(base_dir, "figures", "diagnostics", paste0("Diagnostics_ARMA_", stamp, ".pdf"))
grDevices::pdf(diag_pdf, width = 10, height = 7)
on.exit(grDevices::dev.off(), add = TRUE)

diag_countries <- unique(data_clean$Country)
for (ct in diag_countries){
  yy <- data_clean %>% dplyr::filter(Country == ct) %>% dplyr::pull(gdp_growth_seas)
  fit <- tryCatch({
    forecast::auto.arima(yy, seasonal = FALSE, ic = "bic",
                         stepwise = FALSE, approximation = FALSE)
  }, error = function(e) NULL)
  if (is.null(fit)) next
  par(mfrow = c(2,2))
  ts.plot(yy, main = paste0(ct, " — gdp_growth_seas"))
  acf(residuals(fit), main = paste0(ct, " — Residuals ACF"))
  pacf(residuals(fit), main = paste0(ct, " — Residuals PACF"))
  lb <- Box.test(residuals(fit), lag = min(12, length(yy)-1), type = "Ljung-Box")
  plot(0,0,type="n", axes=FALSE, xlab="", ylab="",
       main = paste0(ct, " — Ljung-Box p-value: ", signif(lb$p.value, 4)))
}
grDevices::dev.off()

# 12) Exports finaux ----------------------------------------------------------
writexl::write_xlsx(
  list(
    ADF_results = adf_results,
    Growth_with_seasonal_adjustment = df_seas,
    ARMA_orders_BIC = arma_orders_BIC,
    ARMA_orders_AIC = arma_orders_AIC
  ),
  file.path(base_dir, "output", paste0("ADF_STL_ARMA_", stamp, ".xlsx"))
)

cat("\n✅ Pipeline terminé.\n",
    "Exports:\n",
    " • Growth table:          output/Euro_Countries_GDP_Growth_Log_", stamp, ".xlsx\n",
    " • Main figure:           figures/Euro_GDP_Growth_BigSmall_", stamp, ".png\n",
    " • BA (raw vs STL) PNGs:  figures/BA_*_", stamp, ".png\n",
    " • ADF & STL & ARMA:      output/ADF_STL_ARMA_", stamp, ".xlsx\n",
    " • ARMA orders (BIC/AIC): output/ARMA_orders_*_", stamp, ".csv\n",
    " • Diagnostics PDF:       figures/diagnostics/Diagnostics_ARMA_", stamp, ".pdf\n", sep="")

#### Optionnel !!!! =====
# ============================================================
# (Option) Tableau Stationnarité (seuil 5%) à partir de adf_results
# ============================================================

stopifnot(exists("adf_results"))

stationarity_tbl <- adf_results %>%
  dplyr::mutate(
    decision_5pct = dplyr::case_when(
      is.na(p_value) ~ "Insufficient data",
      p_value < 0.05 ~ "Stationary (reject H0)",
      TRUE           ~ "Non-stationary (fail to reject H0)"
    )
  ) %>%
  dplyr::select(Country, n, adf_stat, p_value, decision_5pct) %>%
  dplyr::arrange(Country)

# Export CSV dédié
csv_stationarity <- file.path(base_dir, "output", paste0("ADF_stationarity_5pct_", stamp, ".csv"))
readr::write_csv(stationarity_tbl, csv_stationarity)

cat("🧪 Tableau stationnarité (5%) exporté : ", csv_stationarity, "\n", sep = "")

# (Option) Ajouter au gros XLSX si tu utilises déjà writexl avec une liste
# -> ajoute 'Stationarity_5pct = stationarity_tbl' dans ta liste d'exports XLSX :
# writexl::write_xlsx(
#   list(
#     ADF_results = adf_results,
#     Stationarity_5pct = stationarity_tbl,
#     Growth_with_seasonal_adjustment = df_seas,
#     ARMA_orders_BIC = arma_orders_BIC,
#     ARMA_orders_AIC = arma_orders_AIC
#   ),
#   file.path(base_dir, "output", paste0("ADF_STL_ARMA_", stamp, ".xlsx"))
# )

### PASSONS AU ROLLING WINDOW Q1.2000 - Q4.2009 POUR TOUTE L'ANNEE 2010
# ============================================================
# ============================================================
# 13) Rolling Window — Prévoir 2010Q1–Q4 (fenêtre 10 ans)
#    BLOC AUTONOME (définit la fonction + targets + exécution)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(readr); library(ggplot2); library(tidyr); library(zoo); library(forecast)
})

# --- Garde-fous souples (pas d'arrêt brutal si manquants) ---
if (!exists("df_seas")) stop("df_seas est introuvable : exécute d'abord la partie STL (df_seas).")
if (!all(c("Country","Quarter") %in% names(df_seas))) stop("df_seas doit contenir Country et Quarter.")
if (!("gdp_growth_seas" %in% names(df_seas))) {
  warning("Colonne gdp_growth_seas manquante — utilisation provisoire de gdp_growth si dispo.")
  if ("gdp_growth" %in% names(df_seas)) {
    df_seas <- df_seas %>% mutate(gdp_growth_seas = gdp_growth)
  } else {
    stop("Aucune colonne gdp_growth_seas ni gdp_growth dans df_seas.")
  }
}
if (!inherits(df_seas$Quarter, "yearqtr")) {
  # normalise au besoin
  df_seas <- df_seas %>%
    mutate(Quarter = zoo::as.yearqtr(gsub("[ _]", "-", as.character(Quarter)), "%Y-Q%q"))
}

# --- Fonction rolling 1-step pour (pays, trimestre cible) ---
roll_fc_one <- function(dat_country, target_q){
  start_window <- target_q - 10     # 10 ans avant
  end_window   <- target_q - 0.25   # trimestre précédent
  
  dd <- dat_country %>%
    arrange(Quarter) %>%
    filter(Quarter >= start_window, Quarter <= end_window)
  
  # observé au trimestre cible (peut être absent)
  y_act <- dat_country$gdp_growth_seas[dat_country$Quarter == target_q]
  y_act <- if (length(y_act) == 0) NA_real_ else y_act
  
  if (nrow(dd) < 40 || all(is.na(dd$gdp_growth_seas))) {
    return(tibble(
      Country = unique(dat_country$Country),
      forecast_quarter = target_q,
      train_start = start_window, train_end = end_window,
      n_train = nrow(dd),
      model_type = "insufficient data",
      y_forecast = NA_real_, y_actual = y_act
    ))
  }
  
  y <- dd$gdp_growth_seas
  fit <- tryCatch(
    auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(tibble(
      Country = unique(dat_country$Country),
      forecast_quarter = target_q,
      train_start = start_window, train_end = end_window,
      n_train = nrow(dd),
      model_type = "fit error",
      y_forecast = NA_real_, y_actual = y_act
    ))
  }
  
  fc   <- forecast::forecast(fit, h = 1)
  y_fc <- as.numeric(fc$mean[1])
  ord  <- forecast::arimaorder(fit)
  
  tibble(
    Country = unique(dat_country$Country),
    forecast_quarter = target_q,
    train_start = start_window, train_end = end_window,
    n_train = nrow(dd),
    model_type = paste0("ARIMA(", paste(ord[c("p","d","q")], collapse=","), ")"),
    y_forecast = y_fc, y_actual = y_act
  )
}

# --- Targets pour 2010 ---
targets <- zoo::as.yearqtr(paste("2010 Q", 1:4), format = "%Y Q%q")

# --- Exécution (split pays -> map targets -> rbind) ---
rolling_2010 <- df_seas %>%
  select(Country, Quarter, gdp_growth_seas) %>%
  group_split(Country) %>%
  map_dfr(function(dat_ct){
    map_dfr(targets, function(tq){
      roll_fc_one(dat_country = dat_ct, target_q = tq)
    })
  })

# --- Erreurs & exports ---
rolling_2010 <- rolling_2010 %>%
  mutate(
    forecast_error = y_forecast - y_actual,
    abs_error      = abs(forecast_error),
    ape            = if_else(is.na(y_actual) | y_actual == 0, NA_real_,
                             100 * abs_error / abs(y_actual))
  )

csv_roll <- file.path(base_dir, "output", paste0("RollingForecast_2010_full_", stamp, ".csv"))
readr::write_csv(rolling_2010, csv_roll)
cat("✅ Rolling 2010 (Q1–Q4) exporté : ", csv_roll, "\n", sep = "")

rolling_summary <- rolling_2010 %>%
  group_by(Country) %>%
  summarise(
    n_forecast = sum(!is.na(y_forecast)),
    MAE  = ifelse(all(is.na(abs_error)), NA_real_, mean(abs_error, na.rm = TRUE)),
    MAPE = ifelse(all(is.na(ape)), NA_real_, mean(ape, na.rm = TRUE)),
    .groups = "drop"
  ) %>% arrange(MAPE)

csv_roll_sum <- file.path(base_dir, "output", paste0("RollingForecast_2010_summary_", stamp, ".csv"))
readr::write_csv(rolling_summary, csv_roll_sum)
cat("📊 Résumé erreurs (2010) exporté : ", csv_roll_sum, "\n", sep = "")

# (Optionnel) graphiques
p_mape <- ggplot(rolling_summary, aes(x = reorder(Country, MAPE), y = MAPE)) +
  geom_col() + coord_flip() +
  labs(title = "MAPE – Rolling Forecast (2010Q1–Q4)",
       x = "Country", y = "MAPE (%)") +
  theme_minimal(base_size = 12)
ggsave(file.path(base_dir, "figures", paste0("Rolling_2010_MAPE_", stamp, ".png")),
       p_mape, width = 9, height = 6, dpi = 300)


# ============================================================
# 14) Rolling Window — 2010Q1 → 2019Q4 (fenêtre 10 ans)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(forecast); library(zoo); library(purrr)
  library(readr); library(ggplot2); library(tidyr)
})

# Garde-fous (df_seas et colonnes nécessaires)
stopifnot(exists("df_seas"))
stopifnot(all(c("Country","Quarter","gdp_growth_seas") %in% names(df_seas)))
stopifnot(inherits(df_seas$Quarter, "yearqtr"))

# Séquence des trimestres cibles: 2010Q1 -> 2019Q4
targets_all <- seq(zoo::as.yearqtr("2010 Q1", "%Y Q%q"),
                   zoo::as.yearqtr("2019 Q4", "%Y Q%q"),
                   by = 0.25)

# Helper: prévision 1-step-ahead pour (pays, trimestre cible) avec fenêtre 10 ans
roll_fc_one <- function(dat_country, target_q){
  # fenêtre d'entraînement: 10 ans se terminant au trimestre précédent la cible
  start_window <- target_q - 10
  end_window   <- target_q - 0.25
  
  dd <- dat_country %>%
    arrange(Quarter) %>%
    filter(Quarter >= start_window, Quarter <= end_window)
  
  # observation réelle (peut être absente)
  y_act <- dat_country$gdp_growth_seas[dat_country$Quarter == target_q]
  y_act <- if (length(y_act) == 0) NA_real_ else y_act
  
  # données insuffisantes
  if (nrow(dd) < 40 || all(is.na(dd$gdp_growth_seas))) {
    return(tibble(
      Country = unique(dat_country$Country),
      forecast_quarter = target_q,
      train_start = start_window,
      train_end   = end_window,
      n_train     = nrow(dd),
      model_type  = "insufficient data",
      y_forecast  = NA_real_,
      y_actual    = y_act
    ))
  }
  
  y <- dd$gdp_growth_seas
  
  # Ajustement ARIMA non saisonnier (série déjà STL-ajustée)
  fit <- tryCatch(
    forecast::auto.arima(y, seasonal = FALSE, stepwise = FALSE, approximation = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(tibble(
      Country = unique(dat_country$Country),
      forecast_quarter = target_q,
      train_start = start_window,
      train_end   = end_window,
      n_train     = nrow(dd),
      model_type  = "fit error",
      y_forecast  = NA_real_,
      y_actual    = y_act
    ))
  }
  
  fc   <- forecast::forecast(fit, h = 1)
  y_fc <- as.numeric(fc$mean[1])
  ord  <- forecast::arimaorder(fit)
  
  tibble(
    Country = unique(dat_country$Country),
    forecast_quarter = target_q,
    train_start = start_window,
    train_end   = end_window,
    n_train     = nrow(dd),
    model_type  = paste0("ARIMA(", paste(ord[c("p","d","q")], collapse = ","), ")"),
    y_forecast  = y_fc,
    y_actual    = y_act
  )
}

# Exécution pour tous les pays et toutes les cibles
countries <- sort(unique(df_seas$Country))
rolling_all <- purrr::map_dfr(
  countries,
  function(ct){
    dat_ct <- df_seas %>%
      dplyr::select(Country, Quarter, gdp_growth_seas) %>%
      dplyr::filter(Country == ct)
    purrr::map_dfr(targets_all, ~ roll_fc_one(dat_ct, .x))
  }
)

# Mesures d'erreur (MAE/MAPE)
rolling_all <- rolling_all %>%
  mutate(
    forecast_error = y_forecast - y_actual,
    abs_error      = abs(forecast_error),
    year           = as.integer(floor(as.numeric(forecast_quarter))),  # année civile
    ape            = dplyr::if_else(is.na(y_actual) | y_actual == 0,
                                    NA_real_, 100 * abs_error / abs(y_actual))
  )

# Résumé global par pays (2010–2019)
rolling_summary_country <- rolling_all %>%
  group_by(Country) %>%
  summarise(
    n_forecast = sum(!is.na(y_forecast)),
    MAE  = ifelse(all(is.na(abs_error)), NA_real_, mean(abs_error, na.rm = TRUE)),
    MAPE = ifelse(all(is.na(ape)), NA_real_, mean(ape, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(MAPE, MAE)

# Résumé par pays ET par année
rolling_summary_country_year <- rolling_all %>%
  group_by(Country, year) %>%
  summarise(
    n_forecast = sum(!is.na(y_forecast)),
    MAE  = ifelse(all(is.na(abs_error)), NA_real_, mean(abs_error, na.rm = TRUE)),
    MAPE = ifelse(all(is.na(ape)), NA_real_, mean(ape, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Country, year)

# Exports CSV
csv_roll_all  <- file.path(base_dir, "output", paste0("RollingForecast_2010_2019_full_", stamp, ".csv"))
csv_roll_sum1 <- file.path(base_dir, "output", paste0("RollingForecast_2010_2019_summary_country_", stamp, ".csv"))
csv_roll_sum2 <- file.path(base_dir, "output", paste0("RollingForecast_2010_2019_summary_country_year_", stamp, ".csv"))

readr::write_csv(rolling_all,                  csv_roll_all)
readr::write_csv(rolling_summary_country,      csv_roll_sum1)
readr::write_csv(rolling_summary_country_year, csv_roll_sum2)

cat("✅ Rolling forecasts 2010–2019 exportés :\n",
    " - ", csv_roll_all,  "\n",
    " - ", csv_roll_sum1, "\n",
    " - ", csv_roll_sum2, "\n", sep = "")

# (Optionnel) Barplot MAPE moyen 2010–2019 par pays
p_mape_all <- ggplot(rolling_summary_country, aes(x = reorder(Country, MAPE), y = MAPE)) +
  geom_col() + coord_flip() +
  labs(title = "MAPE moyen — Rolling Forecast (2010–2019)",
       x = "Country", y = "MAPE (%)") +
  theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(base_dir, "figures", paste0("Rolling_2010_2019_MAPE_", stamp, ".png")),
                p_mape_all, width = 9, height = 6, dpi = 300)

# (Optionnel) Heatmap MAPE par pays × année
heat_data <- rolling_summary_country_year %>%
  mutate(year = factor(year))
p_heat <- ggplot(heat_data, aes(x = year, y = Country, fill = MAPE)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "MAPE par pays et par année — Rolling (2010–2019)",
       x = "Year", y = "Country", fill = "MAPE (%)") +
  theme_minimal(base_size = 12)
ggplot2::ggsave(file.path(base_dir, "figures", paste0("Rolling_2010_2019_MAPE_heatmap_", stamp, ".png")),
                p_heat, width = 11, height = 8, dpi = 300)


# ============================================================
# 15) Rolling mean (quarterly) — moyennes mobiles sur gdp_growth_seas
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(zoo); library(readr); library(ggplot2) })

stopifnot(exists("df_seas"))
stopifnot(all(c("Country","Quarter","gdp_growth_seas") %in% names(df_seas)))
stopifnot(inherits(df_seas$Quarter, "yearqtr"))

# Paramètres (tu peux changer k=4,8, etc.)
k_trailing <- 4   # moyenne mobile "retardée" sur 4 trimestres (1 an)
k_centered <- 4   # moyenne mobile "centrée" sur 4 trimestres
k_trailing_long <- 8  # moyenne mobile retardée sur 8 trimestres (2 ans)

# Fonction utilitaire sûre
roll_mean_safe <- function(x, k, align = c("right","center","left"), min_obs = NULL){
  align <- match.arg(align)
  # min_obs: nb min d'obs pour calculer la moyenne (par défaut = k)
  if (is.null(min_obs)) min_obs <- k
  zoo::rollapply(x, width = k, FUN = function(v) mean(v, na.rm = TRUE),
                 align = align, partial = FALSE) |>
    # pad avec NA pour garder la même longueur
    (\(y){
      n <- length(x); m <- length(y)
      if (align == "right")  c(rep(NA_real_, n - m), y)
      else if (align == "left") c(y, rep(NA_real_, n - m))
      else { # center
        left_pad  <- floor((k - 1)/2)
        right_pad <- ceiling((k - 1)/2)
        c(rep(NA_real_, left_pad), y, rep(NA_real_, right_pad))
      }
    })()
}

# Calcul des moyennes mobiles par pays
df_rollmean <- df_seas %>%
  group_by(Country) %>%
  arrange(Quarter, .by_group = TRUE) %>%
  mutate(
    rm_4Q_trailing = roll_mean_safe(gdp_growth_seas, k = k_trailing, align = "right"),
    rm_4Q_center   = roll_mean_safe(gdp_growth_seas, k = k_centered, align = "center"),
    rm_8Q_trailing = roll_mean_safe(gdp_growth_seas, k = k_trailing_long, align = "right")
  ) %>%
  ungroup()

# Export CSV
csv_rm <- file.path(base_dir, "output", paste0("RollingMean_quarterly_", stamp, ".csv"))
readr::write_csv(df_rollmean, csv_rm)
cat("📄 Rolling means exporté : ", csv_rm, "\n", sep = "")

# Plot d’illustration (ex: France) — brut vs moyennes mobiles
country_example <- "France"  # change si tu veux
plot_rm <- df_rollmean %>%
  filter(Country == country_example) %>%
  select(Country, Quarter, gdp_growth_seas, rm_4Q_trailing, rm_4Q_center, rm_8Q_trailing) %>%
  tidyr::pivot_longer(-c(Country, Quarter), names_to = "series", values_to = "value") %>%
  mutate(series = dplyr::recode(series,
                                gdp_growth_seas = "Raw (seasonally adjusted)",
                                rm_4Q_trailing = "Rolling mean 4Q (trailing)",
                                rm_4Q_center   = "Rolling mean 4Q (centered)",
                                rm_8Q_trailing = "Rolling mean 8Q (trailing)")) %>%
  ggplot(aes(x = Quarter, y = value, linetype = series)) +
  geom_line(na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = paste0("GDP Growth — ", country_example, " (rolling means)"),
       x = "Quarter", y = "Growth (%)", linetype = "") +
  theme_minimal(base_size = 12)

fig_rm <- file.path(base_dir, "figures", paste0("RollingMeans_", country_example, "_", stamp, ".png"))
ggsave(fig_rm, plot_rm, width = 10, height = 6, dpi = 300)
cat("📈 Graph rolling means enregistré : ", fig_rm, "\n", sep = "")



#### TEST BENCHMARK
# ============================================================
# 16) Benchmarking — Comparaison Rolling ARIMA vs Naïf & Moyenne
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(forecast)
})

# Vérification que le jeu de rolling forecasts existe
if (!exists("rolling_all")) stop("L'objet 'rolling_all' (résultats rolling 2010–2019) est introuvable.")

# ------------------------------------------------------------
# 1. Créer les prévisions benchmarks naïf et moyenne glissante
# ------------------------------------------------------------

# On va reconstituer les prédictions naïves directement depuis df_seas
benchmarks <- df_seas %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(
    naive_fc    = dplyr::lag(gdp_growth_seas, 1),                   # Naïf (y_{t-1})
    mean4_fc    = zoo::rollapply(gdp_growth_seas, width = 4, align = "right",
                                 FUN = mean, fill = NA, na.rm = TRUE) # Moyenne 4 trimestres
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 2. Fusion avec les résultats rolling
# ------------------------------------------------------------

rolling_bench <- rolling_all %>%
  left_join(benchmarks, by = c("Country" = "Country", "forecast_quarter" = "Quarter")) %>%
  mutate(
    naive_error  = naive_fc - y_actual,
    mean4_error  = mean4_fc - y_actual,
    model_error  = y_forecast - y_actual
  )

# ------------------------------------------------------------
# 3. Calcul des métriques de performance
# ------------------------------------------------------------

bench_summary <- rolling_bench %>%
  group_by(Country) %>%
  summarise(
    RMSE_model = sqrt(mean(model_error^2, na.rm = TRUE)),
    RMSE_naive = sqrt(mean(naive_error^2, na.rm = TRUE)),
    RMSE_mean4 = sqrt(mean(mean4_error^2, na.rm = TRUE)),
    TheilU_naive = RMSE_model / RMSE_naive,
    TheilU_mean4 = RMSE_model / RMSE_mean4,
    MAE_model = mean(abs(model_error), na.rm = TRUE),
    MAE_naive = mean(abs(naive_error), na.rm = TRUE),
    MAE_mean4 = mean(abs(mean4_error), na.rm = TRUE),
    MAPE_model = mean(abs(model_error / y_actual) * 100, na.rm = TRUE),
    MAPE_naive = mean(abs(naive_error / y_actual) * 100, na.rm = TRUE),
    MAPE_mean4 = mean(abs(mean4_error / y_actual) * 100, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(TheilU_naive)

# ------------------------------------------------------------
# 4. Test de Diebold-Mariano (optionnel)
# ------------------------------------------------------------

# Exemple : test sur la France (peut être généralisé par boucle)
dm_results <- rolling_bench %>%
  filter(Country == "France", !is.na(model_error), !is.na(naive_error)) %>%
  summarise(
    DM_statistic = tryCatch({
      test <- forecast::dm.test(model_error, naive_error, alternative = "less")
      unname(test$statistic)
    }, error = function(e) NA_real_),
    p_value = tryCatch({
      test <- forecast::dm.test(model_error, naive_error, alternative = "less")
      test$p.value
    }, error = function(e) NA_real_)
  )

cat("\n📊 Diebold-Mariano Test (France):\n  DM statistic =", dm_results$DM_statistic,
    "\n  p-value =", dm_results$p_value, "\n")

# ------------------------------------------------------------
# 5. Export et visualisation
# ------------------------------------------------------------

csv_bench <- file.path(base_dir, "output", paste0("Benchmark_Rolling_vs_Naive_", stamp, ".csv"))
readr::write_csv(bench_summary, csv_bench)

cat("\n✅ Benchmark exporté :", csv_bench, "\n")

# Barplot Theil’s U
p_theil <- ggplot(bench_summary, aes(x = reorder(Country, TheilU_naive), y = TheilU_naive)) +
  geom_col(fill = "steelblue") + coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Theil’s U (ARIMA vs Naïf) – Rolling 2010–2019",
       x = "Country", y = "Theil’s U (Model / Naïf)") +
  theme_minimal(base_size = 12)
ggsave(file.path(base_dir, "figures", paste0("Benchmark_TheilU_", stamp, ".png")),
       p_theil, width = 9, height = 6, dpi = 300)

# ============================================================
# 16-BIS) Visualisation du Benchmark — Theil’s U, MAE, MAPE
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr)
})

# Vérification que bench_summary existe
if (!exists("bench_summary")) stop("⚠️ L'objet 'bench_summary' est introuvable. Exécute d'abord le code du Benchmarking (point 15).")

# ------------------------------------------------------------
# 1️⃣ Graphique principal : Theil’s U (ARIMA vs Naïf)
# ------------------------------------------------------------
p_theil <- ggplot(bench_summary, aes(x = reorder(Country, TheilU_naive), y = TheilU_naive)) +
  geom_col(fill = "#2E86AB", alpha = 0.9) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 1) +
  coord_flip() +
  labs(
    title = "Comparaison Benchmark — Theil’s U (ARIMA vs Naïf)",
    subtitle = "Theil’s U < 1 → le modèle Rolling ARIMA est meilleur que le modèle naïf",
    x = "Country",
    y = "Theil’s U"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

# Sauvegarde du graphe
fig_theil <- file.path(base_dir, "figures", paste0("Benchmark_TheilU_", stamp, ".png"))
ggsave(fig_theil, p_theil, width = 9, height = 6, dpi = 300)
cat("✅ Graph Theil’s U enregistré :", fig_theil, "\n")

# ------------------------------------------------------------
# 2️⃣ Graphique comparatif MAPE (ARIMA / Naïf / Moyenne)
# ------------------------------------------------------------

# Conversion au format long pour ggplot
bench_long <- bench_summary %>%
  select(Country, MAPE_model, MAPE_naive, MAPE_mean4) %>%
  pivot_longer(cols = starts_with("MAPE"), names_to = "Model", values_to = "MAPE") %>%
  mutate(Model = recode(Model,
                        MAPE_model = "Rolling ARIMA",
                        MAPE_naive = "Naïf",
                        MAPE_mean4 = "Moyenne 4Q"))

# Graphique MAPE
p_mape <- ggplot(bench_long, aes(x = reorder(Country, MAPE), y = MAPE, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.9) +
  coord_flip() +
  labs(
    title = "MAPE par Modèle et par Pays (2010–2019)",
    subtitle = "Comparaison des erreurs moyennes de prévision (%)",
    x = "Country",
    y = "MAPE (%)",
    fill = "Modèle"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Sauvegarde du graphe
fig_mape <- file.path(base_dir, "figures", paste0("Benchmark_MAPE_comparison_", stamp, ".png"))
ggsave(fig_mape, p_mape, width = 10, height = 7, dpi = 300)
cat("✅ Graph MAPE comparatif enregistré :", fig_mape, "\n")

# ------------------------------------------------------------
# 3️⃣ (Optionnel) Heatmap Theil’s U par pays (classement visuel)
# ------------------------------------------------------------
p_heat_theil <- ggplot(bench_summary, aes(x = "", y = reorder(Country, TheilU_naive), fill = TheilU_naive)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(TheilU_naive, 2)), color = "black", size = 3) +
  scale_fill_viridis_c(option = "C", name = "Theil’s U") +
  labs(
    title = "Heatmap — Theil’s U (ARIMA vs Naïf)",
    subtitle = "Plus la couleur est foncée, meilleure est la performance (U < 1)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

fig_heat <- file.path(base_dir, "figures", paste0("Benchmark_Heatmap_TheilU_", stamp, ".png"))
ggsave(fig_heat, p_heat_theil, width = 8, height = 6, dpi = 300)
cat("✅ Heatmap Benchmark enregistrée :", fig_heat, "\n")

