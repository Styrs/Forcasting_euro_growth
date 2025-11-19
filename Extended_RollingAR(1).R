#### Libraries

library(forecast)
library(readxl)
library(dplyr)

# ----------------------------------------------------------------------------
# B1 – PARAMÈTRES GLOBAUX & DONNÉES EUROZONE
# ----------------------------------------------------------------------------

# Paramètres de la fenêtre "expanding" (mêmes dates que le code pays)
first_quarter       <- 2000.25      # début de l’échantillon
initial_end_quarter <- 2010.00      # fin de la première fenêtre d’estimation
last_end_quarter    <- 2022.75      # dernière fin de fenêtre autorisée
max_data_quarter    <- 2025.25      # dernier trimestre pour lequel on a des données

forecast_horizons <- 1:10           # h = 1,...,10

# --- Import de la série Eurozone désaisonnalisée & winsorisée ---

url_Eurozone_GDPgrowth_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_GDPgrowth_seas.xlsx"
download.file(url_Eurozone_GDPgrowth_seas, destfile = "Eurozone_GDPgrowth_seas.xlsx", mode = "wb")
Eurozone_GDPgrowth_seas <- read_excel("Eurozone_GDPgrowth_seas.xlsx")

# On crée un code numérique de trimestre cohérent avec vos autres scripts
# (par ex. 2010-Q1 -> 2010.00, 2010-Q2 -> 2010.25, etc.)
if (inherits(Eurozone_GDPgrowth_seas$Quarter, "yearqtr")) {
  Eurozone_GDPgrowth_seas <- Eurozone_GDPgrowth_seas %>%
    mutate(Quarter_Value = as.numeric(Quarter))
} else {
  Eurozone_GDPgrowth_seas <- Eurozone_GDPgrowth_seas %>%
    mutate(Quarter_Value = as.numeric(Quarter))
}

# On garde la série de croissance observée pour la suite
ea_obs <- Eurozone_GDPgrowth_seas %>%
  select(Quarter_Value, Observed_Growth = seasonal_adjusted_wins)

# ----------------------------------------------------------------------------
# B2 – PRÉPARER LA SÉRIE TEMPORELLE EUROZONE POUR UNE FENÊTRE
# ----------------------------------------------------------------------------

prepare_euro_ts <- function(data, start_quarter, end_quarter) {
  
  # 1) On filtre la fenêtre d’estimation
  train_data <- data %>%
    filter(Quarter_Value >= start_quarter,
           Quarter_Value <= end_quarter) %>%
    arrange(Quarter_Value)
  
  # 2) Définition du début de la ts
  start_year    <- floor(start_quarter)
  start_q_index <- as.integer((start_quarter - start_year) * 4 + 1)  # 1..4
  
  # 3) Construction de la ts sur la croissance observée
  ts_data <- ts(
    train_data$seasonal_adjusted_wins,
    start     = c(start_year, start_q_index),
    frequency = 4
  )
  
  return(list(
    train_data = train_data,
    ts_data    = ts_data
  ))
}


# ----------------------------------------------------------------------------
# B3 – FORECAST AR(1) + DATES FUTURES
# ----------------------------------------------------------------------------

forecast_ar1_growth <- function(ts_data, end_quarter, horizons) {
  
  # 1) Estimation du modèle AR(1) (benchmark)
  ar1_model <- Arima(
    ts_data,
    order        = c(1, 0, 0),  # AR(1)
    include.mean = TRUE
  )
  
  # 2) Prévisions jusqu'au max des horizons
  fc <- forecast(ar1_model, h = max(horizons))
  
  # 3) On garde uniquement les horizons demandés
  fc_values <- as.numeric(fc$mean[horizons])
  
  # 4) Conversion des horizons en Quarter_Value numérique
  #    end_quarter est la fin de la fenêtre (ex : 2010.00),
  #    donc Quarter_Value_future = end_quarter + 0.25 * h
  Quarter_Value <- end_quarter + 0.25 * horizons
  
  # 5) Tableau de résultats
  results <- data.frame(
    Quarter_Value   = Quarter_Value,
    Horizon         = horizons,
    Growth_Forecast = fc_values
  )
  
  return(results)
}


# ----------------------------------------------------------------------------
# B4 – EXTENDED ROLLING WINDOW AR(1) SUR LA ZONE EURO
# ----------------------------------------------------------------------------

all_iterations_results <- list()

# Initialisation de la fenêtre (comme dans le code pays)
start_quarter <- first_quarter
end_quarter   <- initial_end_quarter
iteration     <- 1

while (TRUE) {
  
  # Dernier trimestre forecasté si on va jusqu'à h = max(forecast_horizons)
  last_forecast_quarter <- end_quarter + max(forecast_horizons) * 0.25
  
  # Même règle d'arrêt que tes collègues
  if (end_quarter > last_end_quarter || last_forecast_quarter > max_data_quarter) {
    message("Stopping benchmark loop: reached last allowed estimation or forecast date.")
    break
  }
  
  message(paste("Iteration", iteration,
                "- estimation window:", start_quarter, "to", end_quarter))
  
  # 1) Préparer la ts sur la fenêtre courante
  window_data <- prepare_euro_ts(
    data          = Eurozone_GDPgrowth_seas,
    start_quarter = start_quarter,
    end_quarter   = end_quarter
  )
  
  ts_data <- window_data$ts_data
  
  # 2) Forecast AR(1) sur cette fenêtre
  fc_table <- forecast_ar1_growth(
    ts_data     = ts_data,
    end_quarter = end_quarter,
    horizons    = forecast_horizons
  )
  
  # 3) Ajouter les métadonnées (utile pour la matrice d'erreurs)
  fc_table$Estimation_End_Qtr <- end_quarter
  fc_table$Loop               <- iteration   # numéro de la fenêtre (comme "Loop" dans Ch.7)
  
  # 4) Stocker le résultat de cette itération
  all_iterations_results[[iteration]] <- fc_table
  
  # 5) Étendre la fenêtre d'un trimestre
  end_quarter <- end_quarter + 0.25
  iteration   <- iteration + 1
}

# Combiner toutes les itérations en un seul data.frame
benchmark_fc_df <- do.call(rbind, all_iterations_results)


# ----------------------------------------------------------------------------
# B5 – MATRICE D'ERREURS POUR LE BENCHMARK AR(1)
# ----------------------------------------------------------------------------

# 1) Joindre les prévisions AR(1) avec la croissance observée
benchmark_eval <- merge(
  benchmark_fc_df,
  ea_obs,
  by = "Quarter_Value",
  all.x = TRUE
)

# Erreur de forecast : AR(1) - observé
benchmark_eval$Forecast_Error <- benchmark_eval$Growth_Forecast -
  benchmark_eval$Observed_Growth

# 2) On a déjà Loop dans benchmark_fc_df, donc pas besoin de recréer un mapping

# 3) Passer en matrice large (rows = Quarter_Value, cols = Loop)
error_matrix_long_bench <- benchmark_eval[, c("Quarter_Value", "Loop", "Forecast_Error")]

error_matrix_wide_bench <- reshape(
  error_matrix_long_bench,
  idvar   = "Quarter_Value",
  timevar = "Loop",
  direction = "wide"
)

# Trier par date
error_matrix_wide_bench <- error_matrix_wide_bench[order(error_matrix_wide_bench$Quarter_Value), ]

# Renommer les colonnes : Forecast_Error.1 → Error_L1, etc.
colnames(error_matrix_wide_bench) <- gsub("Forecast_Error\\.", "Error_L", colnames(error_matrix_wide_bench))

# 4) Ajouter la croissance observée en première colonne (comme dans le code pays)
final_error_table_benchmark <- merge(
  ea_obs,
  error_matrix_wide_bench,
  by = "Quarter_Value",
  all.y = TRUE     # on garde seulement les périodes forecastées
)

final_error_table_benchmark <- final_error_table_benchmark[order(final_error_table_benchmark$Quarter_Value), ]

# Optionnel : export
# write.csv(final_error_table_benchmark,
#           "final_error_table_benchmark_AR1.csv",
#           row.names = FALSE)
