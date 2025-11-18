############################################################
# BENCHMARK AR(1) SUR LA ZONE EURO - ROLLING WINDOW
############################################################

library(readxl)
library(dplyr)
library(zoo)
library(forecast)

## 1) Import de la série agrégée zone euro (déjà désaisonnalisée + winsorisée)
url_Eurozone_GDPgrowth_seas <- "https://github.com/Styrs/Forcasting_euro_growth/raw/refs/heads/main/Eurozone_GDPgrowth_seas.xlsx"

download.file(
  url_Eurozone_GDPgrowth_seas,
  destfile = "Eurozone_GDPgrowth_seas.xlsx",
  mode     = "wb"
)

Eurozone_GDPgrowth_seas <- read_excel("Eurozone_GDPgrowth_seas.xlsx")

# On crée une version numérique de Quarter cohérente (2000.25, 2000.50, etc.)
if (inherits(Eurozone_GDPgrowth_seas$Quarter, "yearqtr")) {
  Eurozone_GDPgrowth_seas <- Eurozone_GDPgrowth_seas %>%
    mutate(Quarter_num = as.numeric(Quarter))
} else {
  Eurozone_GDPgrowth_seas <- Eurozone_GDPgrowth_seas %>%
    mutate(Quarter_num = as.numeric(Quarter))
}

# On garde seulement les obs non manquantes et on trie par date
ez_data <- Eurozone_GDPgrowth_seas %>%
  arrange(Quarter_num) %>%
  filter(!is.na(seasonal_adjusted_wins))

# Vérification rapide
head(ez_data)
tail(ez_data)

## 2) Paramètres du rolling (on prend les mêmes que pour le modèle global)
debut_total <- 2000.25     # début de la période exploitable
fin_total   <- 2025.00     # fin des données (2025-Q1)
nb_boucles  <- 40          # nombre max de fenêtres

## 3) Grille temporelle pour le tableau triangulaire du benchmark AR(1)

start_year    <- floor(debut_total)
start_quarter <- (debut_total - start_year) * 4 + 1
periode_debut <- debut_total + 9.75   # fin de la 1ère fenêtre + 1er forecast

# On génère la liste des périodes qui couvriront toutes les prévisions
periodes_tableau_ar1 <- character(0)
current_date <- periode_debut

for (i in 1:(nb_boucles + 9)) {   # +9 pour couvrir tous les horizons
  year    <- floor(current_date)
  quarter <- ((current_date - year) * 4) + 1
  periodes_tableau_ar1 <- c(periodes_tableau_ar1,
                            paste0(year, "-Q", quarter))
  current_date <- current_date + 0.25
}

## 4) Tableau triangulaire des erreurs (benchmark AR(1))
tableau_erreurs_croissance_ar1 <- matrix(
  NA,
  nrow = length(periodes_tableau_ar1),
  ncol = nb_boucles
)

rownames(tableau_erreurs_croissance_ar1) <- periodes_tableau_ar1
colnames(tableau_erreurs_croissance_ar1) <- paste0("Boucle_", 1:nb_boucles)

## Horizons de prévision (les mêmes que pour ton modèle global)
horizons <- 1:10   # 10 horizons, on fera l'erreur à chaque période

# Boucle rolling sur les fenêtres (une boucle = une fenêtre temporelle)
for (i in 1:nb_boucles) {
  
  cat("\n=============================\n")
  cat("=== BOUCLE AR(1) GLOBALE", i, "===\n")
  
  # 3.1 Fenêtre d'estimation : 10 ans de données
  start_date <- debut_total + (i - 1) * 0.20    # même décalage que ton code : 0.20
  end_date   <- start_date + 9.75               # 10 ans = 40 trimestres
  
  # Si on ne peut plus forecast 10 périodes sans sortir de l'échantillon, on stoppe
  if (end_date + 2.50 > fin_total) {            # 10 horizons * 0.25 = 2.50
    cat("Arrêt de la boucle : fin des données atteinte à l'itération", i, "\n")
    break
  }
  
  cat("Période d'entraînement :", start_date, "à", end_date, "\n")
  cat("Forecasts jusqu'à      :", end_date + 2.50, "\n")
  
  # 3.2 Extraire les données de la zone euro dans la fenêtre
  train_data <- ez_data %>%
    filter(Quarter_num >= start_date,
           Quarter_num <= end_date)
  
  # Sécurité : vérifier qu'on a assez d'observations
  if (nrow(train_data) < 20) {
    cat("Fenêtre trop courte à l'itération", i, "- on passe.\n")
    next
  }
  
  # 3.3 Construction de la ts trimestrielle
  start_year_train  <- floor(start_date)
  start_quarter_train <- as.integer((start_date - start_year_train) * 4 + 1)
  
  ez_ts <- ts(
    train_data$seasonal_adjusted_wins,
    start     = c(start_year_train, start_quarter_train),
    frequency = 4
  )
  
  # 3.4 Modèle AR(1) imposé (benchmark)
  ar1_model <- Arima(
    ez_ts,
    order        = c(1, 0, 0),  # AR(1)
    include.mean = TRUE
  )
  
  cat("Modèle utilisé (benchmark) : AR(1)\n")
  
  # 3.5 Forecast 10 périodes en avant
  fc <- forecast(ar1_model, h = length(horizons))
  forecast_values <- as.numeric(fc$mean)   # longueur 10
  
  # 3.6 Calcul des erreurs par horizon (1 à 10)
  erreurs_croissance <- numeric(length(horizons))
  
  # On va aussi calculer les dates correspondantes
  total_quarters <- (end_date - start_date) * 4   # nb de trimestres dans la fenêtre
  
  for (j in seq_along(horizons)) {
    horizon <- horizons[j]
    
    # Date future : même logique que dans ton code global
    year    <- start_year_train + floor((total_quarters + horizon) / 4)
    quarter <- ((horizon - 1) %% 4) + 1
    quarter_value <- year + (quarter - 1) * 0.25   # version numérique
    
    # Vraie croissance observée pour la zone euro
    true_val <- ez_data$seasonal_adjusted_wins[
      ez_data$Quarter_num == quarter_value
    ]
    
    if (length(true_val) == 0 || is.na(true_val[1])) {
      # pas de vraie valeur → on laisse NA
      erreurs_croissance[j] <- NA_real_
    } else {
      erreurs_croissance[j] <- forecast_values[j] - true_val[1]
    }
  }
  
  # 3.7 Position de départ dans la matrice triangulaire
  start_periode_index <- i   # comme ton code : boucle 1 → ligne 1, boucle 2 → ligne 2, etc.
  
  # On remplit 10 lignes (une par horizon) dans la colonne i
  for (j in 1:10) {
    if ((start_periode_index + j - 1) <= nrow(tableau_erreurs_croissance_ar1)) {
      tableau_erreurs_croissance_ar1[start_periode_index + j - 1, i] <- erreurs_croissance[j]
    }
  }
  
  cat("Boucle", i, "terminée - erreurs AR(1) ajoutées à partir de la ligne", start_periode_index, "\n")
}

# ----------------------------------------------------------------------------
### AFFICHAGE & SAUVEGARDE DU TABLEAU AR(1)
# ----------------------------------------------------------------------------

cat("\n=== TABLEAU FINAL DES ERREURS DE CROISSANCE - BENCHMARK AR(1) ===\n")
print(tableau_erreurs_croissance_ar1)

# Sauvegarde CSV
write.csv(
  tableau_erreurs_croissance_ar1,
  "tableau_erreurs_croissance_benchmark_AR1_triangulaire.csv"
)

# Résumé statistique des erreurs AR(1)
cat("\n=== RÉSUMÉ STATISTIQUE - BENCHMARK AR(1) ===\n")
cat("Nombre de boucles (max théorique) :", nb_boucles, "\n")
cat("Nombre de périodes dans le tableau :", nrow(tableau_erreurs_croissance_ar1), "\n")
cat("Moyenne des erreurs de croissance (AR(1)) :",
    round(mean(tableau_erreurs_croissance_ar1, na.rm = TRUE), 4), "\n")
cat("Écart-type des erreurs de croissance (AR(1)) :",
    round(sd(tableau_erreurs_croissance_ar1, na.rm = TRUE), 4), "\n")

