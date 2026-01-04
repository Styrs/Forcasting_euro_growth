#-------------------------------------------------------------------------------
# 1. Import the other files and install packages 
#-------------------------------------------------------------------------------
library(pacman)

pacman::p_load(
  readxl, dplyr, tidyr, zoo, seasonal, writexl, scales,
  forecast, ggplot2, tidyverse, rlang, lmtest, xtable, stats
)

source("Data_transformation.R")
source("Final_forecast_extended.R")
source("Deflator_forecast.R")
source("Functions_Data_Forecast_Evaluation.R")
source("Benchmark_model.R")
source("Iterated_model_evaluation.R")

#create a results folder for future results. 
dir.create("Results", showWarnings = FALSE, recursive = TRUE)


#-------------------------------------------------------------------------------
# 2. Import raw data and transform them
#-------------------------------------------------------------------------------


#################### Create the data frame and transform them ##################
Create_the_data_sets_result <- Create_the_data_sets()


Euro_Countries_GDP_Growth_Log       <- Create_the_data_sets_result$Euro_Countries_GDP_Growth_Log
annual_growth_observed              <- Create_the_data_sets_result$annual_growth_observed
annual_growth_observed_display      <- Create_the_data_sets_result$annual_growth_observed_display

#-------------------------------------------------------------------------------
# 3. Launch the forecast for 2025 to 2027
#-------------------------------------------------------------------------------

################### constant for the forecast ##################################


confidence_quantiles <- 0.1 #confidence intervals quantiles


#################### Forecast the deflator #####################################

#Forecast the log-diff deflator of the countries, and treat them
annual_deflator_forecast <- run_deflator_forecast(Euro_Countries_GDP_Growth_Log,annual_growth_observed,confidence_quantiles)

#################### Forecast the nominal log-diff #############################

#Forecast the log-diff nominal GDP of the countries, and treat them
run_the_forecast_result <- run_the_forecast(Euro_Countries_GDP_Growth_Log,annual_growth_observed,annual_deflator_forecast,confidence_quantiles)


countries_gowth_contributions       <-run_the_forecast_result$countries_gowth_contributions
annual_growth_all                   <-run_the_forecast_result$annual_growth_all
annual_growth_all_display           <-run_the_forecast_result$annual_growth_all_display

#plot to see the forecats
plot_real_growth_forecast_ci(
  annual_growth_all = annual_growth_all,
  annual_growth_observed = annual_growth_observed,
  country = "Eurozone",
  year_from = 2019
)

#################### register the results: #####################################

writexl::write_xlsx(annual_growth_all_display,
                    "Results/annual_growth_all_display.xlsx")

capture.output(
  print(xtable::xtable(annual_growth_all_display, digits = 3),
        include.rownames = FALSE),
  file = "Results/annual_growth_all_display.tex"
)


writexl::write_xlsx(countries_gowth_contributions,
                    "Results/countries_gowth_contributions.xlsx")

capture.output(
  print(xtable::xtable(countries_gowth_contributions, digits = 3),
        include.rownames = FALSE),
  file = "Results/countries_gowth_contributions.tex"
)



#-------------------------------------------------------------------------------
# 4. Lauch the forecast evaluation
#-------------------------------------------------------------------------------

#################### define the parameters for the forecast evaluations#########


first_quarter       <- 2000.25      
initial_end_quarter <- 2010.00

last_end_quarter    <- 2022.75    # 2022.75 for full data set 
max_data_quarter    <- 2025.25    

forecast_horizons <- 1:10 

logdiff_to_train_on <- "gdp_growth_log_wins_001"

number_of_draw_of_Forecast <- 200 #number of draw we do to estimate the forecast distribution of the Eurozone.


#################### lauch the benchmark evaluation ############################
evaluate_benchmark_model_results <- evaluate_benchmark_model(Euro_Countries_GDP_Growth_Log,first_quarter,initial_end_quarter,
                         last_end_quarter,max_data_quarter,forecast_horizons,logdiff_to_train_on,number_of_draw_of_Forecast)

all_growth_results_bench            <-evaluate_benchmark_model_results$all_growth_results_bench
results_table_bench                 <-evaluate_benchmark_model_results$results_table_bench
MZ_table_bench                      <-evaluate_benchmark_model_results$MZ_table_bench
lb_table_bench                      <-evaluate_benchmark_model_results$lb_table_bench
error_matrix_bench                  <-evaluate_benchmark_model_results$error_matrix_bench
pred_var_table_bench                 <-evaluate_benchmark_model_results$pred_var_table_bench

#################### register the results: #####################################
writexl::write_xlsx(
  data.frame(Horizon = rownames(results_table_bench), results_table_bench),
  "Results/results_table_bench.xlsx"
)

capture.output(
  print(xtable::xtable(results_table_bench, digits = 4),
        include.rownames = TRUE),
  file = "Results/results_table_bench.tex"
)


writexl::write_xlsx(MZ_table_bench,
                    "Results/MZ_table_bench.xlsx")

capture.output(
  print(xtable::xtable(MZ_table_bench, digits = 3),
        include.rownames = FALSE),
  file = "Results/MZ_table_bench.tex"
)


writexl::write_xlsx(
  data.frame(Horizon = rownames(lb_table_bench), lb_table_bench),
  "Results/lb_table_bench.xlsx"
)

capture.output(
  print(xtable::xtable(lb_table_bench, digits = 3),
        include.rownames = TRUE),
  file = "Results/lb_table_bench.tex"
)




#################### lauch the model evaluation ################################

evaluate_model_results <- evaluate_model(Euro_Countries_GDP_Growth_Log,first_quarter,initial_end_quarter,
               last_end_quarter,max_data_quarter,forecast_horizons,results_table_bench,
               error_matrix_bench,logdiff_to_train_on,number_of_draw_of_Forecast,pred_var_table_bench)



results_table                       <-evaluate_model_results$results_table
error_matrix                        <-evaluate_model_results$error_matrix
MZ_table                            <-evaluate_model_results$MZ_table
lb_table                            <-evaluate_model_results$lb_table
all_growth_results                  <-evaluate_model_results$all_growth_results
pred_var_table                      <-evaluate_model_results$pred_var_table
simulated_path_all_with_eurozone    <-evaluate_model_results$simulated_path_all_with_eurozone


#################### register the results: #####################################

writexl::write_xlsx(
  data.frame(Horizon = rownames(results_table), results_table),
  "Results/results_table.xlsx"
)

capture.output(
  print(xtable::xtable(results_table, digits = 4),
        include.rownames = TRUE),
  file = "Results/results_table.tex"
)


writexl::write_xlsx(MZ_table,
                    "Results/MZ_table.xlsx")

capture.output(
  print(xtable::xtable(MZ_table, digits = 3),
        include.rownames = FALSE),
  file = "Results/MZ_table.tex"
)


writexl::write_xlsx(
  data.frame(Horizon = rownames(lb_table), lb_table),
  "Results/lb_table.xlsx"
)

capture.output(
  print(xtable::xtable(lb_table, digits = 3),
        include.rownames = TRUE),
  file = "Results/lb_table.tex"
)


