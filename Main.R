#-------------------------------------------------------------------------------
# 1. Import the other files
#-------------------------------------------------------------------------------

source("Data_transformation.R")
source("Final_forecast_extended.R")
source("Deflator_forecast.R")
source("Functions_Data_Forecast_Evaluation.R")
source("Benchmark_model.R")
source("Iterated_model_evaluation.R")


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

#################### Forecast the deflator #####################################

#Forecast the log-diff deflator of the countries, and treat them
annual_deflator_forecast <- run_deflator_forecast(Euro_Countries_GDP_Growth_Log,annual_growth_observed)

#################### Forecast the nominal log-diff #############################

#Forecast the log-diff nominal GDP of the countries, and treat them
run_the_forecast_result <- run_the_forecast(Euro_Countries_GDP_Growth_Log,annual_growth_observed,annual_deflator_forecast)


countries_gowth_contributions       <-run_the_forecast_result$countries_gowth_contributions
annual_growth_all                   <-run_the_forecast_result$annual_growth_all
annual_growth_all_display           <-run_the_forecast_result$annual_growth_all_display


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




#################### lauch the benchmark evaluation ############################
evaluate_benchmark_model_results <- evaluate_benchmark_model(Euro_Countries_GDP_Growth_Log,first_quarter,initial_end_quarter,
                         last_end_quarter,max_data_quarter,forecast_horizons,logdiff_to_train_on)

all_growth_results_bench            <-evaluate_benchmark_model_results$all_growth_results_bench
results_table_bench                 <-evaluate_benchmark_model_results$results_table_bench
MZ_table_bench                      <-evaluate_benchmark_model_results$MZ_table_bench
lb_table_bench                      <-evaluate_benchmark_model_results$lb_table_bench
error_matrix_bench                  <-evaluate_benchmark_model_results$error_matrix_bench


#################### lauch the model evaluation ################################

evaluate_model_results <- evaluate_model(Euro_Countries_GDP_Growth_Log,first_quarter,initial_end_quarter,
               last_end_quarter,max_data_quarter,forecast_horizons,results_table_bench,error_matrix_bench,logdiff_to_train_on)



results_table                       <-evaluate_model_results$results_table
error_matrix                        <-evaluate_model_results$error_matrix
MZ_table                            <-evaluate_model_results$MZ_table
lb_table                            <-evaluate_model_results$lb_table
all_growth_results                  <-evaluate_model_results$all_growth_results