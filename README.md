Overview: 
This project develops a comprehensive framework to forecast Eurozone real GDP growth and to evaluate the statistical performance of the forecasts. Using quarterly national accounts and deflator data from Eurostat, the code first constructs a harmonized and deseasonalized dataset for major Eurozone countries and an aggregate of smaller members. Forecasts are produced using ARMA time-series models estimated at the country level, with nominal GDP forecasts combined through aggregation and deflation to obtain Eurozone real growth projections. Annual forecasts for 2025–2027 are accompanied by confidence intervals derived from predictive distributions. The framework also computes country-level weights and contributions to Eurozone growth, both in nominal and real terms. Model performance is assessed through an extensive forecast evaluation exercise, including MSFE, Ljung–Box and Mincer–Zarnowitz tests, as well as density-based diagnostics such as the Berkowitz test and predictive density comparisons against a benchmark model. All results are automatically exported in reproducible Excel and LaTeX formats.
How to Run the Code
To run the scripts and generate forecasts of Eurozone real GDP growth, please follow the steps below.
Prerequisites
Before running the code, make sure that:
1.	Make sure that all the scripts. 
"Data_transformation.R", “Final_forecast_extended.R” ,”Benchmark_model.R”, "Iterated_model_evaluation.R", "Deflator_forecast.R", "Functions_Data_Forecast_Evaluation.R" are in the same folder as “Main.R”. 

2.	The R package pacma is installed.
3.	You have an active internet connection (the raw data are downloaded directly from GitHub). 
4.	A folder named Results exists in the same directory as Main.R.
(If it already exists, this will not cause any issue.)
Once these conditions are satisfied, you can safely run Main.R.
Running the full script will:
1.	Load all required scripts and libraries,
2.	Import and transform the data,
3.	Produce forecasts,
4.	Run a comprehensive set of statistical tests to evaluate the model.
Note: Running the full evaluation can take some time.
If you do not want to run the entire pipeline, you can:
•	Execute Part 1 and Part 2 (scripts loading and data transformation),
•	Then run either Part 3 (Forecast) or Part 4 (Evaluation) independently.
For more details, see the section “Structure of the Code” below.
All outputs are saved in the Results folder in both Excel and LaTeX formats.
Details are provided in the section “The Results”.




Structure of the code:
The codebase is organized around a main script (Main.R), which coordinates all other scripts.
Main.R centralizes parameters, calls the relevant functions, and stores the resulting tables.
The code is structured into four main parts.

The first part “1. Import the other files and install packages”:
This section loads all required libraries and source files.
•	The package pacman is used to ensure that all required libraries are installed and loaded: “readxl, dplyr, tidyr, zoo, seasonal, writexl, scales, forecast, ggplot2, tidyverse, rlang, lmtest, xtable, stats”

•	Then it charges the other scripts in the same folder executed later in the main file. 
o	"Data_transformation.R", 
o	“Final_forecast_extended.R”,
o	”Benchmark_model.R”, 
o	"Iterated_model_evaluation.R", 
o	"Deflator_forecast.R". 
Each of these scripts is important for the execution and their purpose will be detailed later. "Functions_Data_Forecast_Evaluation.R" is a bit different, while never directly used, it contains nearly all the functions executed in all the code. They are separated from their place of execution in order to gain clarity. 
The second part “2. Import raw data and transform them”:
This step prepares all data needed for forecasting and evaluation using the script “Data_transformation.R”.
•	We execute Create_the_data_sets which is the function that execute nearly all the “Data_transformation.R” script. It imports from Github the raw data, transforms them completely to be ready for the next steps and is never touched after it.

•	The raw data are “Countries_Excel_euro_GDP.xlsx”, “Data_GDP_SmallEuroCountries.xlsx” and “2005_linked_deflator.xlsx”. They are available in this public GitHub link: https://github.com/Styrs/Forcasting_euro_growth. We decided to have the raw data imported form internet and not directly download on the computers in order to being able to executes easily the code on any computer. It is also easier to update the data in this centralized way. 
The data contains the quarterly nominal GDP of each Eurozone country from 2000 to 2025. It also contains the quarterly deflator of the Eurozone and each Eurozone country from 2000 to 2025. All the data comes from Eurostat, https://ec.europa.eu/eurostat. 
•	The key output of this part is:
o	 “Euro_Countries_GDP_Growth_Log” a data set containing all the treated quarterly data and core to nearly every script.
o	“annual_growth_observed” is a data set with annualize historical values. Useful for the final annual forecasts.
o	“annual_growth_observed_display” is just a table easier to read but not used in the code. 
The third part “3. Launch the forecast for 2025 to 2027”:
This part generates forecasts for annual real GDP growth in 2025, 2026, and 2027.. In this part we use the scripts “Final_forecast_extended.R” and “Deflator_forecast.R”.
•	We first define important variables:
o	confidence_quantiles = 0.1 by default, this variable defines the confidence intervals shown and computed in the forecast. The default value is 10%.

•	Then we execute run_deflator_forecast. This function simply executes the hole “Deflator_forecast.R” script. It produces a quarterly forecast of the log-diff of the deflator for each country from the second half of 2025 to 2027. After some transformations, it finally returns the forecasted annual deflator for each country and its confidence interval. 

•	Then we execute run_the_forecast. This function simply executes the hole “Final_forecast_extended.R” script. It produces a quarterly forecast of the log-diff of the nominal gdp for each country from the second half of 2025 to 2027. It then transforms them into annual nominal GDP and growth. After that, it takes the deflator forecasts and also computes the real GDP and growth rate. It finally computes the weights and contribution of each’s countries to the Eurozone growth rate. It returns the country’s contributions, the real/nominal GDP/growth and their confidence intervals. 


•	Finaly, we call a function plot_real_growth_forecast_ci from the script “Functions_Data_Forecast_Evaluation.R”. It shows a graph of last year’s growth and the forecasted growth with confidence intervals. 

•	The key outputs of this are part is:
o	countries_gowth_contributions, a table with the relative and pure contribution of each country to the Eurozone GDP.
o	annual_growth_all, a data frame with the annual forecasts and their confidence intervals. 
o	annual_growth_all_display, the same table with more readable numbers.
The fourth part “4. Lauch the forecast evaluation”:
In this part we evaluate the model by applying a series of statistical tests and comparing the model with a simpler “naïve” model. The script used are “Benchmark_model.R” and “Iterated_model_evaluation.R”

•	We first define important variables:
o	first_quarter = 2000.25 (default value); Form when we use historical data.
o	initial_end_quarter = 2010.00 (default value); From when we produced forecast to compare the model with real values. 
o	last_end_quarter = 2022.75 (default value); When expending its scope of historical data, this value prevents the model of taking newer historical value and will stop there. If you want to stop the model before this date, change this value.
o	max_data_quarter = 2025.25 (default value); When forecasting values and comparing to historical data, this is the furthest value he will forecast because we have no longer historical data to compare to. If this value is reach, the model will stop forecasting like with the last_end_quarter. We advise you to not touch this value to play with the model but to do so with last_end_quarter.
o	forecast_horizons = 1:10. (default value) this is the numbers of horizons the model will forecast. 
o	logdiff_to_train_on is which value the model use to produce it’s forecast. We have 3 possiblilty: gdp_growth_log, gdp_growth_log_wins_001 (default), gdp_growth_log_wins_005. Basically, not winsorized, winsorized at 1%, winsorized at 5%.
o	number_of_draw_of_Forecast = 200 (default). The model estimate the forecast mean (a classic forecast) and the 200 random forecast that are not the mean but follow the standard deviation of the forecast. It is used to compute the forecast’s variance of the Eurozone. 
•	Then it executes evaluate_benchmark_model. This function executes the hole “Benchmark_model.R” script. It run a series of quarterly nominal GDP log-diff forecast form initial_end_quarter to max_data_quarter. 
Then it applies a series of tests ljung_box, Mincer_Zarnowitz. And a list of useful statistical information MSFE, a matrix of the errors…. This is important for the next step, where we evaluate the model and compare it to this one. 
•	Finaly, we execute evaluate_model. This function simply executes the hole “Iterated_model_evaluation.R” script. It run a series of quarterly nominal GDP log-diff forecasts form initial_end_quarter to max_data_quarter. It also compute the density forecast for each period in addition to the mean forecast.
Then Then it applies a series of tests ljung_box, Mincer_Zarnowitz and Berkowitz. It also computes a comparison to the benchmark with a ration of the MSFE, Diebold-Mariano test and compares the density forecast on the benchmark and the model.
•	The key outputs are:
o	results_table and results_table_bench; tables that summarize the tests mentioned above for the model and the benchmark.
o	MZ_table and MZ_table_bench: tables that show more information of the Mincer_Zarnowitz tests for the model and benchmark.
o	lb_table and lb_table_bench: tables that show more information of the ljung_box test for the model and the benchmark.
The results: 
The important results are:
•	annual_growth_all_display; which contains the forecasts, the confidence intervals and the observed past data.

•	countries_gowth_contributions; which contains the relative and pure contribution of all countries to the Eurozone growth rate real/nominal. 

•	results_table and results_table_bench; tables that summarize the statistical tests for the model and the benchmark

•	MZ_table and MZ_table_bench: tables that show more information of the Mincer_Zarnowitz tests for the model and benchmark.

•	lb_table and lb_table_bench: tables that show more information of the ljung_box test for the model and the benchmark.
All these results can be visioned in R studio when the code has been runed. There are also available in Excel format, in the folder “Results” that will be created in the same folder as Main.py. The code also imports the results in Latex format.
Contact 
This work has been done by Mathieu Schneider, Dominique Ziele-Comiati, Samuel Vermeulen.
If you have any questions, write us at Mathieu.Schneider@Unil.ch, dominique.ziele-comiati@unil.ch or samuel.vermeulen@unil.ch

