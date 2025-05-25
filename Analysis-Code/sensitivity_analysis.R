### Sensitivity Analysis

# Jenna Greene May 2025

setwd('../')
setwd('02_scripts')
source('clean_hatch_data.R')
source('bivariate_analysis.R')
## 
setwd('05_robustness')
setwd('sensitivity')

#### FILTER R2 Values ####
# Filter dataset
R2_filter <- 0.95
# Filter to only include logistic fits
hatch_growth_filter <- hatch_growth[which(hatch_growth$Logistic.Fit >= 0 & 
                                            hatch_growth$Logistic.Fit != 'err' &
                                            hatch_growth$Logistic.Fit <= 5 &
                                            hatch_growth$logistic.goodness > R2_filter), ]

num_variables_gov <-c("direction_scitech_Average", 
                      "easier_scitech_Average",
                      "Capacity_Average",
                      "gee_Average",
                      "rdspending_Average",
                      "Log_GDPavg", 
                      "dem")

num_variables_tech <-c("log_Granularity.Numerical",
                       "log_Material.Use.Numerical",
                       "log_Average.lifetime",
                       "FirstCommercialYr")

cat_variables_tech <- c("Complexity", 
                        "Need.for.Customization",
                        "Type.of.Adopter",
                        "Feedstock",
                        "Broad.replacement")

## 
tech_num_df <- select(hatch_growth_filter, "log_Granularity.Numerical",
                      "log_Material.Use.Numerical",
                      "log_Average.lifetime",
                      "FirstCommercialYr")

tech_cat_df <- select(hatch_growth_filter, 
                      "Complexity", 
                      "Need.for.Customization",
                      "Type.of.Adopter",
                      "Feedstock",
                      "Broad.replacement")
gov_df <- select(hatch_growth_filter, 
                 "direction_scitech_Average", 
                 "easier_scitech_Average",
                 #"gee_Average",
                 #"dem",
                 "Capacity_Average",
                 "rdspending_Average",
                 "Log_GDPavg") 
#"dem")
#"regime_Mode")

### Create regressions of lists of variables
reg_list <- function(df, dep_var, indep_vars) {
  modelList <- lapply(indep_vars, function(x){
    # Remove rows with infinite values in the current independent variable
    df_no_inf <- df[!is.infinite(df[[x]]), ]
    # Fit linear model for the current independent variable
    lm(as.formula(paste(dep_var, "~", x)), data = df_no_inf)
  })
  
  names(modelList) <- indep_vars
  modelList
}


#### Bivariates
# Run regressions bivariate
modelList_gov <- reg_list(hatch_growth_filter, "Logistic.Fit", num_variables_gov)
modelList_num_tech <- reg_list(hatch_growth_filter, "Logistic.Fit", num_variables_tech)
modelList_cat_tech <- reg_list(hatch_growth_filter, "Logistic.Fit", cat_variables_tech)

modelsummary(modelList_num_tech, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_num_tech_95_high.html')

modelsummary(modelList_gov, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_gov_95_high.html')

modelsummary(modelList_cat_tech, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_cat_tech_95_high.html')
library(dplyr)
library(modelsummary)
library(tidyr)
library(broom)
library(fs)
library(dunn.test)  # For Dunn's test
# Function to perform Dunn's test and save results

# Function to perform Dunn's test and compute summary statistics
dunn_test_results <- function(df, response_var, cat_vars, output_dir, r2_filter, fit_filter, fit_type) {
  dir.create(output_dir, showWarnings = FALSE)
  
  result_list <- lapply(cat_vars, function(cat_var) {
    df_no_na <- dplyr::filter(df, !is.na(df[[cat_var]]))
    
    dunn_result <- dunn.test(df_no_na[[response_var]], df_no_na[[cat_var]], method = "none")
    
    dunn_df <- data.frame(
      Comparison = dunn_result$comparisons,
      P.value = dunn_result$P,
      Adjusted.P.value = dunn_result$P.adjusted,
      Chi.squared = dunn_result$Z,
      Categorical_Variable = cat_var,
      stringsAsFactors = FALSE
    )
    
    # Save file with fit type included in name
    save_path_dunn <- file.path(output_dir, paste0("dunn_test_results_R2_", r2_filter, "_", fit_type, "_", fit_filter, "_", cat_var, ".csv"))
    write.csv(dunn_df, save_path_dunn, row.names = FALSE)
    
    summary_df <- dplyr::group_by(df_no_na, dplyr::across(dplyr::all_of(cat_var))) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Median = median(!!rlang::sym(response_var), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::rename(Group = !!rlang::sym(cat_var)) %>%
      dplyr::mutate(Group = as.character(Group))
    
    save_path_summary <- file.path(output_dir, paste0("summary_statistics_R2_", r2_filter, "_", fit_type, "_", fit_filter, "_", cat_var, ".csv"))
    write.csv(summary_df, save_path_summary, row.names = FALSE)
    
    list(dunn_df = dunn_df, summary_df = summary_df)
  })
  
  dunn_results_df <- dplyr::bind_rows(lapply(result_list, `[[`, "dunn_df"))
  summary_stats_df <- dplyr::bind_rows(lapply(result_list, `[[`, "summary_df"))
  
  list(dunn_results = dunn_results_df, summary_stats = summary_stats_df)
}
run_and_save_regressions <- function(data, r2_levels, logistic_filters) {
  
  # Directory for saving results
  results_dir <- "reports"
  dir.create(results_dir, showWarnings = FALSE)  # Create the directory if it doesn't exist
  
  # Function to run regressions and Dunn's test for a given R^2 and Logistic.Fit filter
  run_regressions <- function(df, r2_filter, logistic_filter) {
    
    # Filter dataset by R^2 and Logistic.Fit
    filtered_data <- df %>%
      filter(Logistic.Fit >= 0 & 
               Logistic.Fit <= logistic_filter &
               logistic.goodness > r2_filter)
    
    # Define regression variables
    num_variables_gov <- c("direction_scitech_Average", 
                           "easier_scitech_Average",
                           "Capacity_Average",
                           "rdspending_Average",
                           "Log_GDPavg", 
                           "dem")
    num_variables_tech <- c("log_Granularity.Numerical",
                            "log_Material.Use.Numerical",
                            "log_Average.lifetime",
                            "FirstCommercialYr")
    cat_variables_tech <- c("Complexity", 
                            "Need.for.Customization",
                            "Type.of.Adopter",
                            "Feedstock",
                            "Broad.replacement")
    
    # Run regressions
    reg_list <- function(df, dep_var, indep_vars) {
      modelList <- lapply(indep_vars, function(x){
        df_no_inf <- df[!is.infinite(df[[x]]), ]
        lm(as.formula(paste(dep_var, "~", x)), data = df_no_inf)
      })
      names(modelList) <- indep_vars
      modelList
    }
    
    modelList_gov <- reg_list(filtered_data, "Logistic.Fit", num_variables_gov)
    modelList_num_tech <- reg_list(filtered_data, "Logistic.Fit", num_variables_tech)
    
    # Perform Dunn's test for categorical technology variables
    dunn_results <- dunn_test_results(filtered_data, "Logistic.Fit", cat_variables_tech, results_dir, r2_filter, logistic_filter, "Logistic")
    
    
    
    # Save regression results to HTML
    modelsummary(modelList_num_tech, stars = TRUE, statistic = 'p.value', 
                 output = file.path(results_dir, paste0('bivariate_num_tech_R2_', r2_filter, '_Logistic_', logistic_filter, '.html')))
    modelsummary(modelList_gov, stars = TRUE, statistic = 'p.value', 
                 output = file.path(results_dir, paste0('bivariate_gov_R2_', r2_filter, '_Logistic_', logistic_filter, '.html')))
    
    return(list(
      modelList_gov = modelList_gov,
      modelList_num_tech = modelList_num_tech,
      dunn_results = dunn_results
    ))
  }
  
  # Initialize list to store results
  results_list <- list()
  
  # Loop over R^2 levels and Logistic.Fit filters
  for (r2 in r2_levels) {
    for (logistic_filter in logistic_filters) {
      results_list[[paste0('R2_', r2, '_Logistic_', logistic_filter)]] <- run_regressions(data, r2, logistic_filter)
    }
  }
  
  # Function to create summary table
  create_summary_table <- function(results, filter_type) {
    result_df <- data.frame()
    
    for (filter_name in names(results)) {
      models <- results[[filter_name]]
      
      # Initialize empty data frames for results
      num_tech_results <- data.frame()
      gov_results <- data.frame()
      dunn_results <- data.frame()
      
      # Collect results for numerical technology variables
      if (length(models$modelList_num_tech) > 0) {
        num_tech_results <- bind_rows(
          lapply(models$modelList_num_tech, function(model) {
            tibble(
              Variable = names(coef(model)),
              Estimate = coef(model),
              StdError = sqrt(diag(vcov(model))),
              pValue = summary(model)$coefficients[, 4]
            )
          })
        )
      }
      
      # Collect results for government variables
      if (length(models$modelList_gov) > 0) {
        gov_results <- bind_rows(
          lapply(models$modelList_gov, function(model) {
            tibble(
              Variable = names(coef(model)),
              Estimate = coef(model),
              StdError = sqrt(diag(vcov(model))),
              pValue = summary(model)$coefficients[, 4]
            )
          })
        )
      }
      
      # Collect results for Dunn's test
      if (!is.null(models$dunn_results)) {
        dunn_results <- models$dunn_results
      }
      
      # Ensure all results have the same number of rows
      all_variables <- unique(c(num_tech_results$Variable, gov_results$Variable))
      
      num_tech_results <- num_tech_results %>% complete(Variable = all_variables)
      gov_results <- gov_results %>% complete(Variable = all_variables)
      
      # Combine results into final dataframe
      combined_results <- bind_rows(
        tibble(
          Filter = filter_name,
          ModelType = 'num_tech',
          Variable = num_tech_results$Variable,
          Estimate = num_tech_results$Estimate,
          StdError = num_tech_results$StdError,
          pValue = num_tech_results$pValue
        ),
        tibble(
          Filter = filter_name,
          ModelType = 'gov',
          Variable = gov_results$Variable,
          Estimate = gov_results$Estimate,
          StdError = gov_results$StdError,
          pValue = gov_results$pValue
        ),
        tibble(
          Filter = filter_name,
          ModelType = 'dunn_test',
          Variable = dunn_results$Categorical_Variable,
          Chi.squared = dunn_results$Chi.squared,
          P.value = dunn_results$P.value
        )
      )
      
      # Append to result dataframe
      result_df <- bind_rows(result_df, combined_results)
    }
    
    # Save to CSV
    write.csv(result_df, file.path(results_dir, paste0('combined_results_', filter_type, '_', '.csv')), row.names = FALSE)
  }
  
  # Create summary table
  create_summary_table(results_list, "combined")
}
run_and_save_regressions_gomp <- function(data, r2_levels, gompertz_filters) {
  
  # Directory for saving results
  results_dir <- "reports"
  dir.create(results_dir, showWarnings = FALSE)  # Create the directory if it doesn't exist
  
  # Function to run regressions and Dunn's test for a given R^2 and Logistic.Fit filter
  run_regressions <- function(df, r2_filter, gompertz_filter) {
    
    # Filter dataset by R^2 and Logistic.Fit
    filtered_data <- df %>%
      filter(Gompertz.Fit >= 0 & 
               Gompertz.Fit <= gompertz_filter &
               gompertz.goodness > r2_filter)
    
    # Define regression variables
    num_variables_gov <- c("direction_scitech_Average", 
                           "easier_scitech_Average",
                           "Capacity_Average",
                           "rdspending_Average",
                           "Log_GDPavg", 
                           "dem")
    num_variables_tech <- c("log_Granularity.Numerical",
                            "log_Material.Use.Numerical",
                            "log_Average.lifetime",
                            "FirstCommercialYr")
    cat_variables_tech <- c("Complexity", 
                            "Need.for.Customization",
                            "Type.of.Adopter",
                            "Feedstock",
                            "Broad.replacement")
    
    # Run regressions
    reg_list <- function(df, dep_var, indep_vars) {
      modelList <- lapply(indep_vars, function(x){
        df_no_inf <- df[!is.infinite(df[[x]]), ]
        lm(as.formula(paste(dep_var, "~", x)), data = df_no_inf)
      })
      names(modelList) <- indep_vars
      modelList
    }
    
    modelList_gov <- reg_list(filtered_data, "Gompertz.Fit", num_variables_gov)
    modelList_num_tech <- reg_list(filtered_data, "Gompertz.Fit", num_variables_tech)
    
    # Perform Dunn's test for categorical technology variables
    dunn_results <- dunn_test_results(filtered_data, "Gompertz.Fit", cat_variables_tech, results_dir, r2_filter, gompertz_filter, "Gompertz")

    
    
    # Save regression results to HTML
    modelsummary(modelList_num_tech, stars = TRUE, statistic = 'p.value', 
                 output = file.path(results_dir, paste0('bivariate_num_tech_R2_', r2_filter, '_Gompertz_', gompertz_filter, '.html')))
    modelsummary(modelList_gov, stars = TRUE, statistic = 'p.value', 
                 output = file.path(results_dir, paste0('bivariate_gov_R2_', r2_filter, '_Gompertz_', gompertz_filter, '.html')))
    
    return(list(
      modelList_gov = modelList_gov,
      modelList_num_tech = modelList_num_tech,
      dunn_results = dunn_results
    ))
  }
  
  # Initialize list to store results
  results_list <- list()
  
  # Loop over R^2 levels and Logistic.Fit filters
  for (r2 in r2_levels) {
    for (gompertz_filter in gompertz_filters) {
      results_list[[paste0('R2_', r2, '_Gompertz_', gompertz_filter)]] <- run_regressions(data, r2, gompertz_filter)
    }
  }
  
}

# Ensure data is clean and numeric
hatch_growth$Logistic.Fit <- as.numeric(hatch_growth$Logistic.Fit)
hatch_growth$logistic.goodness <- as.numeric(hatch_growth$logistic.goodness)
hatch_growth$FirstCommercialYr <- as.numeric(hatch_growth$FirstCommercialYr)
hatch_growth$Feedstock <- gsub("No ", "No", hatch_growth$Feedstock)
hatch_growth$Gompertz.Fit <- as.numeric(hatch_growth$Gompertz.Fit)
hatch_growth$gompertz.goodness <- as.numeric(hatch_growth$gompertz.goodness)


# Define parameters
r2_levels <- c(0.85, 0.9, 0.95, 0.99)
logistic_filters <- c(1, 2, 5)
gompertz_filters <-c(1, 2, 5)

# Run the function
run_and_save_regressions(hatch_growth, r2_levels, logistic_filters)
run_and_save_regressions_gomp(hatch_growth, r2_levels, gompertz_filters)


################################################################################
################## Bivariate Vintage and Growth Speed ##########################
################################################################################

## Load in data
#setwd("/Users/jennagreene/Documents/GitHub/country-tech-growth/country_tech_growth_analysis") # Set to overall folder
file_path = "00_data/technology_growth_Update.csv" # Set File Path
hatch_growth_summ <- read.csv(file_path) # Load in data file
hatch_growth_summ$Logistic.Fit <- as.numeric(hatch_growth_summ$Logistic.Fit)
hatch_growth_summ$logistic.goodness <- as.numeric(hatch_growth_summ$logistic.goodness)
hatch_growth_summ$Vintage <- as.numeric(hatch_growth_summ$Vintage)

# Filter dataset
R2_filter <- 0.95
# Filter to only include logistic fits
hatch_growth_summ <- hatch_growth_summ[which(hatch_growth_summ$Logistic.Fit >= 0 & 
                                             hatch_growth_summ$Logistic.Fit != 'err' &
                                             hatch_growth_summ$Logistic.Fit <= 1 &
                                             hatch_growth_summ$logistic.goodness > R2_filter), ]

p <- ggplot(hatch_growth_summ, aes(y = Logistic.Fit, x = Number.of.Points)) +
  geom_point() +
  ggtitle("Logistic Fit and Length of Time Series") +        # Add title
  xlab("Number of Data Points") +                      # Edit x-axis label
  stat_smooth(colour = "#45b39d") +
  theme_minimal()# Fit exponential regression line
  ggsave('logfit_numpoints_sensitivity.png', plot = p, width = 6, height = 6, units = 'in')


