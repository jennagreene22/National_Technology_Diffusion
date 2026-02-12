# Generate tables for supplementary text of national technology diffusion paper
# Greene, Brutschin, Gidden, Nemet



#setwd('../../')# <- set proper wd
#setwd('04_reports/supplemental') # <- set proper wd

## Supplementary Tables 2 and 3 --------

## ------------ Describe technologies and Countries in the data


### Summary of data after filtering (Supplementary TABLE 2 and 3)

# Supplementary table 2
ctry_per_tech <- hatch_growth_filter %>% 
  group_by(technology) %>% 
  summarize(count_by_tech = n())

write.csv(ctry_per_tech, file = "tech_summary_filter.csv", row.names = FALSE)

# Supplementary table 3
tech_per_country <- hatch_growth_filter %>% 
  group_by(country) %>% 
  summarize(count_by_country = n())
write.csv(tech_per_country, file = "ctry_summary_filter.csv", row.names = FALSE)

## Supplementary Table 12 ------

## Summary of tech characteristics after filtering (Supplementary Table 11)
tech_char_hatch_filtered <- hatch_growth_filter %>%
  group_by(technology) %>%
  summarize(
    Material.Use.Numerical = first(Material.Use.Numerical),
    Final.Material.Use = first(Final.Material.Use),
    FirstCommercialYr = first(FirstCommercialYr),
    Need.for.Customization = first(Need.for.Customization),
    Complexity = first(Complexity),
    Type.of.Adopter = first(Type.of.Adopter),
    Granularity.Numerical = first(Granularity.Numerical),
    Granularity = first(Granularity),
    Average.lifetime = first(Average.lifetime),
    Technology.Lifetime = first(Technology.Lifetime),
    Strict.replacement = first(Strict.replacement),
    Broad.replacement = first(Broad.replacement),
    Feedstock = first(Feedstock),
    Category.Type = first(Category.Type)
  ) %>%
  ungroup()
write.csv(tech_char_hatch_filtered, file = "tech_characteristics_filtered.csv", row.names = FALSE)

## Supplementary Table 15 ----
hatch_filtering_initial <- nrow(hatch_growth)
step_1 <- sum(is.na(hatch_growth$Logistic.Fit))
hatch_filtering_aftererr <- hatch_growth[!is.na(hatch_growth$Logistic.Fit), ]
step_2 <- sum(hatch_filtering_aftererr$logistic.goodness < 0.95)
hatch_filtering_95 <- hatch_filtering_aftererr %>% dplyr::filter(logistic.goodness >= 0.95)
nrow(hatch_filtering_95)
step_3 <- sum(hatch_filtering_95$Logistic.Fit > 1)
hatch_filtering_1 <- hatch_filtering_95 %>% dplyr::filter(Logistic.Fit <= 1)
nrow(hatch_filtering_1)
step_4 <- sum(hatch_filtering_1$Logistic.Fit < 0)
hatch_filtering_0 <- hatch_filtering_1 %>% dplyr::filter(Logistic.Fit > 0)
nrow(hatch_filtering_0)
nrow(hatch_growth_filter) 

## Supplementary Table 16 ----
hatch_filtering_initial <- nrow(hatch_growth)
hatch_growth$Gompertz.Fit <- as.numeric(hatch_growth$Gompertz.Fit)

step_1_g <- sum(is.na(hatch_growth$Gompertz.Fit))
hatch_filtering_aftererr_g <- hatch_growth[!is.na(hatch_growth$ Gompertz.Fit), ]
nrow(hatch_filtering_aftererr_g)
step_2_g <- sum(hatch_filtering_aftererr_g$gompertz.goodness < 0.95)
hatch_filtering_95_g <- hatch_filtering_aftererr_g %>% dplyr::filter(gompertz.goodness >= 0.95)
nrow(hatch_filtering_95_g) 

step_3_g <- sum(hatch_filtering_95_g$Gompertz.Fit > 1)
hatch_filtering_1_g <- hatch_filtering_95_g %>% dplyr::filter(Gompertz.Fit <= 1)
nrow(hatch_filtering_1_g)

step_4 <- sum(hatch_filtering_1_g$Gompertz.Fit < 0)
hatch_filtering_0_g <- hatch_filtering_1_g %>% dplyr::filter(Gompertz.Fit > 0)
nrow(hatch_filtering_0_g)

## Supplementary Table 17 - 29 ----
categorical_variables_tech <- c("Final.Material.Use", 
                                "Technology.Lifetime", 
                                "Granularity",
                                "Type.of.Adopter",
                                "Complexity",
                                "Need.for.Customization",
                                "Feedstock",
                                "Broad.replacement", 
                                "Category.Type")  
categorical_variables_national <- c("econlevel", 
                                    "cap_cat") 
response_var <- "Logistic.Fit"

# Perform Tukey's test and ANOVA, and save results as tidy tibbles
stats_results <- perform_statistical_tests(hatch_growth_filter, categorical_variables_tech, response_var)

stats_results <- perform_statistical_tests(hatch_growth_filter, categorical_variables_national, response_var)

## Supplementary Table 30

# time and granularity
M1 <- lm(Logistic.Fit~ log_Granularity.Numerical + 
           FirstCommercialYr, 
         data = hatch_growth_filter)

# time and lifetime
M2 <- lm(Logistic.Fit ~ log_Average.lifetime + 
           FirstCommercialYr, 
         data = hatch_growth_filter)

# time and material use

M3 <- lm(Logistic.Fit ~ log_Material.Use.Numerical +
           FirstCommercialYr, 
         data = hatch_growth_filter)

# time and government type
M4 <- lm(Logistic.Fit ~ dem +
           FirstCommercialYr,
         data = hatch_growth_filter)

# granularity and government type
M5 <- lm(Logistic.Fit ~ dem + 
           log_Granularity.Numerical,
         data = hatch_growth_filter)

# lifetime and government type
M6 <- lm(Logistic.Fit ~ dem + 
           log_Average.lifetime,
         data = hatch_growth_filter)

# material use and gov type
M7 <- lm(Logistic.Fit ~ dem + 
           log_Material.Use.Numerical,
         data = hatch_growth_filter)

# capacity and gov type
M8 <- lm(Logistic.Fit ~ dem + 
           Capacity_Average,
         data = hatch_growth_filter)

# capacity, gov type, gdp
M9 <- lm(Logistic.Fit ~ dem + 
           Capacity_Average + 
           log_Granularity.Numerical,
         data = hatch_growth_filter)

# complex and need for custom
M10 <- lm(Logistic.Fit ~ Complexity + 
            Need.for.Customization, 
          data = hatch_growth_filter)


M11 <- lm(Logistic.Fit ~ FirstCommercialYr + 
            Capacity_Average, 
          data = hatch_growth_filter)
M12 <- lm(Logistic.Fit ~ dem + 
            Log_GDPavg, 
          data = hatch_growth_filter)

M13 <- lm(Logistic.Fit ~ Capacity_Average + 
            log_Granularity.Numerical, 
          data = hatch_growth_filter)

M14 <- lm(Logistic.Fit ~ Capacity_Average + 
            log_Material.Use.Numerical, 
          data = hatch_growth_filter)

#Type of adopter and granularity
M15 <- lm(Logistic.Fit ~ Type.of.Adopter +
            log_Granularity.Numerical, 
          data = hatch_growth_filter)

M16 <- lm(Logistic.Fit ~ Complexity +
            Capacity_Average, 
          data = hatch_growth_filter)

M17 <- lm(Logistic.Fit ~ Type.of.Adopter + 
            dem + Capacity_Average, 
          data = hatch_growth_filter)

M18 <- lm(Logistic.Fit ~ FirstCommercialYr + 
            dem + Capacity_Average, data = hatch_growth_filter)

M19 <- lm(Logistic.Fit ~ 
            Complexity + FirstCommercialYr, 
          data = hatch_growth_filter)

#Type of adopter and material use
M20 <-  lm(Logistic.Fit ~ Type.of.Adopter +
             log_Material.Use.Numerical, 
           data = hatch_growth_filter)
#Type of adopter and lfietime
M21 <-  lm(Logistic.Fit ~ Type.of.Adopter +
             log_Average.lifetime, 
           data = hatch_growth_filter)

output_file <- "regression_table_combined_short.html"


report_multiple_reg <- stargazer(M1, M3, M2, M15, M20, M21, M19, 
                                 type = "text",  
                                 dep.var.labels=c("Speed of Diffusion"),
                                 order = c("FirstCommercialYr", "log.Granularity.Numerical", "log_Material.Use.Numerical", "log_Average.lifetime", 
                                           'Type.of.AdopterIndividuals[2]', 'Type.of.AdopterBoth[3], Complexity.L, Complexity.Q'),
                                 title = "Multiple Technology Characteristics and Growth Speed", out = output_file)

## Supplementary Table 31 - 39 ----
## 
#### FILTER R2 Values
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

# Function to perform Dunn's test and save results

# Function to perform Dunn's test and compute summary statistics
dunn_test_results <- function(df, response_var, cat_vars, r2_filter, fit_filter, fit_type) {

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
    save_name_dunn <- paste0("dunn_test_results_R2_", r2_filter, "_", fit_type, "_", fit_filter, "_", cat_var, ".csv")
    write.csv(dunn_df, save_name_dunn, row.names = FALSE)
    
    summary_df <- dplyr::group_by(df_no_na, dplyr::across(dplyr::all_of(cat_var))) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Median = median(!!rlang::sym(response_var), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::rename(Group = !!rlang::sym(cat_var)) %>%
      dplyr::mutate(Group = as.character(Group))
    
    save_name_summary <- paste0("summary_statistics_R2_", r2_filter, "_", fit_type, "_", fit_filter, "_", cat_var, ".csv")
    write.csv(summary_df, save_name_summary, row.names = FALSE)
    
    list(dunn_df = dunn_df, summary_df = summary_df)
  })
  
  dunn_results_df <- dplyr::bind_rows(lapply(result_list, `[[`, "dunn_df"))
  summary_stats_df <- dplyr::bind_rows(lapply(result_list, `[[`, "summary_df"))
  
  list(dunn_results = dunn_results_df, summary_stats = summary_stats_df)
}
run_and_save_regressions <- function(data, r2_levels, logistic_filters) {
  
  # Directory for saving results
  
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
    dunn_results <- dunn_test_results(filtered_data, "Logistic.Fit", cat_variables_tech, r2_filter, logistic_filter, "Logistic")
    
    
    
    # Save regression results to HTML
    modelsummary(modelList_num_tech, stars = TRUE, statistic = 'p.value', 
                 output = paste0('bivariate_num_tech_R2_', r2_filter, '_Logistic_', logistic_filter, '.html'))
    modelsummary(modelList_gov, stars = TRUE, statistic = 'p.value', 
                 output = paste0('bivariate_gov_R2_', r2_filter, '_Logistic_', logistic_filter, '.html'))
    
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
    write.csv(result_df,paste0('combined_results_', filter_type, '_', '.csv'), row.names = FALSE)
  }
  
  # Create summary table
  create_summary_table(results_list, "combined")
}
run_and_save_regressions_gomp <- function(data, r2_levels, gompertz_filters) {
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
    dunn_results <- dunn_test_results(filtered_data, "Gompertz.Fit", cat_variables_tech, r2_filter, gompertz_filter, "Gompertz")
    
    
    
    # Save regression results to HTML
    modelsummary(modelList_num_tech, stars = TRUE, statistic = 'p.value', 
                 output = paste0('bivariate_num_tech_R2_', r2_filter, '_Gompertz_', gompertz_filter, '.html'))
    modelsummary(modelList_gov, stars = TRUE, statistic = 'p.value', 
                 output = paste0('bivariate_gov_R2_', r2_filter, '_Gompertz_', gompertz_filter, '.html'))
    
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

## Supplementary Table 40 ---
# setwd('04_reports') <- set wd

### Robustness Regressions 
model1 <- lm(Logistic.Fit ~ Vintage + FirstCommercialYr, data = hatch_growth_filter)
model2 <- lm(Logistic.Fit ~ Number.of.Points + FirstCommercialYr, data = hatch_growth_filter)
model3 <- lm(Logistic.Fit ~ Number.of.Points + log_Granularity.Numerical, data = hatch_growth_filter)
model4 <- lm(Logistic.Fit ~ Number.of.Points + log_Material.Use.Numerical, data = hatch_growth_filter)
model5 <- lm(Logistic.Fit ~ Number.of.Points + log_Average.lifetime, data = hatch_growth_filter)
model6 <- lm(Logistic.Fit ~ Number.of.Points + Capacity_Average, data = hatch_growth_filter)
model7 <- lm(Logistic.Fit ~ Number.of.Points + Log_GDPavg, data = hatch_growth_filter)
model8 <- lm(Logistic.Fit ~ Number.of.Points + dem, data = hatch_growth_filter)

output_file <-"regression_table_sensitivity.html"
report_multiple_reg_sens <- stargazer(model1, model2, model3, model4, model5, model7,model6,
                                      type = "text",  
                                      dep.var.labels=c("Speed of Diffusion"),
                                      order = c("Vintage", "Number.of.Points",
                                                "FirstCommercialYr", 
                                                "log_Granularity.Numerical", 
                                                "log_Material.Use.Numerical", 
                                                "log_Average.Lifetime", 
                                                'Log_GDPavg', 'Capacity_Average'),
                                      title = "Technology and Country Characteristics: Growth Speed", out = output_file)


