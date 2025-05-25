# Function for significance tests of HATCH data

# Jenna Greene Apr 25
library(dunn.test)

perform_statistical_tests <- function(data, variables, response_var, output_dir) {
  tukey_results <- lapply(variables, function(var) {
    # Perform ANOVA
    anova_result <- aov(as.formula(paste(response_var, "~", var)), data)
    anova_summary <- summary(anova_result)
    
    # Extract ANOVA information
    anova_df <- as.data.frame(anova_summary[[1]]) # Extract the first element of the summary, which contains the ANOVA table
    
    # Save ANOVA results
    save_path_anova <- file.path(output_dir, paste0("Update_anova_", var, ".csv"))
    write.csv(anova_df, save_path_anova, row.names = TRUE)
    
    # Kruskal-Wallis Test
    kruskal_result <- kruskal.test(as.formula(paste(response_var, "~", var)), data)
    kruskal_tidy <- tidy(kruskal_result)
    #print(kruskal_result)
    
    # Save Kruskal's test results
    save_path_kruskal <- file.path(output_dir, paste0("Update_kruskal_", var, ".csv"))
    write.csv(kruskal_tidy, save_path_kruskal, row.names = FALSE)
    
    # Perform Dunn's test (NON-PARAMETRIC POST HOC)
    dunn_result <- dunn.test(data[[response_var]], data[[var]], method = "none")
    
    # Create a data frame for Dunn's test results
    dunn_df <- data.frame(
      Comparison = dunn_result$comparisons,
      P.value = dunn_result$P,
      Adjusted.P.value = dunn_result$P.adjusted
    )
    
    # Save Dunn's test results
    save_path_dunn <- file.path(output_dir, paste0("Update_dunn_", var, ".csv"))
    write.csv(dunn_df, save_path_dunn, row.names = FALSE)
    
    # Perform Tukey's test (NORMAL)
    tukey_test <- TukeyHSD(anova_result)
    tukey_tidy <- tidy(tukey_test)
    
    # Save Tukey's test results
    save_path_tukey <- file.path(output_dir, paste0("Update_tukey_", var, ".csv"))
    write.csv(tukey_tidy, save_path_tukey, row.names = FALSE)
    
    # Return results
    return(list(Variable = var, ANOVA = anova_df, Tukey = tukey_tidy, Dunn = dunn_df))
  })
  return(tukey_results)
} 