# Function for significance tests of HATCH data

# Jenna Greene Apr 25
library(dunn.test)

perform_statistical_tests <- function(data, variables, response_var) {
  tukey_results <- lapply(variables, function(var) {
    # Perform ANOVA
    anova_result <- aov(as.formula(paste(response_var, "~", var)), data)
    anova_summary <- summary(anova_result)
    
    # Extract ANOVA information
    anova_df <- as.data.frame(anova_summary[[1]]) 
    
    # Save ANOVA results
    save_name_anova <-paste0("Update_anova_", var, ".csv")
    write.csv(anova_df, save_name_anova, row.names = TRUE)
    
    # Kruskal-Wallis Test
    kruskal_result <- kruskal.test(as.formula(paste(response_var, "~", var)), data)
    kruskal_tidy <- tidy(kruskal_result)
    #print(kruskal_result)
    
    # Save Kruskal's test results
    save_name_kruskal <- paste0("Update_kruskal_", var, ".csv")
    write.csv(kruskal_tidy, save_name_kruskal, row.names = FALSE)
    
    # Perform Dunn's test (Non-parametric post hoc test)
    num_groups <- length(unique(na.omit(data[[var]])))
    if (num_groups > 2){
    dunn_result <- dunn.test(data[[response_var]], data[[var]], method = "holm")
    
    # Create a data frame for Dunn's test results
    dunn_df <- data.frame(
      Comparison = dunn_result$comparisons,
      Z.Statistic = dunn_result$Z,
      P.value = dunn_result$P,
      Adjusted.P.value = dunn_result$P.adjusted
    ) 
    
    # Save Dunn's test results
    save_name_dunn <- paste0("Update_dunn_", var, ".csv")
    write.csv(dunn_df, save_name_dunn, row.names = FALSE)
    }
    else {dunn_df <- NULL}
    # Perform Tukey's test (NORMAL)
    tukey_test <- TukeyHSD(anova_result)
    tukey_tidy <- tidy(tukey_test)
     
    # Save Tukey's test results
    save_name_tukey <- paste0("Update_tukey_", var, ".csv")
    write.csv(tukey_tidy, save_name_tukey, row.names = FALSE)
    
    # Return results
    return(list(Variable = var, ANOVA = anova_df, Tukey = tukey_tidy, Dunn = dunn_df))
  })
  return(tukey_results)
} 