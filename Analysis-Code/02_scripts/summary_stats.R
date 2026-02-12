# Function summarizes each data with logistic fit 
# Outputs a CSV file Dec 2025

### Summarize each variable ------------------------------------------
summarize_variable <- function(df, variables) {
  for (variable in variables) {
    # Aggregate the data
    aggregated <- aggregate(df$Logistic.Fit, list(df[[variable]]), summary)
    
    # Construct the filename
    filename <- paste0(variable, ".csv")
     
    # Export to CSV
    write.csv(aggregated, filename, row.names = FALSE)
  }
}
  