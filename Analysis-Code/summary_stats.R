# Function summarizes each data with logistic fit 
# Outputs a CSV file

### Summarize each variable ------------------------------------------
summarize_variable <- function(df, variables, output_dir) {
  for (variable in variables) {
    # Aggregate the data
    aggregated <- aggregate(df$Logistic.Fit, list(df[[variable]]), summary)
    
    # Construct the filename
    filename <- paste0(output_dir, variable, ".csv")
     
    # Export to CSV
    write.csv(aggregated, filename, row.names = FALSE)
  }
}