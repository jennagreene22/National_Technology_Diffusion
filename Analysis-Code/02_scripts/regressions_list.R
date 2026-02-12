# Function creates basic linear regression of bivariates 
# For list of variable

# Jenna Greene Apr 25, Dec 2025 R2
 
### Create regressions of lists of variables
reg_list <- function(df, dep_var, indep_vars) {
  modelList <- lapply(indep_vars, function(x){
    # Remove rows with infinite values in the current independent variable
    df_no_inf <- df[!is.infinite(df[[x]]), ] 
    # Fit linear model for the current independent variable 
    lm(as.formula(paste(dep_var, "~", x)), data = df_no_inf)
  })
  names(modelList) <- indep_vars
  modelList}

 
reg_list_poly <- function(df, dep_var, indep_vars) {
  modelList <- lapply(indep_vars, function(x) {
    # Remove rows with infinite or NA values in the current independent variable
    df_clean <- df[!is.infinite(df[[x]]) & !is.na(df[[x]]), ]
    
    # Fit a polynomial regression with raw = False 
    lm(as.formula(paste0(dep_var, " ~ poly(", x, ", 2, raw = FALSE)")), data = df_clean)
  })
  names(modelList) <- indep_vars
  return(modelList) 
}

reg_list_cubic <- function(df, dep_var, indep_vars) {
  modelList <- lapply(indep_vars, function(x) {
    #Remove rows with infinite or NA values in the current independent variable
    df_clean <- df[!is.infinite(df[[x]]) & !is.na(df[[x]]), ]
           
    # Fit a polynomial regression with raw = False 
    lm(as.formula(paste0(dep_var, " ~ poly(", x, ", 3, raw = FALSE)")), data = df_clean)
   })
  names(modelList) <- indep_vars
  return(modelList) 
}