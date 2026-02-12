# Generate tables for National Diffusion of Technologies Paper
# Jenna Greene 2025

# Load in libraries
library(dplyr)
library(modelsummary)
library(broom)
library(stats)


#setwd('../') # <- set proper working directory
#setwd('02_scripts')  # <- set proper working directory
#source('regressions_list.R')  # <- run function
#setwd('../')  # <- set proper working directory
#setwd('04_reports/main')  # <- set proper working directory


# Table 4 (Main) ------

## Set variables of interest
country_variables <- c("Capacity_Average",
                       'dem',
                       "rdspending_Average",
                       "Log_GDPavg",
                       "direction_scitech_Average", 
                       "easier_scitech_Average")

technology_variables <- c("FirstCommercialYr",
                          "log_Granularity.Numerical",
                          "log_Material.Use.Numerical",
                          "log_Average.lifetime",
                          "Complexity", 
                          "Need.for.Customization",
                          "Type.of.Adopter", 
                          "Feedstock", "Broad.replacement")

variables_of_interest <- c(technology_variables, country_variables)

# Create a list of variable categories (Technology vs Country)
variable_categories <- c(rep("Technology", length(technology_variables)),
                         rep("Country", length(country_variables)))
variable_labels <- c( "First Year of Commercialization",
                      "Granularity (log)",
                      "Material Use (log)",
                      "Lifetime (log)",
                      'Complexity',
                      #'Complexity (Design-Intensive)',
                      #'Complexity (Complex)',
                      'Need for Customization',
                      #"Need for Customization (Mass-Customized)",
                      #"Need for Customization (Customized)",
                      'Type of Adopter',
                      #"Type of Adopter (Individuals)",
                      #"Type of Adopter (Both)",
                      'Feedstock',
                      #"Feedstock (Yes)",
                      'Broad Replacement',
                      #"Broad Replacement (Yes)",
                      "Gov. Capacity", 
                      'Type of Gov.',
                      #"Type of Gov. (Not Democracy)",
                      "R&D Spending",
                      "GDP per capita (log)",
                      "Public Acceptance - Tech. Headed in Right Direction",
                      "Public Acceptance - Tech. Making Life Easier")

y_labels <- c( "First Year of Commercialization",
               "Granularity (log)",
               "Material Use (log)",
               "Lifetime (log)",
               #'Complexity',
               'Complexity (Design-Intensive)',
               'Complexity (Complex)',
               #'Need for Customization',
               "Need for Customization (Mass-Customized)",
               "Need for Customization (Customized)",
               #'Type of Adopter',
               "Type of Adopter (Individuals)",
               "Type of Adopter (Both)",
               #'Feedstock',
               "Feedstock (Yes)",
               #'Broad Replacement',
               "Broad Replacement (Yes)",
               "Gov. Capacity", 
               #'Type of Gov.',
               "Type of Gov. (Not Democracy)",
               "R&D Spending",
               "GDP per capita (log)",
               "Public Acceptance - Tech. Headed in Right Direction",
               "Public Acceptance - Tech. Making Life Easier")

# Create a data frame for variable info, including labels
variable_info <- data.frame(
  variable = variables_of_interest,
  category = variable_categories,
  label = variable_labels)


#### Bivariates
# Run regressions bivariate
bivariatemodelList <- reg_list(hatch_growth_filter, "Logistic.Fit", variables_of_interest)


# Save bivariates
modelsummary(bivariatemodelList, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_variables_95.html')

## Methods tables 1, 2, 4 ---------
# Summarize each variable -
## For tables in methods section table 1, 2, and 4

categorical_variables_tech <- c("Final.Material.Use", 
                                "Technology.Lifetime", 
                                "Granularity",
                                "Type.of.Adopter",
                                "Complexity",
                                "Need.for.Customization",
                                "Feedstock",
                                "Broad.replacement", 
                                "Category.Type") 
summarize_variable(hatch_growth_filter, categorical_variables_tech)
categorical_variables_country <- c("dem", "cap_cat")
summary(hatch_growth_filter$cap_cat)

aggregate(hatch_growth_filter$Logistic.Fit, list(hatch_growth_filter$dem), summary)
aggregate(hatch_growth_filter$Logistic.Fit, list(hatch_growth_filter$cap_cat), summary)

## Summarize year of first comm. 
## For description in text
df_commercialYr <- hatch_growth_filter %>%
  select(technology, FirstCommercialYr) %>%
  distinct()


# Summary Table for Methods Section
vars_of_interest <- c("Logistic.Fit", "Number.of.Points", "rdspending_Average", 
                      "direction_scitech_Average", "easier_scitech_Average", 
                      "Log_GDPavg", "Capacity_Average", "dem", 
                      "Need.for.Customization", "FirstCommercialYr", "Complexity", 
                      "Type.of.Adopter", "Broad.replacement", "Feedstock", 
                      "log_Granularity.Numerical", "log_Material.Use.Numerical", 
                      "log_Average.lifetime", 'Category.Type')

# Subset the data and run summary
hatch_summary_output <- summary(hatch_growth_filter[, vars_of_interest])
hatch_summary_df <- as.data.frame.matrix(hatch_summary_output)

#Export to CSV
write.csv(hatch_summary_df, "hatch_summary_stats.csv", row.names = FALSE)


