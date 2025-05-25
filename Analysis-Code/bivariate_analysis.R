## This code will test the significance of relationships for categorical data
## and linear regressions for 

# From 2024 code, updated Apr 2025
# Jenna Greene

# Libraries
library(ggplot2)
library(modelsummary)
library(scales)
library(dotwhisker)



### Load in data
#setwd('../../')
#setwd('02_scripts')
source('clean_hatch_data.R')


## ------------- Bivariate linear regressions ------------- 

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
                       "Feedstock",
                       "Broad.replacement")

bracket_information <- list(c("Technology Variables", technology_variables), 
                            c("Country Variables", country_variables))

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
setwd('../')
setwd('02_scripts')
source('regressions_list.R')

#### Bivariates
# Run regressions bivariate
modelList <- reg_list(hatch_growth_filter, "Logistic.Fit", variables_of_interest)

setwd('../')
setwd('04_reports')

# Save bivariates
modelsummary(modelList, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_variables_95.html')

#### Bivariates of polynomial
modelListPoly <- reg_list_poly(hatch_growth_filter, "Logistic.Fit", variables_of_interest)

# Save bivariates
modelsummary(modelListPoly, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_variables_poly.html')

## ------  Figure of coefficients (individual) ------

# Generate the dot-and-whisker plot
dw_plot <- dwplot(modelList) +
  theme_minimal() +
  # Add a dotted vertical line at 0.00
  geom_vline(xintercept = 0, linetype = "dotted", color = "darkgray", size = 1) +

  scale_y_discrete(labels = rev(y_labels)) + theme(legend.position = "none") +
  labs(x = "Regression Coefficient Estimate") +
  ggtitle(" Relationship with Growth Speed") +theme(plot.title = element_text(hjust = 0.5))
#setwd('../')
#setwd('03_plots/bivariate')
ggsave('coeff_plot_bivariate.png', dw_plot, width = 6, height = 6, dpi = 300)

# Source function on significance tests
setwd('../')
setwd('02_scripts')
source('significance_tests.R')
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
#setwd('../')
output_dir <- file.path("04_reports", "significance_tests")

# Perform Tukey's test and ANOVA, and save results as tidy tibbles
stats_results <- perform_statistical_tests(hatch_growth_filter, categorical_variables_tech, response_var, output_dir)

stats_results <- perform_statistical_tests(hatch_growth_filter, categorical_variables_national, response_var, output_dir)
