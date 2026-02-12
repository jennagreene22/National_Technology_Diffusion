## This code will summarize the national level HATCH data 
# Jenna Greene, 2025

#install.packages('ggplot2')

library(ggplot2)
library(ggpubr)
library(dplyr)
 
## ------ Load in Data -----
setwd('../')
setwd('02_scripts')

# Run clean hatch data
source('clean_hatch_data.R')

## ---------------------- Description of HATCH --------------------
                              ## In text ##
# summary of technologies
num_tech <-length(unique(hatch_growth$technology))
print(paste0("Number of Technologies in HATCH (pre-filtering): ", num_tech))

# summary of countries
num_countries <- length(unique(hatch_growth$country))
print(paste0("Number of Countries in HATCH (pre-filtering): ", num_countries))

# Technologies with data for only one country
one_country <- nrow(countries_per_technology_df %>% filter(Countries ==1))
print(paste0("There are ", one_country, " technologies with only one country in HATCH"))

# Median number of countries per tech
median_country_count <- median(countries_per_technology_df$Countries)
print(paste0("Median number of countries for each tech ", median_country_count))

# Median number of tech per country
median_tech_per_country <- median(technologies_per_country_df$Technologies)
print(paste0("Median number of technologies per country is ", median_tech_per_country))

# Country with the most technologies
most_tech_per_country <- max(technologies_per_country_df$Technologies)
print(paste0("The country with the most technologies has ", most_tech_per_country, " technologies."))

