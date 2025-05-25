## This code will clean the national level HATCH data for further analysis

# From 2024 code, updated Apr 2025
# Jenna Greene
#install.packages('dplyr')
library(dplyr)  #data manipulation and cleaning    


## ---- Load in HATCH data ------
setwd('../')
setwd('00_data') # set wd to where the data is saved
data_file_path = 'all_growth_merge_Updated.csv'
hatch_growth <- read.csv(data_file_path) # Load in data file

## ---------------------------- Clean Dataset ---------------------------------- 

## Set relevant columns to convert to numerical
hatch_growth$Logistic.Fit <- as.numeric(hatch_growth$Logistic.Fit)
hatch_growth$logistic.goodness <- as.numeric(hatch_growth$logistic.goodness)
hatch_growth$FirstCommercialYr <- as.numeric(hatch_growth$FirstCommercialYr)
hatch_growth$Delta.T <- as.numeric(hatch_growth$Delta.T)
hatch_growth$Logistic.Saturation <- as.numeric(hatch_growth$Logistic.Saturation)


## Log relevant variables
# In hatch data, some variables are categorical 
exclude_values <- c('Cheap', 'Low', 'Small', 'Medium', 'Large', 'Expensive') 

hatch_growth$log_Granularity.Numerical <- ifelse(is.na(as.numeric(hatch_growth$Granularity.Numerical)), 
                                                        NA, 
                                                        log(as.numeric(hatch_growth$Granularity.Numerical)))

hatch_growth$log_Material.Use.Numerical <- ifelse(is.na(as.numeric(hatch_growth$Material.Use.Numerical)),
                                                         NA,
                                                         log(as.numeric(hatch_growth$Material.Use.Numerical)))

hatch_growth$log_Material.Use.Numerical <- ifelse(is.infinite(as.numeric(hatch_growth$log_Material.Use.Numerical)),
                                                  NA,
                                                  hatch_growth$log_Material.Use.Numerical)

hatch_growth$log_Average.lifetime <- ifelse(is.na(as.numeric(hatch_growth$Average.lifetime)),
                                                   NA,
                                                   log(as.numeric(hatch_growth$Average.lifetime)))

## Rescale categorical variables

# Material Use
hatch_growth$Final.Material.Use <- factor(hatch_growth$Final.Material.Use,
                                          order = TRUE,
                                          levels = c("Low Material Use", 
                                                     "Medium Material Use", 
                                                     "High Material Use"))

# Tech Lifetime
hatch_growth$Technology.Lifetime <- factor(hatch_growth$Technology.Lifetime, 
                                           order = TRUE,
                                           levels = c("Months",
                                                      "Years", 
                                                      "Decades", 
                                                      "Centuries"))

# Granularity
hatch_growth$Granularity <- factor(hatch_growth$Granularity, 
                                   order = TRUE,
                                   levels = c("Low", 
                                              "Medium", 
                                              "High"))

# Type of Adopter
hatch_growth$Type.of.Adopter <- factor(hatch_growth$Type.of.Adopter, 
                                       order = FALSE,
                                       levels = c("Firms [1]",
                                                  "Individuals [2]",
                                                  "Both [3]"))

# Complexity
hatch_growth$Complexity <- factor(hatch_growth$Complexity, 
                                  order = TRUE,
                                  levels = c("Simple [1]", 
                                             "Design-Intensive [2]", 
                                             "Complex [3]"))

# Need for Customization
hatch_growth$Need.for.Customization <- factor(hatch_growth$Need.for.Customization, 
                                              order = TRUE,
                                              levels = c("Standardized [1]",
                                                         "Mass-customized [2]",
                                                         "Customized [3]"))
# Democracy
hatch_growth$dem <- factor(hatch_growth$dem, 
                           order = FALSE, 
                           levels = c('Democracy', 'Not Democracy'))

# Feedstock
hatch_growth$Feedstock <- factor(hatch_growth$Feedstock, 
                                 order = FALSE, 
                                 levels = c('No', 'Yes'))

# Replacement
hatch_growth$Broad.replacement <- factor(hatch_growth$Broad.replacement, 
                                         order = FALSE,
                                         levels = c('No', 'Yes'))

# Patent Category
hatch_growth$Patent.Category.Name <- as.factor(hatch_growth$Patent.Category.Name)
# Broad Categories
hatch_growth$Category.Type <- as.factor(hatch_growth$Category.Type)
# Strict Replacement
hatch_growth$Strict.replacement <- as.factor(hatch_growth$Strict.replacement)

## Ordinal
levels_regime <- c(0, 1, 2, 3, 4, 5)
hatch_growth$regime_Mode <- ordered(hatch_growth$regime_Mode, 
                                    levels = levels_regime)

## Create high / low state capacity data

hatch_growth <- hatch_growth %>%
  mutate(cap_cat = case_when(Capacity_Average > 0 ~ 'High', 
                             Capacity_Average <= 0 ~ 'Low'))
cap_cat_levels <- c('Low', 'High')
hatch_growth$cap_cat <- factor(hatch_growth$cap_cat, levels = cap_cat_levels)


## Create levels of econ data
hatch_growth <- hatch_growth %>%
  mutate(econlevel = case_when(IncomeGroup_Mode == "L" ~ 'Low', 
                               IncomeGroup_Mode == "LM" ~ 'Low',
                               IncomeGroup_Mode == "UM" ~ "High", 
                               IncomeGroup_Mode == "H" ~ "High"))

hatch_growth$econlevel <- factor(hatch_growth$econlevel, 
                                         order = TRUE,
                                         levels = c('Low', 'High'))

###  ------------------------Filter HATCH Dataset -------------------------

# Filter dataset
R2_filter <- 0.95
# Filter to only include logistic fits
hatch_growth_filter <- hatch_growth[which(hatch_growth$Logistic.Fit >= 0 & 
                                            hatch_growth$Logistic.Fit != 'err' &
                                            hatch_growth$Logistic.Fit <= 1 &
                                            hatch_growth$logistic.goodness > R2_filter), ]

### ------------------------ Save R Datasets -----------------------------------
save(hatch_growth, hatch_growth_filter, file = 'hatch_growth_data.RData')


