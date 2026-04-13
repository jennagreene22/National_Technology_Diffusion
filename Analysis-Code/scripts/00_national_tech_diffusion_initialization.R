# Initialize and run scripts for analysis associated with 
# National Diffusion of Technologies in Countries

# Shell to run
### ------ Load in packages ---####
library(broom)
library(car)
library(corrplot) 
library(dplyr)
library(dotwhisker)
library(dunn.test)
library(ggcorrplot)
library(ggExtra)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(interactions)
library(jtools) 
library(lmtest)
library(modelsummary)
library(purrr)
library(sandwich)
library(scales)
library(stargazer)
library(stats)
library(stringr)
library(tidyverse)



#### USER ENTRY #####
# SET ROOT DIRECTORY HERE
# root_folder <- '~/Documents/GitHub/National_Technology_Diffusion/Analysis-Code' # set to directory that contains data folder and scripts folder
setwd(root_folder)

# Create directory structure
data_wd <- file.path(root_folder, "data") 
script_wd <- file.path(root_folder, "scripts")
r_project_folders <- c("plots", "reports")
lapply(r_project_folders, dir.create)
plot_wd <- file.path(root_folder, "plots")
table_wd <- file.path(root_folder, "reports")

# Run functions to clean data
setwd(script_wd)
source("01_clean_hatch_data.R")

# Summarize hatch data for text
setwd(script_wd)
source("02_describe_hatch_data.R")

# Run scripts with background functions
setwd(script_wd)
source("03_regressions_list.R")
source("04_significance_tests.R")
source("05_summary_stats.R")

# Run scripts to create figures
plot_functions_filepath <- file.path(script_wd, 'plot_functions')
setwd(plot_functions_filepath)
source('categorical_plots.R')
source('numerical_plots.R')

# Run script to generate figures for main text
setwd(script_wd)
source("06_generate_figures.R")

# Run script to generate tables for main text
setwd(script_wd)
source("07_generate_tables.R")

# Create figures and tables for SI
setwd(script_wd)
source("08_generate_figures_supplemental.R")
setwd(script_wd)
source("09_generate_tables_supplemental.R")
setwd(script_wd)
source("10_supplemental_analysis.R")