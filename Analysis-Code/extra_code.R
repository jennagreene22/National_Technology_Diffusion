### Technology Characteristics ###
# Apr 2024

# This is for regression analysis of the HATCH data


## Load libraries
install.packages('dplyr')
install.packages('ggplot2')
install.packages('hrbrthemes')
install.packages("Matrix", version = "1.6.0")
install.packages('ggpmisc')
install.packages("tidyverse")
install.packages("modelsummary")
install.packages("gtsummary")
install.packages('corrplot')
install.packages("olsrr")
install.packages("jtools")
install.packages("ggstats")
install.packages("ggExtra")
install.packages("devtools")
install.packages("dplyr")
install.packages("dunn.test")
install.packages("Cairo")
install.packages("cairoDevice")
install.packages('regclass')
install.packages("stargazer")



library(dplyr)  #data manipulation and clearning    
library(ggplot2) #data visualiation
library(hrbrthemes)  #need for text import
library(ggpmisc)    #extension of ggplot
library(rlang)      #extension of r functionality
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # tidy model outputs
#library(plyr)       #split-apply-combine
library(stargazer)    # (Currently not using) creates latex code for regression tables
library(modelsummary)   #provides nice summary of regression results
#library(gtsummary)    #also provides nice summary tables from regressions
library(corrplot)     #creates nice correlation plots
library(olsrr)      #runs each combination of variables in regression
library(jtools)     #correlation plots
library(ggstats)
library(patchwork)
library(ggExtra)
library(dunn.test)
#library(Cairo)
#library(cairoDevice)
library(regclass)
hrbrthemes::import_roboto_condensed()


########################### Multiple Regression Analysis #######################
output_file <- file.path("04_reports", "regression_table_dem.html")

report_multiple_reg <- stargazer( M5,  M7, M6,
                                  type = "text",  
                                  dep.var.labels=c("Speed of Diffusion"),
                                  order = c("FirstCommercialYr", "log_granularity", "log_material", "log_lifetime", 
                                            'dem', 'Capacity_Average'),
                                  title = "Technology and Country Characteristics: Growth Speed", out = output_file)

########## Association of Variables with One Another -- -------

############### 
### Create high.low state capacity variable
hatch_growth_filter_cap <- hatch_growth_filter %>%
  mutate(cap_cat = case_when(Capacity_Average > 0 ~ 'High', 
                             Capacity_Average <= 0 ~ 'Low')) %>%
  filter(!is.na(cap_cat))

# create boxplot
boxplot_colors_statecap <- c("High" = "cadetblue3", "Low" = "darkseagreen")
cap_cat_levels <- c('Low', 'High')
hatch_growth_filter_cap$cap_cat <- factor(hatch_growth_filter_cap$cap_cat, levels = cap_cat_levels)


state_cap_boxplot <- ggplot(hatch_growth_filter_cap, aes(x = cap_cat, y = Logistic.Fit, fill = cap_cat)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot_colors_statecap, 
                    guide = "none") +  
  labs(title = "Capacity and Growth Speed",
       x = "State Capacity Type",
       y = "Speed of Diffusion") +
  theme_minimal() +  # Apply theme_minimal
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14)) 
state_cap_boxplot
# test for differences
cap_aov <- aov(as.formula(Logistic.Fit ~ cap_cat), hatch_growth_filter_cap)
summary(cap_aov)

# Save figure
ggsave(file.path("03_plots", "Update_state_cap_boxplot.png"),
       plot = state_cap_boxplot,
       width = 6, height = 6, units = "in", dpi = 300)

# Regression with numerical state cap
state_cap_model <- lm(Logistic.Fit ~ dem + Capacity_Average + dem*Capacity_Average, data = hatch_growth_filter_cap)
summary(state_cap_model)
############### 
### income group mode figure
### Create income group variable
hatch_growth_filter_income <- hatch_growth_filter %>%
  filter(IncomeGroup_Mode != "") %>%
  filter(!is.na(IncomeGroup_Mode)) %>%
  filter(IncomeGroup_Mode != "<NA>") %>%
  filter(IncomeGroup_Mode != "..")

# create boxplot
boxplot_colors_econ_levels <- c("L" = "cadetblue3", "LM" = "darkseagreen", 
                                "UM" = "deeppink4", "H" = "darkslateblue")
econ_levels <- c('L', 'LM', 'UM', 'H')
hatch_growth_filter_income$IncomeGroup_Mode <- factor(hatch_growth_filter_income$IncomeGroup_Mode, 
                                                      levels = econ_levels)


incomegroup_boxplot <- ggplot(hatch_growth_filter_income, aes(x = IncomeGroup_Mode, 
                                                              y = Logistic.Fit, 
                                                              fill = IncomeGroup_Mode)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot_colors_econ_levels, 
                    guide = "none") +  
  labs(title = "Income Group and Growth Speed",
       x = "",
       y = "Speed of Diffusion") +
  theme_minimal() +  # Apply theme_minimal
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14)) 
incomegroup_boxplot

### Create high.low econ  variable
hatch_growth_filter_econlevel <- hatch_growth_filter %>%
  mutate(econlevel = case_when(IncomeGroup_Mode == "L" ~ 'Low', 
                               IncomeGroup_Mode == "LM" ~ 'Low',
                               IncomeGroup_Mode == "UM" ~ "High", 
                               IncomeGroup_Mode == "H" ~ "High")) %>%
  filter(econlevel != "") %>%
  filter(!is.na(econlevel)) %>%
  filter(econlevel != "<NA>") %>%
  filter(econlevel != "..")

boxplot_colors_econ_highlow <-  c("High" = "cadetblue3", "Low" = "darkseagreen")


econ_levels <- c('Low', 'High')
hatch_growth_filter_econlevel$econlevel <- factor(hatch_growth_filter_econlevel$econlevel, levels = econ_levels)


econhighlow <- ggplot(hatch_growth_filter_econlevel, aes(x = econlevel, 
                                                         y = Logistic.Fit, 
                                                         fill = econlevel)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot_colors_econ_highlow, 
                    guide = "none") +  
  labs(title = "Income Group and Growth Speed",
       x = "",
       y = "Speed of Diffusion") +
  theme_minimal() +  # Apply theme_minimal
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14)) 
econhighlow


#difference in levels democracies
dunn_result_econ <- dunn.test(hatch_growth_filter_econlevel$"Logistic.Fit", 
                              hatch_growth_filter_econlevel$"econlevel", method = "none")

# Create a data frame for Dunn's test results
dunn_df <- data.frame(
  Comparison = dunn_result_econ$comparisons,
  P.value = dunn_result_econ$P,
  Adjusted.P.value = dunn_result_econ$P.adjusted
)

# Save Dunn's test results
save_path_dunn <- file.path(output_dir, 'econ_difference_sig.csv')
write.csv(dunn_df, save_path_dunn, row.names = FALSE)

econ_aov <- aov(as.formula(Logistic.Fit ~ econlevel), hatch_growth_filter_econlevel)
summary(econ_aov)

#Medians
medians <- boxplot_data_compare %>%
  group_by(Dem_Cap) %>%
  summarise(median = median(Logistic.Fit))


