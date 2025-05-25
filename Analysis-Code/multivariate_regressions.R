# This script runs multivariate regressions on HATCH national data
#install.packages('interactions')
library(stargazer)
library(tidyverse)
library(broom) # formatting for tidy objects
library(ggplot2)
library(interactions)

setwd('02_scripts')

# Run clean hatch data
source('clean_hatch_data.R')


## ---------------------- Multiple Regression Analysis ----------------------

# time and granularity
M1 <- lm(Logistic.Fit~ log_Granularity.Numerical + 
           FirstCommercialYr, 
         data = hatch_growth_filter)

# time and lifetime
M2 <- lm(Logistic.Fit ~ log_Average.lifetime + 
           FirstCommercialYr, 
         data = hatch_growth_filter)

# time and material use

M3 <- lm(Logistic.Fit ~ Material.Use.Numerical +
           FirstCommercialYr, 
         data = hatch_growth_filter)

# time and government type
M4 <- lm(Logistic.Fit ~ dem +
           FirstCommercialYr,
         data = hatch_growth_filter)

# granularity and government type
M5 <- lm(Logistic.Fit ~ dem + 
           log_Granularity.Numerical,
         data = hatch_growth_filter)

# lifetime and government type
M6 <- lm(Logistic.Fit ~ dem + 
           log_Average.lifetime,
         data = hatch_growth_filter)

# material use and gov type
M7 <- lm(Logistic.Fit ~ dem + 
           log_Material.Use.Numerical,
         data = hatch_growth_filter)

# capacity and gov type
M8 <- lm(Logistic.Fit ~ dem + 
           Capacity_Average,
         data = hatch_growth_filter)

# capacity, gov type, gdp
M9 <- lm(Logistic.Fit ~ dem + 
           Capacity_Average + 
           log_Granularity.Numerical,
         data = hatch_growth_filter)

# complex and need for custom
M10 <- lm(Logistic.Fit ~ Complexity + 
            Need.for.Customization, 
          data = hatch_growth_filter)


M11 <- lm(Logistic.Fit ~ FirstCommercialYr + 
            Capacity_Average, 
          data = hatch_growth_filter)
M12 <- lm(Logistic.Fit ~ dem + 
            Log_GDPavg, 
          data = hatch_growth_filter)

M13 <- lm(Logistic.Fit ~ Capacity_Average + 
            log_Granularity.Numerical, 
          data = hatch_growth_filter)

M14 <- lm(Logistic.Fit ~ Capacity_Average + 
            log_Material.Use.Numerical, 
          data = hatch_growth_filter)

#Type of adopter and granularity
M15 <- lm(Logistic.Fit ~ Type.of.Adopter +
            log_Granularity.Numerical, 
          data = hatch_growth_filter)

M16 <- lm(Logistic.Fit ~ Complexity +
            Capacity_Average, 
          data = hatch_growth_filter)

M17 <- lm(Logistic.Fit ~ Type.of.Adopter + 
            dem + Capacity_Average, 
          data = hatch_growth_filter)

M18 <- lm(Logistic.Fit ~ FirstCommercialYr + 
            dem + Capacity_Average, data = hatch_growth_filter)

M19 <- lm(Logistic.Fit ~ 
            log_Granularity.Numerical + FirstCommercialYr + 
            dem + Capacity_Average, 
          data = hatch_growth_filter)
setwd('../')

output_file <- file.path("04_reports", "regression_table_combined.html")

report_multiple_reg <- stargazer(M1, M3, M2, M8, M12, M4,M11, M13, M9, M15, M16,
                                 type = "text",  
                                 dep.var.labels=c("Speed of Diffusion"),
                                 order = c("FirstCommercialYr", "log_granularity", "log_material", "log_lifetime", 
                                           'Type.of.Adopter', 'Complexity', 'dem', 'Capacity_Average','log_GDPavg'),
                                 title = "Technology and Country Characteristics: Growth Speed", out = output_file)



## ------------------------------- Visuals ------------------------------------
## Coefficient Plots
models_hatch <- list(M18, M9, M16, M17, M19)
model_names <- c("Time and Institutional Variables", 
                 "Granularity and Institutional Variables",
                 "Complexity and State Capacity", 
                 "Type of Adopter and Institutional Variables", 
                 "Time, Granularity, and Institutional Variables")

# models of mixed effects
tidy_models <- lapply(models_hatch, tidy, conf.int = TRUE)
tidy_df <- bind_rows(tidy_models, .id = "model")
tidy_df$model <- factor(tidy_df$model, labels = model_names)

tidy_df_noint <- tidy_df %>%filter(term != '(Intercept)')

# Classify variables into categories
tidy_df_noint <- tidy_df_noint %>%
  mutate(var_type = case_when(
    grepl("dem|Capacity_Average", term) ~ "Country",
    grepl("log_Granularity.Numerical|FirstCommercialYr|Type.of.Adopter|Complexity", term) ~ "Technology",
    TRUE ~ "Other"  # Default case
  ))

# Adjust labels for variables
tidy_df_noint <- tidy_df_noint %>%
  mutate(label = case_when(
    term == "FirstCommercialYr" ~ "Year of First Comm.",
    term == "demNot Democracy" ~ "Not Democracy",
    term == "Capacity_Average" ~ "State Capacity",
    term == "Complexity.L" ~ "Complexity (Design-Intensive)",
    term == "Complexity.Q" ~ "Complexity (Complex)",
    term == "log_Granularity.Numerical" ~ "Granularity (log)",
    term == "Type.of.AdopterIndividuals [2]" ~ "Type of Adopter (Individuals)",
    term == "Type.of.AdopterBoth [3]" ~ "Type of Adopter (Both)",
    TRUE ~ term  # Keep original name for others
  ))

facet_labels <- c('Time and Institutional Variables' = 'a',
                  "Granularity and Institutional Variables" = 'b',
                  "Complexity and State Capacity" = 'c', 
                  "Type of Adopter and Institutional Variables" = 'd',
                  "Time, Granularity, and Institutional Variables" = 'e')
# Create a new data frame for the labels
label_data <- data.frame(
  model = names(facet_labels), 
  label = facet_labels,
  x = rep(0, length(facet_labels)),  # x-position for labels (can adjust as needed)
  y = rep(Inf, length(facet_labels))  # y-position for labels (can adjust as needed)
)
# Create the plot
coeff_plot <- ggplot(tidy_df_noint, aes(x = estimate, y = label, colour = var_type)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  facet_wrap(~ model, scales = 'free', nrow = 3, ncol = 2) +
  labs(title = "Coefficient Estimates from Linear Regression Models",
       x = "Estimate of effect on growth speed",
       y = NULL, 
       color = "Variable Type") +
  scale_color_manual(values = c("Technology" = "#c4959b", "Country" = "#94bbb6", "Other" = "grey")) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme_minimal(base_size = 14)
print(coeff_plot)

setwd('03_plots/multivariate')
ggsave("multivariate_coefficient_estimates_plot.png", plot = coeff_plot, width = 12, height = 10, dpi = 300)

## Multivariate Robustness ------

### Robustness Regressions 
model1 <- lm(Logistic.Fit ~ Vintage + FirstCommercialYr, data = hatch_growth_filter)
model2 <- lm(Logistic.Fit ~ Number.of.Points + FirstCommercialYr, data = hatch_growth_filter)
model3 <- lm(Logistic.Fit ~ Number.of.Points + log_Granularity.Numerical, data = hatch_growth_filter)
model4 <- lm(Logistic.Fit ~ Number.of.Points + log_Material.Use.Numerical, data = hatch_growth_filter)
model5 <- lm(Logistic.Fit ~ Number.of.Points + log_Average.lifetime, data = hatch_growth_filter)
model6 <- lm(Logistic.Fit ~ Number.of.Points + Capacity_Average, data = hatch_growth_filter)
model7 <- lm(Logistic.Fit ~ Number.of.Points + Log_GDPavg, data = hatch_growth_filter)
model8 <- lm(Logistic.Fit ~ Number.of.Points + dem, data = hatch_growth_filter)

output_file <- file.path("regression_table_sensitivity.txt")

setwd('../../')
setwd('04_reports') 
report_multiple_reg_sens <- stargazer(model1, model2, model3, model4, model5, model7,model6,
                                      type = "text",  
                                      dep.var.labels=c("Speed of Diffusion"),
                                      order = c("Vintage", "Number.of.Points",
                                                "FirstCommercialYr", 
                                                "log_Granularity.Numerical", 
                                                "log_Material.Use.Numerical", 
                                                "log_Average.Lifetime", 
                                                'Log_GDPavg', 'Capacity_Average'),
                                      title = "Technology and Country Characteristics: Growth Speed", out = output_file)
# Interaction of granularity and 
complex_custom <- lm(Logistic.Fit ~ Complexity * Need.for.Customization, data = hatch_growth_filter)
cat_plot(complex_custom, pred = Complexity, modx = Need.for.Customization, interval = TRUE)
aov_complex_custom <- aov(Logistic.Fit ~ Complexity * Need.for.Customization, data = hatch_growth_filter)
summary(aov_complex_custom)


## Tech Characteristics Interactions

#  granularity and type of adopter
RobustTechM1 <- lm(Logistic.Fit~ log_Granularity.Numerical + 
                     Type.of.Adopter, 
                   data = hatch_growth_filter)
  
  # time and lifetime
RobustTimeM1 <- lm(Logistic.Fit ~ log_Granularity.Numerical + 
                       FirstCommercialYr, 
                     data = hatch_growth_filter)
RobustTimeM2 <- lm(Logistic.Fit ~ log_Material.Use.Numerical + 
                     FirstCommercialYr, 
                   data = hatch_growth_filter)
RobustTimeM3 <- lm(Logistic.Fit ~ log_Average.lifetime + 
                     FirstCommercialYr, 
                   data = hatch_growth_filter)

#setwd('../')

output_file <- file.path("04_reports", "tech_character_interactions.html")

report_multiple_reg <- stargazer(RobustTechM1, RobustTimeM1, 
                                 RobustTimeM2, RobustTimeM3, 
                                 type = "text",  
                                 dep.var.labels=c("Speed of Diffusion"),
                                 order = c("FirstCommercialYr", "log_granularity", "log_material", "log_lifetime", 
                                           'Type.of.Adopter'),
                                 title = "Technology Characteristics Interactions" , out = output_file)

hatch_filter_gran_indiv <- hatch_growth_filter %>% filter(Type.of.Adopter == 'Individuals [2]')
interact_gran_indiv <- lm(Logistic.Fit ~ log_Granularity.Numerical,data = hatch_filter_gran_indiv )
interact_mat_indiv <- lm(Logistic.Fit ~ log_Material.Use.Numerical,data = hatch_filter_gran_indiv )
interact_life_indiv <- lm(Logistic.Fit ~ log_Average.lifetime, data = hatch_filter_gran_indiv)
interact_firstcomm_indiv <- lm(Logistic.Fit ~ FirstCommercialYr, data = hatch_filter_gran_indiv)
summary(interact_gran_indiv)

hatch_filter_gran_firms <- hatch_growth_filter %>% filter(Type.of.Adopter == 'Firms [1]')
interact_gran_firms <- lm(Logistic.Fit ~ log_Granularity.Numerical,data = hatch_filter_gran_firms )
interact_mat_firms <- lm(Logistic.Fit ~ log_Material.Use.Numerical,data = hatch_filter_gran_firms )
interact_life_firms <- lm(Logistic.Fit ~ log_Average.lifetime, data = hatch_filter_gran_firms)
interact_firstcomm_firms <- lm(Logistic.Fit ~ FirstCommercialYr, data = hatch_filter_gran_firms)

summary(interact_gran_firms)

hatch_filter_gran_both <- hatch_growth_filter %>% filter(Type.of.Adopter == 'Both [3]')
interact_gran_both <- lm(Logistic.Fit ~ log_Granularity.Numerical,data = hatch_filter_gran_both )
interact_mat_both <- lm(Logistic.Fit ~ log_Material.Use.Numerical,data = hatch_filter_gran_both )
interact_life_both <- lm(Logistic.Fit ~ log_Average.lifetime, data = hatch_filter_gran_both)
summary(interact_gran_both)
