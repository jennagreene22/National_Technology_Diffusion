## This file checks the robustness offindings 
# of correlations for the HATCH dataset

# Jenna Greene
# Updated April 7 2025

setwd('../')
setwd('02_scripts')
source('clean_hatch_data.R')
source('bivariate_analysis.R')
## 
setwd('05_robustness')

#### Different metrics
# Create new column of short metrics
hatch_growth_filter <- hatch_growth_filter %>%
  mutate(Short_Metric = case_when(
    grepl("Share", Metric) ~ "Share",
    grepl("Annual Production", Metric) ~ "Annual Production",
    grepl("Annual production", Metric) ~ "Annual Production",
    grepl("Capacity", Metric) ~ "Capacity/Power",
    grepl("Power", Metric) ~ "Capacity/Power",
    TRUE ~ "Total Number of Units"))

# Share
hatch_growth_filter_share <- hatch_growth_filter %>%
  filter(Short_Metric == "Share")

hatch_growth_filter_production <- hatch_growth_filter %>%
  filter(Short_Metric == "Annual Production")

hatch_growth_filter_cap <- hatch_growth_filter %>%
  filter(Short_Metric == "Capacity/Power")

hatch_growth_filter_numunits <- hatch_growth_filter %>%
  filter(Short_Metric == "Total Number of Units")

hatch_growth_filter_allbutshare <- hatch_growth_filter %>%
  filter(Short_Metric != "Share")  

## Set variables of interest -------
country_variables <- c("Capacity_Average",
                       'dem',
                       "rdspending_Average",
                       "Log_GDPavg",
                       "direction_scitech_Average", 
                       "easier_scitech_Average")

technology_variables <- c("FirstCommercialYr",
                          "log_Granularity.Numerical",
                          "log_Material.Use.Numerical",
                          "log_Average.lifetime",#)#,
                          "Complexity", 
                          #"Need.for.Customization",
                          "Type.of.Adopter",
                          #"Feedstock",
                          "Broad.replacement")
variables_of_interest <- c(technology_variables, country_variables)


# Correlations (simple) ------
modelList_share <- reg_list(hatch_growth_filter_share, "Logistic.Fit", variables_of_interest)
modelList_production <- reg_list(hatch_growth_filter_production, "Logistic.Fit", variables_of_interest)
modelList_cap <- reg_list(hatch_growth_filter_cap, "Logistic.Fit", variables_of_interest)
modelList_numunits <- reg_list(hatch_growth_filter_numunits, "Logistic.Fit", variables_of_interest)
#hatch_growth_filter_allbutshare
modelList_allNOSHARE <- reg_list(hatch_growth_filter_allbutshare, "Logistic.Fit", variables_of_interest)


# Save bivariates -------
modelsummary(modelList_share, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_share.html')
modelsummary(modelList_production, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_produc.html')
modelsummary(modelList_cap, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_cap.html')
modelsummary(modelList_numunits, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_numunits.html')

compare_metrics_logistic_speed <- describe_categorical_tech(data = hatch_growth_filter, 
                          x_var = "Short_Metric", 
                          y_var = "Logistic.Fit", 
                          category_var = "Short_Metric", 
                          category_order = c('Share', 'Annual Production', 'Capacity/Power', 'Total Number of Units'), 
                          label_vars =  c('Share', 'Annual Prod.', 'Cap./Power', 'Total # of Units'), 
                          plot_title = "Metrics", 
                          save_filename = "short_metrics.png")


# Tidy and label each model list
tidy_share <- map_df(modelList_share, tidy) %>% mutate(model = "Market Share")
tidy_production <- map_df(modelList_production, tidy) %>% mutate(model = "Production")
tidy_cap <- map_df(modelList_cap, tidy) %>% mutate(model = "Capacity")
tidy_numunits <- map_df(modelList_numunits, tidy) %>% mutate(model = "Number of Units")

# Combine all tidy models
all_models <- bind_rows(tidy_share, tidy_production, tidy_cap, tidy_numunits)

# Make the dot-and-whisker plot with facets
dw_plot_short_metrics <- dwplot(all_models) +
  facet_wrap(~ model, scales = "free_y") +  # Facet by model label
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "darkgray", size = 1) +
  labs(
    x = "Regression Coefficient Estimate",
    y = element_blank(),
    #title = "Bivariate Regressions by Growth Metric"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.text = element_text(size = 13, face = "bold"), 
    legend = element_blank()
  )
ggsave("Bivariate_short_metric.png", dw_plot_short_metrics, width = 8, height = 8, units = "in", dpi = 300)



# Divide into two sections on first year of commercialization ---
# Supplementary analysis C5
# Pre 1960
hatch_growth_pre75 <- hatch_growth_filter %>%
  filter(FirstCommercialYr < 1975)
hatch_growth_post75 <- hatch_growth_filter %>%
  filter(FirstCommercialYr > 1974)

modelList_late <- reg_list(hatch_growth_post75, "Logistic.Fit", variables_of_interest)
modelList_early <- reg_list(hatch_growth_pre75, "Logistic.Fit", variables_of_interest)

# Tidy and label each model list
tidy_late <- map_df(modelList_late, tidy) %>% mutate(model = "Since 1975")
tidy_early <- map_df(modelList_early, tidy) %>% mutate(model = "Before 1975")

# Combine all tidy models
all_models_time <- bind_rows(tidy_late, tidy_early)

# Make the dot-and-whisker plot with facets
dw_plot_time<- dwplot(all_models_time) +
  facet_wrap(~ model, scales = "free_y") +  # Facet by model label
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "darkgray", size = 1) +
  labs(
    x = "Regression Coefficient Estimate",
    y = element_blank(),
    #title = "Bivariate Regressions by Time"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.text = element_text(size = 18, face = "bold"), 
    legend = element_blank()
  )
ggsave("Bivariate_time_robustness.png", dw_plot_time, width = 8, height = 6, units = "in", dpi = 300)

# Multivariate of metrics
model_holdgran <- lm(Logistic.Fit ~ Capacity_Average +
                       log_Granularity.Numerical, 
                     data = hatch_growth_post75)
model_holdgran <- lm(Logistic.Fit ~ Capacity_Average +
                       Log_GDPavg, 
                     data = hatch_growth_post75)
 
model_holdtypeofadopter <- lm(Logistic.Fit ~ Complexity + Capacity_Average, data = hatch_growth_post75)   

latest <- hatch_growth_filter %>% filter(FirstCommercialYr == 2011)
earliest <- hatch_growth_filter %>% filter(FirstCommercialYr == 1743)
#### GROWTH METRICS -------
hatch_growth_filter$InverseDeltaT <- 1 / hatch_growth_filter$Delta.T

# Correlations - Delta T -----
modelList_deltaT <- reg_list(hatch_growth_filter, "InverseDeltaT", variables_of_interest)
modelsummary(modelList_deltaT, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_deltaT.html')

# Correlations - Saturation
modelList_saturation <- reg_list(hatch_growth_filter, "Logistic.Saturation", variables_of_interest)
modelsummary(modelList_saturation, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_saturation.html')


# Summary Table
summarize_hatch <- summary(hatch_growth_filter)
data_stuct_summary <- data.frame(summarize_hatch)

stargazer(data_stuct_summary[c("Logistic.Fit", "Number.of.Points", "rdspending_Average", 
                       "direction_scitech_Average", 
                       "easier_scitech_Average", "Log_GDPavg", "Capacity_Average", 
                       "dem", "Need.for.Customization", "FirstCommercialYr", "Complexity", 
                       "Type.of.Adopter", "Broad.replacement","Feedstock", 
                       "log_Granularity.Numerical", "log_Material.Use.Numerical", 
                       "log_Average.lifetime", "econlevel")],type="text",title="Summary Statistics", out="table1.txt")
print(summarize_hatch, file="my_table_summaryHatch.tex")
write.csv(summarize_hatch, 'hatch_summary.csv')


# Checking for examples of fst growth and low saturation
# Find top 10% of growth rate
threshold <- quantile(hatch_growth_filter$Logistic.Fit, 0.90, na.rm = TRUE)

# Filter rows where the column value is in the top 10%
top_10_percent <- hatch_growth_filter[hatch_growth_filter$Logistic.Fit >= threshold, ]
unique_tech_fast <- unique(top_10_percent$technology)
top_10_percent_marketshare <- top_10_percent %>%
  filter(Short_Metric == "Share")

marketshare <- hatch_growth_filter %>%
  filter(Short_Metric == 'Share')
marketshare <- marketshare[marketshare$Logistic.Saturation < 1, ]
plot(top_10_percent_marketshare$Logistic.Saturation, top_10_percent_marketshare$Logistic.Fit,
     xlab = "Saturation",
     ylab = "Growth Speed",
     main = "Saturation vs. Growth Speed",
     pch = 19, col = "blue")


significant_models <- hatch_growth_filter %>%
  group_by(technology) %>%
  filter(n_distinct(country) > 1) %>%
  group_modify(~ {
    model <- try(lm(Logistic.Saturation ~ Logistic.Fit, data = .x), silent = TRUE)
    
    if (inherits(model, "try-error")) {
      return(tibble())
    }
    
    tidy_model <- tidy(model)
    p_value <- tidy_model$p.value[2]  # slope p-value
    
    if (!is.na(p_value) && p_value < 0.05) {
      return(.x)
    } else {
      return(tibble())
    }
  }) %>%
  ungroup()
ggplot(significant_models, aes(x = Logistic.Fit, y = Logistic.Saturation)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ technology, scales = "free_y") +
  labs(title = "Logistic Fit vs. Logistic Saturation (Significant Only)",
       x = "Logistic Fit",
       y = "Logistic Saturation") +
  theme_minimal()
