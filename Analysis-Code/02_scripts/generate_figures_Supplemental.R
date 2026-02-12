# Generate figures for supplementary text of national technology diffusion paper
# Greene, Brutschin, Gidden, Nemet


library(tidyverse)
library(dplyr)
library(modelsummary)
library(broom)
library(purrr)
library(stargazer)
library(gridExtra)
library(dotwhisker)
library(dunn.test)
#setwd('02_scripts/plot_functions')
#source('numerical_plots.R') # <- run plotting function
#source('categorical_plots.R') # <- run plotting function
#setwd('03_plots/supplemental') #<- set to proper working directory


#### Supplementary Figure 4 ------
## ------  Figure of coefficients (bivariates)

# Generate dot-and-whisker plot
dw_plot <- dwplot(bivariatemodelList) +
  theme_minimal() +
  # Add a dotted vertical line at 0
  geom_vline(xintercept = 0, linetype = "dotted", color = "darkgray", size = 1) +
scale_y_discrete(labels = rev(y_labels)) + theme(legend.position = "none") +
  labs(x = "Regression Coefficient Estimate") +
  ggtitle(" Relationship with Growth Speed") +theme(plot.title = element_text(hjust = 0.5))

ggsave('coeff_plot_bivariate.png', dw_plot, width = 6, height = 6, dpi = 300)

#### Prep for numerical plots-
## Legend plot -
## Color mapping
specific_technologies <- c("All Biofuels", "Aquaculture Production", 
                           "BCG Vaccine", 
                           "Beer Production", 
                           "Biogas", 
                           "Capture Fisheries", 
                           "Cellphones", 
                           "Crop Harvester", 
                           "DTP1 Vaccine", 
                           "DTP3 Vaccine", 
                           "Electricity", 
                           "Household Internet Access",
                           #"Hydrochloric Acid", 
                           #"Jet Aircraft",
                           "MCV1 Vaccine", 
                           "Milk",
                           "Motorcycles",
                           "Natural Gas Pipelines",
                           "Natural Gas Production", 
                           "Objects Launched into Space",
                           "Oil Refining Capacity", 
                           "Onshore Wind Energy", 
                           "POL3 Vaccine", 
                           "Passenger Cars",
                           "Postal Traffic", 
                           "Radio", 
                           "Railroad", 
                           "Renewable Power", 
                           "Solar Photovoltaic", 
                           "Solid Biofuels", 
                           #"Steamships", 
                           "Sulphuric Acid",
                           "Synthetic Fibers",
                           "Telegraph Traffic", 
                           "Telephones", 
                           "Television") 

specific_colors <- c("#1BA3C6FF", "#2CB5C0FF", "#30BCADFF", "#21B087FF", "#33A65CFF", 
                     "#57A337FF", "#A2B627FF", "#D5BB21FF", "#F8B620FF", "#F89217FF", 
                     "#F06719FF", '#CC0000', "#E03426FF", "#F64971FF", "#FF9898FF", 
                     "#FC719EFF", "#EB73B3FF", "#CE69BEFF", "#A26DC2FF", "#938DD2FF", 
                     "#7873C0FF", '#663399', "#2D6D66FF", "#4F7CBAFF", "#083ED0FF", 
                     "#AE904EFF", "#9E3E1CFF", "#DB7C3CFF", "#EACB85FF", "#63BDD3FF",
                     "#96ABC6FF", '#996600',  '#663300') #

gray_tech <- c("Ammonia Synthesis","Cable TV", "Cadmium Refining", 
               "Canals", "Cane Sugar","Coal Production", "Copper|Mining", "Copper|Refining", "Crude Oil","Dishwashers", 
               "Disk Brakes", "Electric Bicycles", "Ethanol", "Flush Toilet", "Flywheel Battery Storage",
               "Freezer"  , "Geothermal Energy", "Gold", "Ground Source Heat Pumps", "HEPB3 Vaccine", "HEPBB Vaccine",
               "HIB3 Vaccine", "Herbicide-Tolerant Corn", "Herbicide-Tolerant Cotton", "Herbicide-Tolerant Soybeans",
               "High Speed Rail", "Home Air Conditioning", "Home Computers",
               "Hydrochloric Acid", "Hydroelectricity", "Insect-Resistant Corn", "Insect-Resistant Cotton", 
               "Iron Ore", "Jet Aircraft", "Laundry Dryers", "Lead", 
               "Lead-Acid Battery Storage", "Liquefied Natural Gas", "MCV2 Vaccine",
               "Marine Energy", "Microcomputers" , "Microwaves","Nickel", "Nitric Acid",
               "Nitrogen Fertilizer", "Nox Pollution Controls (Boilers)", "Nuclear Energy",
               "Nuclear Weapons", "Offshore Wind Energy", "Oil Pipelines","Oil Production",                      
               "PCV3 Vaccine", "Phosphate Fertilizer", "Podcasting", "Potash Fertilizer", "Power Steering", 
               "Primary Aluminum Production", "Primary Bauxite Production", "Public Roads", "Pumped Hydro Storage",                
               "RCV1 Vaccine", "ROTAC Vaccine", "Raw Steel Production", "Real-Time Gross Settlement Adoption", 
               "Refrigerators", "Salt Production", "Sensible Heat Storage", "Shale Production", 
               "Silver", "Social Media Usage", "Sodium-Based Battery Storage", "Steamships", 
               "Stove", "Washing Machines", "Wet Flue Gas Desulfurization Systems", "YFV Vaccine", "Zinc")  
# Create a data frame for legend plotting
legend_data <- data.frame(technology = specific_technologies, color = specific_colors)

# Create the legend plot
legend_plot <- ggplot(legend_data, aes(x = 1, y = technology, color = technology)) +
  geom_point(size = 5) +
  scale_color_manual(values = specific_colors) +
  theme_void() +  # Remove background and axis
  guides(color = guide_legend(nrow = 5))

# Save the legend plot as an image file
ggsave("legend_plot_Update.png", legend_plot, width = 12.5, height = 4, dpi = 300)

# Gray dots

filtered_short_tech<- hatch_growth_filter %>%
  count(technology) %>%
  filter(n < 10) %>%
  pull(technology)


filtered_large_tech<- hatch_growth_filter %>%
  count(technology) %>%
  filter(n > 9) %>%
  pull(technology)

#### Supplementary Figure 15 ------
# Category plot
describe_categorical_tech(data = hatch_growth_filter, 
                          x_var = "Category.Type",
                          y_var = "Logistic.Fit", 
                          category_var = "Category.Type", 
                          category_order = c("Appliances", 
                                             "Chemicals and Industrial",
                                             "Digitalization",
                                             "Energy Supply",
                                             "Food and Health",
                                             "Infrastructure",
                                             "Materials",
                                             "Sea and Water",
                                             "Space and Defense",
                                             "Storage Technology",
                                             "Transportation"),
                          label_vars = c("Appliances", 
                                         "Chemicals \n and \n Industrial",
                                         "Digitalization",
                                         "Energy \n Supply",
                                         "Food \n and \n Health",
                                         "Infrastructure",
                                         "Materials",
                                         "Sea \n and \n Water",
                                         "Space \n and \n Defense",
                                         "Storage",
                                         "Transportation"),
                          plot_title = "Category",
                          save_filename = 'Update_tech_category.png')
#### Supplementary Figure 16 ------
# Scatterplots of National Level Data
comm_scatter_cubic <- create_scatterplot_cubic(hatch_growth_filter, "FirstCommercialYr", "", "Year of First Commercialization")
gran_scatter_cubic <- create_scatterplot_cubic(hatch_growth_filter, "log_Granularity.Numerical", "", "Granularity")
mat_scatter_cubic <- create_scatterplot_cubic(hatch_growth_filter, "log_Material.Use.Numerical", "", "Material Use Intensity")
life_scatter_cubic <- create_scatterplot_cubic(hatch_growth_filter, "log_Average.lifetime", "", "Technology Lifetime")
comm_scatter_cubic_med <- create_scatterplot_cubic(merge_hatch_medians, "FirstCommercialYr", "", "Year of First Commercialization")
gran_scatter_cubic_med <-create_scatterplot_cubic(merge_hatch_medians, "log_Granularity.Numerical", "", "Granularity")
mat_scatter_cubic_med <-create_scatterplot_cubic(merge_hatch_medians, "log_Material.Use.Numerical", "", "Material Use Intensity")
life_scatter_cubic_med <-create_scatterplot_cubic(merge_hatch_medians, "log_Average.lifetime", "", "Technology Lifetime")

# Combine cubic plots
grid_arrange_plots_poly <- ggarrange(comm_scatter_cubic_med, 
                                     comm_scatter_cubic, 
                                     gran_scatter_cubic_med, 
                                     gran_scatter_cubic, 
                                     mat_scatter_cubic_med, 
                                     mat_scatter_cubic, 
                                     life_scatter_cubic_med, 
                                     life_scatter_cubic, 
                                     labels = c("a", "b", 'c', 'd', 'e', 'f', 'g', 'h'), 
                                     font.label= list(size = 24, face = 'bold'),
                                     ncol =2, nrow = 4)
ggsave(file.path("Update_Combined_Scatterplots_Cubic.png"), grid_arrange_plots,
       width = 14, height = 24, dpi = 300)

#### Supplementary Figure 17 ------
# Income group
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "econlevel",
                          y_var = "Logistic.Fit", 
                          category_var = "econlevel", 
                          category_order = c("High", "Low"),
                          label_vars = c("High", "Low"),
                          plot_title = "Income Group and Growth Speed",
                          save_filename = 'Update_incomelevel_boxplot.png')

gdp_scatter <- create_scatterplot(hatch_growth_filter, "Log_GDPavg", "GDP per Capita (log)", "GDP per Capita")
ggsave('gdp_scatter.png',gdp_scatter, width = 12, height = 8, dpi = 300)

gdp_scatter_poly <- create_scatterplot_poly(hatch_growth_filter, "Log_GDPavg", "GDP per Capita (log)", "GDP per Capita (log)")
ggsave('gdp_scatter_poly.png', gdp_scatter_poly,width = 12, height = 8, dpi = 300)

#### Supplementary Figure 18 -----
# R&D
rd_scatter <- create_scatterplot(hatch_growth_filter, "rdspending_Average", "R&D Spending (as % of GDP)", "R&D Spending")
ggsave('rd_scatter.png',rd_scatter, width = 12, height = 8, dpi = 300)
rd_scatter_poly<- create_scatterplot_poly(hatch_growth_filter, "rdspending_Average", "R&D Spending (as % of GDP)", "R&D Spending")
ggsave('rd_scatter_poly.png',rd_scatter_poly, width = 12, height = 8, dpi = 300)

#### Supplementary Figure 19 -----
wvs_scatter_1 <- create_scatterplot(hatch_growth_filter, "direction_scitech_Average", "WVS Survey Responses", "Is the world better off because of science and technology?")
wvs_scatter_2 <- create_scatterplot(hatch_growth_filter, "easier_scitech_Average", "WVS Survey Responses", "Is science and technology making life easier?")
wvs_arrange <- ggarrange(wvs_scatter_1, wvs_scatter_2,
                         labels = c("a", "b"),
                         font.label= list(size = 24, face = 'bold'),
                         ncol =2, nrow = 1)
ggsave('wvs_scatter.png',wvs_arrange, width = 20, height = 10, dpi = 300)

wvs_scatter_poly_1 <- create_scatterplot_poly(hatch_growth_filter, "direction_scitech_Average", "WVS Survey Responses", "Is the world better off because of science and technology?")
wvs_scatter_poly_2 <- create_scatterplot_poly(hatch_growth_filter, "easier_scitech_Average", "WVS Survey Responses", "Is science and technology making life easier?")
wvs_arrange_poly <- ggarrange(wvs_scatter_poly_1, wvs_scatter_poly_2,
                              labels = c("a", "b"),
                              font.label= list(size = 24, face = 'bold'),
                              ncol =2, nrow = 1)
ggsave('wvs_scatter_poly.png',wvs_arrange_poly, width = 20, height = 10, dpi = 300)

wvs_scatter_1 <- create_scatterplot(hatch_growth_filter, "direction_scitech_Average", "WVS Survey Responses", "Is the world better off because of science and technology?")
wvs_scatter_2 <- create_scatterplot(hatch_growth_filter, "easier_scitech_Average", "WVS Survey Responses", "Is science and technology making life easier?")
wvs_arrange <- ggarrange(wvs_scatter_1, wvs_scatter_2,
                         labels = c("a", "b"),
                         font.label= list(size = 24, face = 'bold'),
                         ncol =2, nrow = 1)
ggsave('wvs_scatter.png',wvs_arrange, width = 20, height = 10, dpi = 300)

#### Supplementary Figure 20 ------

# State Capacity
hatch_growth_filter_cap<- hatch_growth_filter %>%
  filter(!is.na(cap_cat ))

describe_categorical_tech(data = hatch_growth_filter_cap,
                          x_var = "cap_cat",
                          y_var = "Logistic.Fit", 
                          category_var = "cap_cat", 
                          category_order = c("High", "Low"),
                          label_vars = c("High", "Low"),
                          plot_title = "State Capacity Category",
                          save_filename = 'Update_statecap_boxplot.png')

state_cap_scatter <- create_scatterplot(hatch_growth_filter, "Capacity_Average", "State Capacity", "State Capacity")
ggsave('state_cap_scatter.png',state_cap_scatter, width = 12, height = 8, dpi = 300)


state_cap_scatter_poly <- create_scatterplot_poly(hatch_growth_filter, "Capacity_Average", "State Capacity", "State Capacity")
ggsave('state_cap_scatter_poly.png',state_cap_scatter_poly, width = 12, height = 8, dpi = 300)


#### Supplementary Figure 21 ------
# Democracy Type
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "dem",
                          y_var = "Logistic.Fit", 
                          category_var = "dem", 
                          category_order = c("Democracy", "Not Democracy"),
                          label_vars = c("Democracy", "Not Democracy"),
                          plot_title = "Democracy and Growth Speed",
                          save_filename = 'Update_democracy_boxplot.png')
#### Supplementary Figure 22 ------
# Visualize democracy and tech type
# Create the plot to compare democracy by technology type

# Only include countries for which democracy data is available
hatch_growth_filter_dem <- hatch_growth_filter[!is.na(hatch_growth_filter$dem) & hatch_growth_filter$dem != "", ]

democracy_category_plot <- ggplot(hatch_growth_filter_dem, aes(x = Category.Type, y = Logistic.Fit, fill = dem)) +
  geom_boxplot(color = "black", outlier.color = "black") +
  labs(title = "Government Type and Technology Type",
       x = "",
       y = "Speed of Diffusion",
       fill = "Gov Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0.5, hjust = 0.5),  # Adjust angle and hjust
    legend.position = "bottom",
    plot.background = element_rect(fill = "white"),  # Set background color to white
    panel.background = element_rect(fill = "white"), # Set panel background color to white
    panel.border = element_blank(),  # Remove the outside border of the plot
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  theme_minimal()+
  scale_fill_manual(values = c("darkolivegreen3", "cornflowerblue"))  # Blue and green colors

# Wrap x axis labels to not overlap
democracy_category_plot <- democracy_category_plot + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  

# Add significance tests and indicators of significant difference for each 
# category
for (cat_type in unique(hatch_growth_filter$Category.Type)) {
  df_cat <- subset(hatch_growth_filter, Category.Type == cat_type)
  dem <- subset(df_cat, dem == "Democracy")$Logistic.Fit
  notdem <- subset(df_cat, dem == "Not Democracy")$Logistic.Fit
  
  if (length(dem) > 0 && length(notdem) > 0) {
    p_val <- t.test(dem, notdem)$p.value
    #print(paste0("Number of Technologies in HATCH (pre-filtering): ", num_tech))
    
    print(paste0('Category Type: ', cat_type))
    print(paste0('Number of democracies: ', length(dem)))
    print(paste0('Number of non-democracies: ', length(notdem)))
    print(paste0('P-Value difference: ', p_val))
    if (p_val < 0.05) 
      democracy_category_plot <- democracy_category_plot + annotate("text", 
                                                                    x = unique(df_cat$Category.Type)[1], 
                                                                    y = max(df_cat$Logistic.Fit) + 0.05, label = "*", size = 10, 
                                                                    color = "purple") 
  }
}

# Save the plot as an image file 
ggsave(file.path("update_category_gov_significance.png"), democracy_category_plot, width = 10, height = 6)
#### Supplementary Figure 23 ------
# State Capacity and Dem

describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "dem",
                          y_var = "Capacity_Average", 
                          category_var = "dem", 
                          category_order = c("Democracy", "Not Democracy"),
                          label_vars = c("Democracy", "Not Democracy"),
                          plot_title = "Democracy and State Capacity ",
                          save_filename = 'Update_statecapdem_boxplot.png')
#### Supplementary Figure 24 ------

# Democracy and State Capacity Category

# Prepare data for plotting
boxplot_data_compare <- hatch_growth_filter_cap %>%
  filter(!is.na(dem)) %>%
  arrange(cap_cat, dem) %>%
  mutate(Dem_Cap = paste0(dem, "_", cap_cat)) %>%
  select(Dem_Cap, Logistic.Fit) 


demcap_colors <- c("Democracy_High" = "cadetblue3", 
                   "Democracy_Low" = "darkseagreen", 
                   "Not Democracy_High" = "deeppink4", 
                   "Not Democracy_Low" = "darkslateblue")

# Create the plot
dem_cap_box <- ggplot(boxplot_data_compare, aes(x = Dem_Cap, y = Logistic.Fit, fill = Dem_Cap)) +
  geom_boxplot(color = "black", outlier.color = "black") +
  labs(title = "Democracy Type and Capacity",
       x = "",
       y = "Speed of Diffusion") + 
  #fill = "Dem_Cap") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust angle and hjust
    legend.position = "None",
    plot.background = element_rect(fill = "white"),  # Set background color to white
    panel.background = element_rect(fill = "white"),  # Set panel background color to white
    plot.title = element_text(hjust = 0.5),
    panel.border = element_blank()
  ) +
  scale_fill_manual(values = demcap_colors) # Adjust colors as needed

# Save the plot as an image file
ggsave(file.path("Update_dem_cap_compare.png"), dem_cap_box, width = 10, height = 6)

#difference in levels democracies
dunn_result_demcap <- dunn.test(boxplot_data_compare$"Logistic.Fit", 
                                boxplot_data_compare$"Dem_Cap", method = "none")

# Create a data frame for Dunn's test results
dunn_df <- data.frame(
  Comparison = dunn_result_demcap$comparisons,
  P.value = dunn_result_demcap$P,
  Adjusted.P.value = dunn_result_demcap$P.adjusted
)

# Save Dunn's test results
write.csv(dunn_df, 'demcap_sig.csv', row.names = FALSE)



#### Supplementary Figure 25 -----
# Relationship between logistic fit and num. of points in time series
plot_num_points_speed <- ggplot(hatch_growth, aes(y = Logistic.Fit, x = Number.of.Points)) +
  geom_point() +
  ggtitle("Logistic Fit and Length of Time Series") +        # Add title
  xlab("Number of Data Points") +                      # Edit x-axis label
  stat_smooth(colour = "#45b39d") +
  theme_minimal()# Fit exponential regression line
ggsave('logfit_numpoints_sensitivity.png', plot = plot_num_points_speed, width = 6, height = 6, units = 'in')



#### Supplementary Figure 26 -----
# setwd('05_robustness') <- set wd

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

## Set variables of interest -
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


# Correlations (simple)
modelList_share <- reg_list(hatch_growth_filter_share, "Logistic.Fit", variables_of_interest)
modelList_production <- reg_list(hatch_growth_filter_production, "Logistic.Fit", variables_of_interest)
modelList_cap <- reg_list(hatch_growth_filter_cap, "Logistic.Fit", variables_of_interest)
modelList_numunits <- reg_list(hatch_growth_filter_numunits, "Logistic.Fit", variables_of_interest)
#hatch_growth_filter_allbutshare
modelList_allNOSHARE <- reg_list(hatch_growth_filter_allbutshare, "Logistic.Fit", variables_of_interest)


# Save bivariates
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
modelsummary(modelList_allNOSHARE, 
             stars = TRUE,
             statistic = 'p.value',
             output = 'bivariate_noshare.html')

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
# Add signficance columnm
p.value_sig = 0.05
all_models <- all_models %>%
  mutate(significant = ifelse(p.value < p.value_sig, "Yes", "No"))

### Supplementary Figure 27 -----
# Make the dot-and-whisker plot with facets
dw_plot_short_metrics <- dwplot(all_models) +aes(shape = significant) +
  scale_shape_manual(values = c("Yes" = 19, "No" = 1)) +
  facet_wrap(~ model, scales = "free_y") +  # Facet by model label
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "darkgray", size = 1) +
  #scale_shape_manual(values = c("Yes" = 19, "No" = 1)) +
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

## Supplementary Figure 28 -----
# Divide into two sections on first year of commercialization ---
# Supplementary analysis C5
# Pre 1960
hatch_growth_pre75 <- hatch_growth_filter %>%
  filter(FirstCommercialYr < 1975)
hatch_growth_post75 <- hatch_growth_filter %>%
  filter(FirstCommercialYr > 1974)

modelList_late <- reg_list(hatch_growth_post75, "Logistic.Fit", variables_of_interest)
modelsummary(modelList_late,
             stars = TRUE,
             statistic = 'p.value',
             output = 'late_tech.html')
modelList_early <- reg_list(hatch_growth_pre75, "Logistic.Fit", variables_of_interest)
modelsummary(modelList_early,
             stars = TRUE,
             statistic = 'p.value',
             output = 'early_tech.html')

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

