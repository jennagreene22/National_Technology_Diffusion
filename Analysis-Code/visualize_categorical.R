# This calls function to create boxplots and histograms of categorical data

# 2024 analysis, updated April 2025
# Jenna Greene

#### Load in data --------
# load clean hatch data
setwd('../../../')
setwd('02_scripts')
source("clean_hatch_data.R")

#### Load in function ------
# Run boxplot function
setwd('../')
setwd('02_scripts/plot_functions')
source('categorical_plots.R')

#### create bivariate boxplots -------
# Material Use
describe_categorical_tech(data = hatch_growth_filter, 
                      x_var = "Final.Material.Use", 
                      y_var = "Logistic.Fit", 
                      category_var = "Final.Material.Use", 
                      category_order = c('Low Material Use', 'Medium Material Use', 'High Material Use'), 
                      label_vars = c('Low','Medium', 'High'),
                      plot_title = "Material Intensity", 
                      save_filename = "Update_materialuse_boxplot.png")

# Complexity
describe_categorical_tech(data = hatch_growth_filter, 
               x_var = "Complexity", 
               y_var = "Logistic.Fit", 
               category_var = "Complexity", 
               category_order = c('Simple [1]', 'Design-Intensive [2]', 'Complex [3]'), 
               label_vars = c('Simple', 'Design-Intensive', 'Complex'),
               plot_title = "Complexity", 
               save_filename = "Update_complexity.png")

# Need for Customization
describe_categorical_tech(data = hatch_growth_filter,
               x_var = "Need.for.Customization",
               y_var = 'Logistic.Fit',
               category_var = 'Need.for.Customization',
               category_order = c('Standardized [1]', 'Mass-customized [2]', 'Customized [3]'), 
               label_vars = c('Standardized', 'Mass-customized', 'Customized'), 
               plot_title = 'Need for Customization', 
               save_filename = 'Update_custom.png')

# Technology Lifetime
describe_categorical_tech(data = hatch_growth_filter, 
               x_var = "Technology.Lifetime", 
               y_var = "Logistic.Fit", 
               category_var = "Technology.Lifetime", 
               category_order = c('Months', 'Years', 'Decades'), 
               label_vars = c('Months', 'Years', 'Decades'),
               plot_title = "Technology Lifetime", 
               save_filename = "Update_tech_lifetime_boxplot.png")

# Type of Adopter
describe_categorical_tech(data = hatch_growth_filter, 
               x_var = "Type.of.Adopter", 
               y_var = "Logistic.Fit", 
               category_var = "Type.of.Adopter", 
               category_order = c('Firms [1]', 'Individuals [2]', 'Both [3]'), 
               label_vars = c('Firms','Individuals', 'Both'),
               plot_title = "Type of Adopter", 
               save_filename = "Update_adopt_boxplot.png")

# Granularity 
describe_categorical_tech(data = hatch_growth_filter,
               x_var = "Granularity", 
               y_var = "Logistic.Fit",
               category_var = "Granularity",
               category_order = c("Low", "Medium", "High"),
               label_vars =  c("Low", "Medium", "High"),
               plot_title = "Granularity", 
               save_filename = "Update_granularity_boxplot.png")

# Replacement
describe_categorical_tech(data = hatch_growth_filter,
               x_var = "Broad.replacement",
               y_var = "Logistic.Fit", 
               category_var = "Broad.replacement", 
               category_order = c("Yes", "No"),
               label_vars = c("Yes", "No"),
               plot_title = "Broad Replacement",
               save_filename = 'Update_replaceent_boxplot.png')

# Feedstock
describe_categorical_tech(data = hatch_growth_filter,
               x_var = "Feedstock",
               y_var = "Logistic.Fit", 
               category_var = "Feedstock", 
               category_order = c("Yes", "No"),
               label_vars = c("Yes", "No"),
               plot_title = "Feedstock",
               save_filename = 'Update_feedstock_boxplot.png')

# Category
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
                              "Chemicals and Industrial",
                              "Digitalization",
                              "Energy Supply",
                              "Food and Health",
                              "Infrastructure",
                              "Materials",
                              "Sea and Water",
                              "Space and Defense",
                              "Storage",
                              "Transportation"),
               plot_title = "Category",
               save_filename = 'Update_tech_category.png')

# Democracy Type
describe_categorical_tech(data = hatch_growth_filter,
                              x_var = "dem",
                              y_var = "Logistic.Fit", 
                              category_var = "dem", 
                              category_order = c("Democracy", "Not Democracy"),
                              label_vars = c("Democracy", "Not Democracy"),
                              plot_title = "Democracy and Growth Speed",
                              save_filename = 'Update_democracy_boxplot.png')
 
# Income group
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "econlevel",
                          y_var = "Logistic.Fit", 
                          category_var = "econlevel", 
                          category_order = c("High", "Low"),
                          label_vars = c("High", "Low"),
                          plot_title = "Income Group and Growth Speed",
                          save_filename = 'Update_incomelevel_boxplot.png')
 
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

# State Capacity and Dem

describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "dem",
                          y_var = "Capacity_Average", 
                          category_var = "dem", 
                          category_order = c("Democracy", "Not Democracy"),
                          label_vars = c("Democracy", "Not Democracy"),
                          plot_title = "Democracy and State Capacity ",
                          save_filename = 'Update_statecapdem_boxplot.png')

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
setwd('../../../')
setwd('04_reports/significance_tests')
write.csv(dunn_df, 'demcap_sig.csv', row.names = FALSE)

#### Supplementary Figure 19 -------------------------------------
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
#setwd('../../')
setwd('03_plots/description/categorical')
ggsave(file.path("update_category_gov_significance.png"), democracy_category_plot, width = 10, height = 6)
