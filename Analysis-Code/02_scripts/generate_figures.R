# Generate figures for main text of national technology diffusion paper
# Greene, Brutschin, Gidden, Nemet

# Load in libraries
library(car)
library(corrplot) 
library(ggpubr)
library(ggExtra)
library(ggcorrplot)
library(tidyverse)
library(broom)
#setwd('../') # <- ensure proper root directory
#setwd('03_plots/main')  # <- ensure proper working directory

#### Figure 1 ####

##Visualize HATCH data
# Create histograms 
# Count the number of countries per technology
countries_per_technology <- table(hatch_growth$technology)

# Count the number of technologies per country
technologies_per_country <- table(hatch_growth$country)

# Convert the tables to data frames for better manipulation
countries_per_technology_df <- as.data.frame(countries_per_technology)
technologies_per_country_df <- as.data.frame(technologies_per_country)

# Rename the columns
colnames(countries_per_technology_df) <- c("Technology", "Countries")
colnames(technologies_per_country_df) <- c("Country", "Technologies")


# Plot histogram for countries per technology
# 0 to 50 then break above 50
breaks_custom <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
over_50_df <- countries_per_technology_df
over_50_df$Countries [over_50_df$Countries > 50] <- 51

hist <- ggplot(over_50_df, aes(x = Countries)) +
  geom_histogram(color = "#000000", fill = "#DAA5ACFF", breaks = breaks_custom) +
  labs(title = "Number of Countries per Technology in HATCH",
       x = "Number of Countries",
       y = "Frequency (# of Tech)") +
  scale_x_continuous(limits=c(0, 55), breaks=breaks_custom, labels=c(0,  5, 10, 15, 20, 25, 30, 35, 40, 45, 50, "> 50")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 14, face = "bold"))
#ggsave("countries_per_technology_histogram.png", hist, width = 10, height = 6, units = "in", dpi = 300)

# Plot histogram for technologies per country
breaks_custom_2 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
over_50_df_2 <- technologies_per_country_df
over_50_df_2$Technologies [technologies_per_country_df$Technologies > 50] <- 51
hist_2 <- ggplot(over_50_df_2, aes(x = Technologies)) +
  geom_histogram(color = "#000000", fill = "#B6E7E0FF", breaks = breaks_custom) +
  labs(title = "Number of Technologies per Country in HATCH",
       x = "Number of Technologies",
       y = "Frequency (# of Countries)") +
  scale_x_continuous(limits=c(0, 55), breaks=breaks_custom, labels=c(0,  5, 10, 15, 20, 25, 30, 35, 40, 45, 50, "> 50")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 14, face = "bold"))
#ggsave("technologies-per-country.png", hist_2, width = 10, height = 6, units = "in", dpi = 300)


## Combine Histograms
descriptive_HATCH_histograms <- ggarrange(hist, hist_2, 
                                          labels = c("a", "b"), 
                                          ncol =2, nrow = 1)
ggsave("descriptive_histograms_fig1.png", 
       descriptive_HATCH_histograms, width = 11, height = 6, units = "in", dpi = 300)

### ---------------------- Figure 2  -------------------
                  #Visualize Numerical Variables #
#setwd('02_scripts/plot_functions') #<- set working directory to load in function
#source('numerical_plots.R') #<- load in function

# Create median data

## Create median data
median_growth_rate <- aggregate(Logistic.Fit ~ technology, 
                                data = hatch_growth_filter, 
                                FUN = median)
# Define columns to select
columns_to_select <- c("technology", 
                       "Material.Use.Numerical",
                       "Final.Material.Use",
                       "Year.of.Invention",
                       "Year.of.First.Embodiment.of.Tech",
                       "FirstCommercialYr",
                       "Need.for.Customization",
                       "Complexity",
                       "Type.of.Adopter",
                       "Granularity.Numerical",
                       "Granularity",
                       "Average.lifetime",
                       "Technology.Lifetime",
                       "Strict.replacement",
                       "Broad.replacement",
                       "Feedstock",
                       "Category.Type",
                       "Patent.Category.Name",
                       "log_Average.lifetime",
                       "log_Granularity.Numerical", 
                       "log_Material.Use.Numerical")

# Drop duplicates and merge
merge_hatch_medians <- merge(median_growth_rate, 
                             distinct(hatch_growth_filter, technology, .keep_all = TRUE)[, columns_to_select], 
                             by = "technology", 
                             all.x = TRUE)

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
ggsave("legend_plot_fig2.png", legend_plot, width = 12.5, height = 4, dpi = 300)

# Gray dots

filtered_short_tech<- hatch_growth_filter %>%
  count(technology) %>%
  filter(n < 10) %>%
  pull(technology)


filtered_large_tech<- hatch_growth_filter %>%
  count(technology) %>%
  filter(n > 9) %>%
  pull(technology)


# Numerical Scatterplots - Technology --

# Scatterplots of National Level Data
comm_scatter <- create_scatterplot(hatch_growth_filter, "FirstCommercialYr", "", "Year of First Commercialization")
gran_scatter <- create_scatterplot(hatch_growth_filter, "log_Granularity.Numerical", "", "Granularity")
mat_scatter <- create_scatterplot(hatch_growth_filter, "log_Material.Use.Numerical", "", "Material Use Intensity")
life_scatter <- create_scatterplot(hatch_growth_filter, "log_Average.lifetime", "", "Technology Lifetime")

comm_med_scatter <- create_scatterplot(merge_hatch_medians, "FirstCommercialYr", "", "Year of First Commercialization (Median)")
gran_med_scatter <- create_scatterplot(merge_hatch_medians, "log_Granularity.Numerical", "", "Granularity (Median)")
mat_med_scatter <- create_scatterplot(merge_hatch_medians, "log_Material.Use.Numerical", "", "Material Use Intensity (Median)")
lifetime_med_scatter <- create_scatterplot(merge_hatch_medians, "log_Average.lifetime", "", "Technology Lifetime (Median)")

# Combine plots into a grid layout
grid_arrange_plots <- ggarrange(comm_med_scatter, 
                                comm_scatter, 
                                gran_med_scatter, 
                                gran_scatter, 
                                mat_med_scatter, 
                                mat_scatter, 
                                lifetime_med_scatter, 
                                life_scatter, 
                                labels = c("a", "b", 'c', 'd', 'e', 'f', 'g', 'h'), 
                                font.label= list(size = 19, face = 'bold'),
                                ncol =2, nrow = 4)
# Save the combined plot
ggsave(file.path("Combined_Scatterplots_fig2.png"), grid_arrange_plots,
       width = 14, height = 14, dpi = 300)


### ---------------------- Figure 3  -------------------
                  # Visualize Categorical Variables #
#setwd('02_scripts/plot_functions') #<- set working directory to load in function
#source('categorical_plots.R') #<- load in function

# create bivariate boxplots --
# Material Use
describe_categorical_tech(data = hatch_growth_filter, 
                          x_var = "Final.Material.Use", 
                          y_var = "Logistic.Fit", 
                          category_var = "Final.Material.Use", 
                          category_order = c('Low Material Use', 'Medium Material Use', 'High Material Use'), 
                          label_vars = c('Low','Medium', 'High'),
                          plot_title = "Material Intensity", 
                          save_filename = "materialuse_boxplot.png")

# Complexity
describe_categorical_tech(data = hatch_growth_filter, 
                          x_var = "Complexity", 
                          y_var = "Logistic.Fit", 
                          category_var = "Complexity", 
                          category_order = c('Simple [1]', 'Design-Intensive [2]', 'Complex [3]'), 
                          label_vars = c('Simple', 'Design-Intensive', 'Complex'),
                          plot_title = "Complexity", 
                          save_filename = "complexity.png")

# Need for Customization
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "Need.for.Customization",
                          y_var = 'Logistic.Fit',
                          category_var = 'Need.for.Customization',
                          category_order = c('Standardized [1]', 'Mass-customized [2]', 'Customized [3]'), 
                          label_vars = c('Standardized', 'Mass-customized', 'Customized'), 
                          plot_title = 'Need for Customization', 
                          save_filename = 'custom.png')

# Technology Lifetime
describe_categorical_tech(data = hatch_growth_filter, 
                          x_var = "Technology.Lifetime", 
                          y_var = "Logistic.Fit", 
                          category_var = "Technology.Lifetime", 
                          category_order = c('Months', 'Years', 'Decades'), 
                          label_vars = c('Months', 'Years', 'Decades'),
                          plot_title = "Technology Lifetime", 
                          save_filename = "tech_lifetime_boxplot.png")

# Type of Adopter
describe_categorical_tech(data = hatch_growth_filter, 
                          x_var = "Type.of.Adopter", 
                          y_var = "Logistic.Fit", 
                          category_var = "Type.of.Adopter", 
                          category_order = c('Firms [1]', 'Individuals [2]', 'Both [3]'), 
                          label_vars = c('Firms','Individuals', 'Both'),
                          plot_title = "Type of Adopter", 
                          save_filename = "adopt_boxplot.png")

# Granularity 
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "Granularity", 
                          y_var = "Logistic.Fit",
                          category_var = "Granularity",
                          category_order = c("Low", "Medium", "High"),
                          label_vars =  c("Low", "Medium", "High"),
                          plot_title = "Granularity", 
                          save_filename = "granularity_boxplot.png")

# Replacement
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "Broad.replacement",
                          y_var = "Logistic.Fit", 
                          category_var = "Broad.replacement", 
                          category_order = c("Yes", "No"),
                          label_vars = c("Yes", "No"),
                          plot_title = "Broad Replacement",
                          save_filename = 'replaceent_boxplot.png')

# Feedstock
describe_categorical_tech(data = hatch_growth_filter,
                          x_var = "Feedstock",
                          y_var = "Logistic.Fit", 
                          category_var = "Feedstock", 
                          category_order = c("Yes", "No"),
                          label_vars = c("Yes", "No"),
                          plot_title = "Feedstock",
                          save_filename = 'feedstock_boxplot.png')

### ----------------------  Figure 4 -------------------
                 # Visualize Correlations #

# Select data for correlations ---
select_columns_numeric <- select(hatch_growth_filter, 'FirstCommercialYr',
                                 'log_Granularity.Numerical',
                                 'log_Material.Use.Numerical',
                                 'log_Average.lifetime',
                                 'Log_GDPavg',
                                 'rdspending_Average',
                                 #'dem',
                                 'Capacity_Average',
                                 'easier_scitech_Average',
                                 'direction_scitech_Average'
)
colnames(select_columns_numeric) <- c("First Comm. Year", 
                                      "Granularity (log)",
                                      "Material Use (log)",
                                      "Lifetime (log)",
                                      "GDP per capita (log)", 
                                      "R&D Spending",
                                      "State Capacity",
                                      "Sci/Tech making life easier", 
                                      "Sci/Tech making life better")

# Create correlation matrix
correlations_hatch_test <- cor(select_columns_numeric, use = "complete.obs")

# Create correlation plot
correlations_hatch <- ggcorrplot(correlations_hatch_test,# method = "circle",
                                 hc.order = FALSE, # order by correlation
                                 #title = "Correlation Matrix",
                                 colors = c('red', 'white', 'blue'),
                                 ggtheme = ggplot2::theme_minimal,
                                 outline.col = "white", #border color
                                 lab = TRUE) # label the correlation values

## VIF ---
select_columns_vif<- select(hatch_growth_filter, "Logistic.Fit", 
                            'FirstCommercialYr',
                            'log_Granularity.Numerical',
                            'log_Material.Use.Numerical',
                            'log_Average.lifetime',
                            'Complexity',
                            'Need.for.Customization',
                            'Type.of.Adopter',
                            'Feedstock',
                            'Broad.replacement',
                            'Log_GDPavg',
                            'rdspending_Average',
                            'dem',
                            'Capacity_Average',
                            'easier_scitech_Average',
                            'direction_scitech_Average', 'econlevel')

colnames(select_columns_vif) <- c("Logistic Fit", "First Comm. Year", 
                                  "Granularity (log)",
                                  "Material Use (log)",
                                  "Lifetime (log)",
                                  "Complexity", "Need for Customization",
                                  "Type of Adopter", "Feedstock", "Replacement",
                                  "GDP per capita (log)", 
                                  "R&D Spending", "Type of Government",
                                  "State Capacity",
                                  "Sci/Tech making life easier", 
                                  "Sci/Tech making life better", "Econ. Level")

model_num <- lm(`Logistic Fit` ~ `Granularity (log)` +
                  `Material Use (log)` + 
                  `Lifetime (log)` + 
                  `First Comm. Year` + 
                  `State Capacity` + 
                  `GDP per capita (log)` + `R&D Spending` +
                  `Complexity` + `Need for Customization` + `Type of Adopter` + 
                  `Type of Government` + `Feedstock` + `Replacement` +
                  `Sci/Tech making life easier` +  
                  `Sci/Tech making life better` + `Econ. Level`,
                data = select_columns_vif)
vif_num_factors <-  vif(model_num)
vif_variable_names <- rownames(vif_num_factors)
gvif <- vif_num_factors[, 'GVIF^(1/(2*Df))']

vif_df <- data.frame(variable_name = vif_variable_names, 
                     gvif =gvif )


# Plot VIF
vif_plot <- ggplot(vif_df, aes(x = reorder(variable_name, gvif), y = gvif)) +
  geom_bar(stat = "identity", color = "#000000", fill = "#B6E7E0FF") +
  geom_text(aes(label = sprintf("%.1f", gvif)), 
            vjust = -0.3,  # Adjust vertical position of labels
            color = "black", 
            size = 5) +    # Adjust text size
  labs(#title = "Variance Inflation Factors (VIFs)",
    x = "",
    y = "Generalized Variance Inflation Factor") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),  
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 10, 10, 50))

#ggsave("vif_plot_num.png", vif_plot, width = 8, height = 8, units = "in", dpi = 300)

# Combine plot ---
combined_correlations <- ggarrange(correlations_hatch, vif_plot,  nrow = 2, labels = c("a", "b"), font.label= list(size = 24, face = 'bold'))

ggsave("combined_correlations_fig4.png", combined_correlations, width = 12, height = 12, units = "in", dpi = 300)


## ---------------------- Figure 5


# capacity, gov type, gdp
M9 <- lm(Logistic.Fit ~ dem + 
           Capacity_Average + 
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
            Complexity + FirstCommercialYr, 
          data = hatch_growth_filter)



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

ggsave("multivariate_coefficient_estimates_plot_fig5.png", plot = coeff_plot, width = 12, height = 10, dpi = 300)


