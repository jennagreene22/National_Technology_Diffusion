
library(car)
library(corrplot) 
library(ggpubr)
library(ggExtra)
library(ggcorrplot)


# Load in data ------
setwd('02_scripts')
source('clean_hatch_data.R')


# Select data for correlations ------
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

## VIF -------
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
#setwd('../')
#setwd('03_plots/multivariate')
ggsave("Update_vif_plot_num.png", vif_plot, width = 8, height = 8, units = "in", dpi = 300)

# Combine plot --------
combined_correlations <- ggarrange(correlations_hatch, vif_plot,  nrow = 2, labels = c("a", "b"), font.label= list(size = 24, face = 'bold'))

ggsave("combined_correlations.png", combined_correlations, width = 12, height = 12, units = "in", dpi = 300)

