# This function creates boxplots and histograms to summarize numerical data

# 2024 analysis, updated April 2025
# Jenna Greene
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(ggExtra)

# Set working directory
#setwd('../../')
setwd('../../')
setwd('03_plots/description/numerical')

## Scatterplot function
create_scatterplot <- function(df, x_column, detail, plot_title) {
  
  # Filter out string values or NA values
  exclude_values <- c('Cheap', 'Low', 'Small', 'Medium', 'Large', 'Expensive')
  filtered_df <- df[complete.cases(df[x_column]) & 
                      !(df[x_column] %in% exclude_values) & 
                      is.numeric(df[[x_column]]) &
                      !is.infinite(df[[x_column]]), ]
  
  # Find technologies present in the filtered dataset
  present_technologies <- intersect(specific_technologies, unique(filtered_df$technology))
  
  # Create color mapping for present technologies
  color_mapping <- setNames(rep("grey", nrow(filtered_df)), filtered_df$technology)  # Start with grey for all
  for (i in seq_along(present_technologies)) {
    tech <- present_technologies[i]
    color_mapping[filtered_df$technology == tech] <- specific_colors[match(tech, specific_technologies)]
  }
  
  
  # Plot points
  gg <- ggplot(df, aes_string(x = x_column, y = "Logistic.Fit", color = "technology")) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = color_mapping, name = "Technology",
                       labels = unique(df$technology)[order(unique(df$technology))], 
                       breaks = unique(df$technology)[order(unique(df$technology))]) +
    theme_classic() +
    theme(
      legend.text = element_text(size = 8),        
      legend.title = element_blank(),      
      axis.text = element_text(size = 18),          
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.position = "none", 
      plot.margin = unit(c(100, 5, 5, 5), "cm")) +  
    ggtitle(plot_title) +
    ylab("Speed of Diffusion") +
    ylim(0, 1) +
    guides(color = guide_legend(nrow = 12))  
  
  # Fit linear regression line if applicable
  if (x_column %in% c("Average.lifetime", "Material.Use.Numerical", "Granularity.Numerical")) {
    x <- filtered_df[[x_column]]
    y <- filtered_df[["Logistic.Fit"]]
    x <- log(x)
    lm_fit <- lm(y ~ x)
    gg <- gg + 
      geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "sienna", linetype = "solid", size = 1.2) +
      annotate("text", x = Inf, y = Inf, label = paste("R² =", round(summary(lm_fit)$r.squared, 2), 
                                                       "\nn = ", sum(!is.na(x)), 
                                                       "\nLinear Regression of Log(", x_column, ") and Logistic Fit"), 
               hjust = 1, vjust = 135, size = 6, color = "sienna")
  } else {
    if (!is.na(x_column)) {
      x <- filtered_df[[x_column]]
      y <- filtered_df[["Logistic.Fit"]]
      lm_fit <- lm(y ~ x)
      gg <- gg + 
        geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "sienna", linetype = "solid", size = 1.2) +
        annotate("text", x = Inf, y = Inf, label = paste("R² =", round(summary(lm_fit)$r.squared, 2), "\nn = ", sum(!is.na(x))), 
                 hjust = 1, vjust = 1.35, size = 6, color = "sienna")
    }
  }
  
  # Set x-axis label based on column_name
  if (x_column == 'log_Material.Use.Numerical') {
    gg <- gg + xlab("Log Material Use (kg/dollar)")
  } else if (x_column == 'log_Granularity.Numerical') {
    gg <- gg + xlab("Log Granularity ($09 in first year of tech)")
  } else if (x_column == 'log_Average.lifetime') {
    gg <- gg + xlab("Log Years")
  } else if (x_column == 'FirstCommercialYr') {
    gg <- gg + xlab("Year of First Comercialization")
  } else {
    gg <- gg + xlab(detail)
  }
  
  # Add histograms on x and y axes outside of the plot
  gg <- ggMarginal(p = gg, type = "histogram", theme = theme_classic())
  #gg <- gg + theme_classic()
  return(gg)
  # Save and show the plot
  plot_filename <- paste0("Update_Scatterplot_", plot_title, ".png")
  ggsave(file.path("03_plots", plot_filename), plot = gg, device = "png", width = 12, height = 12, dpi = 300)
  print(gg)
  return(gg)
}

create_scatterplot_poly <- function(df, x_column, detail, plot_title) {
  
  # Filter out string values or NA values
  exclude_values <- c('Cheap', 'Low', 'Small', 'Medium', 'Large', 'Expensive')
  filtered_df <- df[complete.cases(df[x_column]) & 
                      !(df[x_column] %in% exclude_values) & 
                      is.numeric(df[[x_column]]) &
                      !is.infinite(df[[x_column]]), ]
  
  # Find technologies present in the filtered dataset
  present_technologies <- intersect(specific_technologies, unique(filtered_df$technology))
  
  # Create color mapping for present technologies
  color_mapping <- setNames(rep("grey", nrow(filtered_df)), filtered_df$technology)  # Start with grey for all
  for (i in seq_along(present_technologies)) {
    tech <- present_technologies[i]
    color_mapping[filtered_df$technology == tech] <- specific_colors[match(tech, specific_technologies)]
  }
  
  # Plot points
  gg <- ggplot(df, aes_string(x = x_column, y = "Logistic.Fit", color = "technology")) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_manual(values = color_mapping, name = "Technology",
                       labels = unique(df$technology)[order(unique(df$technology))], 
                       breaks = unique(df$technology)[order(unique(df$technology))]) +
    theme_classic() +
    theme(
      legend.text = element_text(size = 8),        
      legend.title = element_blank(),      
      axis.text = element_text(size = 18),          
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.position = "none", 
      plot.margin = unit(c(100, 5, 5, 5), "cm")) +  
    ggtitle(plot_title) +
    ylab("Speed of Diffusion") +
    ylim(0, 1) +
    guides(color = guide_legend(nrow = 12))  
  
  # Fit linear regression line if applicable
  if (x_column %in% c("Average.lifetime", "Material.Use.Numerical", "Granularity.Numerical")) {
    x <- filtered_df[[x_column]]
    y <- filtered_df[["Logistic.Fit"]]
    x <- log(x)
    lm_fit <- lm(y ~ x)
    gg <- gg + 
      geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "sienna", linetype = "solid", size = 1.2) +
      annotate("text", x = Inf, y = Inf, label = paste("R² (Linear) =", round(summary(lm_fit)$r.squared, 2), 
                                                       "\nn = ", sum(!is.na(x)), 
                                                       "\nLinear Regression of Log(", x_column, ") and Logistic Fit"), 
               hjust = 1, vjust = 135, size = 6, color = "sienna")
  } else {
    if (!is.na(x_column)) {
      x <- filtered_df[[x_column]]
      y <- filtered_df[["Logistic.Fit"]]
      lm_fit <- lm(y ~ x)
      gg <- gg + 
        geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "sienna", linetype = "solid", size = 1.2) +
        annotate("text", x = Inf, y = Inf, label = paste("R² (Linear) =", round(summary(lm_fit)$r.squared, 2), "\nn = ", sum(!is.na(x))), 
                 hjust = 1, vjust = 1.45, size = 6, color = "sienna")
    }
  }
  
  # Add polynomial regression line (degree 2) (dashed)
  poly_fit <- lm(Logistic.Fit ~ poly(filtered_df[[x_column]], 2, raw = TRUE), data = filtered_df)
  gg <- gg + 
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "mediumorchid", linetype = "dashed", size = 1.2) +
    annotate("text", x = Inf, y = Inf, label = paste("R² (Polynomial) =", round(summary(poly_fit)$r.squared, 2)), 
             hjust = 1, vjust = 1.1, size = 6, color = "mediumorchid")
  
  # Set x-axis label based on column_name
  if (x_column == 'log_Material.Use.Numerical') {
    gg <- gg + xlab("Log Material Use (kg/dollar)")
  } else if (x_column == 'log_Granularity.Numerical') {
    gg <- gg + xlab("Log Granularity ($09 in first year of tech)")
  } else if (x_column == 'log_Average.lifetime') {
    gg <- gg + xlab("Log Years")
  } else if (x_column == 'FirstCommercialYr') {
    gg <- gg + xlab("Year of First Comercialization")
  } else {
    gg <- gg + xlab(detail)
  }
  
  # Add histograms on x and y axes outside of the plot
  gg <- ggMarginal(p = gg, type = "histogram", theme = theme_classic())
 # gg <- gg + theme_classic()
  return(gg)
  
  # Save and show the plot
  plot_filename <- paste0("Update_Scatterplot_", plot_title, ".png")
  ggsave(file.path("03_plots", plot_filename), plot = gg, device = "png", width = 12, height = 12, dpi = 300)
  print(gg)
  return(gg)
}
