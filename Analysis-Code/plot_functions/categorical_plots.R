# This function creates boxplots and histograms to summarizze technology categorical data

# 2024 analysis, updated April 2025
# Jenna Greene


library(ggplot2)
library(ggpubr)
library(gridExtra)
library(stringr)

# Set working directory
setwd('../../')
setwd('03_plots/description/categorical')


# Set colors of boxplots
boxplot_colors <- c("coral2", "darkgoldenrod1", "darkturquoise",
                    "darkseagreen", "darkorange2", "burlywood2", 
                    "cadetblue3", "deeppink4", "darkslateblue", 
                    "chocolate4", "chartreuse4")


## Boxplot function - Technology ---------------------------------------------------
describe_categorical_tech <- function(data, x_var, y_var, category_var, category_order, label_vars, plot_title, save_filename) {
  
  # Filter out rows with blank category values, NAs, or "None"
  data <- data[!(is.na(data[[category_var]]) | data[[category_var]] == "" | data[[category_var]] == "None!"), ]
  
  # Convert category_var to factor with desired levels
  data[[category_var]] <- factor(data[[category_var]], levels = category_order)
  
  # Calculate counts for each category
  counts <- as.data.frame(table(data[[category_var]]))
  colnames(counts) <- c("category", "count")
  
  # Calculate unique countries and technologies for each category
  unique_stats <- lapply(category_order, function(category) {
    filtered_data <- data[data[[category_var]] == category, ]
    unique_countries <- length(unique(filtered_data$country))
    unique_technologies <- length(unique(filtered_data$technology))
    return(data.frame(category = category, unique_countries = unique_countries, unique_technologies = unique_technologies))
  })
  
  # Combine unique_stats into a single dataframe
  unique_stats <- do.call(rbind, unique_stats)
  unique_stats <- merge(unique_stats, counts, by = "category")
  unique_stats$category <- factor(unique_stats$category, levels = category_order)
  
  
  # Create the boxplot
  boxplot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[category_var]])) +
    geom_boxplot() +
    labs(x = plot_title, y = "Speed of Diffusion") +
    scale_fill_manual(values = boxplot_colors, 
                      name = plot_title, 
                      guide = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(labels = label_vars) +
    labs(title = plot_title)        
  
  # Create annotation plot underneath the boxplot
  annotation_plot <- ggplot() +
    geom_text(data = unique_stats, aes(x = category, y = -0.1,
                                       label = paste("n =", count,
                                                     "\nCountries =", unique_countries,
                                                     "\nTechnologies =", unique_technologies)),
              size = 4, hjust = 0.5, vjust = 0.5) +
    theme_void() +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  # Combine boxplot and annotation plot 
  final_plot <- grid.arrange(boxplot, annotation_plot, heights = c(5, 1))
                              
  # Save the plot
  ggsave(file.path(save_filename),
         plot = final_plot,
         width = 6, height = 6, units = "in", dpi = 300)
  #setwd('../')
  # # Create histograms
  histograms <- lapply(category_order, function(category) {
    filtered_data <- data[data[[category_var]] == category, ]
    shapiro_results <- shapiro.test(filtered_data[[y_var]])
    shapiro_text <- paste("Shapiro-Wilk Test of Normality\np-value:", format.pval(shapiro_results$p.value))

    hist_plot <- ggplot(filtered_data, aes(x = .data[[y_var]])) +
      geom_histogram(binwidth = 0.1, color = "#000000", fill = "darkolivegreen3") +
      labs(title = paste("Histogram of", y_var, "for", category),
           x = y_var, y = "Frequency") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold")) +
      annotate("text", x = max(filtered_data[[y_var]]), y = 50,
               label = shapiro_text, hjust = 1)

    # Save each histogram
    ggsave(file.path(paste0("Update_histogram_", category_var, "_", category, ".png")),
           plot = hist_plot,
           width = 6, height = 6, units = "in", dpi = 300)

    return(hist_plot)
  })
  
  return(list(final_plot = final_plot, histograms = histograms))
}





