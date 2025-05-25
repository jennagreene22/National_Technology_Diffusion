## This code will summarize the national level HATCH data 

# From 2024 code, updated Apr 2025
# Jenna Greene

#install.packages('ggplot2')

library(ggplot2)
library(ggpubr)
library(dplyr)

## ------ Load in Data -----
setwd('../')
setwd('02_scripts')

# Run clean hatch data
source('clean_hatch_data.R')


## ---------------------- Description of HATCH --------------------
# summary of technologies
num_tech <-length(unique(hatch_growth$technology))
print(paste0("Number of Technologies in HATCH (pre-filtering): ", num_tech))

# summary of countries
num_countries <- length(unique(hatch_growth$country))
print(paste0("Number of Countries in HATCH (pre-filtering): ", num_countries))

## ---------------------- Visualize HATCH  Data--------------------
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

# Technologies with data for only one country
one_country <- nrow(countries_per_technology_df %>% filter(Countries ==1))
print(paste0("There are ", one_country, " technologies with only one country in HATCH"))

# Median number of countries per tech
median_country_count <- median(countries_per_technology_df$Countries)
print(paste0("Median number of countries for each tech ", median_country_count))

# Median number of tech per country
median_tech_per_country <- median(technologies_per_country_df$Technologies)
print(paste0("Median number of technologies per country is ", median_tech_per_country))

# Country with the most technologies
most_tech_per_country <- max(technologies_per_country_df$Technologies)

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
setwd('../03_plots/description')
ggsave("countries_per_technology_histogram_Update.png", hist, width = 10, height = 6, units = "in", dpi = 300)

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
#setwd('../03_plots/description')
ggsave("technologies-per-country_Update.png", hist_2, width = 10, height = 6, units = "in", dpi = 300)


## Combine Histograms
descriptive_HATCH_histograms <- ggarrange(hist, hist_2, 
                                          labels = c("a", "b"), 
                                          ncol =2, nrow = 1)
ggsave("Update_descriptive_histograms.png", 
       descriptive_HATCH_histograms, width = 11, height = 6, units = "in", dpi = 300)

### Summarize each variable ------------------------------------------

setwd('../')
# Load in function to create CSVs of summary stats
source('summary_stats.R')
setwd('../')
categorical_variables_tech <- c("Final.Material.Use", 
                                "Technology.Lifetime", 
                                "Granularity",
                                "Type.of.Adopter",
                                "Complexity",
                                "Need.for.Customization",
                                "Feedstock",
                                "Broad.replacement", 
                                "Category.Type") 
summarize_variable(hatch_growth_filter, categorical_variables_tech,output_dir = "04_reports/description/")


## Summarize year of first comm. 
df_commercialYr <- hatch_growth_filter %>%
  select(technology, FirstCommercialYr) %>%
  distinct()

# Merge and summarize
# #merge_avg_comm_df <- df_commercialYr %>%
# #  left_join(median_growth_rate, by = "technology")
# 
# #merge_avg_comm_df <- merge_avg_comm_df %>%
# #  filter(!is.na(FirstCommercialYr))
# 
# ## 
# min_year_info <- merge_avg_comm_df %>%
#   filter(FirstCommercialYr == min(FirstCommercialYr)) %>%
#   select(FirstCommercialYr, Logistic.Fit)
# 
# max_year_info <- merge_avg_comm_df %>%
#   filter(FirstCommercialYr == max(FirstCommercialYr)) %>%
#   select(FirstCommercialYr, Logistic.Fit)

## ------------ Describe technologies and Countries in the data ------------ 
setwd('04_reports/description')

### Summary of data after filtering (Supplementary TABLE 2 and 3)

# Supplementary table 2
ctry_per_tech <- hatch_growth_filter %>% 
  group_by(technology) %>% 
  summarize(count_by_tech = n())

write.csv(ctry_per_tech, file = "tech_summary_filter.csv", row.names = FALSE)

# Supplementary table 3
tech_per_country <- hatch_growth_filter %>% 
  group_by(country) %>% 
  summarize(count_by_country = n())
write.csv(tech_per_country, file = "ctry_summary_filter.csv", row.names = FALSE)

## Summary of tech characteristics after filtering (Supplementary Table 11)
tech_char_hatch_filtered <- hatch_growth_filter %>%
  group_by(technology) %>%
  summarize(
    Material.Use.Numerical = first(Material.Use.Numerical),
    Final.Material.Use = first(Final.Material.Use),
    FirstCommercialYr = first(FirstCommercialYr),
    Need.for.Customization = first(Need.for.Customization),
    Complexity = first(Complexity),
    Type.of.Adopter = first(Type.of.Adopter),
    Granularity.Numerical = first(Granularity.Numerical),
    Granularity = first(Granularity),
    Average.lifetime = first(Average.lifetime),
    Technology.Lifetime = first(Technology.Lifetime),
    Strict.replacement = first(Strict.replacement),
    Broad.replacement = first(Broad.replacement),
    Feedstock = first(Feedstock),
    Category.Type = first(Category.Type)
  ) %>%
  ungroup()
write.csv(tech_char_hatch_filtered, file = "tech_characteristics_filtered.csv", row.names = FALSE)


## ------------ Describe filtering by fitting ------------ 
# Supplementary tables 14 and 15

## Supplementary Table 14
hatch_filtering_initial <- nrow(hatch_growth)
step_1 <- sum(is.na(hatch_growth$Logistic.Fit))
hatch_filtering_aftererr <- hatch_growth[!is.na(hatch_growth$Logistic.Fit), ]
step_2 <- sum(hatch_filtering_aftererr$logistic.goodness < 0.95)
hatch_filtering_95 <- hatch_filtering_aftererr %>% dplyr::filter(logistic.goodness >= 0.95)
nrow(hatch_filtering_95)
step_3 <- sum(hatch_filtering_95$Logistic.Fit > 1)
hatch_filtering_1 <- hatch_filtering_95 %>% dplyr::filter(Logistic.Fit <= 1)
nrow(hatch_filtering_1)
step_4 <- sum(hatch_filtering_1$Logistic.Fit < 0)
hatch_filtering_0 <- hatch_filtering_1 %>% dplyr::filter(Logistic.Fit > 0)
nrow(hatch_filtering_0)
nrow(hatch_growth_filter) 

## Supplementary Table 15
hatch_filtering_initial <- nrow(hatch_growth)
hatch_growth$Gompertz.Fit <- as.numeric(hatch_growth$Gompertz.Fit)

step_1_g <- sum(is.na(hatch_growth$Gompertz.Fit))
hatch_filtering_aftererr_g <- hatch_growth[!is.na(hatch_growth$ Gompertz.Fit), ]
nrow(hatch_filtering_aftererr_g)
step_2_g <- sum(hatch_filtering_aftererr_g$gompertz.goodness < 0.95)
hatch_filtering_95_g <- hatch_filtering_aftererr_g %>% dplyr::filter(gompertz.goodness >= 0.95)
nrow(hatch_filtering_95_g)

step_3_g <- sum(hatch_filtering_95_g$Gompertz.Fit > 1)
hatch_filtering_1_g <- hatch_filtering_95_g %>% dplyr::filter(Gompertz.Fit <= 1)
nrow(hatch_filtering_1_g)

step_4 <- sum(hatch_filtering_1_g$Gompertz.Fit < 0)
hatch_filtering_0_g <- hatch_filtering_1_g %>% dplyr::filter(Gompertz.Fit > 0)
nrow(hatch_filtering_0_g)

