# Calls function to create boxplots and histograms to summarize numerical data

# 2024 analysis, updated April 2025
# Jenna Greene


# Load clean hatch data
#setwd('../../../')
setwd('02_scripts')
source("clean_hatch_data.R")

# Load function for plotting
#### Load in function ------
setwd('../')
setwd('02_scripts/plot_functions')
source('numerical_plots.R')

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

## Legend plot ------------------------------------------------------------------
## Color mapping
specific_technologies <- c("Aquaculture Production", 
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
                           "Motorcycles",
                           "Natural Gas Pipelines",
                           "Natural Gas Production", 
                           "Objects Launched into Space",
                           "Oil Refining Capacity", 
                           "Onshore Wind Energy", 
                           "POL3 Vaccine", 
                           "Postal Traffic", 
                           "Radio", 
                           "Railroad", 
                           "Renewable Power", 
                           "Solar Photovoltaic", 
                           "Solid Biofuels", 
                           #"Steamships", 
                           "Sulphuric Acid", 
                           "Telegraph Traffic", 
                           "Telephones", 
                           "Television") 

specific_colors <- c("#1BA3C6FF", "#2CB5C0FF", "#30BCADFF", "#21B087FF", "#33A65CFF", 
                     "#57A337FF", "#A2B627FF", "#D5BB21FF", "#F8B620FF", "#F89217FF", 
                     "#F06719FF", "#E03426FF", "#F64971FF", "#FF9898FF", "#FC719EFF", 
                     "#EB73B3FF", "#CE69BEFF", "#A26DC2FF", "#938DD2FF", "#7873C0FF",  
                     "#2D6D66FF", "#4F7CBAFF", "#083ED0FF", "#AE904EFF", "#9E3E1CFF",
                     "#DB7C3CFF", "#EACB85FF", "#63BDD3FF", "#96ABC6FF") #

gray_tech <- c("Air-Source Heat Pumps","Ammonia Synthesis","Cable TV", "Cadmium Refining", 
              "Canals", "Coal Production", "Copper|Mining", "Copper|Refining", "Dishwashers", 
              "Disk Brakes", "Electric Bicycles", "Ethanol", "Flush Toilet", "Flywheel Battery Storage",
              "Freezer"  , "Geothermal Energy", "Gold", "HEPB3 Vaccine", "HEPBB Vaccine",
              "HIB3 Vaccine", "Herbicide-Tolerant Corn", "Herbicide-Tolerant Cotton", "Herbicide-Tolerant Soybeans",
              "High Speed Rail", "Home Air Conditioning", "Home Computers",
              "Hydrochloric Acid", "Hydroelectricity", "Insect-Resistant Corn", "Insect-Resistant Cotton", 
              "Iron Ore", "Jet Aircraft", "Laundry Dryers", "Lead", 
              "Lead-Acid Battery Storage", "Liquefied Natural Gas", "MCV2 Vaccine",
              "Marine Energy", "Microcomputers" , "Microwaves","Nitric Acid",
              "Nitrogen Fertilizer", "Nox Pollution Controls (Boilers)", "Nuclear Energy",
              "Nuclear Weapons", "Offshore Wind Energy", "Oil Pipelines","Oil Production",                      
              "PCV3 Vaccine", "Phosphate Fertilizer", "Podcasting", "Potash Fertilizer", "Power Steering", 
              "Primary Bauxite Production", "Public Roads", "Pumped Hydro Storage",                
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
  guides(color = guide_legend(nrow = 4))

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

## Numerical Scatterplots - Technology -----------------------------------------

# Scatterplots of National Level Data
comm_scatter <- create_scatterplot(hatch_growth_filter, "FirstCommercialYr", "", "Year of First Commercialization")
comm_scatter_poly <- create_scatterplot_poly(hatch_growth_filter, "FirstCommercialYr", "", "Year of First Commercialization")

gran_scatter <- create_scatterplot(hatch_growth_filter, "log_Granularity.Numerical", "", "Granularity")
gran_scatter_poly <- create_scatterplot_poly(hatch_growth_filter, "log_Granularity.Numerical", "", "Granularity")
mat_scatter <- create_scatterplot(hatch_growth_filter, "log_Material.Use.Numerical", "", "Material Use Intensity")
mat_scatter_poly <- create_scatterplot_poly(hatch_growth_filter, "log_Material.Use.Numerical", "", "Material Use Intensity")
life_scatter <- create_scatterplot(hatch_growth_filter, "log_Average.lifetime", "", "Technology Lifetime")
life_scatter_poly <- create_scatterplot_poly(hatch_growth_filter, "log_Average.lifetime", "", "Technology Lifetime")

# Scatterplots of Median Data
lifetime_med_scatter <- create_scatterplot(merge_hatch_medians, "log_Average.lifetime", "", "Technology Lifetime (Median)")
life_med_scatter_poly <-create_scatterplot_poly(merge_hatch_medians, "log_Average.lifetime", "", "Technology Lifetime (Median)")
mat_med_scatter <- create_scatterplot(merge_hatch_medians, "log_Material.Use.Numerical", "", "Material Use Intensity (Median)")
mat_med_scatter_poly <- create_scatterplot_poly(merge_hatch_medians, "log_Material.Use.Numerical", "", "Material Use Intensity (Median)")
gran_med_scatter <- create_scatterplot(merge_hatch_medians, "log_Granularity.Numerical", "", "Granularity (Median)")
gran_med_scatter_poly <- create_scatterplot_poly(merge_hatch_medians, "log_Granularity.Numerical", "", "Granularity (Median)")
comm_med_scatter <- create_scatterplot(merge_hatch_medians, "FirstCommercialYr", "", "Year of First Commercialization (Median)")
comm_med_scatter_poly <- create_scatterplot_poly(merge_hatch_medians, "FirstCommercialYr", "", "Year of First Commercialization (Median)")
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
                                          font.label= list(size = 24, face = 'bold'),
                                          ncol =2, nrow = 4)
# Save the combined plot
ggsave(file.path("Update_Combined_Scatterplots_2.png"), grid_arrange_plots,
       width = 14, height = 14, dpi = 300)

# Combine and save plots (poly)
grid_arrange_plots <- ggarrange(comm_med_scatter_poly, 
                               comm_scatter_poly, 
                               gran_med_scatter_poly, 
                               gran_scatter_poly, 
                               mat_med_scatter_poly, 
                               mat_scatter_poly, 
                               life_med_scatter_poly, 
                               life_scatter_poly, 
                               labels = c("a", "b", 'c', 'd', 'e', 'f', 'g', 'h'), 
                               font.label= list(size = 24, face = 'bold'),
                               ncol =2, nrow = 4)
ggsave(file.path("Update_Combined_Scatterplots_poly.png"), grid_arrange_plots,
       width = 14, height = 14, dpi = 300)
  
## Numerical Scatterplots - National -----------------------------------------
rd_scatter <- create_scatterplot(hatch_growth_filter, "rdspending_Average", "R&D Spending (as % of GDP)", "R&D Spending")
ggsave('rd_scatter.png',rd_scatter, width = 12, height = 8, dpi = 300)

gdp_scatter <- create_scatterplot(hatch_growth_filter, "gdppc_Average", "GDP per Capita", "GDP per Capita")
ggsave('gdp_scatter.png',gdp_scatter, width = 12, height = 8, dpi = 300)

wvs_scatter_1 <- create_scatterplot(hatch_growth_filter, "direction_scitech_Average", "WVS Survey Responses", "Is the world better off because of science and technology?")
wvs_scatter_2 <- create_scatterplot(hatch_growth_filter, "easier_scitech_Average", "WVS Survey Responses", "Is science and technology making life easier?")
wvs_arrange <- ggarrange(wvs_scatter_1, wvs_scatter_2,
                                labels = c("a", "b"),
                                font.label= list(size = 24, face = 'bold'),
                                ncol =2, nrow = 1)
ggsave('wvs_scatter.png',wvs_arrange, width = 20, height = 10, dpi = 300)

state_cap_scatter <- create_scatterplot(hatch_growth_filter, "Capacity_Average", "State Capacity", "State Capacity")
ggsave('state_cap_scatter.png',state_cap_scatter, width = 12, height = 8, dpi = 300)
