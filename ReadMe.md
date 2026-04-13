#  Title
Drivers of Technology Diffusion in Countries
# Code Author
Jenna Greene

## Overview
This code is used to analyze temporal, technology, and country characteristics that impact the speed of technology diffusion using a dataset of historical technology adoption. 
Python code produces growth speeds based on time series in the HATCH dataset using curve fitting. Further analyses of growth speeds generated in the Python file is done in R.
In this repository, the results of the python growth speed analysis are available in `Analysis-Code/00_data` if only running the R portion of the analysis.

## Repository structure
- `Data` — Processed data needed to run Analysis-Code (provided in folder and Zenodo link below)
  - `all_growth_merge.csv` is the HATCH data with growth speed, technology characteristics, and country characteristics for each technology-country time series
  - `technology_growth.csv` is the growth speed data generated from the Python script
- `Hatch-Data-Prep`
    - `Measuring Growth Speed.ipynb` — measures speed of technology diffusion (Python script)
- `Analysis-Code/data`
    - location for data needed to run scripts. `all_growth_merge.csv` is the HATCH data here for use in the R analysis (available also at Zenodo link below).
- `Analysis-Code/scripts` 
    - `00_national_tech_diffusion_initialization` - loads relevant packages and runs all scripts below
    - `01_clean_hatch_data.R` — reads in and cleans HATCH data necessary for analysis (helper code)
    - `02_describe_hatch_data.R` - summary of HATCH data (helper code)
    - `03_regressions_list.R` — function that generates a list of regression results from a list of independent variables with an inputted dependent variable (linear regression, polynomial with squared term, polynomial with cubic term)
    - `04_significance_tests.R` — function that generates reports of significance between variables
    - `05_summary_stats.R` — function that generates tables to summarize each variable
    - `plot_functions/categorical_plots.R` — function that creates categorical plot style
    - `plot_functions/numerical_plots.R` — function that creates numerical plot style
    - `06_generate_figures.R` - generates figures for main text
    - `07_generate_tables.R` - generates tables for main text
    - `08_generate_figures_supplemental.R` - generates figures for supplemental analysis
    - `09_generate_tables_supplemental.R` - generates tables for supplemental analysis
    - `10_supplemental_analysis.R` - analysis for SI not in tables or figures
- `README.md` — this file

### Python (Optional to generate growth speeds directly)
- Create environment and install:
    - python3
    - Load the following modules:
      - pandas, numpy, math, matplotlib.pyplot, warnings, os.path, sklearn.metrics, scripy.optimize
- Optional (if generating growth speeds directly from HATCH data): Run:
    - "Measuring Growth Speed.ipynb"

### R
- Create R environment
- Load packages and run scripts from initialization file:
    - `00_national_tech_diffusion_initialization` 

## Outputs
The files for the main analysis (06_generate_figures.R and 07_generate_tables.R) output the figures, tables, and results for the main text.
`generate_figures.R` generates:
    - figure 1: describing the number of technologies per country and countries per technology in the HATCH dataset ("descriptive_histograms_fig1.png")
    - figure 2: relationships between technology characteristics and growth speed ("Combined_Scatterplots_fig2.png")
    - boxplots that make up figure 3 (further annotated by author): "adopt_boxplot.png", "complexity.png", "custom.png", "feedstock_boxplot.png", "replacement_boxplot.png"
    - figure 4: correlations between characteristic data (combined_correlations_fig4.png)
    - figure 5: describing the multivariate regression models between technology and country characteristics and growth speed ("multivariate_coefficient_estimates_plot_fig5.png)
    - Histograms for each level of each technology characteristic (22 histograms that show the spread of diffusion speed for each level of each technology characteristics)
`generate_tables.R` generates:
    - "bivariate_variables_95.html" describes the relationship between each independent variable and diffusion speed using bivariate linear regression 
    - "hatch_summary_stats.csv" describes summary statistics for each characteristic in the dataset
    - "Broad.replacement.csv" describes summary statistics for each level of the categorical variable of replacement
    - "Category.Type.csv" describes summary statistics for each level of the categoical variable of technology category
    - "Complexity.csv" describes summary statistics for each level of the categorical variable of complexity
    - "Feedstock.csv" describes summary statistics for each level of the categorical variable of feedstock
    - "Final.Material.Use.csv" describes summary statistics for each level of the categorical variable of material use
    - "Granularity.csv" describes summary statistics for each level of the categorical variable of granularity
    - "Need.for.Customization.csv" describes summary statistics for each level of the categorical variable of need for customization
    - "Technology.Lifetime.csv" describes summary statistics for each level of the categorical variable of technology lifetime
    "Type.of.Adopter.csv" describes summary statistics for each level of the categorical variable of adopter type 

## Data availability & privacy
Data should be cited at {insert Zenodo link when public}

## License
CC-BY

## Contributions & Support
- Contact Jenna Greene, jhgreene@wisc.edu, with questions
