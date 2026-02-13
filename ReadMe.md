#  Title
Drivers of Technology Diffusion in Countries
# Code Author
Jenna Greene

## Overview
This code is used to analyze temporal, technology, and country characteristics that impact the speed of technology diffusion using a dataset of historical technology adoption. 

## Repository structure
- `Data` — Processed data needed to run Analysis-Code
- `Hatch-Data-Prep`
    - `Measuring Growth Speed.ipynb` — measures speed of technology diffusion (Python script)
- `Analysis-Code/00_data`
    - location for data needed to run scripts (place data from previous Python script in this folder)
- `Analysis-Code/02_scripts` (run in this order)
    - `clean_hatch_data.R` — reads in and cleans HATCH data necessary for analysis (helper code)
    - `describe_hatch_data.R` - summary of HATCH data (helper code)
    - `significance_tests.R` — function that generates reports of significance between variables
    - `summary_stats.R` — function that generates tables to summarize each variable
    - `regressions_list.R` — function that generates a list of regression results from a list of independent variables with an inputted dependent variable (linear regression, polynomial with squared term, polynomial with cubic term)
    - `plot_functions/categorical_plots.R` — function that creates categorical plot style
    - `plot_functions/numerical_plots.R` — function that creates numerical plot style
    - `generate_figures.R` - generates figures for main text
    - `generate_tables.R` - generates tables for main text
    - `generate_figures_Supplemental.R` - generates figures for supplemental analysis
    - `generate_tables_Supplemental.R` - generates tables for supplemental analysis
    - `supplemental_analysis.R` - analysis for SI not in tables or figures
- `Analysis-Code/03_figures` - location for figures
- `Analysis_Code/04_reports` - location for tables and reports
- `README.md` — this file

### Python
- Create environment and install:
    - python3
- Optional: Run:
    - "Measuring Speed.ipynb"

### R
- Create R environment
- Run scripts:
    - `clean_hatch_data.R` 
    - `describe_hatch_data.R`
    - `significance_tests.R` 
    - `summary_stats.R` 
    - `regressions_list.R`
    - `plot_functions/categorical_plots.R`
    - `plot_functions/numerical_plots.R` 
    - `generate_figures.R`
    - `generate_tables.R` 
    - `generate_figures_Supplemental.R`
    - `generate_tables_Supplemental.R`
    - `supplemental_analysis.R` 

## Usage / Workflow
Workflow for results: 
1. Clean HATCH data:  Analysis-Code/02_scripts/clean_hatch_data.R
    ** Note: data is contained in 00_data folder. Set working directory to relevant file as needed.
2. Run functions:
    - `significance_tests.R` 
    - `summary_stats.R` 
    - `regressions_list.R`
    - `plot_functions/categorical_plots.R`
    - `plot_functions/numerical_plots.R` 
3. Generate figures for main text: Analysis-Code/02_scripts/generate_figures.R
4. Generate tables for main text and methods: Analysis-Code/02_scripts/generate_tables.R
5. Generate figures for supplementary: Analysis-Code/02_scripts/generate_figures_Supplementary.R
6. Generate tables for supplementary: Analysis-Code/02_scripts/generate_tables_Supplementary.R
7. Generate other supplemental analysis: Analysis-Code/02_scripts/supplemental_analysis.R
(optional: run "Measuring Speed.ipynb" to analyze curve fitting processes. Time series data is contained in same folder (Hatch-Data-Prep))

## Outputs
The files for the main analysis (generate_figures.R and generate_tables.R) output the figures, tables, and results for the main text.

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
