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
Step-by-step to reproduce key results:
1. Clean HATCH data:  Analysis-Code/02_scripts/clean_hatch_data.R
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
(optional: run "Measuring Speed.ipynb" to analyze curve fitting processes)

## Data availability & privacy
Data should be cited at {insert Zenodo link when public}

## License
CC-BY

## Contributions & Support
- Contact Jenna Greene, jhgreene@wisc.edu, with questions
