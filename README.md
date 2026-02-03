# The Effects of School Proximity and the School Social Index on Housing Prices

**Authors:** Yanyi Ji, Daniel Tobien, Luc Wichtmann  
**Course:** Data Analysis using R (Winter Term 25/26)  
**Date:** February 3, 2026

---

## 📌 Project Overview

This project analyzes the capitalization of **school proximity** and **school quality** (measured by the official School Social Index) into single-family housing prices in **North Rhine-Westphalia (NRW), Germany**.

Using a Hedonic Price Model on cross-sectional data from 2022, we investigate:
1.  **Distance Effect:** Do housing prices decrease as distance to the nearest primary/secondary school increases?
2.  **Social Index Effect:** Does the socio-economic composition of a school (Social Index) moderate this price premium?
3.  **Robustness:** A spatial sensitivity analysis using a **Queen Contiguity** raster grid approach.

## 🛠️ Prerequisites & Dependencies

The analysis is performed in **R**. Please ensure you have the following packages installed before running the script:

```r
install.packages(c(
  "tidyverse", "readxl", "fs", "scales", "janitor", 
  "modelsummary", "spdep", "RANN", "ggplot2", "gt", 
  "lmtest", "sandwich", "car", "readr", "broom", 
  "kableExtra", "knitr", "interactions", "emmeans", 
  "patchwork", "sf"
))
```

Note: `sf` is required for handling spatial data (shapefiles).

## 📂 Directory Structure

To ensure the scripts run correctly without path errors, please maintain the following directory structure. The working directory in R should be set to the `project_root`.

```text
project_root/
│
├── README.md                   # Project documentation and usage guide
├── Code_Final_Clean.R          # Main R analysis script
│
└── course_data/                # Main Data Directory
    ├── housing_data/
    │   └── cross_section/
    │       ├── CampusFile_HK_2022.csv  # Single-family houses (Sales)
    │       ├── CampusFile_WK_2022.csv  # Flats (Sales) - used for grid construction
    │       └── CampusFile_WM_2022.csv  # Flats (Rent) - used for grid construction
    │
    ├── school_data/
    │   ├── 2022_social_index.csv       # School Social Index (Quality) data
    │   └── distance_to_schools.csv     # Pre-calculated Euclidean distances
    │
    ├── region_data/
    │   └── region_data.csv             # Regional demographics (Income, Population)
    │
    └── VG250/
        └── vg250_ebenen_0101/
            └── VG250_GEM.shp           # Shapefile for NRW municipalities
```

## 🚀 How to Run

1.  **Open the Project:**
    Launch **RStudio** and open the `Code_Final_Clean.R` file.

2.  **Set Working Directory:**
    Ensure your working directory is set to the project root folder (the location of the script). You can do this via the RStudio menu (`Session` -> `Set Working Directory` -> `To Source File Location`) or by running:
    ```r
    setwd("/path/to/your/project_root")
    ```

3.  **Execute the Script:**
    Run the entire script `Code_Final_Clean.R`. The code is structured into three main modules:
    * **Part 1: Data Preparation**
        * Cleans raw housing data.
        * Merges school proximity and regional demographic information.
        * Constructs the Queen Contiguity raster grid for spatial analysis.
    * **Part 2: Analysis**
        * Estimates OLS regression models (Continuous, Binned, and Interaction specifications).
        * Performs spatial robustness checks.
    * **Part 3: Results**
        * Exports formatted LaTeX tables (via `modelsummary` & `kableExtra`).
        * Generates `ggplot2` visualizations for the final presentation.

## 📊 Methodology Highlights

* **Empirical Model:**
    * **Type:** OLS Regression (Log-Linear Specification).
    * **Inference:** Heteroskedasticity-Consistent (HC1) Robust Standard Errors.

* **Key Variables:**
    * **Dependent Variable:** Log of Housing Price (`log_kaufpreis`).
    * **Independent Variables:**
        * Distance to nearest Primary/Secondary school (Linear $km$ and Quadratic $km^2$).
        * School Social Index (Scale 1-9, categorized into Good/Average/Bad).
    * **Controls:** Building Age, Living Area, Plot Area, Number of Rooms, Regional Demographics (Income, Population Density).

* **Robustness Checks:**
    * **Multicollinearity:** Variance Inflation Factors (VIF).
    * **Outliers:** Cook's Distance (Threshold: $4/n$).
    * **Spatial Sensitivity:** Alternative "Queen Distance" metric calculated on a 1km x 1km raster grid to validate Euclidean distance results.

## 📄 License & Data Sources

* **Housing Data:** [RWI Real Estate Data - Campus File](https://www.rwi-essen.de/) (FDZ Ruhr).
* **School Data:** [Ministry of Schools and Education NRW](https://www.schulministerium.nrw/).
* **Geodata:** [Federal Agency for Cartography and Geodesy (BKG)](https://www.bkg.bund.de/).
* **License:** This project is for **Educational Use Only** within the "Data Analysis using R" course context.
