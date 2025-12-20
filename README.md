# Housing Prices & School Distance Analysis

**Last Updated:** 2025-12-20

This project consists of two sequential R scripts: **Data Preparation** and **Econometric Modeling**.

---

##  1. `data_prep.R` (Data Preparation)

This script handles the complete ETL pipeline: importing raw data, cleaning, merging, and generating descriptive statistics. It produces the final `df_analysis` dataset used for modeling.

###  1.1 Data Inputs

| Source Data | Content | Key Identifier |
| :--- | :--- | :--- |
| **Housing** (`CampusFile_HK_2022.csv`) | RWI-GEO-RED v7 listings (NRW only) | `gid2019` |
| **School** (`2022_social_index.csv`) | Social index & metadata | School ID |
| **Distance** (`distance_to_schools.csv`) | Distances from 1km grids to schools | `ergg_1km` |
| **Region** (`region_data.csv`) | Urban/Non-urban classification | `AGS` |

###  1.2 Processing Workflow

1. **Cleaning:**
    * **Housing:** Standardizes `gid2019`, censors extreme values (price, area, rooms), and creates feature variables (`house_age`, `renovated`).
    * **Distance:** Filters invalid distances (<0 or >100km) and keeps only the **nearest school** per grid cell.
    * **School:** Parses social index to numeric; ensures unique School IDs.
2. **Merging:** Sequential join: `Housing` $\to$ `Distance` $\to$ `School` $\to$ `Region`.
3. **Diagnostics:** Generates a merge quality table to distinguish between structural missingness (e.g., non-urban) and data errors.

###  1.3 Sample Definition

**Main Sample (`df_analysis`):**
* **Filters:** Urban regions only, valid distance & social index, positive price & area.
* **Outcome Variable:**
```r
log_ppsqm = log(kaufpreis / wohnflaeche)
```

**Robustness Sample (`df_analysis_all`):** Includes non-urban regions.

###  1.4 Outputs
* **Datasets:** `df_analysis.rds`, `df_analysis_all.rds`
* **Tables:** Merge diagnostics, Descriptive stats (overall & by bins).
* **Plots:** Histograms, Scatter plots (Price vs. Distance), Boxplots.

---

##  2. `model.R` (Econometric Modeling)

This script performs the regression analysis using `df_analysis`. It estimates the interaction between **School Social Index** and **Distance** on housing prices.

###  2.1 Setup & Variables

* **Pre-requisite:** Requires `df_analysis` from step 1.
* **Sanity Check:** Stops if core variables are missing.
* **Standardization:** Maps raw variables to unified modeling names:

| Role | Unified Name | Description |
| :--- | :--- | :--- |
| **Outcome** | `y_log_ppsqm` | Log price per sqm |
| **Key Regressors**| `x_social_index`<br>`x_dist_km` | School Social Index<br>Distance to nearest school (km) |
| **Controls** | `c_area_sqm`<br>`c_house_age`<br>`c_renovated` | Living area, Age, Renovation status |
| **Fixed Effects** | `fe_location` | Location ID (`gid2019`) |
| **Cluster** | `cl_school` | Nearest School ID |

###  2.2 Regression Specifications

The analysis follows a "regression ladder" of increasing complexity. Standard errors are clustered at the school level.

1. **Baseline Models:**
    * **B0:** $Price \sim Distance$
    * **B1:** $Price \sim Distance + SocialIndex$
2. **Interaction Models:**
    * **M2:** $Price \sim SocialIndex \times Distance$
    * **M3:** M2 + Housing Controls
    * **M4 (Main):** M3 + Location Fixed Effects
3. **Robustness (R):** Uses log-distance $\log(Distance + 0.05)$ to test non-linear spatial decay.

###  2.3 Outputs

* **Console:** Coefficient estimates for the main FE model.
* **Tables:** Publication-ready regression table (using `modelsummary`).
* **Visualization:** Marginal effects plot (`plot_marginal_effect_social_by_distance.png`) showing the impact of social index across different distances.

---

###  How to Run
1. Run `data_prep.R` to generate the clean datasets.
2. Run `model.R` to generate regression tables and plots.
