----2025/12/20----
Add two files:
**1. data_prep.R** (This is the data prepration step.)
   
**2. model.R** (This is the modelling step.)
   The script requires a pre-existing dataframe df_analysis containing:

2.1 Core Variables :
   log_ppsqm: log(price per square meter) - **outcome**
   social_index: school social index - **core explanatory variable** 
   dist_nearest_km: distance to nearest school in kilometers - **core explanatory variable** 
   school_ID_nearest: nearest school ID (for clustering)
   gid2019: location ID (for fixed effects)
   wohnflaeche: living area (housing control)
   house_age: house age in years (housing control)
   renovated: renovation indicator (housing control)

2.2 The script creates unified variable names for modeling:
  Outcome: y_log_ppsqm
  Key regressors: x_social_index, x_dist_km
  Controls: c_area_sqm, c_house_age, c_renovated
  FE/clustering: fe_location, cl_school

2.3 Sample Filtering
  Removes observations with missing values in any core variable.

2.4 Regression Ladder
  Baseline 0 (B0): Distance only (y ~ distance)
  Baseline 1 (B1): Distance + Social Index (additive)
  Model 2 (M2): Interaction only (y ~ social_index * distance)
  Model 3 (M3): Interaction + Housing controls
  Model 4 (M4): Main specification​ - Interaction + Controls + Location FE
  Robustness (R): Log-distance specification (non-linear decay)

