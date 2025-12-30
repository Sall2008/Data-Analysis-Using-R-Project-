# ==== 1. Libraries ====
library(dplyr)
library(modelsummary)
library(sandwich)

# ==== 2. Build analysis datasets ====

df_ps <- df_analysis %>%
  mutate(
    school_type2 = case_when(
      school_type_nearest %in% c(2) ~ "primary",
      school_type_nearest %in% c(4, 10, 15, 20) ~ "secondary",
      TRUE ~ NA_character_
    ),
    school_type2 = factor(school_type2, levels = c("primary", "secondary"))
  ) %>%
  filter(!is.na(school_type2))

df_all_bins <- df_analysis %>%
  mutate(
    dist_bin = case_when(
      dist_nearest_km <= 2 ~ "0-2km",
      dist_nearest_km <= 3 ~ "2-3km",
      dist_nearest_km <= 4 ~ "3-4km",
      TRUE ~ "4km+"
    ),
    dist_bin = factor(dist_bin, levels = c("0-2km", "2-3km", "3-4km", "4km+"))
  )

df_ps_bins <- df_ps %>%
  mutate(
    dist_bin = case_when(
      dist_nearest_km <= 2 ~ "0-2km",
      dist_nearest_km <= 3 ~ "2-3km",
      dist_nearest_km <= 4 ~ "3-4km",
      TRUE ~ "4km+"
    ),
    dist_bin = factor(dist_bin, levels = c("0-2km", "2-3km", "3-4km", "4km+"))
  )

# ==== 3. Estimate models ====

m_ps_notype <- lm(
  log_ppsqm ~ dist_nearest_km + wohnflaeche + house_age + renovated + factor(gid2019),
  data = df_ps
)

m_ps_type <- lm(
  log_ppsqm ~ dist_nearest_km + school_type2 + wohnflaeche + house_age + renovated + factor(gid2019),
  data = df_ps
)

m_all_bins <- lm(
  log_ppsqm ~ dist_bin + wohnflaeche + house_age + renovated + factor(gid2019),
  data = df_all_bins
)

m_ps_bins_int <- lm(
  log_ppsqm ~ dist_bin * school_type2 + wohnflaeche + house_age + renovated + factor(gid2019),
  data = df_ps_bins
)

# ==== 4. Cluster-robust vcov (school-level if available, otherwise HC1) ====
vcov_cluster <- function(model_object, data_used) {
  if ("school_ID_nearest" %in% names(data_used)) {
    sandwich::vcovCL(model_object, cluster = data_used$school_ID_nearest, type = "HC1")
  } else {
    sandwich::vcovHC(model_object, type = "HC1")
  }
}

vcov_list <- list(
  vcov_cluster(m_ps_notype, df_ps),
  vcov_cluster(m_ps_type, df_ps),
  vcov_cluster(m_all_bins, df_all_bins),
  vcov_cluster(m_ps_bins_int, df_ps_bins)
)

# ==== 5. Paper-style regression table ====

models_paper <- list(
  "(1) PS: Distance"        = m_ps_notype,
  "(2) PS: Distance + Type" = m_ps_type,
  "(3) All: Distance bins"  = m_all_bins,
  "(4) PS: Bins × Type"     = m_ps_bins_int
)

coef_map <- c(
  "dist_nearest_km" = "Distance to nearest school (km)",
  
  "dist_bin2-3km" = "Distance: 2–3 km",
  "dist_bin3-4km" = "Distance: 3–4 km",
  "dist_bin4km+"  = "Distance: 4 km+",
  
  "school_type2secondary" = "Secondary school (vs Primary)",
  
  "dist_bin2-3km:school_type2secondary" = "2–3 km × Secondary",
  "dist_bin3-4km:school_type2secondary" = "3–4 km × Secondary",
  "dist_bin4km+:school_type2secondary"  = "4 km+ × Secondary",
  
  "wohnflaeche" = "Living area (sqm)",
  "house_age"   = "House age (years)",
  "renovated"   = "Renovated"
)

tab_distance <- modelsummary(
  models_paper,
  vcov = vcov_list,
  coef_map = coef_map,
  statistic = "({std.error})",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_omit = "Intercept",
  gof_map = c("nobs" = "Observations", "r.squared" = "R-squared"),
  gof_omit = "Adj|F|Log|RMSE|IC",
  output = "gt"
)

tab_distance