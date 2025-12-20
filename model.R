# ==== 1. Libraries ====

library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(lmtest)
library(sandwich)
library(modelsummary)

# ==== 2.Sanity checks & consistent variable names ====
# Assumption: df_analysis already exists and is the final analysis sample
stopifnot(exists("df_analysis"))

# Check required core variables for the main research question
required_vars <- c(
  "log_ppsqm",           # outcome: log(price per sqm)
  "social_index",        # school social index
  "dist_nearest_km",     # distance to nearest school
  "school_ID_nearest",   # nearest school id (for clustering)
  "gid2019",             # location id (for FE)
  "wohnflaeche",         # housing control
  "house_age",           # housing control
  "renovated"            # housing control
)

missing_vars <- setdiff(required_vars, names(df_analysis))
if (length(missing_vars) > 0) {
  stop("df_analysis is missing required variables: ", paste(missing_vars, collapse = ", "))
}

# Create unified, meaningful variable names for modeling (keep originals untouched)
df_model <- df_analysis %>%
  mutate(
    # Core outcome and key regressors (unified names)
    y_log_ppsqm      = log_ppsqm,
    x_social_index   = social_index,
    x_dist_km        = dist_nearest_km,
    
    # Controls (unified names)
    c_area_sqm       = wohnflaeche,
    c_house_age      = house_age,
    c_renovated      = renovated,
    
    # Fixed effects and clustering keys (unified names)
    fe_location      = as.factor(gid2019),
    cl_school        = as.factor(school_ID_nearest)
  )

# Remove rows with missing values in variables used across the main specifications
df_model <- df_model %>%
  filter(
    !is.na(y_log_ppsqm),
    !is.na(x_social_index),
    !is.na(x_dist_km),
    !is.na(c_area_sqm),
    !is.na(c_house_age),
    !is.na(c_renovated),
    !is.na(fe_location),
    !is.na(cl_school)
  )

cat("\n[Info] Final modeling sample size:", nrow(df_model), "\n")

# ==== 3. Regression ladder ====

# Interpretation notes:
# - Baseline models are intentionally simple to build intuition.
# - The main specification adds housing controls and fine-grained location fixed effects.
# - Standard errors are clustered at the nearest-school level.

# Baseline 0: distance only (pure raw gradient)
model_baseline_dist <- lm(
  y_log_ppsqm ~ x_dist_km,
  data = df_model
)

# Baseline 1: distance + social index (no interaction)
model_baseline_add_social <- lm(
  y_log_ppsqm ~ x_dist_km + x_social_index,
  data = df_model
)

# Model 2: interaction (core research mechanism), no controls
model_interaction_raw <- lm(
  y_log_ppsqm ~ x_social_index * x_dist_km,
  data = df_model
)

# Model 3: add key housing controls
model_controls <- lm(
  y_log_ppsqm ~ x_social_index * x_dist_km +
    c_area_sqm + c_house_age + c_renovated,
  data = df_model
)

# Model 4 (Main): add location fixed effects (gid2019 FE)
model_main_fe <- lm(
  y_log_ppsqm ~ x_social_index * x_dist_km +
    c_area_sqm + c_house_age + c_renovated +
    fe_location,
  data = df_model
)

# Robustness model: log distance (to allow non-linear decay)
df_model <- df_model %>%
  mutate(
    x_log_dist = log(x_dist_km + 0.05) # avoid log(0) by adding a small constant
  )

model_robust_logdist_fe <- lm(
  y_log_ppsqm ~ x_social_index * x_log_dist +
    c_area_sqm + c_house_age + c_renovated +
    fe_location,
  data = df_model
)

# ==== 4. Clustered inference (school-level clustering) ====

# Define a reusable function for school-clustered covariance (HC1)

vcov_cluster_school <- function(model_object) {
  sandwich::vcovCL(model_object, cluster = df_model$cl_school, type = "HC1")
}

cat("\n=== Main model (FE) with school-clustered SE ===\n")
print(lmtest::coeftest(model_main_fe, vcov. = vcov_cluster_school(model_main_fe)))

# ==== 5. Regression table (publication-style) ====

# Note: We use the same clustering approach for all models for comparability.
model_list <- list(
  "B0: Dist only"            = model_baseline_dist,
  "B1: + Social (no int.)"   = model_baseline_add_social,
  "M2: Interaction (raw)"    = model_interaction_raw,
  "M3: + Controls"           = model_controls,
  "M4: + Location FE (main)" = model_main_fe,
  "R: log(dist) + FE"        = model_robust_logdist_fe
)

# Provide a list of vcov functions (one per model) for clustered SE
vcov_list <- rep(list(vcov_cluster_school), length(model_list))

modelsummary(
  model_list,
  vcov = vcov_list,
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|F|RMSE",
  coef_map = c(
    "x_dist_km"                = "Distance to nearest school (km)",
    "x_social_index"           = "School social index",
    "x_social_index:x_dist_km" = "Social index × Distance",
    "c_area_sqm"               = "Living area (sqm)",
    "c_house_age"              = "House age (years)",
    "c_renovated"              = "Renovated (indicator)",
    "x_log_dist"               = "log(Distance + 0.05)",
    "x_social_index:x_log_dist"= "Social index × log(Distance)"
  )
)

# ==== 6. Interpret the interaction ====

# For the main FE model:
# Marginal effect of +1 social index at distance d:
#   ME(d) = beta_social + beta_interaction * d
# Convert to percent effect on €/sqm:
#   exp(ME(d)) - 1

get_marginal_effect_social <- function(model_object, d_grid, vcov_mat) {
  b <- coef(model_object)
  
  name_b1 <- "x_social_index"
  name_b3 <- "x_social_index:x_dist_km"
  
  if (!(name_b1 %in% names(b)) || !(name_b3 %in% names(b))) {
    stop("Main model does not contain required interaction terms.")
  }
  
  b1 <- b[name_b1]
  b3 <- b[name_b3]
  
  v11 <- vcov_mat[name_b1, name_b1]
  v33 <- vcov_mat[name_b3, name_b3]
  v13 <- vcov_mat[name_b1, name_b3]
  
  me_log <- b1 + d_grid * b3
  se <- sqrt(v11 + (d_grid^2) * v33 + 2 * d_grid * v13)
  
  tibble(
    dist_km = d_grid,
    me_log  = me_log,
    lo_log  = me_log - 1.96 * se,
    hi_log  = me_log + 1.96 * se,
    me_pct  = exp(me_log) - 1,
    lo_pct  = exp(lo_log) - 1,
    hi_pct  = exp(hi_log) - 1
  )
}

# Build a distance grid within the observed range (trim extreme outliers)
d_max <- as.numeric(quantile(df_model$x_dist_km, 0.99, na.rm = TRUE))
d_grid <- seq(0, d_max, length.out = 60)

vc_main <- vcov_cluster_school(model_main_fe)
me_table <- get_marginal_effect_social(model_main_fe, d_grid, vc_main)

p_me <- ggplot(me_table, aes(x = dist_km, y = me_pct)) +
  geom_ribbon(aes(ymin = lo_pct, ymax = hi_pct), alpha = 0.2) +
  geom_line() +
  labs(
    title = "Marginal effect of school social index by distance (Main FE model)",
    subtitle = "Percent change in €/sqm for +1 social index; 95% CI; clustered at school level",
    x = "Distance to nearest school (km)",
    y = "Percent effect on €/sqm (exp(ME) - 1)"
  )

print(p_me)

# Export key outputs
ggsave("plot_marginal_effect_social_by_distance.png", p_me,
      width = 8, height = 5, dpi = 200)
cat("\n[Done] Models estimated and outputs generated.\n")
