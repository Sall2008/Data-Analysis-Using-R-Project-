# ==== 1. Libraries ====
library(dplyr)
library(ggplot2)
library(patchwork)
library(modelsummary)
library(sandwich)
library(lmtest)

# ==== 2. Build analysis dataset ====
df_si <- df_analysis %>%
  mutate(
    fe_location = factor(gid2019),
    cl_school   = school_ID_nearest,
    
    # Social index bins for interpretability (1–9 assumed)
    social_bin = case_when(
      social_index %in% 1:2 ~ "1-2",
      social_index %in% 3:4 ~ "3-4",
      social_index %in% 5:6 ~ "5-6",
      social_index %in% 7:9 ~ "7-9",
      TRUE ~ NA_character_
    ),
    social_bin = factor(social_bin, levels = c("1-2", "3-4", "5-6", "7-9"), ordered = TRUE),
    
    # Distance bins (same as your distance analysis for consistency)
    dist_bin = case_when(
      dist_nearest_km <= 2 ~ "0-2km",
      dist_nearest_km <= 3 ~ "2-3km",
      dist_nearest_km <= 4 ~ "3-4km",
      TRUE ~ "4km+"
    ),
    dist_bin = factor(dist_bin, levels = c("0-2km", "2-3km", "3-4km", "4km+"))
  ) %>%
  filter(
    !is.na(log_ppsqm),
    !is.na(social_index),
    !is.na(dist_nearest_km),
    !is.na(wohnflaeche),
    !is.na(house_age),
    !is.na(renovated),
    !is.na(fe_location),
    !is.na(cl_school)
  )

cat("\n[Info] Social-index modeling sample size:", nrow(df_si), "\n")

# ==== 3. Descriptive plots (non-causal) ====
p_scatter_si <- ggplot(df_si, aes(x = social_index, y = log_ppsqm)) +
  geom_point(alpha = 0.12) +
  geom_smooth(se = TRUE) +
  labs(
    title = "Log price per sqm vs school social index",
    subtitle = "Descriptive relationship (no controls)",
    x = "School social index",
    y = "Log(€/sqm)"
  )

p_box_si <- ggplot(df_si %>% filter(!is.na(social_bin)),
                   aes(x = social_bin, y = log_ppsqm)) +
  geom_boxplot(outlier.alpha = 0.15) +
  labs(
    title = "Log price per sqm by social index bins",
    subtitle = "Descriptive differences across bins",
    x = "Social index bin",
    y = "Log(€/sqm)"
  )

(p_scatter_si | p_box_si)

# ==== 4. Estimate models ====

# (1) Main effect: social index only (core association)
m_si_1 <- lm(
  log_ppsqm ~ social_index + wohnflaeche + house_age + renovated + fe_location,
  data = df_si
)

# (2) Add distance as a control (separates social from proximity)
m_si_2 <- lm(
  log_ppsqm ~ social_index + dist_nearest_km + wohnflaeche + house_age + renovated + fe_location,
  data = df_si
)

# (3) Mechanism: social effect decays (or changes) with distance
m_si_3 <- lm(
  log_ppsqm ~ social_index * dist_nearest_km + wohnflaeche + house_age + renovated + fe_location,
  data = df_si
)

# (4) Non-linearity: social bins (interpretable group effects)
m_si_4 <- lm(
  log_ppsqm ~ social_bin + wohnflaeche + house_age + renovated + fe_location,
  data = df_si %>% filter(!is.na(social_bin))
)

# (5) Robust heterogeneity: social bins × distance bins
m_si_5 <- lm(
  log_ppsqm ~ social_bin * dist_bin + wohnflaeche + house_age + renovated + fe_location,
  data = df_si %>% filter(!is.na(social_bin))
)

# ==== 5. Cluster-robust vcov (school-level) ====
vcov_cluster <- function(model_object, data_used) {
  sandwich::vcovCL(model_object, cluster = data_used$cl_school, type = "HC1")
}

vcov_list <- list(
  vcov_cluster(m_si_1, df_si),
  vcov_cluster(m_si_2, df_si),
  vcov_cluster(m_si_3, df_si),
  vcov_cluster(m_si_4, df_si %>% filter(!is.na(social_bin))),
  vcov_cluster(m_si_5, df_si %>% filter(!is.na(social_bin)))
)

# Interaction model
cat("\n=== Interaction model (m_si_3) with school-clustered SE ===\n")
print(lmtest::coeftest(m_si_3, vcov. = vcov_cluster(m_si_3, df_si)))

# ==== 6. Paper-style regression table ====
models_paper <- list(
  "(1) Social index"                  = m_si_1,
  "(2) + Distance control"            = m_si_2,
  "(3) Social × Distance"             = m_si_3,
  "(4) Social bins"                   = m_si_4,
  "(5) Social bins × Distance bins"   = m_si_5
)

coef_map <- c(
  "social_index" = "School social index",
  "dist_nearest_km" = "Distance to nearest school (km)",
  "social_index:dist_nearest_km" = "Social index × Distance",
  
  "social_bin3-4" = "Social index: 3–4 (vs 1–2)",
  "social_bin5-6" = "Social index: 5–6 (vs 1–2)",
  "social_bin7-9" = "Social index: 7–9 (vs 1–2)",
  
  "dist_bin2-3km" = "Distance: 2–3 km (vs 0–2)",
  "dist_bin3-4km" = "Distance: 3–4 km (vs 0–2)",
  "dist_bin4km+"  = "Distance: 4 km+ (vs 0–2)",
  
  "social_bin3-4:dist_bin2-3km" = "3–4 × 2–3 km",
  "social_bin5-6:dist_bin2-3km" = "5–6 × 2–3 km",
  "social_bin7-9:dist_bin2-3km" = "7–9 × 2–3 km",
  "social_bin3-4:dist_bin3-4km" = "3–4 × 3–4 km",
  "social_bin5-6:dist_bin3-4km" = "5–6 × 3–4 km",
  "social_bin7-9:dist_bin3-4km" = "7–9 × 3–4 km",
  "social_bin3-4:dist_bin4km+"  = "3–4 × 4 km+",
  "social_bin5-6:dist_bin4km+"  = "5–6 × 4 km+",
  "social_bin7-9:dist_bin4km+"  = "7–9 × 4 km+",
  
  "wohnflaeche" = "Living area (sqm)",
  "house_age"   = "House age (years)",
  "renovated"   = "Renovated"
)

tab_social <- modelsummary(
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

tab_social

# ==== 7. Mechanism plot: marginal effect of social index by distance ====
get_me_social <- function(model_object, V, d_grid) {
  b <- coef(model_object)
  n1 <- "social_index"
  n3 <- "social_index:dist_nearest_km"
  if (!(n1 %in% names(b)) || !(n3 %in% names(b))) {
    stop("Model does not contain social_index interaction terms.")
  }
  
  b1 <- b[n1]
  b3 <- b[n3]
  v11 <- V[n1, n1]
  v33 <- V[n3, n3]
  v13 <- V[n1, n3]
  
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

V3 <- vcov_list[[3]]
d_max <- as.numeric(quantile(df_si$dist_nearest_km, 0.99, na.rm = TRUE))
d_grid <- seq(0, d_max, length.out = 60)

me_tab <- get_me_social(m_si_3, V3, d_grid)

p_me <- ggplot(me_tab, aes(x = dist_km, y = me_pct)) +
  geom_ribbon(aes(ymin = lo_pct, ymax = hi_pct), alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Marginal effect of social index by distance",
    subtitle = "Percent change in €/sqm for +1 social index (95% CI), clustered by school",
    x = "Distance to nearest school (km)",
    y = "Percent effect (exp(ME) - 1)"
  )

print(p_me)

cat("\n[Done] Social index analysis completed.\n")
