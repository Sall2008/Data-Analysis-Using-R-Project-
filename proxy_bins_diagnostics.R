## ==== 1. DATA PREPARATION ====
### ==== 1.1 Packages & Global Theme ====

library(tidyverse)
library(readxl)
library(fs)
library(scales)
library(janitor)
library(modelsummary)
library(spdep)
library(RANN)
library(ggplot2)
library(gt)
library(lmtest)
library(sandwich)
library(car)

theme_set(
  theme_minimal(base_size = 18) +
    theme(
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(color = "grey35"),
      axis.title         = element_text(face = "bold"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank()
    )
)

### ==== 1.2 Define Paths & Constants ====

path_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_school  <- "course_data/school_data/2022_social_index.csv"
path_dist    <- "course_data/school_data/distance_to_schools.csv"

year_ref <- 2022

type_primary   <- c("02")
type_secondary <- c("04", "10", "14", "15", "20")
type_all       <- c("02", "04", "10", "14", "15", "20")

# Distance binning (used in descriptive plots AND optionally in regressions)
breaks_km <- c(0, 3, 6, 9, Inf)
labels_km <- c("0-3", "3-6", "6-9", ">9")

### ==== 1.3 Load & Clean Housing Data ====

raw_housing <- read_delim(
  path_housing,
  delim = ",",
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", 
         "Implausible value", "Other missing"),
  show_col_types = FALSE
)

df_housing <- raw_housing %>%
  filter(blid == "North Rhine-Westphalia") %>%
  filter(kategorie_Haus %in% c(
    "Single-family house (detached)",
    "Semi-detached house",
    "Terraced house (end unit)",
    "Terraced house (middle unit)",
    "Bungalow",
    "Mansion",
    "Farmhouse"
  )) %>%
  mutate(
    adat_num = as.numeric(str_replace(adat, "m", "")),
    ergg_1km = as.character(ergg_1km)
  ) %>%
  group_by(ergg_1km) %>%
  slice_max(order_by = kaufpreis, n = 1, with_ties = FALSE) %>%  
  ungroup() %>%
  mutate(across(
    c(kaufpreis, wohnflaeche, grundstuecksflaeche, zimmeranzahl, baujahr),
    ~ parse_number(as.character(.x))
  )) %>%
  mutate(
    kaufpreis           = 
      if_else(kaufpreis > 5e7 | kaufpreis <= 0, NA_real_, kaufpreis),
    wohnflaeche         = 
      if_else(wohnflaeche > 10000 | wohnflaeche <= 0, NA_real_, wohnflaeche),
    grundstuecksflaeche = 
      if_else(grundstuecksflaeche > 5000, NA_real_, grundstuecksflaeche),
    zimmeranzahl        = 
      if_else(zimmeranzahl > 25, NA_real_, zimmeranzahl),
    baujahr             = 
      if_else(baujahr < 1000 | baujahr > year_ref, NA_real_, baujahr),
    house_age           = year_ref - baujahr,
    log_price           = log(kaufpreis),
    log_area            = log(wohnflaeche),
    log_plot_area       = log(grundstuecksflaeche + 1)
  ) %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km)

### ==== 1.4 Load Distance Data & Nearest-School Helper ====

raw_dist <- read_csv(path_dist, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    school_type = str_pad(as.character(school_type), 2, pad = "0"),
    school_id   = as.character(school_id),
    ergg_1km    = as.character(ergg_1km),
    dist_km     = parse_number(as.character(dist))
  )

get_nearest_school <- function(dist_df, types, id_name, dist_name) {
  dist_df %>%
    filter(school_type %in% types) %>%
    group_by(ergg_1km) %>%
    slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      ergg_1km,
      !!id_name   := school_id,
      !!dist_name := dist_km
    )
}

df_dist_primary   <- get_nearest_school(raw_dist, type_primary,   
                                        "school_id_primary",   
                                        "dist_primary_km")
df_dist_secondary <- get_nearest_school(raw_dist, type_secondary, 
                                        "school_id_secondary", 
                                        "dist_secondary_km")
df_dist_any       <- get_nearest_school(raw_dist, type_all,       
                                        "school_id_any",       
                                        "dist_any_km")

### ==== 1.5 Merge Housing + Distances + Derived Variables ====

df_main <- df_housing %>%
  left_join(df_dist_primary,   by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any,       by = "ergg_1km") %>%
  mutate(
    price_per_sqm = kaufpreis / wohnflaeche,
    log_ppsqm     = log(price_per_sqm),
    
    dist_primary_bin = cut(
      dist_primary_km,
      breaks = breaks_km,
      labels = labels_km,
      include.lowest = TRUE,
      right = TRUE
    ),
    dist_secondary_bin = cut(
      dist_secondary_km,
      breaks = breaks_km,
      labels = labels_km,
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  mutate(
    dist_primary_bin   = factor(dist_primary_bin, levels = labels_km),
    dist_secondary_bin = factor(dist_secondary_bin, levels = labels_km)
  )

cat("Final dataset dimensions:", dim(df_main), "\n")

## ==== 2. PROXY - REGRESSIONS ====

### ==== 2.1 Regression Sample ====

df_reg <- df_main %>%
  drop_na(
    log_price,
    dist_primary_km, dist_secondary_km,
    dist_primary_bin, dist_secondary_bin,
    log_area, log_plot_area,
    zimmeranzahl, house_age
  )

### ==== 2.2 OLS Models (Separate Narratives) ====

# --- Continuous distance specifications ---
m1_naive_both_cont <- lm(log_price ~ dist_primary_km + dist_secondary_km, 
                         data = df_reg)

m2_base_both_cont <- lm(
  log_price ~ dist_primary_km + dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

m3_poly_both_cont <- lm(
  log_price ~ dist_primary_km + I(dist_primary_km^2) +
    dist_secondary_km + I(dist_secondary_km^2) +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

m4_base_primary_cont <- lm(
  log_price ~ dist_primary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

m5_base_secondary_cont <- lm(
  log_price ~ dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

# --- Binned distance specifications (factor dummies; baseline = 0-3 km) ---
m6_naive_both_bin <- lm(log_price ~ dist_primary_bin + dist_secondary_bin, 
                        data = df_reg)

m7_base_both_bin <- lm(
  log_price ~ dist_primary_bin + dist_secondary_bin +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

m8_base_primary_bin <- lm(
  log_price ~ dist_primary_bin +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

m9_base_secondary_bin <- lm(
  log_price ~ dist_secondary_bin +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg
)

### ==== 2.3 Output Tables (Separate + Main Model Comparison) ====

# Coefficient labels for continuous terms and controls only
coef_map_cont <- c(
  "dist_primary_km"        = "Distance to primary school (km)",
  "I(dist_primary_km^2)"   = "Distance to primary school (km)^2",
  "dist_secondary_km"      = "Distance to secondary school (km)",
  "I(dist_secondary_km^2)" = "Distance to secondary school (km)^2",
  "log_area"               = "Log living area",
  "log_plot_area"          = "Log plot area",
  "zimmeranzahl"           = "Rooms",
  "house_age"              = "House age",
  "(Intercept)"            = "Intercept"
)

# --- Table 1: Continuous specifications only ---
models_cont <- list(
  "Naive (Both)"        = m1_naive_both_cont,
  "Baseline (Both)"     = m2_base_both_cont,
  "Polynomial (Both)"   = m3_poly_both_cont,
  "Baseline (Primary)"  = m4_base_primary_cont,
  "Baseline (Secondary)"= m5_base_secondary_cont
)

tab_ols_cont <- modelsummary(
  models_cont,
  vcov      = "HC1",
  coef_map  = coef_map_cont,
  statistic = "(p: {p.value})",
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  fmt       = 3,
  output    = "gt",
  title     = "Table 1. OLS regressions (log house price): 
  Continuous distance specifications"
) %>%
  gt::tab_options(
    table.font.size   = gt::px(14),
    data_row.padding  = gt::px(4)
  )

tab_ols_cont

# --- Table 2: Binned specifications only ---
models_bin <- list(
  "Naive (Both)"         = m6_naive_both_bin,
  "Baseline (Both)"      = m7_base_both_bin,
  "Baseline (Primary)"   = m8_base_primary_bin,
  "Baseline (Secondary)" = m9_base_secondary_bin
)

tab_ols_bin <- modelsummary(
  models_bin,
  vcov      = "HC1",
  statistic = NULL,
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  fmt       = 3,
  output    = "gt",
  title     = "Table 2. OLS regressions (log house price): 
  Binned distance specifications (ref: 0–3 km)"
) %>%
  gt::tab_options(
    table.font.size   = gt::px(14),
    data_row.padding  = gt::px(4)
  )

tab_ols_bin

# --- Table 3 (Distance-only focus): Baseline continuous vs baseline binned ---

models_main_compare <- list(
  "Baseline (Continuous)" = m2_base_both_cont,
  "Baseline (Binned)"     = m7_base_both_bin
)

coef_map_bin <- c(
  # Primary bins (ref = 0-3)
  "dist_primary_bin3-6" = "Primary: 3–6 km (ref: 0–3)",
  "dist_primary_bin6-9" = "Primary: 6–9 km (ref: 0–3)",
  "dist_primary_bin>9"  = "Primary: >9 km (ref: 0–3)",
  
  # Secondary bins (ref = 0-3)
  "dist_secondary_bin3-6" = "Secondary: 3–6 km (ref: 0–3)",
  "dist_secondary_bin6-9" = "Secondary: 6–9 km (ref: 0–3)",
  "dist_secondary_bin>9"  = "Secondary: >9 km (ref: 0–3)"
)

coef_map_compare <- c(
  # continuous terms
  "dist_primary_km"   = "Distance to primary school (km)",
  "dist_secondary_km" = "Distance to secondary school (km)",
  
  # binned dummies
  coef_map_bin
)

tab_main_compare <- modelsummary(
  models_main_compare,
  vcov      = "HC1",
  coef_map  = coef_map_compare,
  coef_omit = "log_area|log_plot_area|zimmeranzahl|house_age|\\(Intercept\\)",
  statistic = NULL,
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  fmt       = 3,
  output    = "gt",
  title     = 
    "Table 3. Main comparison (distance effects only): Continuous vs binned"
) %>%
  gt::tab_options(
    table.font.size  = gt::px(14),
    data_row.padding = gt::px(4)
  ) %>%
  gt::tab_source_note(
    gt::md("Binned coefficients are relative to the reference group: 
           0–3 km. Controls are included in the regression but omitted 
           from display.")
  )

tab_main_compare


### ==== 2.4 Model comparison (In-sample + 5-fold CV) ====

# In-sample fit
fit_main <- tibble(
  Model   = c("Baseline (Continuous)", "Baseline (Binned)"),
  N       = c(nobs(m2_base_both_cont), nobs(m7_base_both_bin)),
  R2      = c(summary(m2_base_both_cont)$r.squared, 
              summary(m7_base_both_bin)$r.squared),
  Adj_R2  = c(summary(m2_base_both_cont)$adj.r.squared, 
              summary(m7_base_both_bin)$adj.r.squared),
  AIC     = c(AIC(m2_base_both_cont), AIC(m7_base_both_bin)),
  BIC     = c(BIC(m2_base_both_cont), BIC(m7_base_both_bin))
) %>%
  mutate(
    across(c(R2, Adj_R2), ~ round(.x, 3)),
    across(c(AIC, BIC),   ~ round(.x, 1))
  )

# 5-fold CV helper
set.seed(2025)
k <- 5
fold_id <- sample(rep(1:k, length.out = nrow(df_reg)))

cv_one_model <- function(formula, data, fold_id) {
  k <- max(fold_id)
  rmse <- mae <- r2 <- rep(NA_real_, k)
  
  for (f in 1:k) {
    train <- data[fold_id != f, , drop = FALSE]
    test  <- data[fold_id == f, , drop = FALSE]
    
    mod  <- lm(formula, data = train)
    yhat <- predict(mod, newdata = test)
    y    <- test$log_price
    
    rmse[f] <- sqrt(mean((y - yhat)^2, na.rm = TRUE))
    mae[f]  <- mean(abs(y - yhat), na.rm = TRUE)
    r2[f]   <- 1 - sum((y - yhat)^2, na.rm = TRUE) /
      sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  }
  
  tibble(
    RMSE  = mean(rmse, na.rm = TRUE),
    MAE   = mean(mae,  na.rm = TRUE),
    R2_oos = mean(r2,  na.rm = TRUE)
  )
}

cv_main <- bind_rows(
  cv_one_model(
    log_price ~ dist_primary_km + dist_secondary_km +
      log_area + log_plot_area + zimmeranzahl + house_age,
    data = df_reg, fold_id = fold_id
  ) %>% mutate(Model = "Baseline (Continuous)"),
  
  cv_one_model(
    log_price ~ dist_primary_bin + dist_secondary_bin +
      log_area + log_plot_area + zimmeranzahl + house_age,
    data = df_reg, fold_id = fold_id
  ) %>% mutate(Model = "Baseline (Binned)")
) %>%
  select(Model, everything()) %>%
  mutate(
    across(c(RMSE, MAE), ~ round(.x, 3)),
    R2_oos = round(R2_oos, 3)
  )

# Combined comparison table
compare_all <- fit_main %>%
  left_join(cv_main, by = "Model") %>%
  select(Model, N, R2, Adj_R2, AIC, BIC, RMSE, MAE, R2_oos)

tab_compare_all <- compare_all %>%
  gt() %>%
  tab_header(
    title    = 
      "Model comparison: In-sample fit and 5-fold out-of-sample performance",
    subtitle = 
      "In-sample: higher R²/Adj. R² is better; lower AIC/BIC is better. 
    Out-of-sample: lower RMSE/MAE is better; higher R² is better."
  ) %>%
  cols_label(
    Model  = "Model",
    N      = "N",
    R2     = "R²",
    Adj_R2 = "Adj. R²",
    AIC    = "AIC",
    BIC    = "BIC",
    RMSE   = "CV RMSE",
    MAE    = "CV MAE",
    R2_oos = "CV R²"
  ) %>%
  tab_options(
    table.font.size  = gt::px(14),
    data_row.padding = gt::px(4)
  )

tab_compare_all


## ==== 3. PROXY - DIAGNOSTICS ====

get_model_diagnostics <- function(model) {
  n <- nobs(model)
  
  bp_p <- bptest(model)$p.value
  
  vif_max <- tryCatch(
    max(as.numeric(car::vif(model)), na.rm = TRUE),
    error = function(e) NA_real_
  )
  
  cook <- cooks.distance(model)
  cook_thr <- 4 / n
  
  tibble(
    BP_pvalue    = bp_p,
    Max_VIF      = vif_max,
    Cook_n_gt_4n = sum(cook > cook_thr, na.rm = TRUE),
    Cook_max     = max(cook, na.rm = TRUE)
  )
}

diag_robust <- bind_rows(
  get_model_diagnostics(m2_base_both_cont) %>% 
    mutate(Model = "Baseline (Continuous)"),
  get_model_diagnostics(m7_base_both_bin)  %>% 
    mutate(Model = "Baseline (Binned)")
) %>%
  select(Model, everything()) %>%
  mutate(
    BP_pvalue = signif(BP_pvalue, 3),
    Max_VIF   = round(Max_VIF, 2),
    Cook_max  = signif(Cook_max, 3)
  )

tab_robust_checks <- diag_robust %>%
  gt() %>%
  tab_header(
    title = "Robustness checks (main models)",
    subtitle = "HC1 robust SE used for inference; 
    diagnostics shown for heteroskedasticity, 
    multicollinearity, and influential points."
  ) %>%
  cols_label(
    Model        = "Model",
    BP_pvalue    = "Breusch–Pagan p",
    Max_VIF      = "Max VIF",
    Cook_n_gt_4n = "# Influential (Cook's D > 4/n)",
    Cook_max     = "Max Cook's D"
  ) %>%
  tab_options(
    table.font.size  = gt::px(14),
    data_row.padding = gt::px(4)
  )

tab_robust_checks
## ==== 4. PROXY - VISUALISATIONS ====

### ==== 4.1 Binned Price Gradients (Mean by distance bins) ====
pal_muted <- c(
  "Primary"   = "#4C78A8",
  "Secondary" = "#F58518"
)

df_bins <- df_main %>%
  transmute(
    log_ppsqm,
    primary_bin   = cut(dist_primary_km,   
                        breaks = breaks_km, 
                        labels = labels_km, 
                        include.lowest = TRUE),
    secondary_bin = cut(dist_secondary_km, 
                        breaks = breaks_km, 
                        labels = labels_km, 
                        include.lowest = TRUE)
  ) %>%
  pivot_longer(
    cols = c(primary_bin, secondary_bin),
    names_to = "school_level",
    values_to = "dist_bin"
  ) %>%
  mutate(
    school_level = case_when(
      school_level == "primary_bin"   ~ "Primary",
      school_level == "secondary_bin" ~ "Secondary",
      TRUE ~ school_level
    ),
    dist_bin = factor(dist_bin, levels = labels_km)
  ) %>%
  filter(is.finite(log_ppsqm), !is.na(dist_bin))

df_bin_mean <- df_bins %>%
  group_by(school_level, dist_bin) %>%
  summarise(
    mean_log_ppsqm = mean(log_ppsqm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

fig_price_gradient <- ggplot(
  df_bin_mean,
  aes(x = dist_bin, y = mean_log_ppsqm, 
      color = school_level, group = school_level)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_muted) +
  labs(
    x = "Distance bin (km) — wider bins mitigate centroid measurement error",
    y = "Mean log(price per sqm)",
    color = NULL
  ) +
  theme(legend.position = "top")

print(fig_price_gradient)


### ==== 4.2 Continuous Distance vs Log Price per sqm ====
df_continuous <- df_main %>%
  transmute(
    log_ppsqm,
    Primary   = dist_primary_km,
    Secondary = dist_secondary_km
  ) %>%
  pivot_longer(
    cols = c("Primary", "Secondary"),
    names_to = "school_level",
    values_to = "dist_km"
  ) %>%
  filter(is.finite(log_ppsqm), is.finite(dist_km))

fig_scatter_combined <- ggplot(
  df_continuous,
  aes(x = dist_km, y = log_ppsqm, color = school_level)
) +
  geom_point(alpha = 0.10) +
  geom_smooth(se = TRUE) +
  scale_color_manual(values = pal_muted) +
  labs(
    x = "Distance to nearest school (km)",
    y = "Log(price per sqm)",
    title = 
      "Continuous relationship: prices vs distance (Primary vs Secondary)",
    color = NULL
  ) +
  theme(legend.position = "top")

print(fig_scatter_combined)
