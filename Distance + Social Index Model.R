# ==== 1. DATA PREPARATION =====

## ==== 1.1 Setup ====
library(tidyverse)
library(readxl)
library(fs)
library(scales)
library(janitor)
library(modelsummary)

# Set global theme
theme_set(theme_minimal(base_size = 14))

## ==== 1.2 Define Paths ====
path_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_school  <- "course_data/school_data/2022_social_index.csv"
path_dist    <- "course_data/school_data/distance_to_schools.csv"

## ==== 1.3 Clean Housing Data (HK - Houses for Sale) ====
raw_housing <- read_delim(
  path_housing, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

df_housing_clean <- raw_housing %>%
  # Filter for North Rhine-Westphalia (NRW) and Single-family houses
  filter(blid == "North Rhine-Westphalia") %>%
  filter(kategorie_Haus %in% c("Single-family house (detached)", 
                               "Semi-detached house",
                               "Terraced house (end unit)",
                               "Terraced house (middle unit)",
                               "Bungalow",                    
                               "Mansion",                     
                               "Farmhouse")) %>%
  
  # Remove Duplicates
  mutate(
    # Convert '2022m1' to numeric 20221 for sorting
    adat_num = as.numeric(str_replace(adat, "m", ""))
  ) %>%
  group_by(duplicateid) %>%
  slice_max(order_by = adat_num, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  
  # Parse numeric columns
  mutate(across(c(kaufpreis, wohnflaeche, grundstuecksflaeche, zimmeranzahl, baujahr),
                ~ parse_number(as.character(.x)))) %>%
  
  # Censor outliers based on RWI documentation
  mutate(
    kaufpreis = if_else(kaufpreis > 5e7 | kaufpreis <= 0, NA_real_, kaufpreis),
    wohnflaeche = if_else(wohnflaeche > 10000 | wohnflaeche <= 0, NA_real_, wohnflaeche),
    grundstuecksflaeche = if_else(grundstuecksflaeche > 5000, NA_real_, grundstuecksflaeche),
    zimmeranzahl = if_else(zimmeranzahl > 25, NA_real_, zimmeranzahl),
    baujahr = if_else(baujahr < 1000 | baujahr > 2022, NA_real_, baujahr)
  ) %>%
  
  # Feature Engineering
  mutate(
    house_age = 2022 - baujahr,
    log_price = log(kaufpreis),
    log_area  = log(wohnflaeche),
    # Log plot area (add 1 to avoid log(0))
    log_plot_area = log(grundstuecksflaeche + 1),
    ergg_1km = as.character(ergg_1km)
  ) %>%
  
  # Drop critical missing values
  drop_na(kaufpreis, wohnflaeche, ergg_1km)

## ==== 1.4 Clean School & Distance Data ====

# Define types: 02=Primary; 04,10,15,20=Secondary
type_primary   <- c("02") 
type_secondary <- c("04", "10", "15", "20")
type_all       <- c("02", "04", "10", "15", "20")

# Load Social Index (linked to Primary Schools)
df_school_meta <- read_delim(path_school, 
                             delim = ";",
                             locale = readr::locale(
                               encoding = "Latin1", 
                               decimal_mark = ","),
                             show_col_types = FALSE) %>%
  clean_names() %>%
  rename(school_id = schulnummer, social_index = sozialindexstufe) %>%
  mutate(
    school_id = as.character(school_id),
    social_index = parse_number(as.character(social_index))
  ) %>%
  distinct(school_id, .keep_all = TRUE)

# Load Distances
raw_dist <- read_csv(path_dist, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    school_type = str_pad(as.character(school_type), 2, pad = "0"),
    school_id = as.character(school_id),
    ergg_1km = as.character(ergg_1km),
    dist_km = parse_number(as.character(dist))
  )

### ==== 1.4.1 Process PRIMARY Schools (Core Variable) ====
df_dist_primary <- raw_dist %>%
  filter(school_type %in% type_primary) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_primary = school_id, dist_primary_km = dist_km)

### ==== 1.4.2 Process SECONDARY Schools (Corrected) ====
df_dist_secondary <- raw_dist %>%
  filter(school_type %in% type_secondary) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, dist_secondary_km = dist_km)

### ==== 1.4.3 Process ANY School (Comparison Group 2) ====
df_dist_any <- raw_dist %>%
  filter(school_type %in% type_all) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, dist_any_km = dist_km)

## ==== 1.5 Merging ====

df_final <- df_housing_clean %>%
  # Inner join Primary (must have primary school data)
  inner_join(df_dist_primary, by = "ergg_1km") %>%
  # Left join others (don't drop data if secondary school is missing)
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  # Join Social Index via Primary School ID
  left_join(df_school_meta, by = c("school_id_primary" = "school_id")) %>%
  
  # Create Analysis Bins/Factors
  mutate(
    dist_primary_bin = cut(dist_primary_km, 
                           breaks = c(0, 2, 3, 4, 5, Inf),
                           include.lowest = TRUE, right = TRUE,
                           labels = c("0-2km", "2-3km", "3-4km", "4-5km", ">5km")),
    
    social_bin = factor(case_when(
      social_index <= 2 ~ "High Status (1-2)",
      social_index <= 5 ~ "Medium (3-5)",
      social_index <= 9 ~ "Low Status (6-9)"
    ), levels = c("High Status (1-2)", "Medium (3-5)", "Low Status (6-9)")),
    
    # Center Social Index for interaction interpretation
    social_index_centered = social_index - mean(social_index, na.rm = TRUE)
  )

# Final check
cat("Final Dataset Dimensions:", dim(df_final), "\n")

# ==== 2. REGRESSION ANALYSIS (Hedonic Pricing Model) ====


## ==== Model 1: Naive Model (Just Distance) ====
# Does distance explain price on its own?
m1_naive <- lm(log_price ~ dist_primary_km, data = df_final)

## ==== Model 2: Base Model (Controls included) ====
# Interpreting distance holding house characteristics constant
m2_base <- lm(log_price ~ dist_primary_km + log_area + log_plot_area + 
                zimmeranzahl + house_age, data = df_final)

## ==== Model 3: Non-Linear Distance (Quadratic) ====
# Maybe being TOO close is bad (noise), but TOO far is also bad?
m3_poly <- lm(log_price ~ dist_primary_km + I(dist_primary_km^2) + 
                log_area + log_plot_area + zimmeranzahl + house_age, 
              data = df_final)

## ==== Model 4: Adding Secondary School Distance ====
# Checking if secondary schools matter more or less than primary
m4_multi_dist <- lm(log_price ~ dist_primary_km + dist_secondary_km + 
                      log_area + log_plot_area + zimmeranzahl + house_age, 
                    data = df_final)

# ==== 3. OUTPUT RESULTS ====

## ==== 3.1 Configuration ====
# Rename variables to clear English labels
cm <- c(
  "(Intercept)"          = "Constant",
  "dist_primary_km"      = "Distance to Primary (km)",
  "I(dist_primary_km^2)" = "Distance Squared",
  "dist_secondary_km"    = "Distance to Secondary (km)",
  "log_area"             = "Ln(Living Area)",
  "log_plot_area"        = "Ln(Plot Area)",
  "zimmeranzahl"         = "Number of Rooms",
  "house_age"            = "House Age (Years)"
)

# Define Goodness-of-Fit statistics
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R²", "fmt" = 3)
)

## ==== 3.2 Generate Table ====
modelsummary(
  list(
    "(1) Naive"        = m1_naive, 
    "(2) Controls"     = m2_base, 
    "(3) Non-Linear"   = m3_poly, 
    "(4) Multi-School" = m4_multi_dist
  ),
  coef_map = cm,       # Variable labels
  gof_map = gm,        # R2 and N
  stars = c('*' = .1, '**' = .05, '***' = .01), # Standard academic stars
  vcov = "robust",     # Uses Robust Standard Errors
  title = "Table 1: Hedonic Regression Results - Effect of Distance on Housing Prices",
  notes = list(
    "Notes: Dependent variable is the natural logarithm of housing price.",
    "Robust standard errors are shown in parentheses.",
    "Data source: RWI-GEO-RED 2022."
  ),
  output = "gt")
















# ============================================================
# ==== PART 2 (IMPROVED, 5km): Distance + Quality (Primary vs Secondary) ====
# ==== Builds on Part 1 objects:
#      df_final, raw_dist, df_school_meta, type_primary, type_secondary, type_all
# ==== Plus school_data objects you created:
#      df_school_lookup, df_social_with_type
# ============================================================

library(tidyverse)
library(modelsummary)


path_schooldata <- "course_data/school_data/school_data.xlsx"

# 1) Load school_data and create (school_id -> school_type) lookup
df_school_lookup <- read_excel(path_schooldata) %>%
  clean_names() %>%
  transmute(
    school_id = as.character(school_id),
    school_type_from_schooldata = str_pad(as.character(school_type), 2, pad = "0")
  ) %>%
  filter(!is.na(school_id), school_id != "") %>%
  distinct(school_id, .keep_all = TRUE)

# 2) Attach school_type to social index (from Part 1: df_school_meta)
df_social_with_type <- df_school_meta %>%
  mutate(
    school_id = as.character(school_id),
    social_index = parse_number(as.character(social_index)),
    social_index = if_else(social_index %in% 1:9, social_index, NA_real_)
  ) %>%
  left_join(df_school_lookup, by = "school_id")

# QC (optional)
cat("\n--- QC: df_social_with_type created ---\n")
df_social_with_type %>%
  summarise(
    n_rows = n(),
    share_missing_type = mean(is.na(school_type_from_schooldata)),
    share_missing_si = mean(is.na(social_index))
  ) %>%
  print()



# ==== 1) FIXED RADIUS (5 km): Availability (counts) ====

radius_km <- 5

df_counts_5km <- raw_dist %>%
  mutate(
    ergg_1km = as.character(ergg_1km),
    school_id = as.character(school_id),
    school_type = str_pad(as.character(school_type), 2, pad = "0"),
    dist_km = parse_number(as.character(dist))
  ) %>%
  filter(dist_km <= radius_km) %>%
  group_by(ergg_1km) %>%
  summarise(
    n_primary_5km   = n_distinct(school_id[school_type %in% type_primary]),
    n_secondary_5km = n_distinct(school_id[school_type %in% type_secondary]),
    n_all_5km       = n_distinct(school_id[school_type %in% type_all]),
    .groups = "drop"
  )

# ==== 2) FIXED RADIUS (5 km): Quality within radius (PRIMARY vs SECONDARY) ====
# We now CAN do this, because df_social_with_type links social_index to school_type.

df_quality_5km <- raw_dist %>%
  mutate(
    ergg_1km = as.character(ergg_1km),
    school_id = as.character(school_id),
    dist_km = parse_number(as.character(dist))
  ) %>%
  filter(dist_km <= radius_km) %>%
  left_join(
    df_social_with_type %>%
      select(school_id, social_index, school_type_from_schooldata),
    by = "school_id"
  ) %>%
  filter(social_index %in% 1:9) %>%
  mutate(
    school_type_from_schooldata = str_pad(as.character(school_type_from_schooldata), 2, pad = "0"),
    group = case_when(
      school_type_from_schooldata %in% type_primary ~ "primary",
      school_type_from_schooldata %in% type_secondary ~ "secondary",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  group_by(ergg_1km, group) %>%
  summarise(
    best_si_5km  = min(social_index),
    avg_si_5km   = mean(social_index),
    worst_si_5km = max(social_index),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = c(best_si_5km, avg_si_5km, worst_si_5km)
  ) %>%
  rename(
    best_primary_si_5km   = best_si_5km_primary,
    avg_primary_si_5km    = avg_si_5km_primary,
    worst_primary_si_5km  = worst_si_5km_primary,
    best_secondary_si_5km = best_si_5km_secondary,
    avg_secondary_si_5km  = avg_si_5km_secondary,
    worst_secondary_si_5km= worst_si_5km_secondary
  )

# ==== 3) MERGE radius features into df_final (from Part 1) ====

df_final_part2 <- df_final %>%
  left_join(df_counts_5km, by = "ergg_1km") %>%
  left_join(df_quality_5km, by = "ergg_1km")

# QC
cat("\n--- QC: Missingness after Part 2 merge (houses-level) ---\n")
df_final_part2 %>%
  summarise(
    n_total = n(),
    share_missing_primary_quality = mean(is.na(avg_primary_si_5km)),
    share_missing_secondary_quality = mean(is.na(avg_secondary_si_5km))
  ) %>%
  print()

# ==== 4) BUILD ANALYSIS SAMPLE (NO REGION CONTROLS) ====
# Avoid multicollinearity: do NOT use n_all_5km together with n_primary_5km + n_secondary_5km.

df_analysis_part2 <- df_final_part2 %>%
  drop_na(
    log_price,
    dist_primary_km, dist_secondary_km, dist_any_km,
    log_area, log_plot_area, zimmeranzahl, house_age,
    n_primary_5km, n_secondary_5km,
    avg_primary_si_5km, avg_secondary_si_5km
  ) %>%
  mutate(
    avg_primary_si_5km_c   = avg_primary_si_5km   - mean(avg_primary_si_5km, na.rm = TRUE),
    avg_secondary_si_5km_c = avg_secondary_si_5km - mean(avg_secondary_si_5km, na.rm = TRUE),
    best_primary_si_5km_c  = best_primary_si_5km  - mean(best_primary_si_5km, na.rm = TRUE),
    worst_primary_si_5km_c = worst_primary_si_5km - mean(worst_primary_si_5km, na.rm = TRUE),
    best_secondary_si_5km_c  = best_secondary_si_5km  - mean(best_secondary_si_5km, na.rm = TRUE),
    worst_secondary_si_5km_c = worst_secondary_si_5km - mean(worst_secondary_si_5km, na.rm = TRUE)
  )

cat("\n--- Part 2 analysis sample size (5km, improved) ---\n")
df_analysis_part2 %>%
  summarise(n_obs = n()) %>%
  print()

# ============================================================
# ==== 5) HEDONIC MODELS (NO REGION CONTROLS) =================
# ============================================================

# ---- 5.1 Baseline distances (symmetry with Part 1) ----
m5_primary_dist <- lm(
  log_price ~ dist_primary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

m6_secondary_dist <- lm(
  log_price ~ dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

m7_any_dist <- lm(
  log_price ~ dist_any_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

# ---- 5.2 Availability within 5 km ----
m8_availability <- lm(
  log_price ~ dist_primary_km +
    n_primary_5km + n_secondary_5km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

# ---- 5.3 Quality within 5 km: Primary vs Secondary (Avg SI) ----
m9_quality_avg_both <- lm(
  log_price ~ dist_primary_km +
    avg_primary_si_5km_c + avg_secondary_si_5km_c +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

# ---- 5.4 Quality checks: Best/Worst (Primary) ----
m10_quality_primary_best <- lm(
  log_price ~ dist_primary_km +
    best_primary_si_5km_c +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

m11_quality_primary_worst <- lm(
  log_price ~ dist_primary_km +
    worst_primary_si_5km_c +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

# ---- 5.5 Quality checks: Best/Worst (Secondary) ----
m12_quality_secondary_best <- lm(
  log_price ~ dist_secondary_km +
    best_secondary_si_5km_c +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

m13_quality_secondary_worst <- lm(
  log_price ~ dist_secondary_km +
    worst_secondary_si_5km_c +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_analysis_part2
)

# ============================================================
# ==== 6) OUTPUT TABLES =======================================
# ============================================================

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R²", "fmt" = 3)
)

# ---- 6.1 Distance premium (primary/secondary/any) ----
cm_dist <- c(
  "(Intercept)"       = "Constant",
  "dist_primary_km"   = "Distance to Primary (km)",
  "dist_secondary_km" = "Distance to Secondary (km)",
  "dist_any_km"       = "Distance to Any School (km)",
  "log_area"          = "Ln(Living Area)",
  "log_plot_area"     = "Ln(Plot Area)",
  "zimmeranzahl"      = "Number of Rooms",
  "house_age"         = "House Age (Years)"
)

modelsummary(
  list(
    "(5) Primary distance"   = m5_primary_dist,
    "(6) Secondary distance" = m6_secondary_dist,
    "(7) Any-school distance"= m7_any_dist
  ),
  coef_map = cm_dist,
  gof_map = gm,
  vcov = "robust",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  title = "Table 2A (Improved): Distance Premium — Primary vs Secondary vs Any School (No Region Controls)",
  notes = list(
    "Dependent variable is log housing price.",
    paste0("Fixed radius for quality/availability variables: ", radius_km, " km."),
    "Robust standard errors in parentheses."
  ),
  output = "gt"
)

# ---- 6.2 Availability + Quality (primary vs secondary) ----
cm_q <- c(
  "(Intercept)"            = "Constant",
  "dist_primary_km"        = "Distance to Primary (km)",
  "dist_secondary_km"      = "Distance to Secondary (km)",
  "n_primary_5km"          = "# Primary schools within 5 km",
  "n_secondary_5km"        = "# Secondary schools within 5 km",
  "avg_primary_si_5km_c"   = "Avg Primary SI within 5 km (centered)",
  "avg_secondary_si_5km_c" = "Avg Secondary SI within 5 km (centered)",
  "best_primary_si_5km_c"  = "Best Primary SI within 5 km (centered)",
  "worst_primary_si_5km_c" = "Worst Primary SI within 5 km (centered)",
  "best_secondary_si_5km_c"  = "Best Secondary SI within 5 km (centered)",
  "worst_secondary_si_5km_c" = "Worst Secondary SI within 5 km (centered)",
  "log_area"               = "Ln(Living Area)",
  "log_plot_area"          = "Ln(Plot Area)",
  "zimmeranzahl"           = "Number of Rooms",
  "house_age"              = "House Age (Years)"
)

modelsummary(
  list(
    "(8) + Availability"          = m8_availability,
    "(9) + Quality (Avg: both)"   = m9_quality_avg_both,
    "(10) Primary: Best SI"       = m10_quality_primary_best,
    "(11) Primary: Worst SI"      = m11_quality_primary_worst,
    "(12) Secondary: Best SI"     = m12_quality_secondary_best,
    "(13) Secondary: Worst SI"    = m13_quality_secondary_worst
  ),
  coef_map = cm_q,
  gof_map = gm,
  vcov = "robust",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  title = "Table 2B (Improved): Fixed Radius (5 km) — Availability & Quality (Primary vs Secondary, No Region Controls)",
  notes = list(
    "Dependent variable is log housing price.",
    "School quality uses social index (lower = better).",
    "Counts include primary and secondary schools; total count omitted in regressions to avoid multicollinearity.",
    "Quality metrics computed separately for primary vs secondary schools using school_data-based types.",
    "Robust standard errors in parentheses."
  ),
  output = "gt"
)





























