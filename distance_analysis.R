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

