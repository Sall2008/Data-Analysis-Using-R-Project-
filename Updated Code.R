## ==== 1. DATA PREPARATION =====

## ==== 1.1 Load Required Packages ====
library(tidyverse)
library(readxl)
library(fs)
library(scales)
library(janitor)
library(modelsummary)
library(readr)

# Set global plotting theme
theme_set(theme_minimal(base_size = 14))

## ==== 1.2 Define Data Source Paths ====

# Define file paths for the CSV data
path_housing <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/CampusFile_HK_2022.csv"
path_school  <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/2022_social_index.csv"
path_dist    <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/distance_to_schools.csv"

## ==== 1.3 Prepare Housing Data (HK - Houses for Sale) ====
raw_housing <- read_delim(
  path_housing, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

df_housing_clean <- raw_housing %>%
  # Filter for North Rhine-Westphalia (NRW) and single-family-type houses
  filter(blid == "North Rhine-Westphalia") %>%
  filter(kategorie_Haus %in% c("Single-family house (detached)", 
                               "Semi-detached house",
                               "Terraced house (end unit)",
                               "Terraced house (middle unit)",
                               "Bungalow",                    
                               "Mansion",                     
                               "Farmhouse")) %>%
  
  # Remove duplicates (keep highest price per `ergg_1km`)
  mutate(
    adat_num = as.numeric(str_replace(adat, "m", ""))
  ) %>%
  group_by(ergg_1km) %>%  # Ensure we remove duplicates for each `ergg_1km`
  slice_max(order_by = kaufpreis, n = 1, with_ties = FALSE) %>%  # Select max price
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
  
  # Feature engineering
  mutate(
    house_age = 2022 - baujahr,
    log_price = log(kaufpreis),
    log_area  = log(wohnflaeche),
    log_plot_area = log(grundstuecksflaeche + 1),
    ergg_1km = as.character(ergg_1km)
  ) %>%
  
  # Drop critical missing values
  drop_na(kaufpreis, wohnflaeche, ergg_1km)

## ==== 1.4 Prepare School and Distance Data ====

# Define school types: 02=Primary; 04,10,15,20=Secondary
type_primary   <- c("02") 
type_secondary <- c("04", "10", "14", "15", "20")
type_all       <- c("02", "04", "10", "14", "15", "20")

# Load distance data
raw_dist <- read_csv(path_dist, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    school_type = str_pad(as.character(school_type), 2, pad = "0"), 
    school_id = as.character(school_id),  
    ergg_1km = as.character(ergg_1km), 
    dist_km = parse_number(as.character(dist)) 
  )

# ==== 1.4.1 Derive Distance to Nearest Primary School ====
df_dist_primary <- raw_dist %>%
  filter(school_type %in% type_primary) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_primary = school_id, dist_primary_km = dist_km)

# ==== 1.4.2 Derive Distance to Nearest Secondary School ====
df_dist_secondary <- raw_dist %>%
  filter(school_type %in% type_secondary) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_secondary = school_id, dist_secondary_km = dist_km)

# Derive distance to nearest school (primary or secondary)
df_dist_any <- raw_dist %>%
  filter(school_type %in% type_all) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_any = school_id, dist_any_km = dist_km)

## ==== 1.5 Merge Housing and Distance Data (Without Social Index) ====

df_final <- df_housing_clean %>%
  left_join(df_dist_primary, by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  
  # Create distance categories to primary school
  mutate(
    dist_primary_bin = cut(dist_primary_km, 
                           breaks = c(0, 2, 3, 4, 5, Inf),
                           include.lowest = TRUE, right = TRUE,
                           labels = c("0-2km", "2-3km", "3-4km", "4-5km", ">5km"))
  )

# Final dataset check
cat("Final Dataset Dimensions:", dim(df_final), "\n")

## ==== 1.6 Baseline Regression Models (Without Social Index) ====
m1_naive_both <- lm(log_price ~ dist_primary_km + dist_secondary_km, data = df_final)
summary(m1_naive_both)

m2_base_both <- lm(log_price ~ dist_primary_km + dist_secondary_km + log_area + log_plot_area + 
                     zimmeranzahl + house_age, data = df_final)
summary(m2_base_both)

m3_poly_both <- lm(log_price ~ dist_primary_km + I(dist_primary_km^2) + dist_secondary_km + I(dist_secondary_km^2) + 
                     log_area + log_plot_area + zimmeranzahl + house_age, data = df_final)
summary(m3_poly_both)

m4_base_primary <- lm(log_price ~ dist_primary_km + log_area + log_plot_area + 
                        zimmeranzahl + house_age, data = df_final)
summary(m4_base_primary)

m5_base_secondary <- lm(log_price ~ dist_secondary_km + log_area + log_plot_area + 
                          zimmeranzahl + house_age, data = df_final)
summary(m5_base_secondary)










# --------------- 2. SOCIAL INDEX INTEGRATION AND EXTENDED ANALYSIS ------------------



# 2.1 Load and clean social index data
df_school_meta <- read_delim(path_school, 
                             delim = ";",
                             locale = readr::locale(encoding = "Latin1", decimal_mark = ","),
                             show_col_types = FALSE) %>%
  clean_names() %>%
  rename(school_id = schulnummer, social_index = sozialindexstufe) %>%
  mutate(
    school_id = as.character(school_id),  
    social_index = parse_number(as.character(social_index))  # Convert social index to numeric
  )

# 2.2 Remove invalid social index values (coded as 0)
df_school_meta <- df_school_meta %>%
  filter(social_index != 0)

# 2.3 Use numeric values from "Gemeinde" column as social index where available
df_school_meta <- df_school_meta %>%
  mutate(
    social_index = if_else(!is.na(gemeinde) & str_detect(gemeinde, "^\\d+$"), 
                           parse_number(gemeinde), 
                           social_index)
  )

# 2.4 Derive `school_quality` categories: "good" for social_index 1–2, "bad" for 3+
df_school_meta <- df_school_meta %>%
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",   # Good schools (Social Index 1-2)
      social_index %in% 3:8 ~ "bad",    # Bad schools (Social Index 3-8)
      TRUE ~ "average"                  # If needed (though excluded in this case)
    )
  )


# 2.5 Aggregate housing data to unique `ergg_1km` level
# Ensure df_housing_clean_unique exists
df_housing_clean_unique <- df_housing_clean %>%
  group_by(ergg_1km) %>%
  slice_max(order_by = kaufpreis, n = 1, with_ties = FALSE) %>%
  ungroup()

# 2.6 Merge distance data with social index information
df_final_with_social <- df_housing_clean_unique %>%
  left_join(df_dist_primary, by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  left_join(df_school_meta, by = c("school_id_primary" = "school_id")) %>%
  left_join(df_school_meta, by = c("school_id_secondary" = "school_id")) %>%
  left_join(df_school_meta, by = c("school_id_any" = "school_id"))

# 2.7 Keep complete cases for key variables used in analysis
df_final_with_social <- df_final_with_social %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km, dist_primary_km, dist_secondary_km, 
          dist_any_km, school_id_primary, school_id_secondary, school_id_any, social_index)

# 2.8 Diagnostics: dataset dimensions and social index summary
cat("Merged Dataset Dimensions:", dim(df_final_with_social), "\n")
summary(df_final_with_social$social_index)

# 2.9 Diagnostics: check for duplicates by `ergg_1km`
duplicates_check <- df_final_with_social %>%
  group_by(ergg_1km) %>%
  filter(n() > 1) %>%
  ungroup()
cat("Duplicates (if any):", nrow(duplicates_check), "\n")


# === 2.10 Assess school quality and availability within a 5 km radius ===
radius_km <- 5

# Calculate school availability and quality metrics within a 5 km radius
df_quality_5km <- raw_dist %>%
  mutate(
    ergg_1km = as.character(ergg_1km),
    school_id = as.character(school_id),
    dist_km = parse_number(as.character(dist))
  ) %>%
  filter(dist_km <= radius_km) %>%
  left_join(
    df_school_meta %>%
      select(school_id, social_index),
    by = "school_id"
  ) %>%
  filter(social_index %in% 1:8) %>%  # Filter to exclude social index = 9
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",   # Good schools (Social Index 1-2)
      social_index %in% 3:8 ~ "bad",    # Bad schools (Social Index 3-8)
      TRUE ~ "average"                  # If needed (though excluded in this case)
    )
  ) %>%
  group_by(ergg_1km) %>%
  summarise(
    best_quality_5km = min(school_quality),   # Best available school quality within 5 km
    worst_quality_5km = max(school_quality),  # Worst available school quality within 5 km
    .groups = "drop"
  )

# Integrate 5 km school quality metrics into the main dataset
df_final_with_social_5km <- df_final_with_social %>%
  left_join(df_quality_5km, by = "ergg_1km")


# === 2.11 Regression models including school quality ===

# 2.11.1 Model 1: naive specification – school quality only
# a) Using "bad" as reference category
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("bad", "good")))  # "bad" as reference

m_naive_bad_ref <- lm(log_price ~ school_quality, data = df_final_with_social_5km)
summary(m_naive_bad_ref)

# b) Using "good" as reference category
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("good", "bad")))  # "good" as reference

m_naive_good_ref <- lm(log_price ~ school_quality, data = df_final_with_social_5km)
summary(m_naive_good_ref)


# 2.11.2 Model 2: baseline specification – school quality plus housing characteristics
# a) Using "bad" as reference category
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("bad", "good")))  # "bad" as reference

m_base_bad_ref <- lm(log_price ~ school_quality + log_area + log_plot_area + 
                       zimmeranzahl + house_age, data = df_final_with_social_5km)
summary(m_base_bad_ref)

# b) Using "good" as reference category
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("good", "bad")))  # "good" as reference

m_base_good_ref <- lm(log_price ~ school_quality + log_area + log_plot_area + 
                        zimmeranzahl + house_age, data = df_final_with_social_5km)
summary(m_base_good_ref)



# 2.11.3 Model 3: extended specification – school quality and school distances
# a) Using "bad" as reference category
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("bad", "good")))  # "bad" as reference

m_multi_bad_ref <- lm(log_price ~ school_quality + dist_primary_km + dist_secondary_km + 
                        log_area + log_plot_area + zimmeranzahl + house_age, data = df_final_with_social_5km)
summary(m_multi_bad_ref)

# b) Using "good" as reference category
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("good", "bad")))  # "good" as reference

m_multi_good_ref <- lm(log_price ~ school_quality + dist_primary_km + dist_secondary_km + 
                         log_area + log_plot_area + zimmeranzahl + house_age, data = df_final_with_social_5km)
summary(m_multi_good_ref)


# === 2.12 Visualisation of the impact of social index on house prices ===

library(ggplot2)

# Create factor variable for social index levels for colouring
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(
    social_index_factor = factor(social_index, levels = 1:9, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
  )

# Visualisation: distance to primary school vs. log house price
ggplot(df_final_with_social_5km, aes(x = dist_primary_km, y = log_price, color = social_index_factor)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Influence of Distance to Primary School on House Prices by Social Index Level",
       x = "Distance to Primary School (km)",
       y = "Log-House Price",
       color = "Social Index Level") +
  scale_color_manual(values = c("#FFB6C1", "#98C9E6", "#FFD700", "#D3D3D3", "#8FBC8F", "#D2B48C", "#A9A9A9", "#E6E6FA", "#F08080")) +
  theme_minimal()

# Visualisation: distance to secondary school vs. log house price
ggplot(df_final_with_social_5km, aes(x = dist_secondary_km, y = log_price, color = social_index_factor)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Influence of Distance to Secondary School on House Prices by Social Index Level",
       x = "Distance to Secondary School (km)",
       y = "Log-House Price",
       color = "Social Index Level") +
  scale_color_manual(values = c("#FFB6C1", "#98C9E6", "#FFD700", "#D3D3D3", "#8FBC8F", "#D2B48C", "#A9A9A9", "#E6E6FA", "#F08080")) +
  theme_minimal()




# 2.13 Interaction effects between distance and school quality

# 2.13.1 Ensure `school_quality` is defined in `df_school_meta` (already done above)
df_school_meta <- df_school_meta %>%
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",   # Good schools (Social Index 1-2)
      social_index %in% 3:8 ~ "bad",    # Bad schools (Social Index 3-8)
      TRUE ~ "average"                  # If needed (though excluded in this case)
    )
  )

# Model with interaction effects between distance and school quality – "bad" as reference
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("bad", "good")))  # "bad" as reference

m_interaction_quality_bad_ref <- lm(
  log_price ~ dist_primary_km * school_quality + dist_secondary_km * school_quality + 
    log_area + log_plot_area + zimmeranzahl + house_age, 
  data = df_final_with_social_5km
)

# Model summary for specification with "bad" as reference
summary(m_interaction_quality_bad_ref)



# Model with interaction effects between distance and school quality – "good" as reference
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(school_quality = factor(school_quality, levels = c("good", "bad")))  # "good" as reference

m_interaction_quality_good_ref <- lm(
  log_price ~ dist_primary_km * school_quality + dist_secondary_km * school_quality + 
    log_area + log_plot_area + zimmeranzahl + house_age, 
  data = df_final_with_social_5km
)

# Model summary for specification with "good" as reference
summary(m_interaction_quality_good_ref)




# 2.14 Visualisation of interaction effects for good vs. bad schools

# Visualisation for model with "good" schools as reference
ggplot(df_final_with_social_5km, aes(x = dist_primary_km, y = log_price, color = school_quality)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Impact of Distance to Primary School on House Prices by School Quality (Good Schools as Reference)",
       x = "Distance to Primary School (km)",
       y = "Log-House Price",
       color = "School Quality") +
  scale_color_manual(values = c("good" = "#66C2A5", "bad" = "#FC8D62")) +
  theme_minimal()
