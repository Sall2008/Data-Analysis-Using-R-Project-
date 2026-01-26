## ==== 1. DATA PREPARATION =====

## ==== 1.1 Load Required Packages ====
library(tidyverse)
library(readxl)
library(fs)
library(scales)
library(janitor)
library(modelsummary)
library(readr)
library(gt)
library(broom)
library(ggplot2)
library(dplyr)
library(tidyr)

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














### 1.0 Social Index 

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
  ) %>%
  filter(social_index != 0) %>%  # Filter out invalid social index values (0)
  mutate(
    social_index = if_else(!is.na(gemeinde) & str_detect(gemeinde, "^\\d+$"), 
                           parse_number(gemeinde), 
                           social_index)
  ) %>%
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",      # Gute Schulen: Sozialindex 1-2
      social_index %in% 3:4 ~ "average",   # Durchschnittliche Schulen: Sozialindex 3-4
      social_index %in% 5:8 ~ "bad",       # Schlechte Schulen: Sozialindex 5-8
      TRUE ~ "unknown"
    )
  )


# 2.2 Aggregate housing data to unique `ergg_1km` level
df_housing_clean_unique <- df_housing_clean %>%
  group_by(ergg_1km) %>%
  slice_max(order_by = kaufpreis, n = 1, with_ties = FALSE) %>%
  ungroup()

# 2.3 Merge distance data with school meta data (Ensure school_quality and social_index are included)
df_final_with_social <- df_housing_clean_unique %>%
  left_join(df_dist_primary, by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = c("school_id_primary" = "school_id")) %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = c("school_id_secondary" = "school_id")) %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = c("school_id_any" = "school_id"))

# 2.4 Remove incomplete cases (Ensure that social_index columns from joins are considered)
df_final_with_social <- df_final_with_social %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km, dist_primary_km, dist_secondary_km, 
          dist_any_km, school_id_primary, school_id_secondary, school_id_any, social_index)

# 2.5 Diagnostics: dataset dimensions and social index summary
cat("Merged Dataset Dimensions:", dim(df_final_with_social), "\n")
summary(df_final_with_social$social_index)

# 2.6 Check for duplicates by `ergg_1km`
duplicates_check <- df_final_with_social %>%
  group_by(ergg_1km) %>%
  filter(n() > 1) %>%
  ungroup()
cat("Duplicates (if any):", nrow(duplicates_check), "\n")




### Regressionsmodelle 

# Base Model Social Index without groups 
Base_Model_Social_Index <- lm(
  log_price ~ social_index +
    dist_primary_km + dist_secondary_km +
    log_area + log_plot_area +
    zimmeranzahl + house_age,
  data = df_final_with_social
)

summary(m_si_extended)



## Modellbasis 
base_formula <- log_price ~
  dist_primary_km * school_quality +
  dist_secondary_km * school_quality +
  log_area + log_plot_area +
  zimmeranzahl + house_age


# Referenz Schulqualität good 
df_reg1 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "good"))

m_good_ref <- lm(base_formula, data = df_reg1)
summary(m_good_ref)


#Referenz Schulqualität Average 
df_reg2 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "average"))

m_avg_ref <- lm(base_formula, data = df_reg2)
summary(m_avg_ref)


# Referenz Schulqualität bad 
df_reg3 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "bad"))

m_bad_ref <- lm(base_formula, data = df_reg3)
summary(m_bad_ref)




###  Visualisierung

# Für den Vergleich der verschiedenen Sozialindexklassen
library(emmeans)

emmeans(m4_interaction, pairwise ~ school_quality)


# Karte mit Sozialindizes 
# Jede Gitterzelle nur einmal + zugehöriger Sozialindex (nächste Schule)
df_grid <- df_final_with_social %>%
  distinct(ergg_1km, social_index) %>%        # eine Zeile pro Zelle
  separate(
    ergg_1km,
    into = c("lon", "lat"),
    sep  = "_",          # falls anders (z.B. "-"), hier anpassen
    remove = FALSE,
    convert = TRUE       # macht direkt numeric
  )

head(df_grid)

ggplot(df_grid, aes(x = lon, y = lat)) +
  geom_point(aes(color = social_index), size = 0.8, alpha = 0.8) +
  scale_color_viridis_c(name = "Social Index") +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of School Social Index in NRW",
    subtitle = "Each grid cell colored by the social index of the nearest school"
  )


# Balkendiagramm Sozialindex
df_school_meta %>%
  ggplot(aes(x = factor(social_index))) +
  geom_bar() +
  labs(x = "Social Index (1–9)", y = "Number of schools")


# Ergebnistabelle mit Good als Referenzkategorie 
# Regression mit Interaktionseffekten
m_good_ref <- lm(base_formula, data = df_reg1)

# Anzeige der Ergebnisse in einer Tabelle
modelsummary(
  m_good_ref,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),  # Signifikanzniveaus
  title = "Table 1: Hedonic Regression Results - Effect of Distance on Housing Prices (Good as Reference)",
  output = "gt"  # Ausgabeformat für den Viewer
)



## Simple Slope Analyse 

# Modell mit Schulqualität "good"
m_good_ref <- lm(base_formula, data = df_reg1)

# Simple Slopes Analyse für dist_primary_km
interact_plot(m_good_ref, 
              pred = dist_primary_km, 
              modx = school_quality, 
              modx.values = c("good", "average", "bad"),
              plot.points = TRUE,
              main.title = "Simple Slopes Analysis for `dist_primary_km` by School Quality")

# Modell mit Schulqualität "good" für dist_secondary_km
interact_plot(m_good_ref, 
              pred = dist_secondary_km, 
              modx = school_quality, 
              modx.values = c("good", "average", "bad"),
              plot.points = TRUE,
              main.title = "Simple Slopes Analysis for `dist_secondary_km` by School Quality")






















### 2.0 Schularten GETRENNT BETRACHTET ; keine Signifikanz im Interaktionsbereich

# Filtere nur Primärschulen und bereite die Daten vor
df_primary_schools <- df_final_with_social %>%
  filter(!is.na(dist_primary_km) & !is.na(social_index)) 

# Setze 'good' als Referenzkategorie
df_primary_schools$school_quality <- factor(df_primary_schools$school_quality, levels = c("good", "average", "bad"))

# Regression mit Interaktionseffekt für Primärschulen und Sozialindex
m_primary_schools <- lm(log_price ~ dist_primary_km * school_quality + 
                          log_area + log_plot_area + zimmeranzahl + house_age, 
                        data = df_primary_schools)

summary(m_primary_schools)



# Filtere nur Sekundarschulen und bereite die Daten vor
df_secondary_schools <- df_final_with_social %>%
  filter(!is.na(dist_secondary_km) & !is.na(social_index)) 

# Setze 'good' als Referenzkategorie für die Schulqualität
df_secondary_schools$school_quality <- factor(df_secondary_schools$school_quality, levels = c("good", "average", "bad"))

# Regression mit Interaktionseffekt für Sekundarschulen und Sozialindex
m_secondary_schools <- lm(log_price ~ dist_secondary_km * school_quality + 
                            log_area + log_plot_area + zimmeranzahl + house_age, 
                          data = df_secondary_schools)

summary(m_secondary_schools)




# Visualisierung der Interaktionseffekte für Primärschulen bei EINZELNER BETRACHTUNG 
ggplot(df_primary_schools, aes(x = dist_primary_km, y = log_price, color = school_quality)) +
  geom_point(alpha = 0.6) +  # Punktgrafik, um die Daten anzuzeigen
  geom_smooth(method = "lm", se = FALSE, aes(group = school_quality)) +  # Regressionslinie für jede Schulqualität
  labs(
    title = "Impact of Distance to Primary School on House Prices by School Quality",
    x = "Distance to Primary School (km)",
    y = "Log(House Price)",
    color = "School Quality"
  ) +
  scale_color_manual(values = c("good" = "#66C2A5", "average" = "#FC8D62", "bad" = "#8B0000")) +  
  theme_minimal() 


# Visualisierung der Interaktionseffekte für Sekundärschulen
ggplot(df_secondary_schools, aes(x = dist_secondary_km, y = log_price, color = school_quality)) +
  geom_point(alpha = 0.6) +  # Punktgrafik, um die Daten anzuzeigen
  geom_smooth(method = "lm", se = FALSE, aes(group = school_quality)) +  # Regressionslinie für jede Schulqualität
  labs(
    title = "Impact of Distance to Secondary School on House Prices by School Quality",
    x = "Distance to Secondary School (km)",
    y = "Log(House Price)",
    color = "School Quality"
  ) +
  scale_color_manual(values = c("good" = "#66C2A5", "average" = "#FC8D62", "bad" = "#8B0000")) +  
  theme_minimal()  










