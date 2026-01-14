## ==== 1. DATA PREPARATION =====

## ==== 1.1 Setup ====
library(tidyverse)
library(readxl)
library(fs)
library(scales)
library(janitor)
library(modelsummary)
library(readr)

# Set global theme
theme_set(theme_minimal(base_size = 14))

## ==== 1.2 Define Paths ====

# Definiere den Pfad für die CSV-Dateien
# path_housing <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/CampusFile_HK_2022.csv"
# path_school  <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/2022_social_index.csv"
# path_dist    <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/distance_to_schools.csv"

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
  
  # Remove Duplicates (based on highest price per `ergg_1km`)
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
  
  # Feature Engineering
  mutate(
    house_age = 2022 - baujahr,
    log_price = log(kaufpreis),
    log_area  = log(wohnflaeche),
    log_plot_area = log(grundstuecksflaeche + 1),
    ergg_1km = as.character(ergg_1km)
  ) %>%
  
  # Drop critical missing values
  drop_na(kaufpreis, wohnflaeche, ergg_1km)

## ==== 1.4 Clean School & Distance Data ====

# Define types: 02=Primary; 04,10,15,20=Secondary
type_primary   <- c("02") 
type_secondary <- c("04", "10", "14", "15", "20")
type_all       <- c("02", "04", "10", "14", "15", "20")

# Load Distances
raw_dist <- read_csv(path_dist, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    school_type = str_pad(as.character(school_type), 2, pad = "0"), 
    school_id = as.character(school_id),  
    ergg_1km = as.character(ergg_1km), 
    dist_km = parse_number(as.character(dist)) 
  )

# ==== 1.4.1 Process PRIMARY Schools ====
df_dist_primary <- raw_dist %>%
  filter(school_type %in% type_primary) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_primary = school_id, dist_primary_km = dist_km)

# ==== 1.4.2 Process SECONDARY Schools ====
df_dist_secondary <- raw_dist %>%
  filter(school_type %in% type_secondary) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_secondary = school_id, dist_secondary_km = dist_km)

# Filtere nach allen Schulen (Primär + Sekundär)
df_dist_any <- raw_dist %>%
  filter(school_type %in% type_all) %>%
  group_by(ergg_1km) %>%
  slice_min(order_by = dist_km, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ergg_1km, school_id_any = school_id, dist_any_km = dist_km)

## ==== 1.5 Merging (Ohne Sozialindex) ====

df_final <- df_housing_clean %>%
  left_join(df_dist_primary, by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  
  # Keine Verwendung des Sozialindex hier
  mutate(
    dist_primary_bin = cut(dist_primary_km, 
                           breaks = c(0, 2, 3, 4, 5, Inf),
                           include.lowest = TRUE, right = TRUE,
                           labels = c("0-2km", "2-3km", "3-4km", "4-5km", ">5km"))
  )

# Finalen Check
cat("Final Dataset Dimensions:", dim(df_final), "\n")

## ==== Erste Modelle (Ohne Sozialindex) ====
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








## ==== 2. SOCIAL INDEX =====

# 1. Load and Clean Social Index Data
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

# 2. Clean Data: Remove rows with '0' in the social index (invalid values)
df_school_meta <- df_school_meta %>%
  filter(social_index != 0)

# 3. Transfer numeric values from "Gemeinde" column to social_index if available
df_school_meta <- df_school_meta %>%
  mutate(
    social_index = if_else(!is.na(gemeinde) & str_detect(gemeinde, "^\\d+$"), 
                           parse_number(gemeinde), 
                           social_index)
  )

# 4. Merge Social Index with Distance Data
# Ensure df_housing_clean_unique exists
df_housing_clean_unique <- df_housing_clean %>%
  group_by(ergg_1km) %>%
  slice_max(order_by = kaufpreis, n = 1, with_ties = FALSE) %>%
  ungroup()

# 5. Merge Data (Distance and Social Index)
df_final_with_social <- df_housing_clean_unique %>%
  left_join(df_dist_primary, by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  left_join(df_school_meta, by = c("school_id_primary" = "school_id")) %>%
  left_join(df_school_meta, by = c("school_id_secondary" = "school_id")) %>%
  left_join(df_school_meta, by = c("school_id_any" = "school_id"))

# 6. Drop NA for Critical Variables in the Analysis
df_final_with_social <- df_final_with_social %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km, dist_primary_km, dist_secondary_km, 
          dist_any_km, school_id_primary, school_id_secondary, school_id_any, social_index)

# 7. Verify the New Dataset Dimensions and Social Index
cat("Merged Dataset Dimensions:", dim(df_final_with_social), "\n")
summary(df_final_with_social$social_index)

# 8. Optional: Check for Duplicates to Ensure Correct Merging
duplicates_check <- df_final_with_social %>%
  group_by(ergg_1km) %>%
  filter(n() > 1) %>%
  ungroup()
cat("Duplicates (if any):", nrow(duplicates_check), "\n")


# === Step 1: Calculate School Quality and Availability within a 5 km Radius ===
radius_km <- 5

# Calculate the number of schools and social index statistics within the 5 km radius
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
  filter(social_index %in% 1:9) %>%
  group_by(ergg_1km) %>%
  summarise(
    best_si_5km = min(social_index),   # Best school in the radius (lowest social index)
    avg_si_5km = mean(social_index),   # Average social index in the radius
    worst_si_5km = max(social_index),  # Worst school in the radius (highest social index)
    .groups = "drop"
  )

# Merge school quality and availability data into the main dataset
df_final_with_social_5km <- df_final_with_social %>%
  left_join(df_quality_5km, by = "ergg_1km")


# === Step 2: Run Hedonic Model to Analyze the Influence of School Quality on Housing Prices ===
m_hedonic_5km <- lm(
  log_price ~ dist_primary_km + dist_secondary_km + 
    best_si_5km + avg_si_5km + worst_si_5km +  # School quality (social index)
    log_area + log_plot_area + zimmeranzahl + house_age, 
  data = df_final_with_social_5km
)

# Model Summary
summary(m_hedonic_5km)


# === Step 3: Visualization of the Influence of Social Index on House Prices ===

library(ggplot2)

# Create a new factor variable for social index levels for color mapping
df_final_with_social_5km <- df_final_with_social_5km %>%
  mutate(
    social_index_factor = factor(social_index, levels = 1:9, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))
  )

# Visualization for Distance to Primary School
Testplot <- ggplot(df_final_with_social_5km, aes(x = dist_primary_km, y = log_price, color = social_index_factor)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Influence of Distance to Primary School on House Prices by Social Index Level",
       x = "Distance to Primary School (km)",
       y = "Log-House Price",
       color = "Social Index Level") +
  scale_color_manual(values = c("#FFB6C1", "#98C9E6", "#FFD700", "#D3D3D3", "#8FBC8F", "#D2B48C", "#A9A9A9", "#E6E6FA", "#F08080")) +
  theme_minimal()

# Visualization for Distance to Secondary School
ggplot(df_final_with_social_5km, aes(x = dist_secondary_km, y = log_price, color = social_index_factor)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Influence of Distance to Secondary School on House Prices by Social Index Level",
       x = "Distance to Secondary School (km)",
       y = "Log-House Price",
       color = "Social Index Level") +
  scale_color_manual(values = c("#FFB6C1", "#98C9E6", "#FFD700", "#D3D3D3", "#8FBC8F", "#D2B48C", "#A9A9A9", "#E6E6FA", "#F08080")) +
  theme_minimal()




# Interaktionseffekt

# 1. Erstelle die `school_quality`-Variable im `df_school_meta`-Datensatz
df_school_meta <- df_school_meta %>%
  mutate(
    school_quality = case_when(
      social_index == 1 ~ "good",   # Gute Schule (Sozialindex 1)
      social_index == 2 ~ "good",   # Gute Schule (Sozialindex 2)
      social_index >= 3 ~ "bad",    # Schlechte Schule (ab Sozialindex 3)
      TRUE ~ "average"              # Wenn eine mittlere Kategorie gebraucht wird
    )
  )

# 2. Überprüfen der `school_quality`-Verteilung
table(df_school_meta$school_quality)

# 3. `school_quality` zu `df_final_with_social_5km` hinzufügen
df_final_with_social_5km <- df_final_with_social_5km %>%
  left_join(df_school_meta %>%
              select(school_id, school_quality), by = c("school_id_primary" = "school_id")) %>%
  left_join(df_school_meta %>%
              select(school_id, school_quality), by = c("school_id_secondary" = "school_id")) %>%
  left_join(df_school_meta %>%
              select(school_id, school_quality), by = c("school_id_any" = "school_id"))

# Überprüfen, ob `school_quality` erfolgreich hinzugefügt wurde
head(df_final_with_social_5km)

# 4. Modell mit Interaktionseffekten zwischen der Distanz zu guten und schlechten Schulen
m_interaction_quality <- lm(
  log_price ~ dist_primary_km * school_quality + dist_secondary_km * school_quality + 
    log_area + log_plot_area + zimmeranzahl + house_age, 
  data = df_final_with_social_5km
)

# Zusammenfassung des Modells für gute und schlechte Schulen als Referenz
summary(m_interaction_quality)


# 5. `school_quality` als Faktor mit der Referenzkategorie "bad"
df_school_meta <- df_school_meta %>%
  mutate(
    school_quality = factor(school_quality, levels = c("bad", "good"))  # "bad" als Referenzkategorie
  )

# 6. Modell mit Interaktionseffekten für schlechte Schulen als Referenz
m_interaction_quality_bad_ref <- lm(
  log_price ~ dist_primary_km * school_quality + dist_secondary_km * school_quality + 
    log_area + log_plot_area + zimmeranzahl + house_age, 
  data = df_final_with_social_5km
)

# Zusammenfassung des Modells für schlechte Schulen als Referenz
summary(m_interaction_quality_bad_ref)

# 7. Visualisierung der Interaktionseffekte für das Modell mit guten und schlechten Schulen
# Plot für das Modell mit guten und schlechten Schulen als Referenz

# Visualisierung für das Modell mit guten Schulen als Referenz
ggplot(df_final_with_social_5km, aes(x = dist_primary_km, y = log_price, color = school_quality)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Einfluss der Entfernung zur Primärschule auf Hauspreise nach Schulqualität (Gute Schulen als Referenz)",
       x = "Entfernung zur Primärschule (km)",
       y = "Log-Hauspreis",
       color = "Schulqualität") +
  scale_color_manual(values = c("good" = "#66C2A5", "bad" = "#FC8D62")) +
  theme_minimal()



