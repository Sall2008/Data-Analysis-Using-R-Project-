# ==== 1. DATA PREPARATION =====

## ==== 1.1 Setup ====
library(tidyverse)
library(readxl)
library(fs)
library(scales)
library(janitor)
library(modelsummary)
library(spdep)
library(dplyr)
library(tidyr)
library(RANN)
library(ggplot2) 

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

# ==== 2. Queen contiguity (raster only) =====

# Separate numeric coordinates
df_queen_contiguity <- df_final %>%
  separate(
    ergg_1km,
    into = c("x", "y"),
    sep = "_",
    convert = TRUE,
    remove = FALSE
  )

grid <- df_queen_contiguity %>%
  group_by(x, y) %>%
  summarise(
    dist_primary_km = min(dist_primary_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(x, y)

coords <- as.matrix(grid[, c("x", "y")])
n_grid <- nrow(grid)

# Queen contiguity
nb_queen <- dnearneigh(
  coords,
  d1 = 0,
  d2 = sqrt(2),
  longlat = FALSE
)

nb_lags <- spdep::nblag(nb_queen, 3)

nb_1 <- nb_lags[[1]]
nb_2 <- nb_lags[[2]]
nb_3 <- nb_lags[[3]]

# sanity check
stopifnot(
  length(nb_1) == n_grid,
  length(nb_2) == n_grid,
  length(nb_3) == n_grid
)

# Creating distance based indicators
grid <- grid %>%
  mutate(
    near_q1 = sapply(seq_len(n_grid), function(i) {
      idx <- nb_1[[i]]
      length(idx) > 0 && any(dist_primary_km[idx] <= 1)
    }),
    
    near_q2 = sapply(seq_len(n_grid), function(i) {
      idx <- nb_2[[i]]
      length(idx) > 0 && any(dist_primary_km[idx] <= 2)
    }),
    
    near_q3 = sapply(seq_len(n_grid), function(i) {
      idx <- nb_3[[i]]
      length(idx) > 0 && any(dist_primary_km[idx] <= 3)
    })
  )

# Join back
df_final <- df_queen_contiguity %>%
  left_join(
    grid,
    by = c("x", "y")
  )

# Drop double distance
df_final <- df_final %>%
  rename(dist_primary_km = dist_primary_km.y) %>%
  select(-dist_primary_km.x)

# Regression (no fixed-effects)
lm(
  log_price ~ dist_primary_km + near_q1 + near_q2 + near_q3 +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

# Robustness check
# Q1
lm(
  log_price ~ dist_primary_km + near_q1 +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

# Q2
lm(
  log_price ~ dist_primary_km + near_q2 +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

# Q3
lm(
  log_price ~ dist_primary_km + near_q3 +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

# ==== 3. Queen contiguity (distance) =====
# Distance calculation (might have double steps compared to above)

# Grid (unique raster cells)
grid <- df_final %>%
  separate(ergg_1km, into = c("x", "y"), sep = "_", convert = TRUE) %>%
  group_by(x, y) %>%
  summarise(
    dist_primary_km = min(dist_primary_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(x, y)

nb_queen <- dnearneigh(
  coords,
  d1 = 0,
  d2 = sqrt(2),
  longlat = FALSE
)

# Sanity check
summary(
  raw_dist %>%
    filter(
      school_type == "02",
      nn_order == 1
    ) %>%
    pull(dist_km)
)

# Identify school cells (from previous step)
school_cells <- raw_dist %>%
  filter(
    school_type == "02",
    nn_order == 1,
    dist_km <= 0.75
  ) %>%
  select(ergg_1km) %>%
  distinct() %>%
  separate(ergg_1km, into = c("x", "y"), sep = "_", convert = TRUE)

# Indicator: does this grid cell contain a school?
grid <- grid %>%
  mutate(
    has_school = paste(x, y) %in% paste(school_cells$x, school_cells$y)
  )

### Queen cont
coords <- as.matrix(grid[, c("x", "y")])

# Queen contiguity (8 neighbors)
nb_queen <- dnearneigh(
  coords,
  d1 = 0,
  d2 = sqrt(2),
  longlat = FALSE
)

# Higher-order neighbors
nb_lags <- nblag(nb_queen, maxlag = 15)

# Compute
# Initialize distance
grid$q_dist <- NA_integer_

# Distance 0
grid$q_dist[grid$has_school] <- 0

# Distances 1+
for (k in 1:5) {
  idx <- which(is.na(grid$q_dist))
  if (length(idx) == 0) break
  
  for (i in idx) {
    neigh <- nb_lags[[k]][[i]]
    if (length(neigh) > 0 && any(grid$has_school[neigh])) {
      grid$q_dist[i] <- k
    }
  }
}

# Any remaining → very far
max_lag <- length(nb_lags)
grid$q_dist[is.na(grid$q_dist)] <- max_lag + 1

# Distribution check
table(grid$q_dist)

# Merge back for housing data
df_final <- df_final %>%
  separate(ergg_1km, into = c("x", "y"), sep = "_", convert = TRUE) %>%
  left_join(
    grid %>% select(x, y, q_dist),
    by = c("x", "y")
  )

# Regression
# Each additional queen-contiguity ring away from a primary school is associated with about a
# 5.5% lower listing price, holding housing characteristics constant.
model <- lm(
  log_price ~ q_dist +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model)

# Regression: factor distance
df_final$q_dist_f <- factor(df_final$q_dist)

lm(
  log_price ~ q_dist_f +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

# Houses in the same raster cell as a primary school are the most expensive
# Prices drop sharply already in first- and second-order neighbors
# Effects are non-linear and strongly spatial

## ==== 3.1 Plot ====
grid_map <- df_final %>%
  distinct(x, y, q_dist)

ggplot(grid_map, aes(x = x, y = y, fill = factor(q_dist))) +
  geom_tile(color = NA) +
  scale_fill_viridis_d(
    name = "Contiguity distance",
    option = "C",
    direction = -1
  ) +
  coord_equal() +
  labs(
    x = "Grid X coordinate (1 km)",
    y = "Grid Y coordinate (1 km)",
    title = "Queen-contiguity distance to nearest primary school"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

## ==== 3.2 Diagnostics ====

# Confirming whether row 6 and 7 in "grid" are actually 6+ steps apart

i <- which(grid$x == 4036 & grid$y == 3108)

k_hit <- which(sapply(seq_along(nb_lags), function(k) {
  any(grid$has_school[ nb_lags[[k]][[i]] ])
}))[1]

k_hit  # NA if no school reachable within maxlag

# What would be the first cell to hit a school for row 7
find_school_path <- function(i, nb_lags, grid) {
  if (grid$has_school[i]) {
    return(list(lag = 0, cell = i))
  }
  
  for (k in seq_along(nb_lags)) {
    neigh_k <- nb_lags[[k]][[i]]
    
    if (length(neigh_k) > 0) {
      hits <- neigh_k[grid$has_school[neigh_k]]
      
      if (length(hits) > 0) {
        return(list(
          lag = k,
          cell_index = hits[1],
          x = grid$x[hits[1]],
          y = grid$y[hits[1]]
        ))
      }
    }
  }
  
  return(NULL)
}

find_school_path(i, nb_lags, grid)

# Check graph fragmentation: Number of neighbors per cell
table(spdep::card(nb_queen))



