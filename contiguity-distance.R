# ==== 1. DATA PREPARATION =====

## ==== 1.1 Setup ====
library(tidyverse)
library(readxl)
library(fs)
library(sf)
library(scales)
library(janitor)
library(modelsummary)
library(spdep)
library(dplyr)
library(tidyr)
library(RANN)
library(ggplot2)
library(broom)
library(knitr)
library(kableExtra)

# Set global theme
theme_set(theme_minimal(base_size = 14))

## ==== 1.2 Define Paths ====
path_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_school  <- "course_data/school_data/2022_social_index.csv"
path_dist    <- "course_data/school_data/distance_to_schools.csv"
path_VG250   <- "course_data/VG250/vg250_ebenen_0101/VG250_GEM.shp"

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

# ==== 2. Queen contiguity =====

## ==== 2.1 New Paths ====
path_sale_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_sale_flats <- "course_data/housing_data/cross_section/CampusFile_WK_2022.csv"
path_rent_flats <- "course_data/housing_data/cross_section/CampusFile_WM_2022.csv"

## ==== 2.2 Clean Houses for Sale Data ====
raw_housing_sale <- read_delim(
  path_sale_housing, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

## ==== 2.3 Clean Flats for Sale Data ====
raw_flats_sale <- read_delim(
  path_sale_flats, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

## ==== 2.4 Clean Flats for Rent Data ====
raw_flats_rent <- read_delim(
  path_rent_flats, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

## ==== 2.5 NRW Cells Only ====
raw_housing_sale_NRW <- raw_housing_sale %>% filter(blid == "North Rhine-Westphalia")
raw_flats_sale_NRW <- raw_flats_sale %>% filter(blid == "North Rhine-Westphalia")
raw_flats_rent_NRW <- raw_flats_rent %>% filter(blid == "North Rhine-Westphalia")

## ==== 2.5 Building a more comprehensive grid ====
cells_big <- bind_rows(
  raw_housing_sale_NRW %>% select(ergg_1km),
  raw_flats_sale_NRW %>% select(ergg_1km),
  raw_flats_rent_NRW %>% select(ergg_1km)
) %>%
  distinct() %>%
  filter(!is.na(ergg_1km)) %>%                 
  separate(
    ergg_1km,
    into = c("x","y"),
    sep = "_",
    convert = TRUE,
    remove = TRUE
  ) %>%
  filter(!is.na(x), !is.na(y)) %>%              
  distinct(x, y) %>%
  arrange(x, y)

## ==== 2.6 Identifying school cells in that grid ====
school_cells_primary <- raw_dist %>%
  filter(school_type %in% type_primary, nn_order == 1, dist_km <= 0.75) %>%
  distinct(ergg_1km) %>%
  separate(ergg_1km, into = c("x","y"), sep = "_", convert = TRUE) %>%
  filter(!is.na(x), !is.na(y)) %>%
  distinct(x, y)

school_cells_secondary <- raw_dist %>%
  filter(school_type %in% type_secondary, nn_order == 1, dist_km <= 0.75) %>%
  distinct(ergg_1km) %>%
  separate(ergg_1km, into = c("x","y"), sep = "_", convert = TRUE) %>%
  filter(!is.na(x), !is.na(y)) %>%
  distinct(x, y)

cells_big <- cells_big %>%
  mutate(
    has_primary = paste(x,y) %in% paste(school_cells_primary$x, school_cells_primary$y),
    has_secondary = paste(x,y) %in% paste(school_cells_secondary$x, school_cells_secondary$y)
  )

## ==== 2.7 Building Grid ====
coords <- as.matrix(cells_big[, c("x","y")])

stopifnot(!anyNA(coords))   # safety check

nb_queen <- dnearneigh(coords, d1 = 0, d2 = sqrt(2), longlat = FALSE)

# Drop cells with no neighbors

cells_big <- cells_big %>%
  mutate(n_neighbors = spdep::card(nb_queen))

cells_big <- cells_big %>%
  filter(n_neighbors > 0) %>%
  select(-n_neighbors)

# Rebuild nb_queen
coords <- as.matrix(cells_big[, c("x","y")])

stopifnot(!anyNA(coords))

nb_queen <- dnearneigh(coords, d1 = 0, d2 = sqrt(2), longlat = FALSE)

stopifnot(
  length(nb_queen) == nrow(cells_big),
  length(cells_big$has_primary) == nrow(cells_big),
  length(cells_big$has_secondary) == nrow(cells_big)
)

### ==== 2.7.1 Diagnostics ====
summary(card(nb_queen))
n.comp.nb(nb_queen)$nc
table(spdep::card(nb_queen))

## ==== 2.8 Compute Queens contiguity ====

### ==== 2.8.1 Define function ====
compute_qdist <- function(nb, has_school, max_steps = 200) {
  stopifnot(length(nb) == length(has_school))
  
  q_dist <- rep(NA_integer_, length(has_school))
  q_dist[has_school] <- 0
  
  frontier <- which(has_school)
  k <- 0
  
  while (length(frontier) > 0) {
    k <- k + 1
    if (k > max_steps) break
    
    new_frontier <- integer(0)
    
    for (i in frontier) {
      neigh <- nb[[i]]
      neigh <- neigh[neigh > 0]
      if (length(neigh) == 0) next
      
      to_set <- neigh[is.na(q_dist[neigh])]
      if (length(to_set) > 0) {
        q_dist[to_set] <- k
        new_frontier <- c(new_frontier, to_set)
      }
    }
    
    frontier <- unique(new_frontier)
  }
  
  q_dist
}

### ==== 2.8.2 Apply function ====
cells_big$q_dist_primary <- compute_qdist(
  nb = nb_queen,
  has_school = cells_big$has_primary
)

cells_big$q_dist_secondary <- compute_qdist(
  nb = nb_queen,
  has_school = cells_big$has_secondary
)


## ==== 2.9 Merge back ====
df_final <- df_final %>%
  separate(ergg_1km, into = c("x","y"), sep = "_", convert = TRUE, remove = FALSE) %>%
  left_join(
    cells_big %>%
      select(x, y,
             q_dist_primary,
             q_dist_secondary),
    by = c("x","y")
  )


## ==== 2.10 Regression ====

## ==== 2.10.1 Primary Schools ====
model_primary_all <- lm(
  log_price ~ q_dist_primary +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model_primary_all)

## ==== 2.10.2 Secondary Schools ====
model_secondary_all <- lm(
  log_price ~ q_dist_secondary +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model_secondary_all)

## ==== 2.11 Map ====

## ==== 2.11.1 Primary Schools ====
plot_queens_primary <- ggplot(cells_big, aes(x = x, y = y, fill = q_dist_primary)) +
  labs(
    title = "Queens Distance for Primary Schools"
  ) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c(
    name = "Distance",
    option = "C",
    direction = -1,
    limits = c(0, 5),      # cap visually
    oob = scales::squish
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

plot_queens_primary

table(spdep::card(nb_queen))

## ==== 2.11.2 Secondary Schools ====
plot_queens_secondary <- ggplot(cells_big, aes(x = x, y = y, fill = q_dist_secondary)) +
  labs(
    title = "Queens Distance for Secondary Schools"
  ) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c(
    name = "Distance",
    option = "C",
    direction = -1,
    limits = c(0, 5),      # cap visually
    oob = scales::squish
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

plot_queens_secondary

## ==== 2.11.3 Mean districts ====

df_muni_qdist <- df_final %>%
  filter(
    !is.na(gid2019),
    !is.na(q_dist_primary),
    !is.na(q_dist_secondary)
  ) %>%
  group_by(gid2019) %>%
  summarise(
    q_dist_primary   = mean(q_dist_primary, na.rm = TRUE),
    q_dist_secondary = mean(q_dist_secondary, na.rm = TRUE),
    n_obs            = n(),
    .groups = "drop"
  )

gemeinden_sf <- st_read(path_VG250)

gemeinden_nrw <- gemeinden_sf %>%
  filter(substr(AGS, 1, 2) == "05") %>%
  mutate(
    gid2019 = substr(as.character(AGS), 2, nchar(AGS))
  )

df_muni_qdist <- df_muni_qdist %>%
  mutate(gid2019 = as.character(gid2019))

map_data <- gemeinden_nrw %>%
  left_join(df_muni_qdist, by = "gid2019")

### ==== 2.11.3.1 Mean districts plot primary ====

ggplot(map_data) +
  geom_sf(aes(fill = q_dist_primary), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Queen distance (Primary)",
    option = "C",
    direction = -1,
    limits = c(0, 5),
    oob = scales::squish,
    na.value = "grey85"
  ) +
  labs(
    title = "Average Queen Distance to Primary Schools by Municipality",
    subtitle = "Mean raster-based queen distance within municipalities"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

### ==== 2.11.3.2 Mean districts plot secondary ====

ggplot(map_data) +
  geom_sf(aes(fill = q_dist_secondary), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Queen distance (Primary)",
    option = "C",
    direction = -1,
    limits = c(0, 5),
    oob = scales::squish,
    na.value = "grey85"
  ) +
  labs(
    title = "Average Queen Distance to Primary Schools by Municipality",
    subtitle = "Mean raster-based queen distance within municipalities"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# ==== 3. Queen contiguity + Social Index =====

## ==== 3.1 Regression ====
model_index <- lm(
  log_price ~ q_dist_primary * social_index + q_dist_secondary * social_index +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model_index)

## ==== 3.2 Regression on categorized index  ====

### ==== 3.2.1  Creating the categories ====
df_final <- df_final %>% 
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",
      social_index %in% 3:4 ~ "average",
      social_index %in% 5:8 ~ "bad",
    )
  )

## ==== 3.2.2 Regressions ====

# Model basis
base_formula <- log_price ~
  q_dist_primary * school_quality +
  q_dist_secondary* school_quality +
  log_area + log_plot_area +
  zimmeranzahl + house_age


# Reference school quality good 
df_final1 <- df_final %>%
  mutate(school_quality = factor(school_quality),
         school_quality = relevel(school_quality, ref = "good")
         )

m_good_ref <- lm(base_formula, data = df_final1)
summary(m_good_ref)


# Reference school quality average  
df_final2 <- df_final %>%
  mutate(school_quality = factor(school_quality),
         school_quality = relevel(school_quality, ref = "average"))

m_avg_ref <- lm(base_formula, data = df_final2)
summary(m_avg_ref)


# Reference school quality bad 
df_final3 <- df_final %>%
  mutate(school_quality = factor(school_quality),
         school_quality = relevel(school_quality, ref = "bad"))

m_bad_ref <- lm(base_formula, data = df_final3)
summary(m_bad_ref)

# ==== 4. Result tables ====

# Helper: significance stars
stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.1  ~ "*",
    TRUE     ~ ""
  )
}

## ==== 4.1 Regression Distance only ====

# Extract only the distance coefficients
reg_table <- bind_rows(
  tidy(model_primary_all) %>%
    filter(term == "q_dist_primary") %>%
    mutate(
      Model = "Primary Schools",
      coef  = paste0(round(estimate, 3), stars(p.value)),
      se    = paste0("(", round(std.error, 3), ")")
    ),
  
  tidy(model_secondary_all) %>%
    filter(term == "q_dist_secondary") %>%
    mutate(
      Model = "Secondary Schools",
      coef  = paste0(round(estimate, 3), stars(p.value)),
      se    = paste0("(", round(std.error, 3), ")")
    )
) %>%
  select(Model, coef, se) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(coef, se)
  )

# Add row label and order columns
reg_table_print <- reg_table %>%
  mutate(Variable = "Queen distance") %>%
  select(
    Variable,
    `coef_Primary Schools`,
    `se_Primary Schools`,
    `coef_Secondary Schools`,
    `se_Secondary Schools`
  )

# Print table
reg_table <- reg_table_print %>%
  kbl(
    caption = "Table 1: Regression Results: Queen Distance to Schools",
    booktabs = TRUE,
    align = "c"
  ) %>%
  add_header_above(
    c(
      " " = 1,
      "Primary Schools" = 2,
      "Secondary Schools" = 2
    ),
    bold = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 16,
    latex_options = c("hold_position"),
    stripe_color = "gray!12"
  ) %>%
  row_spec(0, bold = TRUE) %>%        # column headers
  row_spec(1, extra_css = "border-bottom: 2px solid #2C3E50;") %>%
  footnote(
    general = c(
      "Dependent variable: log house price.",
      "Control variables included but not shown.",
      "*** p<0.01, ** p<0.05, * p<0.1"
    ),
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )


# ==== 5. Graphics for presentation ====

## ==== 5.1 Graphics for presentation ====

### ==== 5.1.1 Limitation: Distances ====
cell <- data.frame(
  xmin = 0, xmax = 1,
  ymin = 0, ymax = 1
)

# Key locations (centriod, school and house)
centroid <- data.frame(x = 0.5, y = 0.5)
school   <- data.frame(x = 0.88, y = 0.84)
house    <- data.frame(x = 0.75, y = 0.80)

# Plot cell
ggplot() +
  geom_rect(
    data = cell,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey95",
    color = "grey40",
    linewidth = 0.6
  ) +
  
  labs(title = "Single raster cell used for distance computation") +
  
  # Add centroid to plot
  geom_point(data = centroid, aes(x, y), size = 4) +
  geom_text(
    data = centroid,
    aes(x, y, label = "Cell centroid"),
    vjust = 2.2, size = 4
  ) +
  
  # Add school to plot
  geom_point(data = school, aes(x, y), size = 4, shape = 17) +
  geom_text(
    data = school,
    aes(x, y, label = "School"),
    vjust = -1.3, size = 4
  ) +
  
  # Add house to plot 
  geom_point(data = house, aes(x, y), size = 4, shape = 15) +
  geom_text(
    data = house,
    aes(x, y, label = "House"),
    vjust = -1.3, size = 4
  ) +
  
  # Measured distance (school → centroid)
  geom_segment(
    aes(
      x = school$x, y = school$y,
      xend = centroid$x, yend = centroid$y
    ),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  # True distance as dotted line (school → house)
  geom_segment(
    aes(
      x = school$x, y = school$y,
      xend = house$x, yend = house$y
    ),
    linewidth = 0.8
  ) +
  
  coord_equal(xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5)
  )

### ==== 5.1.2 Method: Queens Distance ====

### ==== 5.1.2.1 Method: Introduction Queens Distance ====
queen_grid_5 <- expand.grid(
  x = 1:5,
  y = 1:5
) %>%
  mutate(
    q_dist = pmax(abs(x - 3), abs(y - 3))
  )

ggplot(queen_grid_5, aes(x = x, y = y, fill = factor(q_dist))) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Mark school cell
  geom_point(
    data = subset(queen_grid_5, x == 3 & y == 3),
    aes(x, y),
    shape = 17,
    size = 4,
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "0" = "#252525",
      "1" = "#969696",
      "2" = "#cccccc",
      "3" = "#f0f0f0"
    ),
    name = "Queen distance"
  ) +
  
  coord_equal() +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(breaks = 1:5) +
  
  labs(
    title = "Queen distance on a 5×5 raster grid",
    x = NULL,
    y = NULL
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

### ==== 5.1.2.2 Method: Exploratory queens distance on a bigger scale ====
grid_15 <- expand.grid(
  x = 1:15,
  y = 1:15
)

# Define multiple school cells
schools <- tibble(
  school_id = 1:3,
  sx = c(4, 8, 12),
  sy = c(11, 4, 10)
)

# Compute Queen distance to each school, then take minimum
queen_15 <- grid_15 %>%
  crossing(schools) %>%
  mutate(
    q_dist = pmax(abs(x - sx), abs(y - sy))
  ) %>%
  group_by(x, y) %>%
  summarise(
    q_dist = min(q_dist),
    .groups = "drop"
  )

ggplot(queen_15, aes(x = x, y = y, fill = factor(q_dist))) +
  geom_tile(color = "white", linewidth = 0.15) +
  
  # Overlay school locations
  geom_point(
    data = schools,
    aes(x = sx, y = sy),
    shape = 17,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  
  scale_fill_viridis_d(
    name = "Queen distance",
    direction = -1
  ) +
  
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(
    title = "Queen distance to nearest school on a 15×15 raster grid",
    x = NULL,
    y = NULL
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
