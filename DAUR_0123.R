## ==== 1. DATA PREPARATION =====
### ==== 1.1 Packages & Theme ====

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


theme_set(
  theme_minimal(base_size = 18) +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(color = "grey35"),
      axis.title      = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
)

### ==== 1.2 Define Data Source Paths ====

#path_housing <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/CampusFile_HK_2022.csv"
#path_school  <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/2022_social_index.csv"
#path_dist    <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/distance_to_schools.csv"

path_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_school  <- "course_data/school_data/2022_social_index.csv"
path_dist    <- "course_data/school_data/distance_to_schools.csv"

year_ref <- 2022

type_primary   <- c("02")
type_secondary <- c("04", "10", "14", "15", "20")
type_all       <- c("02", "04", "10", "14", "15", "20")

breaks_km <- c(0, 3, 6, 9, Inf)
labels_km <- c("0-3", "3-6", "6-9", ">9")

### ==== 1.3 Housing Data (HK) Cleaning ====

raw_housing <- read_delim(
  path_housing,
  delim = ",",
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
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
    kaufpreis          = if_else(kaufpreis > 5e7 | kaufpreis <= 0, NA_real_, kaufpreis),
    wohnflaeche        = if_else(wohnflaeche > 10000 | wohnflaeche <= 0, NA_real_, wohnflaeche),
    grundstuecksflaeche = if_else(grundstuecksflaeche > 5000, NA_real_, grundstuecksflaeche),
    zimmeranzahl       = if_else(zimmeranzahl > 25, NA_real_, zimmeranzahl),
    baujahr            = if_else(baujahr < 1000 | baujahr > year_ref, NA_real_, baujahr),
    house_age          = year_ref - baujahr,
    log_price          = log(kaufpreis),
    log_area           = log(wohnflaeche),
    log_plot_area      = log(grundstuecksflaeche + 1)
  ) %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km)

### ==== 1.4 Distance Data ====

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

df_dist_primary   <- get_nearest_school(raw_dist, type_primary,   "school_id_primary",   "dist_primary_km")
df_dist_secondary <- get_nearest_school(raw_dist, type_secondary, "school_id_secondary", "dist_secondary_km")
df_dist_any       <- get_nearest_school(raw_dist, type_all,       "school_id_any",       "dist_any_km")

### ==== 1.5 Merge: Housing + Distances ====
df_main <- df_housing %>%
  left_join(df_dist_primary,   by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any,       by = "ergg_1km") %>%
  mutate(
    dist_primary_bin = cut(
      dist_primary_km,
      breaks = breaks_km,
      include.lowest = TRUE,
      right = TRUE,
      labels = c("0-3km", "3-6km", "6-9km", ">9km")
    )
  )

cat("Final dataset dimensions:", dim(df_main), "\n")

### ==== 1.6 Proxy Regressions ====

df_reg_16 <- df_main %>%
  drop_na(
    log_price,
    dist_primary_km, dist_secondary_km,
    log_area, log_plot_area,
    zimmeranzahl, house_age
  )

m1_naive_both <- lm(log_price ~ dist_primary_km + dist_secondary_km, data = df_reg_16)

m2_base_both <- lm(
  log_price ~ dist_primary_km + dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_16
)

m3_poly_both <- lm(
  log_price ~ dist_primary_km + I(dist_primary_km^2) +
    dist_secondary_km + I(dist_secondary_km^2) +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_16
)

m4_base_primary <- lm(
  log_price ~ dist_primary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_16
)

m5_base_secondary <- lm(
  log_price ~ dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_16
)

models_16 <- list(
  "Naive (Both)"      = m1_naive_both,
  "Baseline (Both)"   = m2_base_both,
  "Polynomial (Both)" = m3_poly_both,
  "Baseline (Primary)"= m4_base_primary,
  "Baseline (Secondary)"= m5_base_secondary
)

coef_map_16 <- c(
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

tab_16 <- modelsummary(
  models_16,
  coef_map = coef_map_16,
  statistic = "({std.error})",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  fmt = 3,
  output = "gt",
  title = "Table 1. Baseline regressions (Dependent variable: log house price)"
) %>%
  gt::tab_options(
    table.font.size = gt::px(14),
    data_row.padding = gt::px(4)
  )

tab_16

### ==== 1.7 Visualisations (Descriptive) ====

df_plot <- df_main %>%
  mutate(
    price_per_sqm = kaufpreis / wohnflaeche,
    log_ppsqm = log(price_per_sqm)
  )

df_bins <- df_plot %>%
  transmute(
    log_ppsqm,
    primary_bin   = cut(dist_primary_km,   breaks = breaks_km, labels = labels_km, include.lowest = TRUE),
    secondary_bin = cut(dist_secondary_km, breaks = breaks_km, labels = labels_km, include.lowest = TRUE),
    any_bin       = cut(dist_any_km,       breaks = breaks_km, labels = labels_km, include.lowest = TRUE)
  ) %>%
  pivot_longer(ends_with("_bin"), names_to = "school_level", values_to = "dist_bin") %>%
  mutate(
    school_level = recode(
      school_level,
      primary_bin   = "Primary",
      secondary_bin = "Secondary",
      any_bin       = "Any school"
    ),
    dist_bin = factor(dist_bin, levels = labels_km)
  ) %>%
  filter(!is.na(log_ppsqm), !is.na(dist_bin))

df_bin_mean <- df_bins %>%
  group_by(school_level, dist_bin) %>%
  summarise(
    mean_log_ppsqm = mean(log_ppsqm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

fig_price_gradient <- ggplot(
  df_bin_mean,
  aes(x = dist_bin, y = mean_log_ppsqm, color = school_level, group = school_level)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Distance bin (km) — wider bins to mitigate centroid measurement error",
    y = "Mean log(price per sqm)",
    color = NULL
  ) +
  theme(legend.position = "top")

fig_box_primary <- ggplot(
  df_bins %>% filter(school_level == "Primary"),
  aes(x = dist_bin, y = log_ppsqm)
) +
  geom_boxplot(outlier.alpha = 0.2, fill = "grey80") +
  labs(
    x = "Distance to nearest primary school (km, binned)",
    y = "Log(price per sqm)",
    title = "Distribution of house prices by distance bins (Primary schools)"
  ) +
  theme(panel.grid.major.x = element_blank())

print(fig_price_gradient)
print(fig_box_primary)

## ==== 2. SOCIAL INDEX INTEGRATION =====

### ==== 2.1 Load & Clean Social Index ====
df_school_meta <- read_delim(
  path_school,
  delim = ";",
  locale = readr::locale(encoding = "Latin1", decimal_mark = ","),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  rename(
    school_id    = schulnummer,
    social_index_raw = sozialindexstufe
  ) %>%
  mutate(
    school_id = as.character(school_id),
    social_index = case_when(
      str_detect(as.character(social_index_raw), "^\\d+$") ~ 
        as.numeric(social_index_raw),
      !is.na(gemeinde) & str_detect(as.character(gemeinde), "^\\d+$") ~ 
        as.numeric(gemeinde),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(social_index), social_index != 0) %>%
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",
      social_index %in% 3:8 ~ "bad",
      TRUE ~ NA_character_
    ),
    school_quality = factor(school_quality, levels = c("bad", "good"))
  ) %>%
  select(-social_index_raw)

### ==== 2.2 Merge Social Index (nearest schools) ====
df_social <- df_housing %>%
  left_join(df_dist_primary,   by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any,       by = "ergg_1km") %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality),
            by = c("school_id_primary" = "school_id")) %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km, dist_primary_km, dist_secondary_km, social_index)

cat("Merged dataset dimensions (social):", dim(df_social), "\n")
summary(df_social$social_index)

### ==== 2.3 Within-Radius Quality (e.g., 5km) ====
radius_km <- 5

df_quality_radius <- raw_dist %>%
  filter(dist_km <= radius_km) %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = "school_id") %>%
  filter(!is.na(social_index), social_index %in% 1:8) %>%
  group_by(ergg_1km) %>%
  summarise(
    best_quality_5km  = min(as.character(school_quality), na.rm = TRUE),
    worst_quality_5km = max(as.character(school_quality), na.rm = TRUE),
    .groups = "drop"
  )

df_social_5km <- df_social %>%
  left_join(df_quality_radius, by = "ergg_1km")

### ==== 2.4 Models with School Quality ====
m6_quality_naive <- lm(log_price ~ school_quality, data = df_social_5km)

m7_quality_base <- lm(
  log_price ~ school_quality + log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_social_5km
)

m8_quality_dist <- lm(
  log_price ~ school_quality + dist_primary_km + dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_social_5km
)

m_quality_interaction <- lm(
  log_price ~ dist_primary_km * school_quality +
    dist_secondary_km * school_quality +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_social_5km
)

### ==== 2.5 Plots (Social index) ====
df_social_5km <- df_social_5km %>%
  mutate(social_index_f = factor(social_index, levels = 1:9, labels = as.character(1:9)))

ggplot(df_social_5km, aes(x = dist_primary_km, y = log_price, color = social_index_f)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Influence of Distance to Primary School on House Prices by Social Index Level",
    x = "Distance to Primary School (km)",
    y = "Log-House Price",
    color = "Social Index Level"
  ) +
  theme_minimal()

ggplot(df_social_5km, aes(x = dist_secondary_km, y = log_price, color = social_index_f)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Influence of Distance to Secondary School on House Prices by Social Index Level",
    x = "Distance to Secondary School (km)",
    y = "Log-House Price",
    color = "Social Index Level"
  ) +
  theme_minimal()

ggplot(df_social_5km, aes(x = dist_primary_km, y = log_price, color = school_quality)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Impact of Distance to Primary School on House Prices by School Quality",
    x = "Distance to Primary School (km)",
    y = "Log-House Price",
    color = "School Quality"
  ) +
  theme_minimal()
