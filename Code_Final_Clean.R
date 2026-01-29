# ==== 1. Data Preparation ====
## ==== 1.1 Packages & Global Theme ====
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
library(readr)
library(broom)
library(dplyr)
library(tidyr)
library(kableExtra)
library(knitr)
library(interactions)
library(emmeans)

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

## ==== 1.2 Define Paths & Constants ====
path_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_school  <- "course_data/school_data/2022_social_index.csv"
path_dist    <- "course_data/school_data/distance_to_schools.csv"
path_VG250   <- "course_data/VG250/vg250_ebenen_0101/VG250_GEM.shp"

year_ref <- 2022

type_primary   <- c("02")
type_secondary <- c("04", "10", "14", "15", "20")
type_all       <- c("02", "04", "10", "14", "15", "20")

# Distance binning
breaks_km <- c(0, 3, 6, 9, Inf)
labels_km <- c("0-3", "3-6", "6-9", ">9")

## ==== 1.3 Load & Clean Housing Data ====
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

## ==== 1.4 Load Distance Data & Nearest-School Helper ====
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

## ==== 1.5 Merge Housing + Distances + Derived Variables ====
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

## ==== 1.6 Social Index Preparation  ====

df_school_meta <- read_delim(path_school, 
                             delim = ";",
                             locale = readr::locale(encoding = "Latin1", decimal_mark = ","),
                             show_col_types = FALSE) %>%
  clean_names() %>%
  rename(school_id = schulnummer, social_index = sozialindexstufe) %>%
  mutate(
    school_id = as.character(school_id),  
    social_index = parse_number(as.character(social_index)) 
  ) %>%
  filter(social_index != 0) %>%  
  mutate(
    social_index = if_else(!is.na(gemeinde) & str_detect(gemeinde, "^\\d+$"), 
                           parse_number(gemeinde), 
                           social_index)
  ) %>%
  mutate(
    school_quality = case_when(
      social_index %in% 1:2 ~ "good",     
      social_index %in% 3:4 ~ "average",   
      social_index %in% 5:8 ~ "bad",       
      TRUE ~ "unknown"
    )
  )


# 1.6.1 Aggregate housing data to unique `ergg_1km` level
df_housing_clean_unique <- df_housing %>%
  group_by(ergg_1km) %>%
  slice_max(order_by = kaufpreis, n = 1, with_ties = FALSE) %>%
  ungroup()

# 1.6.2 Merge distance data with school meta data (Ensure school_quality and social_index are included)
df_final_with_social <- df_housing_clean_unique %>%
  left_join(df_dist_primary, by = "ergg_1km") %>%
  left_join(df_dist_secondary, by = "ergg_1km") %>%
  left_join(df_dist_any, by = "ergg_1km") %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = c("school_id_primary" = "school_id")) %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = c("school_id_secondary" = "school_id")) %>%
  left_join(df_school_meta %>% select(school_id, social_index, school_quality), by = c("school_id_any" = "school_id"))

# 1.6.3 Remove incomplete cases (Ensure that social_index columns from joins are considered)
df_final_with_social <- df_final_with_social %>%
  drop_na(kaufpreis, wohnflaeche, ergg_1km, dist_primary_km, dist_secondary_km, 
          dist_any_km, school_id_primary, school_id_secondary, school_id_any, social_index)

# 1.6.3 Diagnostics: dataset dimensions and social index summary
cat("Merged Dataset Dimensions:", dim(df_final_with_social), "\n")
summary(df_final_with_social$social_index)

# 1.6.4 Check for duplicates by `ergg_1km`
duplicates_check <- df_final_with_social %>%
  group_by(ergg_1km) %>%
  filter(n() > 1) %>%
  ungroup()
cat("Duplicates (if any):", nrow(duplicates_check), "\n")

# Adding another variable
df_final <- df_final_with_social

## ==== 1.7 Queens Distance Preparation  ====

# Adding other datasets
path_sale_housing <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
path_sale_flats <- "course_data/housing_data/cross_section/CampusFile_WK_2022.csv"
path_rent_flats <- "course_data/housing_data/cross_section/CampusFile_WM_2022.csv"

# Clean Houses for Sale Data 
raw_housing_sale <- read_delim(
  path_sale_housing, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

# Clean Flats for Sale Data
raw_flats_sale <- read_delim(
  path_sale_flats, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

# Clean Flats for Rent Data
raw_flats_rent <- read_delim(
  path_rent_flats, 
  delim = ",", 
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", "", "Implausible value", "Other missing"),
  show_col_types = FALSE
)

# NRW Cells Only
raw_housing_sale_NRW <- raw_housing_sale %>% filter(blid == "North Rhine-Westphalia")
raw_flats_sale_NRW <- raw_flats_sale %>% filter(blid == "North Rhine-Westphalia")
raw_flats_rent_NRW <- raw_flats_rent %>% filter(blid == "North Rhine-Westphalia")

# Building a more comprehensive grid
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

# Identifying school cells in that grid
school_cells_primary <- raw_dist %>%
  filter(
    school_type %in% type_primary,
    nn_order == 1,
    dist_km <= 0.75
  ) %>%
  distinct(ergg_1km, school_id) %>% 
  separate(
    ergg_1km,
    into = c("x","y"),
    sep = "_",
    convert = TRUE
  ) %>%
  filter(!is.na(x), !is.na(y)) %>%
  mutate(school_level = "primary")

school_cells_secondary <- raw_dist %>%
  filter(
    school_type %in% type_secondary,
    nn_order == 1,
    dist_km <= 0.75
  ) %>%
  distinct(ergg_1km, school_id) %>% 
  separate(
    ergg_1km,
    into = c("x","y"),
    sep = "_",
    convert = TRUE
  ) %>%
  filter(!is.na(x), !is.na(y)) %>%
  mutate(school_level = "secondary")

# Combine primary and secondary
school_cells_all <- bind_rows(
  school_cells_primary,
  school_cells_secondary
)

# Create cells
cells_big <- cells_big %>%
  mutate(
    has_primary = paste(x,y) %in%
      paste(
        school_cells_primary$x,
        school_cells_primary$y
      ),
    has_secondary = paste(x,y) %in%
      paste(
        school_cells_secondary$x,
        school_cells_secondary$y
      )
  )

# Sanity check 
# How many schools per cell?
school_cells_all %>%
  count(x, y) %>%
  summarise(
    max_schools_per_cell = max(n),
    mean_schools_per_cell = mean(n)
  )

# Any missing IDs?
stopifnot(!anyNA(school_cells_all$school_id))

# Building Grid
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

# FINAL cell IDs 
cells_big <- cells_big %>%
  mutate(cell_id = row_number())

# FINAL school sources
school_sources <- cells_big %>%
  inner_join(
    school_cells_all,
    by = c("x","y")
  ) %>%
  select(cell_id, school_id, school_level)

# Hard safety checks
stopifnot(length(nb_queen) == nrow(cells_big))
stopifnot(max(school_sources$cell_id) <= nrow(cells_big))

### ==== 1.7.1 Diagnostics ====

summary(card(nb_queen))
n.comp.nb(nb_queen)$nc
table(spdep::card(nb_queen))

# ==== 2. Analysis ====

df_reg_dist <- df_main %>%
  drop_na(
    log_price,
    dist_primary_km, dist_secondary_km,
    dist_primary_bin, dist_secondary_bin,
    log_area, log_plot_area,
    zimmeranzahl, house_age
  )

# Modellbasis 
base_formula <- log_price ~
  dist_primary_km * school_quality +
  dist_secondary_km * school_quality +
  log_area + log_plot_area +
  zimmeranzahl + house_age

# School Quality as Factor 
df_reg <- df_final_with_social %>%
  mutate(school_quality = factor(school_quality, levels = c("good", "average", "bad"))) 

# Maximum Distance of 5km 
df_reg <- df_reg %>%
  mutate(
    dist_primary_km = if_else(dist_primary_km > 5, 5, dist_primary_km),
    dist_secondary_km = if_else(dist_secondary_km > 5, 5, dist_secondary_km)
  )

## ==== 2.1 Distance ====

### ==== 2.1.1 Continuous distance specifications ====
m1_naive_both_cont <- lm(log_price ~ dist_primary_km + dist_secondary_km, 
                         data = df_reg_dist)

m2_base_both_cont <- lm(
  log_price ~ dist_primary_km + dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

m3_poly_both_cont <- lm(
  log_price ~ dist_primary_km + I(dist_primary_km^2) +
    dist_secondary_km + I(dist_secondary_km^2) +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

m4_base_primary_cont <- lm(
  log_price ~ dist_primary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

m5_base_secondary_cont <- lm(
  log_price ~ dist_secondary_km +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

### ==== 2.1.2 Binned distance specifications ====
m6_naive_both_bin <- lm(log_price ~ dist_primary_bin + dist_secondary_bin, 
                        data = df_reg_dist)

m7_base_both_bin <- lm(
  log_price ~ dist_primary_bin + dist_secondary_bin +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

m8_base_primary_bin <- lm(
  log_price ~ dist_primary_bin +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

m9_base_secondary_bin <- lm(
  log_price ~ dist_secondary_bin +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_reg_dist
)

###  ==== 2.1.3 Diagnostics Distance ==== 
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

### ==== 2.1.4 Model comparison Distance ====
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
fold_id <- sample(rep(1:k, length.out = nrow(df_reg_dist)))

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
    data = df_reg_dist, fold_id = fold_id
  ) %>% mutate(Model = "Baseline (Continuous)"),
  
  cv_one_model(
    log_price ~ dist_primary_bin + dist_secondary_bin +
      log_area + log_plot_area + zimmeranzahl + house_age,
    data = df_reg_dist, fold_id = fold_id
  ) %>% mutate(Model = "Baseline (Binned)")
) %>%
  select(Model, everything()) %>%
  mutate(
    across(c(RMSE, MAE), ~ round(.x, 3)),
    R2_oos = round(R2_oos, 3)
  )


## ==== 2.2 Social Index ====

### ==== 2.2.1 Social Index Regression ====

# Base Model Social Index without groups 
Base_Model_Social_Index <- lm(
  log_price ~ social_index +
    dist_primary_km + dist_secondary_km +
    log_area + log_plot_area +
    zimmeranzahl + house_age,
  data = df_final_with_social
)
summary(Base_Model_Social_Index)

# Model with Reference "good"
df_reg1 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "good"))

m_good_ref <- lm(base_formula, data = df_reg1)
summary(m_good_ref)

# Model with Reference "average"
df_reg2 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "average"))

m_avg_ref <- lm(base_formula, data = df_reg2)
summary(m_avg_ref)

# Model with Reference "bad"
df_reg3 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "bad"))

m_bad_ref <- lm(base_formula, data = df_reg3)
summary(m_bad_ref)


###  ==== 2.2.2 Diagnostics Social Index ====  
get_model_diagnostics <- function(model) {
  n <- nobs(model)  # Anzahl der Beobachtungen
  
  bp_p <- bptest(model)$p.value
  
  vif_max <- tryCatch(
    max(as.numeric(car::vif(model)), na.rm = TRUE),
    error = function(e) NA_real_
  )
  
  cook <- cooks.distance(model)
  cook_thr <- 4 / n  # Schwellenwert für Cook's Distance
  
  tibble(
    BP_pvalue    = bp_p,
    Max_VIF      = vif_max,
    Cook_n_gt_4n = sum(cook > cook_thr, na.rm = TRUE),
    Cook_max     = max(cook, na.rm = TRUE)
  )
}

diag_robust <- bind_rows(
  get_model_diagnostics(m_good_ref) %>% mutate(Model = "Good Reference"),
  get_model_diagnostics(m_avg_ref) %>% mutate(Model = "Average Reference"),
  get_model_diagnostics(m_bad_ref) %>% mutate(Model = "Bad Reference")
) %>%
  select(Model, everything()) %>%
  mutate(
    BP_pvalue = signif(BP_pvalue, 3),
    Max_VIF   = round(Max_VIF, 2),
    Cook_max  = signif(Cook_max, 3)
  )

print(diag_robust)

## ==== 2.3 Queens Distance ====

### ==== 2.3.1 Compute Queens contiguity ====

school_sources_primary <- school_sources %>%
  filter(school_level == "primary")

school_sources_secondary <- school_sources %>%
  filter(school_level == "secondary")


# Define function
compute_qdist_school_level <- function(nb, school_sources, n_cells) {
  
  q_dist  <- rep(NA_integer_, n_cells)
  ref_sid <- rep(NA_integer_, n_cells)
  
  frontier <- school_sources$cell_id
  q_dist[frontier]  <- 0
  ref_sid[frontier] <- school_sources$school_id
  
  k <- 0
  
  while (length(frontier) > 0) {
    k <- k + 1
    new_frontier <- integer(0)
    
    for (i in frontier) {
      neigh <- nb[[i]]
      to_set <- neigh[is.na(q_dist[neigh])]
      
      if (length(to_set) > 0) {
        q_dist[to_set]  <- k
        ref_sid[to_set] <- ref_sid[i]
        new_frontier <- c(new_frontier, to_set)
      }
    }
    
    frontier <- unique(new_frontier)
  }
  
  tibble(
    cell_id = seq_len(n_cells),
    q_dist = q_dist,
    ref_school_id = ref_sid
  )
}

# Apply function for primary schools
qdist_primary <- compute_qdist_school_level(
  nb = nb_queen,
  school_sources = school_sources_primary,
  n_cells = nrow(cells_big)
) %>%
  rename(
    q_dist_primary = q_dist,
    ref_school_primary = ref_school_id
  )

# Apply function for secondary schools
qdist_secondary <- compute_qdist_school_level(
  nb = nb_queen,
  school_sources = school_sources_secondary,
  n_cells = nrow(cells_big)
) %>%
  rename(
    q_dist_secondary = q_dist,
    ref_school_secondary = ref_school_id
  )

cells_big <- cells_big %>%
  left_join(qdist_primary,  by = "cell_id") %>%
  left_join(qdist_secondary, by = "cell_id")

# Merge back
cells_big <- cells_big %>%
  left_join(
    df_school_meta %>%
      select(school_id, social_index),
    by = c("ref_school_primary" = "school_id")
  ) %>%
  rename(social_index_primary = social_index)

cells_big <- cells_big %>%
  left_join(
    df_school_meta %>%
      select(school_id, social_index),
    by = c("ref_school_secondary" = "school_id")
  ) %>%
  rename(social_index_secondary = social_index)

# Merge back
df_final <- df_final %>%
  select(
    -matches("^q_dist"),
    -matches("^social_index"),
    -matches("^school_quality")
  ) %>%
  separate(
    ergg_1km,
    into = c("x","y"),
    sep = "_",
    convert = TRUE,
    remove = FALSE
  ) %>%
  left_join(
    cells_big %>%
      select(
        x, y,
        q_dist_primary,
        q_dist_secondary,
        social_index_primary,
        social_index_secondary
      ),
    by = c("x","y")
  )

names(df_final)[grepl("index|quality|q_dist", names(df_final))]



### ==== 2.3.2 Regressions ====

#### ==== 2.3.2.1 Distance ====

# Primary Schools
model_primary_all <- lm(
  log_price ~ q_dist_primary +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model_primary_all)

# Secondary Schools 
model_secondary_all <- lm(
  log_price ~ q_dist_secondary +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model_secondary_all)

#### ==== 2.3.2.2 Social Index ====

# Continuous Regression
model_index <- lm(
  log_price ~
    q_dist_primary * social_index_primary +
    q_dist_secondary * social_index_secondary +
    log_area + log_plot_area + zimmeranzahl + house_age,
  data = df_final
)

summary(model_index)

# Regression on categorized index 

# Creating the categories
df_final <- df_final %>%
  mutate(
    school_quality_primary = case_when(
      social_index_primary %in% 1:2 ~ "good",
      social_index_primary %in% 3:4 ~ "average",
      social_index_primary %in% 5:8 ~ "bad",
      TRUE ~ NA_character_
    ),
    school_quality_secondary = case_when(
      social_index_secondary %in% 1:2 ~ "good",
      social_index_secondary %in% 3:4 ~ "average",
      social_index_secondary %in% 5:8 ~ "bad",
      TRUE ~ NA_character_
    )
  )

# Model basis
base_formula_qc <- log_price ~
  q_dist_primary * school_index_primary +
  q_dist_secondary * school_index_secondary +
  log_area + log_plot_area +
  zimmeranzahl + house_age

# Reference school quality good
df_final1 <- df_final %>%
  mutate(school_quality = factor(school_quality),
         school_quality = relevel(school_quality, ref = "good")
  )

m_good_ref <- lm(base_formula_qc, data = df_final1)
summary(m_good_ref)

# Reference school quality average  
df_final2 <- df_final %>%
  mutate(school_quality = factor(school_quality),
         school_quality = relevel(school_quality, ref = "average"))

m_avg_ref <- lm(base_formula_qc, data = df_final2)
summary(m_avg_ref)


# Reference school quality bad 
df_final3 <- df_final %>%
  mutate(school_quality = factor(school_quality),
         school_quality = relevel(school_quality, ref = "bad"))

m_bad_ref <- lm(base_formula_qc, data = df_final3)
summary(m_bad_ref)

### ==== 2.3.3 Diagnostics ====

# Check if the correct social index is used (primary)
check_ref_primary <- cells_big %>%
  select(
    cell_id,
    x, y,
    q_dist_primary,
    ref_school_primary,
    social_index_primary
  ) %>%
  left_join(
    df_school_meta %>%
      select(school_id, social_index),
    by = c("ref_school_primary" = "school_id")
  ) %>%
  rename(
    social_index_from_meta = social_index
  )


summary(
  check_ref_primary$social_index_primary ==
    check_ref_primary$social_index_from_meta
)

# Check if the correct social index is used (secondary)
check_ref_secondary <- cells_big %>%
  select(
    cell_id,
    x, y,
    q_dist_secondary,
    ref_school_secondary,
    social_index_secondary
  ) %>%
  left_join(
    df_school_meta %>%
      select(school_id, social_index),
    by = c("ref_school_secondary" = "school_id")
  ) %>%
  rename(
    social_index_from_meta = social_index
  )

summary(
  check_ref_secondary$social_index_secondary ==
    check_ref_secondary$social_index_from_meta
)


## ==== 3. Results ====

### ==== 3.1 Distance Graphics and Tables ====

#### ==== 3.1.1 Table 1: Continuous specifications only ====
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
  statistic = NULL,
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

# Quarto version
tab_ols_cont2 <- modelsummary(
  models_cont,
  vcov      = "HC1",
  coef_map  = coef_map_cont,
  statistic = NULL,
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  fmt       = 3,
  output    = "kableExtra",
  title     = "Table 1. OLS regressions (log house price): 
  Continuous distance specifications"
) %>%
  kableExtra::kable_styling(font_size = 7, latex_options = c("scale_down", "hold_position"))

tab_ols_cont2

#### ==== 3.1.2 Table 2: Binned specifications only ====
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

#### ==== 3.1.3  Table 3: Baseline continuous vs baseline binned ====
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


#### ==== 3.1.4  Table 4: Model comparison ====
# Combined comparison table
compare_all <- fit_main %>%
  left_join(cv_main, by = "Model") %>%
  select(Model, N, R2, Adj_R2, AIC, BIC, RMSE, MAE, R2_oos)

tab_compare_all <- compare_all %>%
  gt() %>%
  tab_header(
    title    = "Table 4. Model comparison",
    subtitle = "In-sample fit and 5-fold out-of-sample performance"
  )%>%
  tab_source_note(
    md("**In-sample:** higher R²/Adj. R² is better; lower AIC/BIC is better.  
       **Out-of-sample (5-fold CV):** lower RMSE/MAE is better; 
       higher R² is better.")
  )%>%
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

#### ==== 3.1.5  Table 5: Robustness checks ====
tab_robust_checks <- diag_robust %>%
  mutate(
    BP_pvalue = if_else(is.na(BP_pvalue), NA_character_,
                        if_else(BP_pvalue < 0.001, "<0.001", 
                                sprintf("%.3f", BP_pvalue))),
    Max_VIF   = if_else(is.na(Max_VIF), NA_character_, 
                        sprintf("%.2f", Max_VIF)),
    Cook_max  = if_else(is.na(Cook_max), NA_character_, 
                        sprintf("%.3f", Cook_max)),
    Cook_n_gt_4n = as.integer(Cook_n_gt_4n)
  ) %>%
  gt() %>%
  tab_header(
    title    = "Table 5. Robustness checks (main models)",
    subtitle = "Diagnostics summary 
    (heteroskedasticity, multicollinearity, influence)"
  ) %>%
  tab_source_note(
    md("Inference uses **HC1 robust SE**.  
       Lower BP p-values indicate heteroskedasticity; 
       higher VIF indicates collinearity; 
       larger Cook's D indicates influential points.")
  ) %>%
  cols_label(
    Model        = "Model",
    BP_pvalue    = "BP p-value",
    Max_VIF      = "Max VIF",
    Cook_n_gt_4n = "Influential (D>4/n)",
    Cook_max     = "Max Cook's D"
  ) %>%
  cols_align(
    align = "left",
    columns = Model
  ) %>%
  cols_align(
    align = "center",
    columns = c(BP_pvalue, Max_VIF, Cook_n_gt_4n, Cook_max)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Model)
  ) %>%
  tab_style(
    style = cell_text(color = "grey35"),
    locations = cells_body(columns = 
                             c(BP_pvalue, Max_VIF, Cook_n_gt_4n, Cook_max))
  ) %>%
  tab_options(
    table.font.size  = gt::px(14),
    data_row.padding = gt::px(5)
  )

tab_robust_checks

#### ==== 3.1.6 Plot 1: Binned Price Gradients ====
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
    se_log_ppsqm   = sd(log_ppsqm, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Global y-axis limits for log(price per sqm)
y_limits <- range(
  df_main$log_ppsqm,
  finite = TRUE
)

y_pad <- 0.05 * diff(y_limits)

y_limits <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)

fig_price_gradient <- ggplot(
  df_bin_mean,
  aes(x = dist_bin, y = mean_log_ppsqm, 
      color = school_level, group = school_level)
) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_log_ppsqm - 1.96*se_log_ppsqm, 
                    ymax = mean_log_ppsqm + 1.96*se_log_ppsqm),
                width = 0.1, alpha = 0.5) +
  scale_color_manual(values = pal_muted) +
  scale_y_continuous(limits = y_limits) + 
  labs(
    title = "Binned relationship: mean prices by distance categories",
    x = "Distance bin (km)",
    y = "Mean Log(price per sqm)", 
    color = NULL
  ) +
  theme_minimal(base_size = 18) + 
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey35"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(fig_price_gradient)

#### ==== 3.1.7 Plot 2: Continuous Distance vs Log Price per sqm ====
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
  scale_y_continuous(limits = y_limits) +
  labs(
    x = "Distance to nearest school (km)",
    y = "Log(price per sqm)",
    title = 
      "Continuous relationship: prices vs distance (Primary vs Secondary)",
    color = NULL
  ) +
  theme(legend.position = "top")

print(fig_scatter_combined)



### ==== 3.2 Social Index Graphics and Tables ====

#### ==== 3.2.1  Table 6: Social Index Regression (5km) ====

df_reg1 <- df_reg %>%
  mutate(school_quality = relevel(school_quality, ref = "good"))

m1_reg1 <- lm(
  base_formula,
  data = df_reg1
)

# Coefficient labels für kontinuierliche Variablen und Interaktionen
coef_map <- c(
  "dist_primary_km"                = "Distance to primary school (km)",
  "dist_secondary_km"              = "Distance to secondary school (km)",
  "log_area"                       = "Log living area",
  "log_plot_area"                  = "Log plot area",
  "zimmeranzahl"                   = "Rooms",
  "house_age"                      = "House age",
  "school_qualityaverage"          = "School Quality: Average",
  "school_qualitybad"              = "School Quality: Bad",
  "dist_primary_km:school_qualityaverage" = "Interaction: Distance to primary school * Average School Quality",
  "dist_primary_km:school_qualitybad"    = "Interaction: Distance to primary school * Bad School Quality",
  "school_qualityaverage:dist_secondary_km" = "Interaction: Distance to secondary school * Average School Quality",
  "school_qualitybad:dist_secondary_km"    = "Interaction: Distance to secondary school * Bad School Quality"
)

# Modelle, die du in der Tabelle anzeigen möchtest
models <- list(
  "Reference: Good School Quality" = m1_reg1
)

# Erstellen der Tabelle
tab_ols <- modelsummary(
  models,
  vcov      = "HC1",  
  coef_map  = coef_map,  
  statistic = NULL,  
  stars     = c("*" = .1, "**" = .05, "***" = .01), 
  gof_map   = c("nobs", "r.squared", "adj.r.squared"), 
  fmt       = 3,  
  output    = "gt",  
  title     = "Table 1. Regression Results with School Quality Interactions"
) %>%
  gt::tab_options(
    table.font.size   = gt::px(14),  
    data_row.padding  = gt::px(4)    
  )

# Tabelle anzeigen
tab_ols


#### ==== 3.2.2 Plot 3: Spatial Distribution Social Index Schools in NRW ====

df_grid <- df_final_with_social %>%
  distinct(ergg_1km, social_index) %>%      
  separate(
    ergg_1km,
    into = c("lon", "lat"),
    sep  = "_",          
    remove = FALSE,
    convert = TRUE       
  )

head(df_grid)

spa_dis <- ggplot(df_grid, aes(x = lon, y = lat)) +
  geom_point(aes(color = social_index), size = 0.8, alpha = 0.8) +
  scale_color_viridis_c(name = "Social Index") +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of School Social Index in NRW",
    subtitle = "Each grid cell colored by the social index of the nearest school"
  )


#### ==== 3.2.3 Plot 4: Bar chart Social Index ====

bar_chart_index <- df_school_meta %>%
  ggplot(aes(x = factor(social_index))) +
  geom_bar() +
  labs(x = "Social Index (1–9)", y = "Number of schools")


#### ==== 3.2.4 Plot 5: Simple Slopes for Primary & Secondary Schools ====

# Modell mit Schulqualität "good"
m_good_ref <- lm(base_formula, data = df_reg1)

# Simple Slopes Analyse für dist_primary_km
interact_plot(m_good_ref, 
              pred = dist_primary_km, 
              modx = school_quality, 
              modx.values = c("good", "average", "bad"),
              plot.points = TRUE,
              main.title = "Simple Slopes Analysis for `dist_primary_km` by School Quality")

# Simple Slopes Analyse für dist_secondary_km
interact_plot(m_good_ref, 
              pred = dist_secondary_km, 
              modx = school_quality, 
              modx.values = c("good", "average", "bad"),
              plot.points = TRUE,
              main.title = "Simple Slopes Analysis for `dist_secondary_km` by School Quality")



### ==== 3.3 Queens Distance Graphics and Tables ====

#### ==== 3.3.1 Table 7: Distance only ====

# Helper: significance stars
stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.1  ~ "*",
    TRUE     ~ ""
  )
}

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

#### ==== 3.3.2 Plot: 6 Map Primary Schools ====
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

#### ==== 3.3.3 Plot: 7 Map Secondary Schools ====
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

#### ==== 3.3.4 Plot 8: Mean districts plot primary ====

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

#### ==== 3.3.5 Plot 9: Mean districts plot secondary ====

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

## ==== 4. Appendix ====

### ==== 4.1 Graphics for Limitations ====

#### ==== 4.1.1 Limitation: Distances ====
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

#### ==== 4.1.2 Limitation: Introduction Queens Distance ====
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

#### ==== 4.1.3 Limitation: Queens distance on a bigger scale ====
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
