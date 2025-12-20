# ==== 1. SETUP: Libraries & Global Theme ====

## ==== 1.1 Libraries ====
library(tidyverse)
library(fs)
library(scales)   # For currency/number formatting
library(ggrepel)  # For non-overlapping labels
library(patchwork) # For combining plots
library(gtsummary)
library(gt)

## ==== 1.2 Global Theme ====
# Define a unified theme for all plots
theme_style <- theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "sans", color = "#333333"),
    plot.title = element_text(
      face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)
      ),
    plot.subtitle = element_text(
      color = "grey50", size = 12, hjust = 0.5, margin = margin(b = 20)
      ),
    plot.caption = element_text(
      color = "grey60", size = 10, margin = margin(t = 15)
      ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Cleaner look for histograms
    axis.text = element_text(color = "grey40"),
    axis.title = element_text(face = "bold", margin = margin(r = 10))
  )

# Set global default theme
theme_set(theme_style)

# ==== 2. DATA LOADING ====

## ==== 2.1 Load Housing Data ====
housing_path <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"

raw_HK <- read_delim(
  housing_path,
  delim = ",",
  locale = locale(decimal_mark = "."),
  na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", ""),
  show_col_types = FALSE
) %>%
  # Keep only observations from North Rhine-Westphalia
  filter(blid == "North Rhine-Westphalia") %>%
  
  # Add filename for reference
  mutate(filename = path_file(housing_path))

## ==== 2.2 Load School Data ====

school_path <- "course_data/school_data/2022_social_index.csv"

raw_school <- read_delim(school_path, delim = ";", 
                        locale = locale(encoding = "Latin1", decimal_mark = ","),
                        show_col_types = FALSE)

## ==== 2.3 Load Distance Data ====
dist_path <- "course_data/school_data/distance_to_schools.csv"

raw_dist <- read_delim(dist_path, delim = ",", 
                      locale = locale(decimal_mark = ","), 
                      show_col_types = FALSE)

## ==== 2.4 Load Region Data
region_path <- "course_data/region_data/region_data.csv"
raw_region <- read_delim(region_path)
# ==== 3. DATA CLEANING ====

## ==== 3.1 Housing Data Cleaning ====

# Following RWI-GEO-RED v7 documentation, 
# implausible extreme values were censored according to official thresholds.

df_HK <- raw_HK %>%
  # 1) NA -> 0 for binary amenities
  dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c(
        "balkon", "garten", "aufzug", "einbaukueche",
        "gaestewc", "keller", "denkmalobjekt",
        "ferienhaus", "haustier_erlaubt",
        "rollstuhlgerecht", "parkplatz",
        "kaufvermietet", "foerderung"
      )),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  
  # 2) Parse numeric vars (robust)
  dplyr::mutate(
    dplyr::across(
      c(kaufpreis, wohnflaeche, grundstuecksflaeche, zimmeranzahl, baujahr, letzte_modernisierung),
      ~ readr::parse_number(
        dplyr::na_if(
          dplyr::na_if(as.character(.x), "Implausible value"),
          "Other missing"
        )
      )
    )
  ) %>%
  
  # 3) gid2019: to character + pad to 7 digits
  dplyr::mutate(
    gid2019 = stringr::str_pad(as.character(gid2019), width = 7, side = "left", pad = "0")
  ) %>%
  
  # 4) Censor extremes to NA
  dplyr::mutate(
    kaufpreis = dplyr::if_else(kaufpreis > 5e7, NA_real_, kaufpreis),
    wohnflaeche = dplyr::if_else(wohnflaeche > 10000, NA_real_, wohnflaeche),
    grundstuecksflaeche = dplyr::if_else(grundstuecksflaeche > 5000, NA_real_, grundstuecksflaeche),
    zimmeranzahl = dplyr::if_else(zimmeranzahl > 25, NA_real_, zimmeranzahl),
    baujahr = dplyr::if_else(baujahr < 1000 | baujahr > 2022, NA_real_, baujahr),
    letzte_modernisierung = dplyr::if_else(
      letzte_modernisierung < 1000 | letzte_modernisierung > 2022,
      NA_real_,
      letzte_modernisierung
    )
  ) %>%
  
  # 5) Remove duplicate spells: keep latest by 'adat' within each duplicateid group
  {
    dat <- .
    
    df2 <- dplyr::mutate(
      dat,
      dup_clean = dplyr::na_if(as.character(duplicateid), "Other missing"),
      year = as.integer(stringr::str_extract(as.character(adat), "^\\d{4}")),
      mon  = as.integer(stringr::str_extract(as.character(adat), "(?<=m)\\d{1,2}$")),
      adat_key = year * 12 + mon
    )
    
    df_na <- dplyr::filter(df2, is.na(dup_clean))
    
    df_non <- dplyr::filter(df2, !is.na(dup_clean)) %>%
      dplyr::group_by(dup_clean) %>%
      dplyr::slice_max(order_by = adat_key, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    
    out <- dplyr::bind_rows(df_na, df_non) %>%
      dplyr::select(-dup_clean, -year, -mon, -adat_key)
    
    out
  } %>%
  
  # 6) Feature engineering
  dplyr::mutate(
    price_per_sqm = dplyr::if_else(
      !is.na(kaufpreis) & kaufpreis > 0 & !is.na(wohnflaeche) & wohnflaeche > 0,
      kaufpreis / wohnflaeche,
      NA_real_
    ),
    log_price = dplyr::if_else(!is.na(kaufpreis) & kaufpreis > 0, log(kaufpreis), NA_real_),
    log_ppsqm = dplyr::if_else(!is.na(price_per_sqm) & price_per_sqm > 0, log(price_per_sqm), NA_real_),
    house_age = dplyr::if_else(!is.na(baujahr) & (2022 - baujahr) >= 0, 2022 - baujahr, NA_real_),
    renovated = dplyr::if_else(!is.na(letzte_modernisierung), 1, 0)
  )

## ==== 3.2 Prepare Distance Data for merge ====
# Prepare distance data: keep only the nearest school

df_dist <- raw_dist %>%
  # 1) Standardize join keys (avoid type mismatch in joins)
  mutate(
    ergg_1km = as.character(ergg_1km),
    school_ID = as.character(school_ID)
  ) %>%
  
  # 2) Clean and parse distance into numeric (handles comma/dot decimals + placeholders)
  mutate(
    dist_chr = as.character(dist),
    dist_chr = dplyr::na_if(dist_chr, "Implausible value"),
    dist_chr = dplyr::na_if(dist_chr, "Other missing"),
    dist_chr = stringr::str_trim(dist_chr),
    dist_chr = stringr::str_replace_all(dist_chr, ",", "."),
    dist_km  = readr::parse_number(dist_chr)
  ) %>%
  
  # 3) Remove observations without a valid distance
  filter(!is.na(dist_km)) %>%
  
  # 4) Basic plausibility filter for distances (optional but recommended)
  #    - Negative distances are invalid
  #    - Extremely large distances are very likely data artifacts
  filter(dist_km >= 0, dist_km <= 100) %>%
  
  # 5) Resolve duplicates / ties: choose the globally nearest school per grid cell
  #    - If multiple schools have the same minimum distance, break ties by school_ID
  arrange(ergg_1km, dist_km, school_ID) %>%
  group_by(ergg_1km) %>%
  slice(1) %>%
  ungroup() %>%
  
  # 6) Keep only variables needed for merging and analysis
  transmute(
    ergg_1km,
    school_ID_nearest = school_ID,
    school_type_nearest = school_type,
    dist_nearest_km = dist_km
  )

## ==== 3.3 Prepare school data for merge ====

df_school <- raw_school %>%
  # Ensure the school identifier is a character string (safe for joins)
  mutate(Schulnummer = as.character(Schulnummer)) %>%
  
  # Ensure social index is numeric (1-9); parse_number is robust to mixed formats
  mutate(Sozialindexstufe = readr::parse_number(as.character(Sozialindexstufe))) %>%
  
  # Keep exactly one row per school ID to avoid one-to-many joins
  distinct(Schulnummer, .keep_all = TRUE) %>%
  
  # Keep only the columns needed for analysis / slides
  transmute(
    school_ID = Schulnummer,                # rename to match df_dist naming convention
    social_index = Sozialindexstufe,         # rename to English variable name
    school_name = Kurzbezeichnung,           
    district = Bezirksregierung,             
    kreis = Kreis,                           
    gemeinde = Gemeinde                      
  )

## ==== 3.4 Prepare region data for merge ====
df_region <- raw_region %>%
  mutate(
    AGS = as.character(readr::parse_number(AGS))
  )
# ==== 4. DATA MERGE ====
# Merge nearest-school distance + school social index into housing data

df_final <- df_HK %>%
  # 1) Ensure the grid cell ID is a character string (safe for joins)
  dplyr::mutate(ergg_1km = as.character(ergg_1km)) %>%
  
  # 2) Merge nearest-school distance info by grid cell
  dplyr::left_join(df_dist, by = "ergg_1km") %>%
  
  # 3) Merge school info using the nearest school ID
  dplyr::left_join(df_school, by = c("school_ID_nearest" = "school_ID")) %>%
  
  # 4) Merge region info using AGS (region-level)
  dplyr::left_join(
    df_region %>% dplyr::select(AGS, region_type),
    by = c("gid2019" = "AGS")
  )

# ==== 5. MERGE QUALITY DIAGNOSTICS ====

# Goal: Decompose missingness after merging distance and school data
# Objective: Show missing values arise from data coverage, not coding errors

df_diag <- df_final %>%
  dplyr::mutate(
    # Missing distance information (no nearest school found in distance data)
    miss_dist = is.na(dist_nearest_km),
    
    # Missing school ID (a subset of missing distance)
    miss_school_id = is.na(school_ID_nearest),
    
    # Missing social index information
    miss_social_index = is.na(social_index),
    
    # Missing region info (gid2019 didn't match AGS)
    miss_region = is.na(region_type),
    
    # Urban indicator (only defined when region_type exists)
    is_urban = !miss_region & stringr::str_detect(region_type, "^Urban")
  ) %>%
  dplyr::mutate(
    missing_reason = dplyr::case_when(
      # Case 0: Region did not match (key issue / coverage issue)
      miss_region ~ "0: No region match (gid2019 not matched to AGS)",
      
      # Case 1: Region matched but not urban (sample restriction)
      !miss_region & !is_urban ~ "1: Non-urban region (excluded by design)",
      
      # Case 2: No distance info → no school matched at all
      miss_dist ~ "A: No distance info (grid not covered by distance data)",
      
      # Case 3: Distance exists, school exists, but no social index
      !miss_dist & !miss_school_id & miss_social_index ~
        "B: School has no social index (structural missingness)",
      
      # Case 4: Fully observed for core vars (before price/area restrictions)
      !miss_dist & !miss_social_index ~
        "C: Complete for dist & social index",
      
      TRUE ~ "D: Other / unexpected pattern"
    )
  ) %>%
  dplyr::count(missing_reason, name = "n") %>%
  dplyr::mutate(
    share = n / sum(n),
    share_pct = scales::percent(share, accuracy = 0.1)
  ) %>%
  dplyr::arrange(missing_reason) %>%
  dplyr::select(
    Reason = missing_reason,
    Observations = n,
    Share = share_pct
  )

print(df_diag)

# ==== 6. SAMPLE DEFINITION ====

## ==== 6.1 Main Analysis Sample ====
df_analysis <- df_final %>%
  dplyr::filter(
    !is.na(region_type),
    stringr::str_detect(region_type, "^Urban"),  # urban only (by design)
    
    !is.na(dist_nearest_km),
    !is.na(social_index),
    kaufpreis > 0,
    wohnflaeche > 0
  ) %>%
  dplyr::mutate(
    log_ppsqm = log(kaufpreis / wohnflaeche)
  )

df_analysis %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_price   = mean(kaufpreis, na.rm = TRUE),
    median_price = median(kaufpreis, na.rm = TRUE),
    mean_dist    = mean(dist_nearest_km, na.rm = TRUE),
    median_dist  = median(dist_nearest_km, na.rm = TRUE),
    mean_social  = mean(social_index, na.rm = TRUE),
    median_social= median(social_index, na.rm = TRUE)
  )

## ==== 6.2 Unrestricted version of urban (serving as robustness) ====
df_analysis_all <- df_final %>%
  dplyr::filter(
    !is.na(dist_nearest_km),
    !is.na(social_index),
    kaufpreis > 0,
    wohnflaeche > 0
  ) %>%
  dplyr::mutate(log_ppsqm = log(kaufpreis / wohnflaeche))

# ==== 7. DESCRIPTIVE ANALYSIS ====

## ==== 7.1 Build a descriptive dataset (bins + log outcome) ====
df_desc <- df_analysis %>%
  dplyr::mutate(
    # Distance bins for interpretability in slides/tables
    dist_bin = dplyr::case_when(
      dist_nearest_km < 0.5 ~ "<0.5 km",
      dist_nearest_km < 1.0 ~ "0.5–1 km",
      dist_nearest_km < 2.0 ~ "1–2 km",
      dist_nearest_km < 5.0 ~ "2–5 km",
      TRUE ~ "≥5 km"
    ),
    dist_bin = factor(dist_bin, levels = c("<0.5 km", "0.5–1 km", "1–2 km", "2–5 km", "≥5 km")),
    
    # Social index bins (assumes 1–9)
    social_bin = dplyr::case_when(
      social_index %in% 1:2 ~ "1–2",
      social_index %in% 3:4 ~ "3–4",
      social_index %in% 5:6 ~ "5–6",
      social_index %in% 7:9 ~ "7–9",
      TRUE ~ NA_character_
    ),
    social_bin = factor(social_bin, levels = c("1–2", "3–4", "5–6", "7–9"), ordered = TRUE),
    
    # Log-transform main outcome (consistent with regression specification)
    log_ppsqm = dplyr::if_else(!is.na(price_per_sqm) & price_per_sqm > 0,
                               log(price_per_sqm), NA_real_)
  ) %>%
  dplyr::select(
    kaufpreis, wohnflaeche, price_per_sqm, log_ppsqm,
    dist_nearest_km, dist_bin,
    social_index, social_bin,
    house_age, renovated
  )

## ==== 7.2 Presentation-ready overall descriptive table ====
tbl_overall <- df_desc %>%
  dplyr::select(
    kaufpreis, wohnflaeche, price_per_sqm, log_ppsqm,
    dist_nearest_km, social_index, house_age, renovated
  ) %>%
  gtsummary::tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) | med={median} | IQR={p25}, {p75}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      kaufpreis ~ 0,
      wohnflaeche ~ 1,
      price_per_sqm ~ 0,
      log_ppsqm ~ 3,
      dist_nearest_km ~ 2,
      social_index ~ 2,
      house_age ~ 1,
      renovated ~ 0
    ),
    missing = "no"
  ) %>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  gtsummary::modify_spanning_header(all_stat_cols() ~ "**Overall**") %>%
  gtsummary::bold_labels()

tbl_overall

## ==== 7.3 Grouped descriptive table by distance bins ====
tbl_by_dist <- df_desc %>%
  dplyr::select(
    dist_bin,
    log_ppsqm, price_per_sqm, dist_nearest_km, social_index,
    kaufpreis, wohnflaeche, house_age, renovated
  ) %>%
  gtsummary::tbl_summary(
    by = dist_bin,
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) | med={median}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      log_ppsqm ~ 3,
      price_per_sqm ~ 0,
      dist_nearest_km ~ 2,
      social_index ~ 2,
      kaufpreis ~ 0,
      wohnflaeche ~ 1,
      house_age ~ 1,
      renovated ~ 0
    ),
    missing = "no"
  ) %>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  gtsummary::bold_labels()

tbl_by_dist

## ==== 7.4 Grouped descriptive table by social index bins ====
tbl_by_social <- df_desc %>%
  dplyr::filter(!is.na(social_bin)) %>%
  dplyr::select(
    social_bin,
    log_ppsqm, price_per_sqm, dist_nearest_km, social_index,
    kaufpreis, wohnflaeche, house_age, renovated
  ) %>%
  gtsummary::tbl_summary(
    by = social_bin,
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) | med={median}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      log_ppsqm ~ 3,
      price_per_sqm ~ 0,
      dist_nearest_km ~ 2,
      social_index ~ 2,
      kaufpreis ~ 0,
      wohnflaeche ~ 1,
      house_age ~ 1,
      renovated ~ 0
    ),
    missing = "no"
  ) %>%
  gtsummary::modify_header(label ~ "**Variable**") %>%
  gtsummary::bold_labels()

tbl_by_social

## ==== 7.5 Percent differences vs baseline groups (exp(diff)-1) ====
# Baselines:
#   - Distance baseline: "<0.5 km"
#   - Social baseline: "1–2"

logdiff_to_pct <- function(delta_log) exp(delta_log) - 1

### ==== 7.5.1 Distance: compute group mean(log_ppsqm) and % diff vs baseline ====
tab_pct_dist <- df_desc %>%
  dplyr::group_by(dist_bin) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_log_ppsqm = mean(log_ppsqm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    base_mean_log = mean_log_ppsqm[dist_bin == "<0.5 km"][1],
    delta_log = mean_log_ppsqm - base_mean_log,
    pct_diff_vs_base = logdiff_to_pct(delta_log)
  ) %>%
  dplyr::select(dist_bin, n, mean_log_ppsqm, pct_diff_vs_base)

gt_dist <- tab_pct_dist %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Percent Differences in Mean log(€/sqm) by Distance Bin",
    subtitle = "Relative to baseline: <0.5 km (computed as exp(Δlog) - 1)"
  ) %>%
  gt::fmt_number(columns = mean_log_ppsqm, decimals = 3) %>%
  gt::fmt_percent(columns = pct_diff_vs_base, decimals = 1) %>%
  gt::cols_label(
    dist_bin = "Distance bin",
    n = "N",
    mean_log_ppsqm = "Mean log(€/sqm)",
    pct_diff_vs_base = "% diff vs baseline"
  )

gt_dist

### ==== 7.5.2 Social: compute group mean(log_ppsqm) and % diff vs baseline ====
tab_pct_social <- df_desc %>%
  dplyr::filter(!is.na(social_bin)) %>%
  dplyr::group_by(social_bin) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_log_ppsqm = mean(log_ppsqm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    base_mean_log = mean_log_ppsqm[social_bin == "1–2"][1],
    delta_log = mean_log_ppsqm - base_mean_log,
    pct_diff_vs_base = logdiff_to_pct(delta_log)
  ) %>%
  dplyr::select(social_bin, n, mean_log_ppsqm, pct_diff_vs_base)

gt_social <- tab_pct_social %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Percent Differences in Mean log(€/sqm) by Social Index Bin",
    subtitle = "Relative to baseline: 1–2 (computed as exp(Δlog) - 1)"
  ) %>%
  gt::fmt_number(columns = mean_log_ppsqm, decimals = 3) %>%
  gt::fmt_percent(columns = pct_diff_vs_base, decimals = 1) %>%
  gt::cols_label(
    social_bin = "Social index bin",
    n = "N",
    mean_log_ppsqm = "Mean log(€/sqm)",
    pct_diff_vs_base = "% diff vs baseline"
  )

gt_social

## ==== 7.6 Plots (log outcome everywhere for consistency) ====

p_hist_log <- ggplot(df_desc, aes(x = log_ppsqm)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Log Price per sqm",
    subtitle = "Log transformation reduces skewness and aligns with the regression specification",
    x = "Log price per sqm",
    y = "Count"
  )

p_scatter_dist <- ggplot(df_desc, aes(x = dist_nearest_km, y = log_ppsqm)) +
  geom_point(alpha = 0.15) +
  geom_smooth(se = TRUE) +
  labs(
    title = "Log Price per sqm vs Distance to Nearest School",
    subtitle = "Unconditional relationship; substantial heterogeneity can mask differences",
    x = "Distance (km)",
    y = "Log price per sqm",
    caption = "Smooth is descriptive (not causal) and does not control for confounders."
  )

p_box_dist <- ggplot(df_desc, aes(x = dist_bin, y = log_ppsqm)) +
  geom_boxplot(outlier.alpha = 0.15) +
  labs(
    title = "Log Price per sqm by Distance Bins",
    subtitle = "Bin comparisons are easy to communicate; differences can be small in raw descriptives",
    x = "Distance bin",
    y = "Log price per sqm"
  )

p_box_social <- ggplot(df_desc %>% dplyr::filter(!is.na(social_bin)),
                       aes(x = social_bin, y = log_ppsqm)) +
  geom_boxplot(outlier.alpha = 0.15) +
  labs(
    title = "Log Price per sqm by School Social Index Bins",
    subtitle = "Unconditional patterns can be muted due to housing and location heterogeneity",
    x = "Social index bin",
    y = "Log price per sqm"
  )

(p_hist_log / p_scatter_dist) | (p_box_dist / p_box_social)

