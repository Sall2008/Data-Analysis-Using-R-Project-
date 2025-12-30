# --------------------------------------------------------
# STEP 1 (Distance only):
# Single-family homes + selected school types (02,04,10,15,20)
# Question: Is distance to the nearest relevant school associated with house prices?
# Output: regressions + binned distance effects + cutoff checks + plots
# --------------------------------------------------------

# ============================================================
# 0) Setup
# ============================================================

library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)

# --- Paths (use yours) ---
Housing_Prices   <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/CampusFile_HK_cities.xlsx"
Distance_Schools <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/distance_to_schools.csv"
Data_Schools     <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/school_data (1).xlsx"

stopifnot(file.exists(Housing_Prices), file.exists(Distance_Schools), file.exists(Data_Schools))

# ============================================================
# 1) Helper: robust numeric parsing + key cleaning
# ============================================================

to_num <- function(x) {
  x |>
    as.character() |>
    str_replace_all("\\s", "") |>
    str_replace_all("\\.", "") |>
    str_replace_all(",", ".") |>
    readr::parse_number(locale = readr::locale(decimal_mark = "."))
}

norm_key <- function(x) {
  x |>
    as.character() |>
    str_trim()
}

na_codes <- c("", "NA", "N/A", "Other missing", "Not specified", "Implausiblevalue",
              "-5", "-6", "-7", "-8", "-9", "-11")

# ============================================================
# 2) Read + clean HOUSING (SFH only)  [keep ergg_1km!]
# ============================================================

housing <- readxl::read_excel(Housing_Prices, na = na_codes) |>
  clean_names() |>
  filter(kategorie_haus %in% c("Single-family house (detached)", "Single-family house")) |>
  transmute(
    obid,
    ergg_1km = norm_key(ergg_1km),
    kaufpreis = to_num(kaufpreis),
    wohnflaeche = to_num(wohnflaeche),
    grundstuecksflaeche = to_num(grundstuecksflaeche),
    baujahr = as.integer(baujahr),
    zimmeranzahl = to_num(zimmeranzahl),
    objektzustand
  ) |>
  mutate(
    # plausibility
    kaufpreis = if_else(kaufpreis <= 0 | kaufpreis > 5e7, NA_real_, kaufpreis),
    wohnflaeche = if_else(wohnflaeche <= 0 | wohnflaeche > 2000, NA_real_, wohnflaeche),
    grundstuecksflaeche = if_else(
      !is.na(grundstuecksflaeche) & (grundstuecksflaeche <= 0 | grundstuecksflaeche > 10000),
      NA_real_, grundstuecksflaeche
    ),
    zimmeranzahl = if_else(!is.na(zimmeranzahl) & (zimmeranzahl <= 0 | zimmeranzahl > 50),
                           NA_real_, zimmeranzahl),
    ln_price = log(kaufpreis),
    ln_living_area = log(wohnflaeche)
  ) |>
  filter(!is.na(ergg_1km), ergg_1km != "-9", !is.na(ln_price), !is.na(ln_living_area))

# ============================================================
# 3) Read + filter SCHOOLS (only types 02,04,10,15,20)
# ============================================================

schools_sel <- readxl::read_excel(Data_Schools, na = na_codes) |>
  clean_names() |>
  mutate(
    school_id = as.character(school_id),
    school_type = str_pad(as.character(school_type), 2, pad = "0"),
    ergg_1km = norm_key(ergg_1km)
  ) |>
  filter(school_type %in% c("02", "04", "10", "15", "20")) |>
  select(school_id, school_type, ergg_1km) |>
  distinct()

# ============================================================
# 4) Distances: keep only distances to selected school types
#    then take nearest school per ergg_1km
# ============================================================

dist_nearest <- readr::read_csv(Distance_Schools, show_col_types = FALSE, na = na_codes) |>
  clean_names() |>
  mutate(
    ergg_1km = norm_key(ergg_1km),
    school_id = as.character(school_id),
    school_type = str_pad(as.character(school_type), 2, pad = "0"),
    nn_order = as.integer(nn_order),
    dist_km = to_num(dist) / 1000
  ) |>
  semi_join(schools_sel, by = c("school_id", "school_type", "ergg_1km")) |>
  filter(!is.na(dist_km), dist_km > 0) |>
  group_by(ergg_1km) |>
  slice_min(order_by = nn_order, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ergg_1km, school_id, school_type, dist_km)

# ============================================================
# 5) Build analysis sample (only where distance exists)
# ============================================================

analysis <- housing |>
  inner_join(dist_nearest, by = "ergg_1km") |>
  mutate(
    ln_dist = log(dist_km),
    
    # --- FINAL bins (the improved version you used)
    dist_bin = cut(
      dist_km,
      breaks = c(0, 0.5, 1, 2, 3, 5, 10, Inf),
      include.lowest = TRUE,
      right = TRUE
    )
  ) |>
  mutate(dist_bin = relevel(dist_bin, ref = levels(dist_bin)[1]))

# QC: coverage + bin sizes
cat("\n--- SAMPLE SIZES ---\n")
cat("Housing (SFH cleaned):", nrow(housing), "\n")
cat("Analysis (SFH + nearest selected-school distance):", nrow(analysis), "\n")
cat("Unique ergg_1km in housing:", dplyr::n_distinct(housing$ergg_1km), "\n")
cat("Unique ergg_1km with distance:", dplyr::n_distinct(analysis$ergg_1km), "\n\n")

cat("--- BIN COUNTS ---\n")
print(analysis |> count(dist_bin, sort = FALSE))

# ============================================================
# 6) Models
# ============================================================

# A) Continuous model: tests for a smooth/monotonic distance gradient
m_cont <- lm(
  ln_price ~ ln_dist + ln_living_area + baujahr + grundstuecksflaeche + zimmeranzahl + objektzustand,
  data = analysis
)
cat("\n--- MODEL A: ln(price) ~ ln(distance) + controls ---\n")
print(summary(m_cont))

# B) Binned model: tests non-linear / threshold effects by distance ranges
m_bins <- lm(
  ln_price ~ dist_bin + ln_living_area + baujahr + grundstuecksflaeche + zimmeranzahl + objektzustand,
  data = analysis
)
cat("\n--- MODEL B: ln(price) ~ distance bins + controls ---\n")
print(summary(m_bins))

# C) Cutoff checks: "within X km" dummy (robust)
cutoffs <- c(0.5, 1, 2, 3, 5, 10)

cutoff_table <- lapply(cutoffs, function(cut) {
  
  dat <- analysis |>
    mutate(within = as.integer(dist_km <= cut)) |>
    filter(!is.na(within))
  
  # Wenn within nicht beide Klassen hat, kann lm den Koeffizienten nicht schätzen
  if (dplyr::n_distinct(dat$within) < 2) {
    return(tibble(
      cutoff_km = cut,
      n = nrow(dat),
      share_within = mean(dat$within),
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      pct_effect = NA_real_
    ))
  }
  
  m <- lm(
    ln_price ~ within + ln_living_area + baujahr + grundstuecksflaeche + zimmeranzahl + objektzustand,
    data = dat
  )
  
  coefs <- summary(m)$coefficients
  
  # Sicher auslesen
  est <- unname(coef(m)["within"])
  se  <- sqrt(vcov(m)["within", "within"])
  p   <- coefs["within", "Pr(>|t|)"]
  
  tibble(
    cutoff_km = cut,
    n = nrow(dat),
    share_within = mean(dat$within),
    estimate = est,
    se = se,
    p_value = p,
    pct_effect = 100 * (exp(est) - 1)
  )
}) |>
  bind_rows()

cat("\n--- CUTOFF RESULTS (within X km) ---\n")
print(cutoff_table)
# ============================================================
# 7) Simple visuals
# ============================================================

# 7.1 Raw relationship (restricted x-axis for readability)
p1 <- ggplot(analysis, aes(x = dist_km, y = kaufpreis)) +
  geom_point(alpha = 0.07) +
  geom_smooth(method = "loess", se = TRUE, color = "#2C7FB8") +
  coord_cartesian(xlim = c(0, 6)) +
  labs(
    title = "House price vs. distance to nearest selected school",
    subtitle = "Single-family homes; school types 02/04/10/15/20; x-axis limited to 0–6 km",
    x = "Distance (km)",
    y = "Purchase price"
  )

# 7.2 Mean log-price by distance bin (with 95% CI)
bin_stats <- analysis |>
  group_by(dist_bin) |>
  summarise(
    n = n(),
    mean_ln_price = mean(ln_price, na.rm = TRUE),
    se = sd(ln_price, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) |>
  mutate(
    lo = mean_ln_price - 1.96 * se,
    hi = mean_ln_price + 1.96 * se
  )

p2 <- ggplot(bin_stats, aes(x = dist_bin, y = mean_ln_price)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
  labs(
    title = "Mean log(price) by distance band",
    subtitle = "Single-family homes; nearest school of types 02/04/10/15/20",
    x = "Distance band (km)",
    y = "Mean log(price) (±95% CI)"
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(p1)
print(p2)

cat("\n--- Interpretation tips ---\n")
cat("Model A tests a smooth monotonic gradient (often too restrictive).\n")
cat("Model B allows non-linear effects: each bin compares to the closest (0–0.5km).\n")
cat("Cutoff table shows which distance thresholds (X km) matter most.\n")


# ============================================================
# STEP 2 (Distance x Social Index):
# Does the distance premium depend on the school's social index?
# ============================================================

Social_Index <- "C:/Users/lucwi/OneDrive/Dokumente/Studium/Master/2.Semester/Data Analysis Using R/2022_social_index.csv"
stopifnot(file.exists(Social_Index))

# ---------- 2.1 Read + clean Social Index ----------
# (robust delimiter handling like you did before)
ssi_try <- readr::read_delim(
  Social_Index,
  delim = ";",
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
) |>
  janitor::clean_names()

ssi <- if (ncol(ssi_try) == 1) {
  readr::read_delim(Social_Index, delim = ",", show_col_types = FALSE) |>
    janitor::clean_names()
} else {
  ssi_try
}
rm(ssi_try)

# Expect columns similar to: schulnummer, sozialindexstufe
# Standardize
ssi <- ssi |>
  transmute(
    school_id = as.character(schulnummer),
    social_index_value = as.numeric(sozialindexstufe)
  ) |>
  distinct()

# ---------- 2.2 Attach Social Index to the school matched in analysis ----------
# analysis already contains school_id from dist_nearest
analysis_si <- analysis |>
  mutate(school_id = as.character(school_id)) |>
  left_join(ssi, by = "school_id") |>
  mutate(
    social_index_c = social_index_value - mean(social_index_value, na.rm = TRUE)
  )

# QC: match rate
cat("\n--- SOCIAL INDEX MATCH RATE ---\n")
print(analysis_si |>
        summarise(
          n = n(),
          share_si = mean(!is.na(social_index_value))
        ))

# ---------- 2.3 Choose a robust "near" definition ----------
# Because your bins are highly imbalanced, define near/far by a cutoff that exists.
# Use 5 km as default (you already saw it works), and optionally 3 km.
analysis_si <- analysis_si |>
  mutate(
    within_3km = as.integer(dist_km <= 3),
    within_5km = as.integer(dist_km <= 5)
  )

# Quick check
cat("\n--- NEAR SHARE (should be >0) ---\n")
print(analysis_si |>
        summarise(
          share_within_3km = mean(within_3km),
          share_within_5km = mean(within_5km)
        ))

# ---------- 2.4 Models: main effect + interaction ----------
# Baseline (only SI, no interaction)
m_si0 <- lm(
  ln_price ~ within_5km + social_index_c +
    ln_living_area + baujahr + grundstuecksflaeche + zimmeranzahl + objektzustand,
  data = analysis_si |>
    filter(!is.na(social_index_value))
)

# Interaction: does "being near" matter more/less depending on SI?
m_si1 <- lm(
  ln_price ~ within_5km * social_index_c +
    ln_living_area + baujahr + grundstuecksflaeche + zimmeranzahl + objektzustand,
  data = analysis_si |>
    filter(!is.na(social_index_value))
)

cat("\n--- MODEL SI0: price ~ within_5km + social index + controls ---\n")
print(summary(m_si0))

cat("\n--- MODEL SI1: price ~ within_5km * social index + controls ---\n")
print(summary(m_si1))

# Optional: do the same with within_3km (small share -> may be noisy)
m_si1_3km <- lm(
  ln_price ~ within_3km * social_index_c +
    ln_living_area + baujahr + grundstuecksflaeche + zimmeranzahl + objektzustand,
  data = analysis_si |>
    filter(!is.na(social_index_value))
)

cat("\n--- MODEL SI1 (3km): price ~ within_3km * social index + controls ---\n")
print(summary(m_si1_3km))

# ---------- 2.5 Interpret interaction with simple predicted effects ----------
# What is the "near premium" at low vs high social index?
si_low  <- quantile(analysis_si$social_index_c, 0.10, na.rm = TRUE)
si_high <- quantile(analysis_si$social_index_c, 0.90, na.rm = TRUE)

b <- coef(m_si1)
near_premium_low  <- b["within_5km"] + b["within_5km:social_index_c"] * si_low
near_premium_high <- b["within_5km"] + b["within_5km:social_index_c"] * si_high

cat("\n--- IMPLIED NEAR PREMIUM (within 5km) ---\n")
cat("At low SI (10th pct):  ", round(100 * (exp(near_premium_low) - 1), 1), "%\n")
cat("At high SI (90th pct): ", round(100 * (exp(near_premium_high) - 1), 1), "%\n")

# ---------- 2.6 Simple plot: predicted near premium by SI ----------
pred_grid <- tibble(
  social_index_c = seq(
    min(analysis_si$social_index_c, na.rm = TRUE),
    max(analysis_si$social_index_c, na.rm = TRUE),
    length.out = 100
  )
) |>
  mutate(
    near_effect = b["within_5km"] + b["within_5km:social_index_c"] * social_index_c,
    near_effect_pct = 100 * (exp(near_effect) - 1)
  )

ggplot(pred_grid, aes(x = social_index_c, y = near_effect_pct)) +
  geom_line(linewidth = 1.2, color = "#2C7FB8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Estimated 'near school' premium vs. school social index",
    subtitle = "Near defined as within 5 km; premium derived from interaction model",
    x = "Social index (centered)",
    y = "Near premium (%)"
  )
