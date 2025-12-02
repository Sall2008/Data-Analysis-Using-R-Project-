# =======================================================
# 1. SETUP: Libraries & Global Theme
# =======================================================

library(tidyverse)
library(fs)
library(scales)   # For currency/number formatting
library(ggrepel)  # For non-overlapping labels
library(patchwork) # Optional: For combining plots

# Define a unified theme for all plots
theme_style <- theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "sans", color = "#333333"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey50", size = 12, hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_text(color = "grey60", size = 10, margin = margin(t = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Cleaner look for histograms
    axis.text = element_text(color = "grey40"),
    axis.title = element_text(face = "bold", margin = margin(r = 10))
  )

# Set global default theme
theme_set(theme_style)


# =======================================================
# 2. DATA LOADING & CLEANING
# =======================================================

# --- A. Housing Data ---
housing_path <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"

df_HK <- read_delim(housing_path, delim = ",", 
                    locale = locale(decimal_mark = "."), 
                    na = c("-5", "-6", "-7", "-8", "-9", "-11", "NA", ""),
                    show_col_types = FALSE) %>%
  filter(blid == "North Rhine-Westphalia") %>%
  mutate(filename = path_file(housing_path)) %>%
  # Robust number parsing
  mutate(grundstuecksflaeche_num = parse_number(as.character(grundstuecksflaeche)),
         wohnflaeche_num = parse_number(as.character(wohnflaeche)),
         kaufpreis_num = parse_number(as.character(kaufpreis))) %>%
  type_convert()

# --- B. School Data ---
school_path <- "course_data/school_data/2022_social_index.csv"

df_school <- read_delim(school_path, delim = ";", 
                        locale = locale(encoding = "Latin1", decimal_mark = ","),
                        show_col_types = FALSE)

# --- C. Distance Data ---
dist_path <- "course_data/school_data/distance_to_schools.csv"

df_dist <- read_delim(dist_path, delim = ",", 
                      locale = locale(decimal_mark = ","), 
                      show_col_types = FALSE)


# =======================================================
# 3. VISUALIZATIONS
# =======================================================

# -------------------------------------------------------
# Plot 1: Purchase Price (Raw Distribution)
# Goal: visualize the right-skewed market structure
# -------------------------------------------------------
p1_price_raw <- df_HK %>% 
  ggplot(aes(x = kaufpreis_num)) +
  geom_histogram(bins = 50, fill = "#2C3E50", color = "white", alpha = 0.9) +
  
  # Zoom in on 99% of data (hide extreme outliers without filtering them)
  coord_cartesian(xlim = c(0, quantile(df_HK$kaufpreis_num, 0.99, na.rm = TRUE))) +
  
  scale_x_continuous(labels = label_number(suffix = " €", scale = 1e-3, big.mark = ",")) +
  labs(title = "Distribution of Purchase Prices", 
       subtitle = "Zoomed in on main market (Top 1% outliers hidden)",
       x = "Price (in Thousand EUR)", 
       y = "Count")

print(p1_price_raw)


# -------------------------------------------------------
# Plot 2: Purchase Price (Log Transformation)
# Goal: Check normality for regression models
# -------------------------------------------------------
p2_price_log <- df_HK %>% 
  filter(kaufpreis_num > 0) %>% # Log requires positive values
  ggplot(aes(x = log(kaufpreis_num))) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#2C3E50", color = "white", alpha = 0.5) +
  geom_density(color = "#E74C3C", linewidth = 1.2) +
  labs(title = "Log-transformed Price Distribution", 
       subtitle = "Approximates normal distribution",
       x = "Log(Price)", 
       y = "Density")

print(p2_price_log)


# -------------------------------------------------------
# Plot 3: Construction Year (Age Structure)
# Goal: Analyze building eras (focusing on post-1900)
# -------------------------------------------------------
p3_age <- df_HK %>%
  ggplot(aes(x = baujahr)) +
  geom_histogram(binwidth = 5, fill = "#2C3E50", color = "white", alpha = 0.9) +
  
  # Median Line
  geom_vline(aes(xintercept = median(baujahr, na.rm = TRUE)), 
             color = "#E74C3C", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = median(df_HK$baujahr, na.rm = TRUE) - 5, 
           y = 100, label = "Median Year", color = "#E74C3C", angle = 90, hjust = 0) +
  
  scale_x_continuous(limits = c(1900, 2025), breaks = seq(1900, 2025, by = 20)) +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(title = "Age Structure of Houses",
       subtitle = "Distribution of Construction Year (Baujahr)",
       x = "Construction Year",
       y = "Count")

print(p3_age)


# -------------------------------------------------------
# Plot 4: Plot Area (Log Scale Histogram)
# Goal: Visualize land size including small plots and large estates
# -------------------------------------------------------
p4_area_log <- df_HK %>%
  filter(!is.na(grundstuecksflaeche_num) & grundstuecksflaeche_num >= 50) %>% 
  ggplot(aes(x = grundstuecksflaeche_num)) +
  geom_histogram(bins = 50, fill = "#2C3E50", color = "white", alpha = 0.9) +
  
  # Log X-Axis
  scale_x_log10(breaks = c(50, 100, 200, 500, 1000, 5000, 10000, 50000),
                labels = comma_format(accuracy = 1),
                expand = c(0, 0),
                limits = c(50, NA)) +
  
  # Median Line
  geom_vline(aes(xintercept = median(grundstuecksflaeche_num, na.rm = TRUE)), 
             color = "#E74C3C", linetype = "dashed", linewidth = 1) +
  annotate("text", 
           x = median(df_HK$grundstuecksflaeche_num, na.rm = TRUE) * 1.5, 
           y = 3000, # Adjust based on max count
           label = paste("Median:", round(median(df_HK$grundstuecksflaeche_num, na.rm = TRUE))), 
           color = "#E74C3C", hjust = 0) +
  
  labs(title = "Distribution of Plot Area",
       subtitle = "Histogram with Logarithmic X-Axis (Starts from 50 sqm)",
       x = "Plot Area (sqm) - Log Scale", 
       y = "Count")

print(p4_area_log)


# -------------------------------------------------------
# Plot 5: Living Area vs. Price (Scatter)
# Goal: Sanity Check - Positive correlation expected
# -------------------------------------------------------
p5_sanity <- df_HK %>%
  filter(wohnflaeche_num > 10 & wohnflaeche_num < 600) %>%
  filter(kaufpreis_num > 10000 & kaufpreis_num < 3000000) %>%
  ggplot(aes(x = wohnflaeche_num, y = kaufpreis_num)) +
  
  # Small transparent points to handle overplotting
  geom_point(alpha = 0.1, color = "#2C3E50", size = 0.8) +
  geom_smooth(method = "lm", color = "#E74C3C", se = FALSE, linewidth = 1) +
  
  scale_y_continuous(labels = dollar_format(prefix = "€", big.mark = ",")) +
  scale_x_continuous(labels = comma_format(suffix = " m²")) +
  labs(title = "Sanity Check: Living Area vs. Price",
       subtitle = "Relationship between Wohnflaeche and Price",
       x = "Living Area", 
       y = "Price")

print(p5_sanity)


# -------------------------------------------------------
# Plot 6: Social Index Distribution
# Goal: Balance check of school locations
# -------------------------------------------------------
p6_social <- df_school %>%
  filter(!is.na(Sozialindexstufe)) %>%
  ggplot(aes(x = as.factor(Sozialindexstufe))) +
  geom_bar(fill = "#2C3E50", color = "white", alpha = 0.9) +
  labs(title = "Distribution of School Social Index",
       subtitle = "1 = Low Challenge, 9 = High Challenge",
       x = "Social Index Level", 
       y = "Number of Schools")

print(p6_social)


# -------------------------------------------------------
# Plot 7: Distance to Nearest School
# Goal: Verify walking distance (0.5 - 3km)
# -------------------------------------------------------
p7_distance <- df_dist %>%
  filter(nn_order == 1) %>% # Nearest school only
  mutate(dist_num = as.numeric(dist)) %>%
  filter(!is.na(dist_num) & dist_num < 15) %>% 
  
  ggplot(aes(x = dist_num)) +
  geom_histogram(binwidth = 0.2, fill = "#2C3E50", color = "white", alpha = 0.9) +
  
  # Highlight Walking Zone
  annotate("rect", xmin = 0.5, xmax = 3, ymin = 0, ymax = Inf, 
           fill = "#27AE60", alpha = 0.15) +
  annotate("text", x = 1.75, y = Inf, label = "Walking Zone (0.5-3km)", 
           color = "#27AE60", vjust = 1.5, fontface = "bold") +
  
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(title = "Distance to Nearest School",
       subtitle = "Green zone indicates reasonable walking distance",
       x = "Distance (km)", 
       y = "Frequency (Grid Cells)")

print(p7_distance)
