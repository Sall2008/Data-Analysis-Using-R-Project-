# ======================================================= #
# 1. Installing and Importing Packages                    ####
# ======================================================= #

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")

library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

# ======================================================= #
# 2. Importing and cleaning data                          ####
# ======================================================= #

housing_path <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"
housing_data_NRW <- read_csv(housing_path) %>% 
  filter(blid == "North Rhine-Westphalia") %>% # Only including schools in NRW
  filter(ergg_1km != "-9") %>%  # Dropping rows where 1km identifier is -9
  filter(kategorie_Haus == "Single-family house (detached)") %>% # Only use 
  # single family houses
  select(obid, kaufpreis ,ergg_1km)

school_path <- "course_data/school_data/school_data.xlsx"
school_data_elem <- read_excel(school_path) %>% 
  filter(school_type == "02") # Only including elementary schools

# Creating a raster indicator
school_raster <- school_data_elem %>%
  select(school_ID, ergg_1km) %>%       # keep relevant columns
  distinct(ergg_1km) %>%                # one row per raster
  mutate(elem_school_in_raster = 1)

housing_with_school <- housing_data_NRW %>%
  left_join(school_raster, by = "ergg_1km") %>%
  mutate(
    elem_school_in_raster = ifelse(
      is.na(elem_school_in_raster), 0, elem_school_in_raster
    )
  )

# ======================================================= #
# 3. Evaluating price differences due to elementary       ####
#    school proximity                                     ####
# ======================================================= #

# Without taking ratsers into consideration
housing_with_school %>%
  group_by(elem_school_in_raster) %>%
  summarise(
    mean_price   = mean(kaufpreis, na.rm = TRUE),
    median_price = median(kaufpreis, na.rm = TRUE),
    n            = n()
  )



