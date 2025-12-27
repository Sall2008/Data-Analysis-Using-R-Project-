# ==== 1 Importing Libraries ====

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("readxl")

library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

# ==== 2. Loading and Cleaning Data ====

## ==== 2.1 Load and Clean Housing Data ====

housing_path <- "course_data/housing_data/cross_section/CampusFile_HK_2022.csv"

housing_data_NRW <- read_csv(housing_path) %>% 
  filter(blid == "North Rhine-Westphalia") %>% # Only including schools in NRW
  filter(ergg_1km != "-9") %>% # Dropping rows where 1km identifier is -9
  filter(kategorie_Haus == "Single-family house (detached)") %>% # Only use 
  # single family houses
  mutate(across(
    where(is.character),
    ~ na_if(.x, "Implausible value")
  )) %>%
  select(obid, 
         kaufpreis,
         baujahr,
         wohnflaeche,
         grundstuecksflaeche, 
         zimmeranzahl,
         ergg_1km
         ) %>% 
  filter(
    wohnflaeche > 0,
    grundstuecksflaeche > 0,
    zimmeranzahl > 0
    ) %>% 
  drop_na(
    kaufpreis,
    wohnflaeche,
    baujahr,
    ergg_1km
    )

## ==== 2.2 Load and Clean School Data ====

school_path <- "course_data/school_data/school_data.xlsx"
school_dist_path <-"course_data/school_data/distance_to_schools.csv"
school_index_path <-"course_data/school_data/2022_social_index.csv"

### ==== 2.2.1 Load Plain School Data ====

school_data_elem <- read_excel(school_path) %>% 
  filter(school_type == "02") %>% # Only including elementary schools
  select(school_ID, ergg_1km)

### ==== 2.2.2 Load School Distance Data ====

school_data_dist <- read_csv(school_dist_path) %>% 
  filter(school_type == "2") %>% # Only including elementary schools
  filter(nn_order == 1) # only keep closest school

### ==== 2.2.2 Load School Index Data ====

school_data_index <- read_delim(
  school_index_path,
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")) %>% 
  rename(school_ID = Schulnummer) %>% 
  select(school_ID, Sozialindexstufe)

## ==== 2.3 Merge Data ====

school_data <- school_data_dist %>% 
  left_join(school_data_index, by = "school_ID")

data <- housing_data_NRW %>%
  left_join(school_data, by = "ergg_1km")
  
# ==== 3. Models ====

## ==== 3.1 Distance to Schools ====

data <- data %>% 
  rename(
    rooms = zimmeranzahl,
    social_index = Sozialindexstufe
  ) %>% 
  mutate(
    log_price = log(kaufpreis),
    log_liv_sqm = log(wohnflaeche),
    log_prop_sqm = log(as.numeric(grundstuecksflaeche)),
    rooms = as.numeric(rooms)
  )

model_dist <- lm(
  log_price ~ dist +
    log_liv_sqm + log_prop_sqm + rooms,
  data = data
)

summary(model_dist)

## ==== 3.2 School Social Index with Interaction ====

model_index <- lm(
  log_price ~ social_index + 
    dist + log_liv_sqm + log_prop_sqm + rooms,
  data = data
)

summary(model_index)

## ==== 3.3 Interaction School Social Index and Distance ====

model_interaction <- lm(
  log_price ~ dist * social_index +
    log_liv_sqm + log_prop_sqm + rooms,
  data = data
)

summary(model_index)