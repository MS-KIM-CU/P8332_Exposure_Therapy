# -------------------------------
# Data Cleaning of Precipitation
# and Asthma Rates in NYC (2016–2019)
# -------------------------------

# Load required libraries
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(janitor)

# -------------------------------
#Github path set up I didn't use 
# -------------------------------

# Path to .rds files
#file_list <- list.files(
  #path = "datasets/",
  #pattern = "\\.rds$",
  #full.names = TRUE
#)

# Read and combine all .rds files into one dataset
#combined_ppt <- file_list |>
  #lapply(readRDS) |>
  #bind_rows()


# Reading precipitation files manually
ppt_2016 <- readRDS("datasets/weighted_area_raster_fips_ppt_daily_2016.rds")
ppt_2017 <- readRDS("datasets/weighted_area_raster_fips_ppt_daily_2017.rds")
ppt_2018 <- readRDS("datasets/weighted_area_raster_fips_ppt_daily_2018.rds")
ppt_2019 <- readRDS("datasets/weighted_area_raster_fips_ppt_daily_2019.rds")

# read and combine the files
combined_ppt <- bind_rows(
  ppt_2016,
  ppt_2017,
  ppt_2018,
  ppt_2019
)

# filter by NYC counties (FIPS)
nyc_fips <- c(36005, 36047, 36061, 36081, 36085)
# fips for bronx, brooklyn, manhattan, queens, staten island (respectively)


# Clean column names, filter to NYC, and create a date column
nyc_precip <- combined_ppt |>
  clean_names() |>
  filter(fips %in% nyc_fips) |>
  mutate(date = make_date(year, month, day))

# create a combined 'date' column
nyc_precip <- combined_ppt |> 
  janitor::clean_names() |> 
  filter(fips %in% nyc_fips) |> 
  mutate(date = make_date(year, month, day))

# check for the ppt distribution and any extremes
summary(nyc_precip$ppt)

# add borough category with actual names
nyc_precip <- nyc_precip |> 
  mutate(borough = case_when(
    fips == 36061 ~ "Manhattan",
    fips == 36047 ~ "Brooklyn",
    fips == 36081 ~ "Queens",
    fips == 36085 ~ "Bronx",
    fips == 36005 ~ "Staten Island"
  ))  |> 
  relocate(borough, .after = fips)

unique(nyc_precip$borough)

#######################

library(dplyr)
library(stringr)
library(lubridate)
library(janitor)

# read asthma
asthma <- read_delim("datasets/asthma_ed_visits.csv",
                     delim = "\t",
                     locale = locale(encoding = "UTF-16")) %>%
  clean_names()

# filter to All age groups
asthma_filtered <- asthma %>%
  filter(dim2value == "All age groups")

# clean columns
asthma_clean <- asthma_filtered %>%
  select(dim1value, date, x9) %>%
  rename(borough = dim1value,
         asthma_er_count = x9) %>%
  mutate(borough = str_trim(borough),
         date = mdy(str_replace_all(date, "[^0-9/]", "")))

# remove unknown/Citywide boroughs
asthma_clean <- asthma_clean %>%
  filter(!borough %in% c("Citywide", "Unknown")) %>%
  group_by(borough, date) %>%
  summarise(asthma_er_count = sum(asthma_er_count, na.rm = TRUE),
            .groups = "drop")

# merge with precipitation
asthma_precip <- asthma_clean %>%
  left_join(nyc_precip %>% select(borough, date, ppt),
            by = c("borough", "date"))

write_csv(asthma_precip, "asthma_precip_clean.csv")

# load daily pm 2.5 csv files
pm25_2016 <- read.csv("datasets/dailypm25_data_2016.csv")
pm25_2017 <- read.csv("datasets/dailypm25_data_2017.csv")
pm25_2018 <- read.csv("datasets/dailypm25_data_2018.csv")
pm25_2019 <- read.csv("datasets/dailypm25_data_2019.csv")

# combine pm25 files
pm25_all <- rbind(pm25_2016, pm25_2017, pm25_2018, pm25_2019) |> 
  janitor::clean_names()

# filter for boroughs we want 
# (Kings aka Brooklyn, New York aka Manhattan, Queens, Bronx, 
# and Richmond aka Staten Island)

selected_counties <- c("New York", "Kings", "Queens", "Bronx", 
                       "Richmond County")

pm25_all <- pm25_all |> 
  filter(county %in% selected_counties)

# take the average pm25 daily mean conc for each county
# across the sites per day
pm25_all <- pm25_all |>
  dplyr::group_by(county, date, state_fips_code, county_fips_code) |>
  dplyr::summarise(pm25 = mean(daily_mean_pm2_5_concentration, 
                               na.rm = TRUE), .groups = "drop")

###################