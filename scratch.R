
## ===========================================
##            Packages & Raw Data         ----
## ===========================================
# load libraries ----
library(tidyverse) 
library(here)
library(janitor)
library(readxl)
library(ggspatial)
library(sf)

# read frog data ----
frogs_raw <- read_csv(here("data", "frog_cmr", "cmrData.csv")) %>% 
  clean_names()

# read water data ----
env_raw <- read_csv(here("data", "frog_cmr", "waterCov.csv"))

# species range data
range_map <- read_sf(here("data", "usfws_complete_species_current_range",
                          "usfws_complete_species_current_range_1.shp"))



## ===========================================
##               Data Wrangling           ----
## ===========================================

# wrangle environmental data ----
env <- env_raw %>% 
  filter(reach == "Middle Jack" | reach == "Upper Jamison")

# wrangle frog survey data ----
frogs <- frogs_raw %>% 
  
  # filter to most surveyed reaches
  filter(reach == "Middle Jack" | reach == "Upper Jamison") %>% 
  
  pivot_longer(cols = 5:43,
               names_to = "year_visit",
               values_to = "frog_detected") %>% 
  
  # split year and visit number into two columns %>% 
  separate(year_visit, 
           c("year", "visit"),
           '_') %>% 
  
  # remove x that precedes the year (x2010, x2011, etc)
  mutate(year = str_remove(year, 'x')) %>% 
  
  # rename size to include units
  rename(sul_mm = sul)



