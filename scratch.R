
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

# species range data ----
query <- "SELECT * FROM usfws_complete_species_current_range_2 WHERE SCINAME='Rana pretiosa' "

range_map <- st_read(here("data", "usfws_complete_species_current_range",
                          "usfws_complete_species_current_range_2.shp"),
                     query = query) %>% 
  st_make_valid() %>% 
  clean_names()

# full Oregon map ----
state_map <- st_read(here("data", "cb_2018_us_state_500k", "cb_2018_us_state_500k.shp")) %>% 
  st_make_valid() %>% 
  clean_names() %>% 
  filter(name == "Oregon")


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
  rename(sul_mm = sul) %>% 
  
  # remove years w/no frog surveys at Upper Jamison
  filter(year %in% c(2009:2019))

# crop spp range ----
range_map <- st_intersection(range_map, state_map)

rm(env_raw, frogs_raw, query)



## ===========================================
##                 Plot Ideas             ----
## ===========================================

# male vs. female comparison ----
mf_count <- frogs %>% 
  group_by(sex) %>% 
  summarise(frog_catch = sum(frog_detected)) 

# plot
ggplot(data = mf_count) +
  geom_point(aes(x = frog_catch, y = sex)) +
  
  # add hop line for males
  geom_curve(aes(x = 0, xend = 267, y = 2, yend = 2), linetype = 2,
             curvature = -0.4) +
  
  # add hop line for females
  geom_curve(aes(x = 0, xend = 350, y = 1, yend = 1), linetype = 2,
             curvature = -0.4) +
  
  theme_minimal()

# size comparison ----

frogs %>%
  
  # count frogs detected for each size
  filter(frog_detected == 1) %>% 
  group_by(sul_mm) %>% 
  
  summarise(frog_catch = sum(frog_detected)) %>% 
  
  # lollipop plot (trying to mimic blades of grass using lollipop or windmill)
  ggplot(aes(x = sul_mm, y = frog_catch)) +
  
  
  geom_segment( aes(x= sul_mm, xend=sul_mm, y=0, yend=frog_catch),
                linewidth = 2.9, col = "seagreen") +
  geom_point(col = "seagreen", shape = 17, size = 2) +
  
  theme_minimal()
  

# vegetation comparison ----

# ATTEMPT 1 ----
# plot NDVI over years, size by frog_count (bubble)
# test <- env %>%
#   select(c("year", "reach", "mdNDVI")) %>%
#   filter(year %in% c(2010:2019)) %>%
#   mutate(year = as.factor(year))
# 
# group <- frogs %>%
#   filter(frog_detected != 0) %>%
#   filter(year %in% c(2010:2019)) %>%
#   mutate(year = as.factor(year)) %>%
#   group_by(reach, year) %>%
#   summarise(frog_catch = sum(frog_detected)) %>%
#   left_join(test)
# 
# ggplot(data = group) +
#   geom_hex(aes(x = year, y = frog_catch, col = mdNDVI))

# ATTEMPT 2 ----
# boring ole pie chart, works but wouldn't look like lilly pad
reach_count <- frogs %>% 
  group_by(reach) %>% 
  summarise(frog_catch = sum(frog_detected))

ggplot(reach_count, aes(x = "", y = frog_catch, fill = reach)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

# ATTEMPT 3 ----

# density plot of NDVI for both sites, put count values in text
# add arrows pointing left and right w/labels for vegetation
ggplot(env, aes(x = mdNDVI, y = reach, fill = reach)) +
  geom_vline(xintercept = 0) +
  geom_density_ridges(scale = 1) +
  theme_minimal()









