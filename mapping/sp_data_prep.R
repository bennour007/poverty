################################################################################

library(geojsonio)
library(tidyverse)
library(clue)
library(broom)
library(TripleR)
library(stringdist)
library(widyr)
library(sf)

################################################################################

source("mapping/geojson.R")
source("mapping/data_prep.R")
#source("mapping/functions.R")

################################################################################

county_sp <- read_sf(county_url) 
county_sp
################################################################################
################################################################################
# STATE NAMES

# State names of our data 

da_states <- data[1:24] %>%
  bind_rows() %>%
  pull(state) %>%
  unique()

################################################################################
################################################################################

states_matched <- county_sp %>%
  mutate(matchy = map(gov_name_f, function(.) rep(., 24) %>% 
                        as_tibble_col("reps"))) %>%
  mutate(matchy = map(matchy, function(.) mutate(., da_states))) %>%
  mutate(matchy = map(matchy, function(.) mutate(., dist = stringdist(a = reps, 
                                                                      b = da_states, 
                                                                      method = "jaccard")))) %>%
  mutate(matchy = map(matchy, function(.) filter(., dist == min(.$dist)))) %>%
  unnest("matchy") %>%
  mutate(gov_name_f = da_states) %>%
  select(-reps, -da_states, -dist)

################################################################################
################################################################################
unnested_data <- data[1:24] %>%
  bind_rows() %>%
  mutate(county =tolower(county)) %>%
  rename(gov_name_f = "state") 

unnested_states <- unnested_data %>%
  add_row(county = "unkown", gov_name_f = "Monastir") # Found another mistake in the data, a water body affected as delegation

# This is a mistake from the data I have, the name has been changed in the early 2000

unnested_states$county[unnested_states$county == "sidi ali ben nasrallah"] <- "nasrallah"


nested_states <- unnested_states %>%
  nest_by(gov_name_f)


county_matched <- states_matched 

county_matched_processed <- county_matched[county_matched$engtype_2 != "Delegation",] %>% 
  add_column(dropout_rate_primary = NA,
             dropout_rate_secondary = NA,
             dropout_rate_ps = NA,
             poverty = NA)

county_matched_processed <- county_matched[county_matched$engtype_2 == "Delegation",] %>%
  mutate(deleg_na_1 = tolower(deleg_na_1)) %>%
  mutate(deleg_na_1 = if_else(deleg_na_1 %>% is.na(), "unkown", deleg_na_1)) %>% 
  left_join(nested_states, by = "gov_name_f") %>%
  mutate(nums = map_dbl(data, function(.) nrow(.))) %>%
  mutate(reps = map2(deleg_na_1, nums, function(x,y) rep(x,y) %>% as_tibble_col("repeated_names"))) %>%
  mutate(data = map2(data,reps, function(x,y)  bind_cols(x,y))) %>%
  mutate(data = map(data, function(.) mutate(., 
                                             dist = stringdist(a = county,
                                                               b = repeated_names,
                                                               method = "jw")))) %>%  
  mutate(data = map(data, function(.) filter(., dist == min(.$dist)))) %>%
  unnest("data") %>%
  mutate(deleg_na_1 = county) %>%
  select(1:27, -county) %>%
  bind_rows(county_matched_processed)
