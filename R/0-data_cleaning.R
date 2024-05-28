library(tidyverse)
library(googlesheets4)
library(dplyr)

eth_dep_sus_visibility_cleaning <- read_csv("data/raw/eth_dep_sus_visibility_raw.csv")
glimpse(eth_dep_sus_visibility_cleaning)

#Data cleaning



# waste_streams_zh_processed <-  waste_streams_zh_processed %>% 
#   select(-sum) %>%
#   relocate(Bin_ID, .before = 1) %>% 
#   pivot_longer(cols = paper:other,
#                names_to = "waste_category",
#                values_to = "weight")
# 
# write_csv(waste_streams_zh_processed, "/cloud/project/data/processed/waste-streams_zh_processed.csv")
