library(tidyverse)
library(googlesheets4)
library(dplyr)

waste_streams_zh_processed <- read_csv("/cloud/project/data/raw/waste-streams_zh_raw.csv")

#write_csv(waste_streams_zh, "data/raw/waste-streams_zh_raw.csv")
waste_streams_zh_processed <-  waste_streams_zh_processed %>% 
  select(-sum) %>%
  relocate(Bin_ID, .before = 1) %>% 
  pivot_longer(cols = paper:other,
               names_to = "waste_category",
               values_to = "weight")

write_csv(waste_streams_zh_processed, "/cloud/project/data/processed/waste-streams_zh_processed.csv")
