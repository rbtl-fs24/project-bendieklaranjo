library(tidyverse)
library(googlesheets4)
library(dplyr)

waste_streams_zh <- read_sheet("https://docs.google.com/spreadsheets/d/18MvqHvZcvM74AEhgdKB7_YZIkWBWmqm5Ek14pqm9uoY/edit?usp=sharing")
write_csv(waste_streams_zh, "data/raw/waste-streams_zh_raw.csv")
