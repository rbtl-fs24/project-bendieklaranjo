library(tidyverse)
library(googlesheets4)
library(dplyr)


# Read the Google Sheet
eth_dep_sus_visibility_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1GzwT0LwPlmeAZpEhcjgFMgqak0Cbk_9cmayDwuuZYhM/edit?usp=sharing")

# Write the data to a CSV file in data/raw folder
write_csv(eth_dep_sus_visibility_raw, "data/raw/eth_dep_sus_visibility_raw.csv")

