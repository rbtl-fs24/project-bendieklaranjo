library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)

#Data import
eth_dep_sus_visibility_cleaning <- read_csv("data/raw/eth_dep_sus_visibility_raw.csv")

#Data exploration
glimpse(eth_dep_sus_visibility_cleaning)
eth_dep_sus_visibility_cleaning %>% 
  arrange(Timestamp)

#Data cleaning using dplyr
## Renaming of columns to short variable names
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>% 
  rename(
    timestamp = Timestamp,
    consent = `Your responses to this questionnaire may be used for research purposes. Your privacy is ensured throughout this survey. No personal identifiable information such as your name, phone number, email address, or residence will be collected.`,
    affiliation = `Please select your affiliation.`,
    group = `Please select your group. If you belong to multiple groups, please select the one you mostly work in or primarily identify with.`,
    work_location = `Where do you usually work?`,
    heard_eth_sus = `Have you heard of the ETH Sustainability Department?`,
    know_eth_sus_work = `How well do you know the work of ETH Sustainability Department?`,
    learn_eth_sus = `How did you learn about the ETH Sustainability Department?\nPlease select the most applying option.`,
    heard_net_zero = `Have you heard of the ETH Net zero initiative?`,
    know_net_zero_scope = `How well do you know the scope of the ETH Net Zero initiative?`,
    heard_measure_dec_campus = `Which of these ETH Net Zero measures have you heard of? [Decarbonised campus]`,
    heard_measure_indirect_emissions = `Which of these ETH Net Zero measures have you heard of? [Reduction of indirect emissions from electricity procurement and district heating]`,
    heard_measure_living_lab = `Which of these ETH Net Zero measures have you heard of? [Campus as a net-​zero living lab]`,
    heard_measure_bus_travel = `Which of these ETH Net Zero measures have you heard of? [Emission-​reduced business travel]`,
    heard_measure_scope3 = `Which of these ETH Net Zero measures have you heard of? [Scope 3 exploration]`,
    heard_measure_procurement = `Which of these ETH Net Zero measures have you heard of? [Emission-​reduced procurement of goods, construction and services]`,
    heard_measure_community_engage = `Which of these ETH Net Zero measures have you heard of? [Communication and Community Engagement]`,
    heard_measure_data_monitor = `Which of these ETH Net Zero measures have you heard of? [Data Availability, Monitoring and Reporting]`,
    heard_measure_expertise = `Which of these ETH Net Zero measures have you heard of? [Strengthening Expertise in Sustainability and Ethics]`,
    learn_net_zero = `Where did you learn about the ETH Net Zero initiative?\nPlease select the most applying option.`,
    heard_net_zero_day = `Have you heard of the Net Zero Day on 28 May 2024 at ETH?`,
    learn_net_zero_day = `If yes, where did you learn about the Net Zero Day?`,
    heard_air_travel = `Have you heard of the  Air Travel Project?`,
    know_air_travel_scope = `How well do you know the scope of the Air Travel Project?`,
    learn_air_travel = `Where did you learn about the Air Travel initiative?\nPlease select the most applying option.`,
    heard_sus_gastronomy = `Have you heard of the Sustainable Gastronomy Project?`,
    know_sus_gastronomy_scope = `How well do you know the scope of the Sustainable Gastronomy Project?`,
    learn_sus_gastronomy = `Where did you learn about the Sustainable Gastronomy Project?\nPlease select the most applying option.`,
    heard_sdg_lecture = `Have you heard about the Sustainable Development Goals (SDG) Public Lecture Series?`,
    attended_sdg_lecture = `Have you ever attended a lecture from the SDG Public Lecture Series?`,
    learn_sdg_lecture = `Where did you learn about the SDG Public Lecture Series ?\nPlease select the most applying option.`,
    fam_eth_recycling = `How familiar are you with the ETH waste recycling concept?`,
    know_recycling_hubs = `Do you know there are Recycling Hubs located on the ETH campus?`,
    locate_recycling_hubs = `Do you know where the Recycling Hubs are located on the ETH campus?`,
    learn_recycling_hubs = `Where did you learn about the Recycling Hubs? Please select the most applying option.`,
    interest_sustainability = `How interested are you in sustainability and climate change mitigation?`,
    importance_eth_strategy = `How important do you consider an ETH-wide sustainability strategy?`,
    vis_eth_sus_work = `How visible is the work of the ETH Sustainability Department?`,
    notice_eth_sus_work = `Where would you most likely notice the ETH Sustainability Department's work?`,
    suggest_visibility = `How would you suggest improving the visibility?`,
    comments = `If you have any further comments, thoughts or questions, feel free to leave a comment below.`,
    email_info = `If you would like to receive more information of the survey outcome, please insert your email-address.`
  )

## Adding of participant ID
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>% 
  mutate(id=1:n()) %>% 
  relocate(id)
  
##Splitting of timestamp
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  mutate(date = as_date(timestamp),
         time = format(as_datetime(timestamp), "%H:%M:%S")) %>% 
  select(-timestamp) %>%
  relocate(time, .before = consent) %>% 
  relocate(date, .before = time)

##Elimination of columns with high proportion of missing values (more than 50%)
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  select(-matches("know_eth_sus_work", "learn_eth_sus"))


# Visualization
## Subset the data for awareness of ETH Sustainability Department and its Initiatives
awareness_summary <- eth_dep_sus_visibility_cleaning %>%
  select(heard_eth_sus, heard_net_zero, heard_net_zero_day, heard_air_travel, heard_sus_gastronomy, heard_sdg_lecture) %>%
  summarise_all(~ mean(. == "Yes", na.rm = TRUE) * 100) %>%
  pivot_longer(cols = everything(), names_to = "Initiative", values_to = "Percentage")

# Subset the data for individual Net Zero measures
## What is the awareness of the Net Zero measures among the people that have heard of the Net Zero initiative?
net_zero_measures <- eth_dep_sus_visibility_cleaning %>%
  select(starts_with("heard_measure_")) %>%
  summarise_all(~sum(. == "Yes", na.rm = TRUE) / n()) %>%
  gather(key = "Measure", value = "Percentage")

#Subset the data for interest in climate change mitigation
## Define the labels for the interest levels
interest_labels <- c("Not interested at all", "Slightly interested", "Moderately interested", "Neutral", "Interested", "Very interested")

## Create a factor with the specified labels
eth_dep_sus_visibility_cleaning$Interest <- factor(eth_dep_sus_visibility_cleaning$interest_sustainability, levels = 1:6, labels = interest_labels)

# Count the total number of responses
total_responses <- nrow(eth_dep_sus_visibility_cleaning)

# Calculate the percentage of each interest level
interest_percent <- eth_dep_sus_visibility_cleaning %>%
  count(Interest) %>%
  mutate(Percentage = n / total_responses * 100) %>%
  complete(Interest = interest_labels, fill = list(n = 0, Percentage = 0))

#Subset the data for importance of ETH-wide sustainability strategy
## Define the labels for the importance levels
importance_labels <- c("Not important", "Slightly important", "Moderately important", "Neutral", "Important", "Very important")

## Create a factor with the specified labels
eth_dep_sus_visibility_cleaning$ImportanceStrategyt <- factor(eth_dep_sus_visibility_cleaning$importance_eth_strategy, levels = 1:6, labels = importance_labels)

# Count the total number of responses
total_responses <- nrow(eth_dep_sus_visibility_cleaning)

# Calculate the percentage of each importance level
importance_percent <- eth_dep_sus_visibility_cleaning %>%
  count(ImportanceStrategyt) %>%
  mutate(Percentage = n / total_responses * 100) %>%
  complete(ImportanceStrategyt = importance_labels, fill = list(n = 0, Percentage = 0))

glimpse(eth_dep_sus_visibility_cleaning)
# Store data
# write_csv(eth_dep_sus_visibility_cleaning, "data/processed/eth_dep_sus_visibility_processed.csv")
