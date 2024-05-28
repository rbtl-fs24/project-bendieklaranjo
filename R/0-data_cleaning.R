library(tidyverse)
library(googlesheets4)
library(dplyr)

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
    heard_dec_campus = `Which of these ETH Net Zero measures have you heard of? [Decarbonised campus]`,
    heard_indirect_emissions = `Which of these ETH Net Zero measures have you heard of? [Reduction of indirect emissions from electricity procurement and district heating]`,
    heard_living_lab = `Which of these ETH Net Zero measures have you heard of? [Campus as a net-​zero living lab]`,
    heard_bus_travel = `Which of these ETH Net Zero measures have you heard of? [Emission-​reduced business travel]`,
    heard_scope3 = `Which of these ETH Net Zero measures have you heard of? [Scope 3 exploration]`,
    heard_procurement = `Which of these ETH Net Zero measures have you heard of? [Emission-​reduced procurement of goods, construction and services]`,
    heard_community_engage = `Which of these ETH Net Zero measures have you heard of? [Communication and Community Engagement]`,
    heard_data_monitor = `Which of these ETH Net Zero measures have you heard of? [Data Availability, Monitoring and Reporting]`,
    heard_expertise = `Which of these ETH Net Zero measures have you heard of? [Strengthening Expertise in Sustainability and Ethics]`,
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

#Data cleaning
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

# Elimination of columns with high proportion of missing values (e.g., more than 50%)
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  select(-matches("know_eth_sus_work", "learn_eth_sus"))

#Data selection for visualisations


glimpse(eth_dep_sus_visibility_cleaning)
