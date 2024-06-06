library(tidyverse)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

#Data import
eth_dep_sus_visibility_cleaning <- read_csv("data/raw/eth_dep_sus_visibility_raw.csv")

#Data exploration
glimpse(eth_dep_sus_visibility_cleaning)
eth_dep_sus_visibility_cleaning %>% 
  arrange(Timestamp)

#Data cleaning using dplyr
# Filter out participants who did not consent
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  filter(`Your responses to this questionnaire may be used for research purposes. Your privacy is ensured throughout this survey. No personal identifiable information such as your name, phone number, email address, or residence will be collected.` == "I have read and understood the conditions outlined above and voluntarily agree to participate in this survey.")

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
  select(-c(know_eth_sus_work, learn_eth_sus))

# threshold <- 0.7
# for (col in names(eth_dep_sus_visibility_cleaning)) {
#   # Calculate the proportion of missing values for the current column
#   missing_proportion <- sum(is.na(eth_dep_sus_visibility_cleaning[[col]])) / nrow(eth_dep_sus_visibility_cleaning)
# 
#   # Check if the proportion exceeds the threshold
#   if (missing_proportion > threshold) {
#     # If the proportion exceeds the threshold, remove the column
#     eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
#       select(-{{col}})
#   }
# }
# # Check the resulting dataframe
# glimpse(eth_dep_sus_visibility_cleaning)


##Clean eth_dep_sus_visibility_cleaning

## Split the column into multiple columns based on comma as separator
# Set all values in newly created columns to FALSE
# Set all values in the newly created columns to FALSE
# Set all values in the newly created columns to FALSE
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  mutate(
    notice_ETH_main_website = FALSE,
    notice_ETH_mailing_list = FALSE,
    notice_Advertisement_on_Campus = FALSE,
    notice_ETH_Link_TV = FALSE,
    notice_Lectures_and_seminars = FALSE,
    notice_Social_Media = FALSE
  )

# Loop through the notice_eth_sus_work column
for (row in seq(nrow(eth_dep_sus_visibility_cleaning))) {
  # Check if the value in notice_eth_sus_work is not missing
  if (!is.na(eth_dep_sus_visibility_cleaning$notice_eth_sus_work[row])) {
    # Split the values in the notice_eth_sus_work column
    channels <- str_split(eth_dep_sus_visibility_cleaning$notice_eth_sus_work[row], ", ")[[1]]
    
    # Set TRUE in respective columns based on the channels mentioned
    for (channel in channels) {
      channel <- str_trim(channel)
      if (channel == "ETH main website") {
        eth_dep_sus_visibility_cleaning$notice_ETH_main_website[row] <- TRUE
      } else if (channel == "ETH mailing list") {
        eth_dep_sus_visibility_cleaning$notice_ETH_mailing_list[row] <- TRUE
      } else if (channel == "Advertisement on Campus") {
        eth_dep_sus_visibility_cleaning$notice_Advertisement_on_Campus[row] <- TRUE
      } else if (channel == "Advertisement on ETH-Link TV") {
        eth_dep_sus_visibility_cleaning$notice_ETH_Link_TV[row] <- TRUE
      } else if (channel == "Lectures and seminars") {
        eth_dep_sus_visibility_cleaning$notice_Lectures_and_seminars[row] <- TRUE
      } else if (channel == "Social Media (e.g. ETH instagram account)") {
        eth_dep_sus_visibility_cleaning$notice_Social_Media[row] <- TRUE
      }
    }
  }
}

# Remove the original notice_eth_sus_work column
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  select(-notice_eth_sus_work)


# Function 1 to clean responses: if the response is not "Yes" or "No", set it to NA
clean_single_response <- function(response) {
  response <- str_trim(response)  # Trim whitespace
  ifelse(tolower(response) %in% c("yes", "no"), response, NA_character_)
}

columns_to_clean <- c(
  "heard_measure_dec_campus",
  "heard_measure_indirect_emissions",
  "heard_measure_living_lab",
  "heard_measure_bus_travel",
  "heard_measure_scope3",
  "heard_measure_procurement",
  "heard_measure_community_engage",
  "heard_measure_data_monitor",
  "heard_measure_expertise",
  "learn_sus_gastronomy",
  "learn_air_travel"
)

# Apply the cleaning function to each column
eth_dep_sus_visibility_cleaning <- eth_dep_sus_visibility_cleaning %>%
  mutate(across(all_of(columns_to_clean), clean_single_response))

# Verify the cleaning
eth_dep_sus_visibility_cleaning %>%
  select(all_of(columns_to_clean)) %>%
  summarise_all(~ sum(!. %in% c("Yes", "No", NA)))

# Function 2 to clean "learn_"responses: if the response is not from list, set it to NA
# Define the function to clean the columns
clean_learning_sources <- function(data, columns) {
  # Define the valid answers
  valid_answers <- c(
    "ETH website",
    "ETH intranet",
    "lecture or seminar",
    "other students/colleagues",
    "Polymesse or other faires"
  )
  
  # Iterate over each specified column and clean it
  for (col in columns) {
    cat("Cleaning column:", col, "\n")
    
    # Clean the column only if it exists in the data frame
    if (col %in% colnames(data)) {
      cat("Column exists in the data frame.\n")
      
      # Clean the column
      cleaned_col <- ifelse(data[[col]] %in% valid_answers, data[[col]], NA)
      
      # Check the unique values after cleaning
      cat("Unique values after cleaning:", unique(cleaned_col), "\n")
      
      # Assign the cleaned column back to the data frame
      data[[col]] <- cleaned_col
    } else {
      cat("Column does not exist in the data frame.\n")
    }
  }
  
  return(data)
}

# Define the columns to clean
learn_columns <- c(
  "learn_net_zero",
  "learn_net_zero_day",
  "learn_air_travel",
  "learn_sus_gastronomy",
  "learn_sdg_lecture"
)

# Clean the data
eth_dep_sus_visibility_cleaning <- clean_learning_sources(eth_dep_sus_visibility_cleaning, learn_columns)

#----------------------------------------------------------------------
# Inspect the cleaned data
glimpse(eth_dep_sus_visibility_cleaning)

# Store data
eth_dep_sus_visibility_processed <- eth_dep_sus_visibility_cleaning
print(eth_dep_sus_visibility_processed)
write_csv(eth_dep_sus_visibility_processed, "data/processed/eth_dep_sus_visibility_processed.csv")
