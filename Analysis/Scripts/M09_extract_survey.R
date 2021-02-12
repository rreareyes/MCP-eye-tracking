# Process pre-experiment survey data to extract demographics

# This data comes from Qualtrics, as a .csv file. We changed
# its name to simplify the import (the default name from
# Qualtrics includes a lot of special charachters).

require(tidyverse) 


# Define base directories -------------------------------------------------
folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_survey <- file.path(folder_root, "Data", "Survey")
folder_data <- file.path(folder_root, "Results", "Datasets")
folder_keys <- file.path(folder_root, "Results", "Keys")
folder_summaries <- file.path(folder_root, "Results", "Summaries")

# Define data types and names for import
column_names <- c("subject", "gender", "age", "handiness", 
                  "native_language1", "native_language2", 
                  "languages_fluent", "languages_fluent2", 
                  "illness", "illness_type", 
                  "ethnicity", "race", "experiment")

column_type  <- c("NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                  "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                  "NULL", "NULL", "NULL", "NULL", 
                  "character", "factor", "NULL", "integer", 
                  "factor", "factor", "factor", "factor", "factor", 
                  "factor", "factor", "NULL", "factor", "factor", "NULL")


# Import the data ---------------------------------------------------------
raw_experiment1 <- read.csv(file = file.path(folder_survey, 
                                             "survey_experiment1.csv"), 
                            header = F, skip = 7, 
                            colClasses = column_type, strip.white = TRUE) %>% 
  mutate(experiment = 1)

raw_experiment2 <- read.csv(file = file.path(folder_survey, 
                                             "survey_experiment2.csv"), 
                            header = F, skip = 7, 
                            colClasses = column_type, strip.white = TRUE) %>% 
  mutate(experiment = 2)

colnames(raw_experiment1) <- column_names
colnames(raw_experiment2) <- column_names


# Get subjects with usable data -------------------------------------------
load(file.path(folder_keys, "binomial_test_key.RData"))


# Merge datasets from both experiments ------------------------------------
raw_experiment1 <- raw_experiment1 %>%  #We collected year of birth in this stage
  mutate(age = 2019 - age)

survey_data <- full_join(raw_experiment1, raw_experiment2) %>% 
  mutate(subject = as.integer(subject))


# Clean datasets and create summaries -----------------------------------
demographic_data <- survey_data %>% 
  filter(subject %in% binomial_test_key$subject) %>% 
  arrange(subject) %>% 
  na.omit() %>% 
  droplevels() %>% 
  select(subject, experiment, gender, age)

summary_demographics <- demographic_data %>% 
  group_by(experiment, gender) %>% 
  summarise(mean_age = mean(age), sd_age = sd(age), n = n()) %>% 
  ungroup()

# Export results ----------------------------------------------------------
 save(demographic_data, file = file.path(folder_data, "demographic_data.RData"))
 save(summary_demographics, file = file.path(folder_summaries, "summary_demographics.RData"))

  
