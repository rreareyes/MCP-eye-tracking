# EXPORT ANALYSIS DATABASE
 
# This script generates the fixation databases for the different benchmarks
# used for the statistical analysis (first, last and proportion of fixations)

# The datasets are left ready for their processing as multinomial models
# using the package brms. For this reason, the dependent variable (location
# of the fixations) is stored in a variable with 4 columns.
 
# Since we are using only the results from T1, we filter the data for that
# phase from the get go, once the data is loaded. 

# Load required packages --------------------------------------------------
require(tidyverse)

# Define base directories and load data -----------------------------------
folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_data <- file.path(folder_root, "Results", "Datasets")
folder_keys <- file.path(folder_root, "Results", "Keys")

load(file = file.path(folder_data, "labeled_fixations.RData")) 
load(file = file.path(folder_data, "performance_data.RData")) 
load(file = file.path(folder_keys, "model_key_reduced.RData"))

# Create datasets for analysis --------------------------------------------

# Performance
performance <- performance_data %>% 
  left_join(model_key_reduced) %>% 
  na.omit() %>% 
  droplevels()

# First fixation made in each trial
first_fixations <- labeled_fixations %>% 
  group_by(subject, strategy, phase, block, trial) %>% 
  filter(start == min(start)) %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, rank) %>% 
  summarise(n_rank = n()) %>% 
  mutate(total = sum(n_rank)) %>% 
  ungroup() %>% 
  droplevels() %>% 
  pivot_wider(names_from = rank, 
              values_from = n_rank, 
              values_fill = list(n_rank = 0))

first_fixations$y <- cbind(first_fixations$`1`, first_fixations$`2`, 
                           first_fixations$`3`, first_fixations$`4`)  

first_fixations_scenario <- labeled_fixations %>% 
  group_by(subject, strategy, phase, block, trial) %>% 
  filter(start == min(start)) %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, scenario, rank) %>% 
  summarise(n_rank = n()) %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, scenario) %>% 
  mutate(total = sum(n_rank)) %>% 
  ungroup() %>% 
  droplevels() %>% 
  pivot_wider(names_from = rank, 
              values_from = n_rank, 
              values_fill = list(n_rank = 0))

first_fixations_scenario$y <- cbind(first_fixations_scenario$`1`, 
                                    first_fixations_scenario$`2`, 
                                    first_fixations_scenario$`3`, 
                                    first_fixations_scenario$`4`) 

# Last fixation made in each trial
last_fixations <- labeled_fixations %>% 
  group_by(subject, strategy, phase, block, trial) %>% 
  filter(start == max(start)) %>% 
  droplevels() %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, rank) %>% 
  summarise(n_rank = n()) %>% 
  mutate(total = sum(n_rank)) %>% 
  ungroup() %>% 
  droplevels() %>% 
  pivot_wider(names_from = rank, 
              values_from = n_rank, 
              values_fill = list(n_rank = 0))

last_fixations$y <- cbind(last_fixations$`1`, last_fixations$`2`, 
                          last_fixations$`3`, last_fixations$`4`) 

last_fixations_scenario <- labeled_fixations %>% 
  group_by(subject, strategy, phase, block, trial) %>% 
  filter(start == max(start)) %>% 
  droplevels() %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, scenario, rank) %>% 
  summarise(n_rank = n()) %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, scenario) %>% 
  mutate(total = sum(n_rank)) %>% 
  ungroup() %>% 
  droplevels() %>% 
  pivot_wider(names_from = rank, 
              values_from = n_rank, 
              values_fill = list(n_rank = 0))

last_fixations_scenario$y <- cbind(last_fixations_scenario$`1`, 
                                   last_fixations_scenario$`2`, 
                                   last_fixations_scenario$`3`, 
                                   last_fixations_scenario$`4`) 

# Allocation of fixations across domains
proportion_fixations <- labeled_fixations %>% 
  group_by(subject, strategy, phase, rank) %>% 
  summarise(n_rank = n()) %>% 
  mutate(total = sum(n_rank)) %>% 
  ungroup() %>% 
  droplevels() %>% 
  pivot_wider(names_from = rank, 
              values_from = n_rank, 
              values_fill = list(n_rank = 0))

proportion_fixations$y <- cbind(proportion_fixations$`1`, 
                                proportion_fixations$`2`, 
                                proportion_fixations$`3`, 
                                proportion_fixations$`4`) 

proportion_fixations_scenario <- labeled_fixations %>% 
  group_by(subject, strategy, phase, rank, scenario) %>% 
  summarise(n_rank = n()) %>% 
  ungroup() %>% 
  group_by(subject, strategy, phase, scenario) %>% 
  mutate(total = sum(n_rank)) %>% 
  ungroup() %>% 
  droplevels() %>% 
  pivot_wider(names_from = rank,
              values_from = n_rank, 
              values_fill = list(n_rank = 0))

proportion_fixations_scenario$y <- cbind(proportion_fixations_scenario$`1`, 
                                         proportion_fixations_scenario$`2`, 
                                         proportion_fixations_scenario$`3`, 
                                         proportion_fixations_scenario$`4`) 

# Save datasets -----------------------------------------------------------
save(performance,
     first_fixations, first_fixations_scenario,
     proportion_fixations, proportion_fixations_scenario,
     last_fixations, last_fixations_scenario,
     file = file.path(folder_data, "analysis_database.RData"))


