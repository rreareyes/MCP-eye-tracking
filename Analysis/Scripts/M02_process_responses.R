# Process Responses

# This script cleans the data from the participant's responses during the task.
# Here we remove variables that we are not using for the analysis that follows
# to slim down the size of the datasets. We also perform a binomial test to 
# remove subjects that show random choice performance.
# 
# Additionally, we create a summary of their performance, indicating the number 
# of correct trials and the total of trials completed.
# 
# On the other hand, we use the information of each participant's probabilistic
# setup to create labels for each domain and its relative importance. These 
# will be combined with the fixation data from the tracker to pin point the 
# location of the participant's gaze in each trial.
# 
# Finally, we add/edit some pieces of information:
# 
#   - correct{0,1}: participant chose the cue (side) with the highest 
#                   probability of winning.
#                   
#   - scenario{0:3}: labels for the different decision scenarios for serial
#                    search. The levels are defined like this to have them 
#                    centered on the first one for the statistical models 
#                    later on.
#                 
#   - c1-c4{-1:1}: these are created by subtracting the states presented in
#                  each domain (left - right). This data will be used in the 
#                  logistic regression later on


# Get base paths and packages-----------------------------------------------
require(tidyverse)

folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_data <- file.path(folder_root, "Results", "Datasets")
folder_keys <- file.path(folder_root, "Results", "Keys")

# Load the response database ----------------------------------------------
column_class <- c("factor", "factor", "factor", "factor", 
                  "NULL", "NULL", "NULL", "NULL", "NULL", 
                  "NULL", "NULL", "NULL", "NULL",
                  "numeric", "numeric", "NULL", "character", "numeric", 
                  "integer", "integer", "integer", "integer", 
                  "integer", "integer", "integer", "integer", 
                  "integer", "integer", "integer", "integer") 
                  
raw_responses <- read.csv(file = file.path(folder_data, "responses_dataset.csv"), 
                          colClasses = column_class, header = T) 


# Process response data ---------------------------------------------------
response_data <- raw_responses %>% 
  select(-c(rank_cue1:rank_cue4)) %>% 
  mutate_at(.vars = c("c1_left", "c2_left", "c3_left", "c4_left",
                      "c1_right", "c2_right","c3_right","c4_right"), 
            ~ifelse(. == 2, 0, .)) %>% #losing state (2) needs to be 0
  
  mutate(c1  = c1_left - c1_right, 
         c2  = c2_left - c2_right, 
         c3  = c3_left - c3_right,
         c4  = c4_left - c4_right) %>% 
  
  mutate(response = recode(response, "1" = "1", "-1" = "0"),
         response = as.numeric(response),
         scenario = 0) %>% 
  
  # Label decision scenarios
  mutate(scenario = replace(scenario, c1 == 0 & c2 == 0 & c3 == 0 & c4 != 0, 3),
         scenario = replace(scenario, c1 == 0 & c2 == 0 & c3 != 0, 2),
         scenario = replace(scenario, c1 == 0 & c2 != 0, 1),
         scenario = replace(scenario, c1 != 0, 0)) %>% 
  
  mutate(scenario = as.factor(scenario)) %>% 
  
  select(-(c1_left:c4_right))

# Label trials and scenarios ----------------------------------------------
response_data$trial <- rep(1:60, (dim(response_data)[1]/480))
response_data$trial_global <- rep(1:480, (dim(response_data)[1]/480))

scenario_key <- response_data %>% 
  select(subject, phase, block, trial, scenario) %>% 
  mutate(trial =  as.factor(trial))

# Label correct responses -------------------------------------------------
scored_responses <- response_data %>% 
  filter(!is.na(response)) %>% 
  filter(p_left != 0.5) %>% 
  mutate(correct  = 0)

scored_responses$correct[round(scored_responses$p_left, 2) < 0.5 & scored_responses$response == 0] <- 1
scored_responses$correct[round(scored_responses$p_left, 2) > 0.5 & scored_responses$response == 1] <- 1

# Do binomial test to identify random performers --------------------------
binomial_test_key <- scored_responses %>% 
  filter(phase != 3) %>% 
  group_by(subject) %>% 
  mutate(p_binom = as.numeric(binom.test(x = sum(correct), 
                                         n = 324, #Total of non random trials
                                         p = 0.5, 
                                         alternative = "two.sided")[[3]])) %>% 
  filter(p_binom < 0.05) %>% 
  mutate(accuracy = mean(correct)) %>% 
  select(subject, p_binom, accuracy) %>% 
  unique() %>% 
  ungroup()

# Remove subjects with random behavior
scored_responses <- scored_responses %>% 
  filter(subject %in% binomial_test_key$subject)


# Get subject accuracy across phases --------------------------------------
performance_data <- scored_responses %>% 
  select(subject, experiment, phase, p_left, response, correct) %>%
  filter(phase != 1) %>% 
  
  mutate(phase = as.factor(as.numeric(phase) - 2), #set T2 as reference for stats
         experiment = as.factor(as.numeric(experiment) - 1)) %>%  #set e1 as ref
  
  group_by(subject, experiment, phase) %>% 
  summarise(total = n(), correct = sum(correct)) %>% 
  ungroup()


# Create dataset for the variational bayesian modeling --------------------
model_data <- response_data %>% 
  filter(subject %in% binomial_test_key$subject) %>% 
  filter(phase != 3) %>% 
  filter(!is.na(response)) %>% 
  select(subject, phase, block, trial, c1, c2, c3, c4, response) %>% 
  mutate(subject = as.numeric(as.character(subject)),
         phase = as.numeric(phase),
         block = as.numeric(block),
         trial = as.numeric(trial),
         response = as.numeric(response))

# Rank the different domains for each subject -----------------------------
domain_rank_key <- raw_responses %>% 
  filter(phase == 1) %>% 
  select(subject, rank_cue1, rank_cue2, rank_cue3, rank_cue4) %>% 
  pivot_longer(cols  = starts_with("rank"),
               names_to  = "dimension", 
               values_to = "rank") %>% 
  separate(col  = dimension,
           into = c(NA, "position"),
           sep  = "cue") %>% 
  mutate(position = as.factor(position)) %>% 
  unique()

# Save data ---------------------------------------------------------------
write.csv(model_data, 
          file = file.path(folder_data, "model_data.csv"), 
          row.names = F)

save(performance_data,
     file = file.path(folder_data, "performance_data.RData"))

save(response_data,
     file = file.path(folder_data, "processed_responses.RData"))

save(domain_rank_key,
     file = file.path(folder_keys, "domain_rank_key.RData"))

save(scenario_key,
     file = file.path(folder_keys, "scenario_key.RData"))

save(binomial_test_key,
     file = file.path(folder_keys, "binomial_test_key.RData"))




