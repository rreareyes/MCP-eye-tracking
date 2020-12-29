# Create Analysis Database

# This script generates a set of summaries from the both the
# behavioral and fixation databases obtained, in order to
# latter run statistical analyses on them.

# Mainly, it performs classification of the detected fixations
# according to the domain they belong to, and ranks each domain
# according to each subject's setup during the experiment.

# Latter it computes measures of mean accuracy, RT, and fixations 
# across domains in different specific conditions of interest.

# Finally, it saves all these summaries in a .rds file which we 
# will use to perform the statistical analysis in future steps.

# The fixation dataset comes straight from the I2MC output, so
# it requires a bit of cleaning and renaming certain variables
# to make it easier to work with.

# Details in the definitions of AOIs corresponding to each domain
# are provided further down in the corresponding section of this
# script.

# Written by Eduardo Rea 
# Project Multicue-Probabilistic
# NLP Lab
# December 2019  

# Load the required packages ####
require(tidyverse)
require(spatialEco)
require(sp)
require(papaja)

# Define base paths ####
folder_root <- "C:/Users/rerr_/Google Drive/Graduate/Lab/Studies/MultiCue_Probabilistic/three_phases/INV/Analysis"
setwd(folder_root)

folder_scripts      <- file.path(folder_root, "Scripts")
folder_results      <- file.path(folder_root, "Results")
folder_tracking     <- file.path(folder_root, "Eye_tracking", "Results")
folder_behavior     <- file.path(folder_root, "Behavior", "Results")
folder_demographics <- file.path(folder_root, "Demographics_scales")

# Load the fixation data ####
raw_data <- read.delim2(file = file.path(folder_tracking, "fixation_data.txt"), 
                        stringsAsFactors = F) 

fixation_data <- raw_data %>%        
  separate(Trial, into = c("ID", "phase", "block", "trial")) %>% 
  select(-c(ID)) %>% 
  mutate(XPos = as.numeric(XPos),
         YPos = as.numeric(YPos)) %>% 
  mutate(position = NA)

# Get number subjects
subject_ids <- unique(fixation_data$Participant)
n_subjects <-  length(subject_ids)

# Classify fixations according to location ####
# Define the AOIS polygons
# Domain number corresponds to reading order (L-R, T-B):

#        o-------o-------o         o-------o-------o
#        |       |       |         |       |       |
#        |   1   |   2   |         |   5   |   6   |
#        |       |       |         |       |       |
#        o-------o-------o    +    o-------o-------o
#        |       |       |         |       |       |
#        |   3   |   4   |         |   7   |   8   |
#        |       |       |         |       |       |
#        o-------o-------o         o-------o-------o
    
# Polygons are defined as follows:

# (x[4], y[4]) o--------o (x[3], y[3])
#              |        |
#              |        |
#              |        |
#              |        |
# (x[1], y[1]) o--------o (x[2], y[2)

# Regardless of the starting point chosen, the order of the 
# vertex should be sequential with the shape you intend to 
# sample from.

# Left side
domain1_left_x <- c(233, 433, 433, 233) # All the x coordinates from the polygon's vertices
domain1_left_y <- c(289, 289, 489, 489) # All the y coordinates from the polygon's vertices

domain2_left_x <- c(530, 730, 730, 530)
domain2_left_y <- c(289, 289, 489, 489)

domain3_left_x <- c(233, 433, 433, 233)
domain3_left_y <- c(586, 586, 786, 786)

domain4_left_x <- c(530, 730, 730, 530)
domain4_left_y <- c(586, 586, 786, 786)

# Right side
domain1_right_x <- c(1193, 1393, 1393, 1193)
domain1_right_y <- c(289, 289, 489, 489)

domain2_right_x <- c(1490, 1690, 1690, 1490)
domain2_right_y <- c(289, 289, 489, 489)

domain3_right_x <- c(1193, 1393, 1393, 1193)
domain3_right_y <- c(586, 586, 786, 786)

domain4_right_x <- c(1490, 1690, 1690, 1490)
domain4_right_y <- c(586, 586, 786, 786)

# Grouped sets of coordinates to make it easier to loop
aois_x <- list(domain1_left_x, domain2_left_x,
               domain3_left_x, domain4_left_x,
               domain1_right_x, domain2_right_x,
               domain3_right_x, domain4_right_x)

aois_y <- list(domain1_left_y, domain2_left_y,
               domain3_left_y, domain4_left_y,
               domain1_right_y, domain2_right_y,
               domain3_right_y, domain4_right_y)

# Define the number of regions to sample
n_AOIs <- length(aois_x)/2

# Loop through all the fixations and detect if they fall within any of the AOIS
for (iAOI in 1:n_AOIs) {
  
  # Check if they fall within the left AOIS
  within_left <- as.logical(point.in.polygon(fixation_data$XPos, 
                                             fixation_data$YPos, 
                                             aois_x[[iAOI]], 
                                             aois_y[[iAOI]]))
  # Check if they fall within the right AOIS
  within_right <- as.logical(point.in.polygon(fixation_data$XPos, 
                                              fixation_data$YPos, 
                                              aois_x[[iAOI + 4]], 
                                              aois_y[[iAOI + 4]]))
  
  # Label the fixations according to the AOI where they fall
  fixation_data$position[within_left]  <- iAOI
  fixation_data$position[within_right] <- iAOI + 4
  
}
      
# Rename variables, convert types and clean up the database
aoi_data <- fixation_data %>% 
  rename("id"       = Participant, 
         "duration" = FixDur, 
         "start"    = FixStart, 
         "end"      = FixEnd) %>% 
  
  mutate(phase = recode(phase, 
                        "TP" = "1", 
                        "NP" = "2", 
                        "EP" = "3")) %>% 
  
  mutate(trial = as.integer(trial), 
         phase = as.integer(phase), 
         block = as.integer(block)) %>% 
  
  na.omit() %>% 
  
  mutate(phase = as.factor(phase)) %>% 
  
  mutate(block = as.factor(block)) %>% 
  
  arrange(id, phase, block, trial, start) %>% 
  
  group_by(id, phase, block, trial) %>% 
  
  mutate(fixation_id = 1:n()) %>% # Define order of fixations
  
  mutate(domain = "0") %>% 
  
  mutate(side = "x") %>% 
  
  ungroup()

# Label domains (position in the grid, regardless of side)
aoi_data$domain[aoi_data$position == 1 | aoi_data$position == 5] <- "1"
aoi_data$domain[aoi_data$position == 2 | aoi_data$position == 6] <- "2"
aoi_data$domain[aoi_data$position == 3 | aoi_data$position == 7] <- "3"
aoi_data$domain[aoi_data$position == 4 | aoi_data$position == 8] <- "4"

aoi_data$side[aoi_data$position == 1 | aoi_data$position == 2 | aoi_data$position == 3 | aoi_data$position == 4] <- "left"
aoi_data$side[aoi_data$position == 5 | aoi_data$position == 6 | aoi_data$position == 7 | aoi_data$position == 8] <- "right"

# Process the behavior data ####
# Load the file database and rename columns as needed
behavior_data <- read.csv(file = file.path(folder_behavior, "behavior_dataset.csv"), encoding = "UTF-8") %>% 
  mutate(c1_left = replace(c1_left, c1_left == 2, 0)) %>% 
  mutate(c2_left = replace(c2_left, c2_left == 2, 0)) %>% 
  mutate(c3_left = replace(c3_left, c3_left == 2, 0)) %>% 
  mutate(c4_left = replace(c4_left, c4_left == 2, 0)) %>% 
  mutate(c1_right = replace(c1_right, c1_right == 2, 0)) %>% 
  mutate(c2_right = replace(c2_right, c2_right == 2, 0)) %>% 
  mutate(c3_right = replace(c3_right, c3_right == 2, 0)) %>% 
  mutate(c4_right = replace(c4_right, c4_right == 2, 0)) %>% 
  mutate(response = replace(response, response == -1, 0)) %>% 
  mutate(c1  = c1_left - c1_right) %>% 
  mutate(c2  = c2_left - c2_right) %>% 
  mutate(c3  = c3_left - c3_right) %>% 
  mutate(c4  = c4_left - c4_right) %>% 
  mutate(correct = 0) %>% 
  mutate(phase = as.factor(phase)) %>% 
  mutate(block = as.factor(block))

# Define correct responses
behavior_data$correct[round(behavior_data$p_left, 2) < 0.5 & behavior_data$response == 0] <- 1
behavior_data$correct[round(behavior_data$p_left, 2) > 0.5 & behavior_data$response == 1] <- 1

behavior_data$trial <- rep(1:60, (dim(behavior_data)[1]/480))
behavior_data$trial_global <- rep(1:480, (dim(behavior_data)[1]/480))

# Do binomial test to identify random performers
binomial_data <- behavior_data %>% 
  filter(phase != 3) %>% 
  filter(round(p_left, 2) != 0.5) %>% 
  na.omit() %>% 
  group_by(id) %>% 
  mutate(p_binom = as.numeric(binom.test(x = sum(correct), 
                                         n = 324, #Total of non random trials
                                         p = 0.5, 
                                         alternative = "two.sided")[[3]])) %>% 
  filter(p_binom < 0.05) %>% 
  mutate(accuracy = mean(correct)) %>% 
  select(id, p_binom, accuracy) %>% 
  unique() %>% 
  ungroup()

# Remove subjects random response subjects
behavior_data <- behavior_data %>% 
  filter(id %in% binomial_data$id)

# We round p  
accuracy_data <- behavior_data %>% 
  filter(round(p_left, 2) != 0.5) %>% 
  na.omit() %>% 
  group_by(id, phase) %>% 
  mutate(accuracy = mean(correct)) %>% 
  select(id, phase, accuracy) %>% 
  unique() %>% 
  ungroup()

# Define groups based on quartiles of performance ####
accuracy_thresholds <- quantile(accuracy_data$accuracy[accuracy_data$phase == 2])

group_data <- accuracy_data %>%
  filter(phase == 2) %>% 
  filter(id %in% binomial_data$id) %>% 
  mutate(group = 4) %>% 
  mutate(group = replace(group, accuracy >= accuracy_thresholds[2], 3)) %>% 
  mutate(group = replace(group, accuracy >= accuracy_thresholds[3], 2)) %>% 
  mutate(group = replace(group, accuracy >= accuracy_thresholds[4], 1)) %>% 
  mutate(group = as.factor(group)) %>% 
  select(id, group) 

behavior_data <- behavior_data %>% 
  full_join(group_data)

accuracy_data <- accuracy_data %>% 
  full_join(group_data)

# Get domain ranks ####
ranked_data <- behavior_data %>% 
  filter(phase == 1) %>% 
  select(id, rank_cue1, rank_cue2, rank_cue3, rank_cue4) %>% 
  gather(key   = dimension, 
         value = rank, 
         c(rank_cue1, rank_cue2, rank_cue3, rank_cue4)) %>% 
  
  separate(col  = dimension,
           into = c("thrash", "domain"),
           sep  = "cue") %>% 
  
  select(-c(thrash)) %>% 
  
  unique()

# Identify cue states ####
state_data <- behavior_data %>% 
  
  select(id, phase, block, trial, 
         c1_left, c2_left, c3_left, c4_left, 
         c1_right, c2_right, c3_right, c4_right) %>% 
  
  gather(key   = cue,
         value = state, 
         -c(id, phase, block, trial)) %>%
  
  separate(col = cue,
           into = c("rank", "side"),
           sep = "_") %>% 
  
  separate(col = rank,
           into = c("thrash", "rank"),
           sep = "c") %>%
  
  select(-c(thrash)) 

state_data$state[state_data$state == 1] <- "A"
state_data$state[state_data$state == 2] <- "B"

state_data$rank <- as.numeric(state_data$rank)

# Create database from survey responses ####
survey_data <- read.csv(file = file.path(folder_demographics, "demographics.csv")) %>% 
  select(id = "ID", gender = "Gender", age = "Age") %>% 
  filter(id %in% binomial_data$id)

# Merge behavior and demographic datasets ####
ranked_fixations <- aoi_data %>% 
  full_join(ranked_data) 

demo_behavior_data <- behavior_data %>% 
  full_join(survey_data) %>% 
  
  mutate(case = 0,
         inv_case = 0,
         evidence = round(evidence, 2)) %>% 
  
  # Define cases and inverted cases according to trial setup
  mutate(inv_case = replace(inv_case, c4 == 0 & c3 == 0 & c2 == 0 & c1 != 0, 4),
         inv_case = replace(inv_case, c4 == 0 & c3 == 0 & c2 != 0, 3),
         inv_case = replace(inv_case, c4 == 0 & c3 != 0, 2),
         inv_case = replace(inv_case, c4 != 0, 1)) %>% 
  
  mutate(case = replace(case, c1 == 0 & c2 == 0 & c3 == 0 & c4 != 0, 4),
         case = replace(case, c1 == 0 & c2 == 0 & c3 != 0, 3),
         case = replace(case, c1 == 0 & c2 != 0, 2),
         case = replace(case, c1 != 0, 1)) %>% 
  mutate(case = as.factor(case), inv_case = as.factor(inv_case))

# Get the trials for each scenario (normal and inverted) ####
n_case_trials <- demo_behavior_data %>% 
  filter(id == 520) %>% 
  select(phase, block, trial, case) %>% 
  
  group_by(phase, case) %>% 
  mutate(n_case = n()) %>% 
  ungroup() %>% 
  
  select(phase, case, n_case) %>% 
  
  unique()

n_inv_case_trials <- demo_behavior_data %>% 
  filter(id == 520) %>% 
  select(phase, block, trial, inv_case) %>% 

  group_by(phase, inv_case) %>% 
  mutate(n_inv_case = n()) %>% 
  ungroup() %>% 
  
  select(phase, inv_case, n_inv_case) %>% 
  
  unique()

# Merge fixation database with demo-behavior ####
merged_data <- ranked_fixations  %>% 
  full_join(demo_behavior_data) %>% 
  full_join(n_case_trials) %>% 
  full_join(n_inv_case_trials) %>% 
  
  select(id, gender, age, 
         phase, block, trial, 
         weight1, weight2, weight3, weight4, 
         sorted1, sorted2, sorted3, sorted4, c1, c2, c3, c4, case, inv_case, n_case, n_inv_case,
         evidence, fixation_id, position, domain,
         rank, side, start, end, duration, p_left, response, correct, rt) %>% 
  
  mutate(start    = as.numeric(start),
         end      = as.numeric(end),
         duration = as.numeric(duration)) %>% 
  
  # Remove subjects who didn't pass the binomial test
  filter(id %in% binomial_data$id) %>% 
  filter(!is.na(position)) 

# Create fixation database with counts ####
fixation_data_full <- merged_data %>%
  full_join(group_data) %>% 
  
  # Get counts
  group_by(id, group, phase, block, trial) %>% 
  
  mutate(total_fixations_trial = n()) %>% 
  
  mutate(total_fixtime_trial = sum(duration)) %>% 
  
  ungroup() %>% 
  
  mutate(phase    = as.factor(phase)) %>% 
  mutate(position = as.factor(position)) %>% 
  mutate(block    = as.factor(block)) %>% 
  mutate(rank     = as.factor(rank)) %>% 
  mutate(case     = as.factor(case)) %>% 
  mutate(inv_case = as.factor(inv_case)) %>% 
  mutate(domain   = as.factor(domain)) %>% 
  
  filter(!is.na(response)) %>% 
  filter(total_fixations_trial > 1)
  
# Get number of fixations per trial ####
count_data <- fixation_data_full %>% 
  
  select(id, group, age, gender,
         phase, block, trial, total_fixations_trial) %>% 
  
  group_by(id, group, phase) %>% 
  
  summarise(mean_fixations = mean(total_fixations_trial)) %>%
  
  ungroup()

# Get proportion of fixations across trials ####
proportion_data <- fixation_data_full %>% 
  
  select(id, group, age, gender,
         phase, block, trial, rank, 
         total_fixations_trial) %>% 
  
  group_by(id, group, phase, block, trial, rank) %>% 
  mutate(n_fixations_trial_rank = n()) %>% 
  unique() %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, age, gender, phase, block, trial, total_fixations_trial), rank, fill = list(n_fixations_trial_rank = 0)) %>% 
  
  group_by(id, group, phase, block, trial, rank) %>%
  mutate(prop_fixations_rank_trial = n_fixations_trial_rank / total_fixations_trial) %>%
  ungroup() %>%

  group_by(id, group, phase, block, rank) %>%
  mutate(mean_fixations_rank_block = mean(prop_fixations_rank_trial)) %>%
  ungroup()
  
# Get first and last fixation  ####
last_fixation_data <- fixation_data_full %>% 
  
  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == max(fixation_id)) %>% 
  ungroup() %>% 
  
  select(id, group, age, gender, phase, block, rank, total_fixations_trial) %>% 
  
  group_by(id, group, phase, block) %>% 
  mutate(n_last_block = n()) %>% 
  ungroup() %>% 
  
  group_by(id, group, phase, block, rank) %>% 
  mutate(n_last_block_rank = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, age, gender, phase, block, n_last_block, total_fixations_trial), rank, fill = list(n_last_block_rank = 0)) %>% 
  
  group_by(id, group, phase, block, rank) %>% 
  mutate(prop_last_block_rank = n_last_block_rank / n_last_block) %>% 
  ungroup() 
  
  
first_fixation_data <- fixation_data_full %>% 

  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == 1) %>% 
  ungroup() %>% 
  
  select(id, group, age, gender, phase, block, rank, total_fixations_trial) %>% 
  
  group_by(id, group, phase, block, rank) %>% 
  mutate(n_first_block_rank = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, age, gender, phase, block, total_fixations_trial), rank, fill = list(n_first_block_rank = 0)) %>% 
  
  group_by(id, group, phase, block, rank) %>% 
  mutate(prop_first_block_rank = n_first_block_rank / 60) %>%
  ungroup() 

# Get fixations by case ####
fixations_case_data <- fixation_data_full %>% 
  
  select(id, group, age, gender, phase, block, trial, case, n_case, total_fixations_trial) %>% 
  
  unique() %>% 
  
  group_by(id, group, phase, case) %>% 
  mutate(mean_fixations_case = sum(total_fixations_trial)/n_case) %>% 
  ungroup() %>% 
  
  select(id, group, phase, case, mean_fixations_case) %>% 
  
  unique()
  
case_data_last <- fixation_data_full %>% 
  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == max(fixation_id)) %>% 
  ungroup() %>% 
  
  select(id, group, age, gender, phase, case, n_case, rank) %>% 
  
  group_by(id, group, phase, case, rank) %>% 
  mutate(n_fixations_phase_case_rank = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, phase, group, age, gender, case, n_case), rank, fill = list(n_fixations_phase_case_rank = 0)) %>% 
  
  unique() %>% 

  group_by(id, group, phase, case, rank) %>%
  mutate(prop_fixations_phase_case_rank = n_fixations_phase_case_rank / n_case) %>%
  ungroup()
  

fixations_invcase_data <- fixation_data_full %>% 

  select(id, group, age, gender, phase, block, trial, inv_case, n_inv_case, total_fixations_trial) %>% 
  
  unique() %>% 
  
  group_by(id, group, phase, inv_case) %>% 
  mutate(mean_fixations_invcase = sum(total_fixations_trial)/n_inv_case) %>% 
  ungroup() %>% 

  select(id, group, phase, inv_case, mean_fixations_invcase) %>% 
  
  unique()

invcase_data_last <- fixation_data_full %>% 
  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == max(fixation_id)) %>% 
  ungroup() %>% 
  
  select(id, group, age, gender, phase, inv_case, n_inv_case, rank) %>% 
  
  group_by(id, group, phase, inv_case, rank) %>% 
  mutate(n_fixations_phase_invcase_rank = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, age, gender, phase, inv_case, n_inv_case), rank, fill = list(n_fixations_phase_invcase_rank = 0)) %>% 
  
  group_by(id, group, phase, inv_case, rank) %>% 
  mutate(prop_fixations_phase_invcase_rank = n_fixations_phase_invcase_rank / n_inv_case) %>% 
  ungroup() 

# Get RT by case ####
rt_case_data <- demo_behavior_data %>% 
  na.omit() %>% 
  
  group_by(id, group, phase, case) %>% 
  
  summarise(mean_rt = mean(rt)) %>% 
  
  ungroup()

rt_inv_case_data <- demo_behavior_data %>% 
  na.omit() %>% 
  
  group_by(id, group, phase, inv_case) %>% 
  
  summarise(mean_rt = mean(rt)) %>% 
  
  ungroup()

# Get transition data ####
transition_data <- fixation_data_full %>% 
  select(id, group, phase, block, trial, fixation_id, rank, side) %>% 
  arrange(id, phase, block, trial, fixation_id) %>% 
  mutate(next_rank = NA,
         next_side = NA) %>% 

  group_by(id, group, phase, block, trial) %>% 
  
  mutate(next_rank = lead(rank)) %>% 
  mutate(next_side = lead(side)) %>% 
  
  mutate(a = rank,
         b = side) %>% 
  unite(col = "current_fix", c(a, b)) %>% 
  
  mutate(a = rank,
         b = next_rank) %>% 
  unite(col = "rank_trans", c(a, b)) %>% 
  
  mutate(a = side,
         b = next_side) %>% 
  unite(col = "side_trans", c(a, b)) %>% 
  
  mutate("next_fix" = lead(current_fix)) %>%
  
  filter(current_fix != next_fix) %>% 
  
  mutate(a = current_fix,
         b = next_fix) %>% 
  unite(col = transition, c(a, b), sep = "-") %>% 
  
  mutate(trans_domain = "Between Domain") %>%
  mutate(trans_order  = "None") %>%
  mutate(trans_cue    = "Between Cue") %>% 
  ungroup() %>% 
  na.omit() %>% 
  
  mutate(trans_cue = replace(trans_cue,
                             side == next_side,
                             "Within Cue")) %>% 

  mutate(type_trans = "Same Domain") %>% 
  
  mutate(type_trans = replace(type_trans, 
                              rank_trans %in% c("1_2", "2_3", "3_4"), 
                              "Serial Descending")) %>% 
  
  mutate(type_trans = replace(type_trans, 
                              rank_trans %in% c("2_1", "3_2", "4_3"), 
                              "Serial Ascending")) %>% 
  
  mutate(type_trans = replace(type_trans, 
                              rank_trans %in% c("1_3", "1_4", "2_4"), 
                              "Non Serial Descending")) %>% 
  
  mutate(type_trans = replace(type_trans, 
                              rank_trans %in% c("3_1", "4_1", "4_2"), 
                              "Non Serial Ascending")) %>% 
  
  mutate(type_trans = factor(type_trans, 
                             levels = c("Same Domain", 
                                        "Serial Descending", 
                                        "Serial Ascending", 
                                        "Non Serial Descending", 
                                        "Non Serial Ascending")))

transition_counts <- transition_data %>% 
  
  mutate(trans_domain = as.factor(trans_domain),
         trans_cue = as.factor(trans_cue),
         trans_order = as.factor(trans_order),
         rank_trans = as.factor(rank_trans)) %>% 
  
  group_by(id, group, phase, block, trial) %>% 
  
  mutate(n_trans_trial = n()) %>% 
  
  ungroup() %>% 
  
  select(id, group, phase, block, trial, trans_cue, type_trans, n_trans_trial)

# Test for reading bias ####
reading_data <- fixation_data_full %>% 
  select(id, group, phase, block, trial, fixation_id, position, domain, total_fixations_trial) %>% 
  
  group_by(id, group, phase, block, trial, position) %>% 
  
  mutate(n_fixations_position = n()) %>% 
  mutate(prop_fixations_position = n_fixations_position/total_fixations_trial) %>% 
  
  ungroup() %>% 

  complete(nesting(id, group, phase, block), position, fill = list(n_fixations_position    = 0,
                                                                   prop_fixations_position = 0))

reading_first <- fixation_data_full %>% 
  select(id, group, phase, block, trial, fixation_id, position, domain, total_fixations_trial) %>% 
  
  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == 1) %>% 
  ungroup() %>% 
  
  group_by(id, group, phase, block, position) %>% 
  mutate(n_first_position = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, phase, block), position, fill = list(n_first_position    = 0)) %>% 
  group_by(id, group, phase, position) %>% 
  mutate(prop_first_position = n_first_position/120) %>% 
  ungroup()

# Test match between last fixation and choice ####

# # This check strict matching between domain last fixated if that the evidendce in that domain was pointing to that side
# matching_data <- fixation_data_full %>% 
#   group_by(id, group, phase, block, trial) %>% 
#   filter(fixation_id == max(fixation_id)) %>% 
#   select(id, gender, age, group, phase, block, trial, case, inv_case, c1, c2, c3, c4, domain, response) %>% 
#   filter(c(c1, c2, c3, c4)[domain] != 0) %>% 
#   mutate(matching = ifelse((c(c1, c2, c3, c4)[domain] > 0 & response == 1) | 
#                            (c(c1, c2, c3, c4)[domain] < 0 & response == 0), 1, 0)) %>% 
#   ungroup() %>% 
#   group_by(id, gender, age, group, phase) %>% 
#   summarise(mean_match = mean(matching)) %>% 
#   ungroup() %>% 
#   filter(phase == 2)

# Side fixated last is the side chosen
matching_data <- fixation_data_full %>% 
  filter(phase == 2) %>% 
  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == max(fixation_id)) %>% 
  select(id, gender, age, group, phase, block, trial, case, inv_case, c1, c2, c3, c4, domain, side, response) %>% 
  mutate(matching = if_else(condition = (side == "left" & response == 1) | (side == "right" & response == 0), true = 1, false = 0)) %>% 
  ungroup() %>% 
  group_by(id, group, phase, case) %>% 
  summarise(matching_score = mean(matching))

# Choice pattern above chance under each case
binomial_case <- demo_behavior_data %>% 
  full_join(n_case_trials) %>% 
  full_join(n_inv_case_trials) %>% 
  filter(phase == 2) %>% 
  filter(round(p_left, 2) != 0.5) %>% 
  select(id, group, phase, block, trial, case, inv_case, correct, n_case) %>% 
  group_by(id, phase, case) %>% 
  mutate(p_binom = round(as.numeric(binom.test(x = sum(correct), 
                                         n = n_case[1], #Total of non random trials
                                         p = 0.5, 
                                         alternative = "greater")[[3]]), 2)) %>% 
  ungroup() %>% 
  select(id, group, phase, case, n_case, p_binom) %>% 
  unique() %>% 
  mutate(non_random = p_binom < 0.05)

fixation_score <- fixation_data_full %>% 
  filter(phase == 2) %>% 
  group_by(id, group, phase, block, trial) %>% 
  filter(fixation_id == max(fixation_id)) %>% 
  ungroup() %>% 
  
  select(id, phase, n_case, case, rank) %>% 
  
  group_by(id, phase, case, rank) %>% 
  mutate(n_fixations_phase_case_rank = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, phase, case, n_case), rank, fill = list(n_fixations_phase_case_rank = 0)) %>% 
  unique() %>% 

  group_by(id, phase, case) %>% 
  summarise(case_score = mean(n_fixations_phase_case_rank[rank == case]) - mean(n_fixations_phase_case_rank[rank != case])) %>%
  ungroup() %>% 
  
  group_by(id, phase) %>% 
  summarise(score = mean(case_score)) %>% 
  ungroup() %>% 
  mutate(group = if_else(score > median(score), "1", "2"))
  
  
# Get subjective cue weights ####
logistic_data <- behavior_data %>% 
  filter(id %in% binomial_data$id) %>% 
  filter(phase != 1) %>% 
  filter(id != 377)

phases = unique(logistic_data$phase)
n_phases <- length(phases)

subject_ids <- unique(logistic_data$id)
n_subjects <- length(subject_ids)

regression_results <- list()

phase_results <- list()

for (iphase in 1:n_phases) {
  
  for (isubject in 1:n_subjects) {
    
    logit_dataset <- logistic_data %>%
      filter(phase == phases[iphase]) %>%
      filter(id == subject_ids[isubject]) 
    
    
    regression <- glm(response ~ c1 + c2 + c3 + c4, 
                      data = logit_dataset, 
                      family = binomial(link = "logit"), maxit = 100)
    
    regression_summary <- apa_print(regression)
    
    betas <- data.frame(id = subject_ids[isubject],
                        phase = phases[iphase],
                        b1  = log10(exp(as.numeric(regression_summary$table$estimate[2]))),
                        b2  = log10(exp(as.numeric(regression_summary$table$estimate[3]))),
                        b3  = log10(exp(as.numeric(regression_summary$table$estimate[4]))),
                        b4  = log10(exp(as.numeric(regression_summary$table$estimate[5])))
    )
    
    regression_results[[isubject]] <- betas
    
  }
  
  phase_results[[iphase]] <- regression_results
  
}

subjective_weights <- do.call(rbind, do.call(rbind, phase_results)) %>% 
  gather(key = "domain", value = "weight", c(b1, b2, b3, b4)) %>% 
  full_join(group_data) %>% 
  mutate(domain = recode(domain, "b1" = 0.9, "b2" = 0.6, "b3" = 0.3, "b4" = 0)) %>% 
  mutate(domain = as.factor(domain)) %>% 
  filter(phase != 1)

# Create summaries to export ####
phase_reading <- reading_data %>% 
  filter(phase != 1) %>% 
  
  group_by(id, group, phase, position) %>% 
  mutate(prop_fixations = mean(prop_fixations_position)) %>% 
  ungroup() %>% 
  
  select(id, group, phase, position, prop_fixations) %>% 
  unique()

phase_reading_first <- reading_first %>% 
  filter(phase != 1) %>% 
  
  group_by(id, group, phase, position) %>% 
  mutate(prop_first = mean(prop_first_position)) %>% 
  ungroup() %>% 
  
  select(id, group, phase, position, prop_first) %>% 
  unique()

phase_transitions_cue <- transition_counts %>% 
  filter(phase != 1) %>% 
  
  select(id, group, phase, block, trial, trans_cue, n_trans_trial) %>% 
  
  group_by(id, group, phase, block, trial, trans_cue) %>% 
  mutate(n_trans_cue_trial = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, phase, block, trial, n_trans_trial), trans_cue, fill = list(n_trans_cue_trial = 0)) %>% 
  
  group_by(id, group, phase, block, trial, trans_cue) %>% 
  mutate(prop_trans_cue_trial = n_trans_cue_trial/n_trans_trial) %>% 
  ungroup() %>% 
  
  group_by(id, group, phase, trans_cue) %>% 
  mutate(prop_trans_cue = mean(prop_trans_cue_trial)) %>% 
  ungroup() %>% 
  
  select(id, group, phase, trans_cue, prop_trans_cue) %>% 
  
  unique()

phase_transitions_cue_type <- transition_counts %>% 
  filter(phase != 1) %>% 
  
  select(id, group, phase, block, trial, trans_cue, n_trans_trial, type_trans) %>% 
  
  group_by(id, group, phase, block, trial, trans_cue, type_trans) %>% 
  mutate(n_trans_cue_type_trial = n()) %>% 
  ungroup() %>% 
  
  complete(nesting(id, group, phase, block, trial, trans_cue, n_trans_trial), type_trans, fill = list(n_trans_cue_type_trial = 0)) %>% 
  
  group_by(id, group, phase, block, trial, trans_cue, type_trans) %>% 
  mutate(prop_trans_cue_type_trial = n_trans_cue_type_trial/n_trans_trial) %>% 
  ungroup() %>% 
  
  group_by(id, group, phase, trans_cue, type_trans) %>% 
  mutate(prop_trans_cue_type = mean(prop_trans_cue_type_trial)) %>% 
  ungroup() %>% 
  
  select(id, group, phase, trans_cue, type_trans, prop_trans_cue_type) %>% 
  
  unique()
  
phase_data_proportion <- proportion_data %>% 
  filter(phase != 1) %>% 
  group_by(id, group, phase, rank) %>% 
  summarise(prop_fixations = mean(prop_fixations_rank_trial)) %>% 
  ungroup()

phase_data_first <- first_fixation_data %>% 
  filter(phase != 1) %>% 
  group_by(id, group, phase, rank) %>% 
  summarise(prop_trials = mean(prop_first_block_rank),
            n_trials    = sum(n_first_block_rank)) %>% 
  ungroup()

phase_data_last <- last_fixation_data %>% 
  filter(phase != 1) %>% 
  group_by(id, group, phase, rank) %>% 
  summarise(prop_trials = mean(prop_last_block_rank)) %>% 
  ungroup()

case_data_last <- case_data_last %>% 
  filter(phase != 1) %>% 
  group_by(id, group, phase, case, rank) %>% 
  summarise(mean_prop_trials = mean(prop_fixations_phase_case_rank), 
            mean_n_trials    = mean(n_fixations_phase_case_rank)) %>% 
  ungroup()

invcase_data_last <- invcase_data_last %>% 
  filter(phase != 1) %>% 
  group_by(id, group, group, phase, inv_case, rank) %>% 
  summarise(mean_prop_trials = mean(prop_fixations_phase_invcase_rank), 
            mean_n_trials    = mean(n_fixations_phase_invcase_rank)) %>% 
  ungroup()

rt_data <- demo_behavior_data %>% 
  select(id, group, phase, rt) %>%
  filter(phase != 1) %>% 
  group_by(id, group, phase) %>% 
  na.omit() %>% 
  summarise(phase_rt = mean(rt))

sampling_domain_case <- fixation_data_full %>% 
  select(id, age, group, phase, block, trial, case, correct, fixation_id, rank, duration) %>% 
  group_by(id, phase, block, trial) %>% 
  mutate(n_domains = length(unique(rank))) %>% 
  ungroup() %>%
  
  group_by(id, group, phase, case) %>% 
  summarise(n_unique = mean(n_domains)) %>% 
  ungroup() 

sampling_domain_invcase <- fixation_data_full %>% 
  select(id, age, group, phase, block, trial, inv_case, correct, fixation_id, rank, duration) %>% 
  group_by(id, phase, block, trial) %>% 
  mutate(n_domains = length(unique(rank))) %>% 
  ungroup() %>%
  
  group_by(id, group, phase, inv_case) %>% 
  summarise(n_unique = mean(n_domains)) %>% 
  ungroup() 


# Save datasets of interest ####
save(behavior_data, group_data, accuracy_data, subjective_weights, 
     rt_data, count_data, sampling_domain_case, sampling_domain_invcase,
     fixations_case_data, fixations_invcase_data, rt_case_data, rt_inv_case_data,
     phase_data_proportion, phase_data_last, phase_data_first,
     phase_transitions_cue, phase_transitions_cue_type,
     case_data_last, invcase_data_last, phase_reading, phase_reading_first,
     file = file.path(folder_results, "summary_datasets_group.RData"))

saveRDS(fixation_data_full, file = file.path(folder_results, "fixation_dataset_group.rds"))

# Behavior dataset for variational Bayes model comparison
write.csv(behavior_data, file.path(folder_behavior, "clean_dataset.csv"), row.names = F)

save(phase_data_proportion, phase_data_first, file = file.path(folder_results, "group_basic_fixations_inv.rds"))

