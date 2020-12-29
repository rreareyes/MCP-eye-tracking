# LABEL FIXATIONS

# This scrip performs classification of the detected fixations, labeling 
# their absolute position and the pair they belong to.

# Using the information from each participant's probabilistic setup, we 
# assign each pair their relative importance (rank) according to the how
# informative they were for each subject. 
 
# We also label each trail according to the decision scenario they belong, 
# according to the configuration of the domain's states in each of them.

# Finally, we filter out the subjects classified as random performers in the
# binomial test performed on their responses in previous steps, and add the
# decision group for each individual, according to the model classification
# results.

# # Written by Eduardo Rea 
# Project Multicue-Probabilistic
# NLP Lab
# December 2019  

# Load the required packages ----------------------------------------------
require(tidyverse)
require(spatialEco)
require(sp)

# Define base paths -------------------------------------------------------
folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_data <- file.path(folder_root, "Results", "Datasets")
folder_keys <- file.path(folder_root, "Results", "Keys")

# Load the domain ranks and scenarios -------------------------------------
load(file.path(folder_keys, "scenario_key.RData"))
load(file.path(folder_keys, "domain_rank_key.RData"))
load(file.path(folder_keys, "binomial_test_key.RData"))

# Load the model classification to assign groups --------------------------
model_classification <- read.csv(file = file.path(folder_keys, 
                                                  "model_classification.csv"), 
                                 header = T, 
                                 col.names = c("subject", "phase1", "strategy")) %>% 
  select(-phase1) %>% 
  
  mutate(experiment = if_else(subject > 500, 1, 0)) %>% 
  mutate(subject = as.factor(subject),
         experiment = as.factor(experiment))


model_key_extended <- model_classification %>% 
  mutate(strategy = recode(strategy,
                           "1" = "1st only",
                           "2" = "2nd only",
                           "3" = "3rd only",
                           "4" = "4th only",
                           "5" = "1st and 2nd",
                           "6" = "1st and 3rd",
                           "7" = "1st and 4th",
                           "8" = "2nd and 3rd",
                           "9" = "2nd and 4th",
                           "10" = "3rd and 4th",
                           "11" = "Drop 4th",
                           "15" = "WADD",
                           "16" = "Tallying",
                           "17" = "Partial Tallying",
                           "18" = "Partial Tallying",
                           "19" = "TTB",
                           "20" = "Serial Search",
                           "21" = "Serial Search"))
  
model_key_reduced <- model_classification %>% 
  mutate(strategy = recode(strategy,
                           "1" = "1st only",
                           "2" = "2nd only",
                           "3" = "Other",
                           "4" = "Other",
                           "5" = "Other",
                           "6" = "Other",
                           "7" = "Other",
                           "8" = "Other",
                           "9" = "Other",
                           "10" = "Other",
                           "11" = "Other",
                           "15" = "Other",
                           "16" = "Tallying",
                           "17" = "Partial Tallying",
                           "18" = "Partial Tallying",
                           "19" = "Serial Search",
                           "20" = "Serial Search",
                           "21" = "Serial Search")) %>% 
  filter(strategy != "Other") %>% 
  droplevels()


# Load the fixation data  -------------------------------------------------
column_class <- c("factor", "factor", "numeric", "numeric", 
                  "numeric", "numeric", "numeric", "numeric", "numeric") 

column_names <- c("subject", "phase", "block", "trial", 
                  "start", "end", "duration", "x", "y")

raw_data <- read.csv(file = file.path(folder_data, "fixation_data.csv"), 
                     colClasses = column_class, header = T) %>% 
  arrange(subject, phase, block, startT)

colnames(raw_data) <- column_names

# Clean dataset -----------------------------------------------------------
fixation_data <- raw_data %>%        
  mutate(phase = recode(phase, "TP" = 1, "NP" = 2, "EP" = 3),
         phase = as.factor(phase),
         block = as.factor(block),
         trial = as.factor(trial)) %>% 
  mutate(subject = fct_inseq(subject),
         phase = fct_inseq(phase),
         block = fct_inseq(block),
         trial = fct_inseq(trial)) %>% 
  mutate(position = NA,
         pair     = 0,
         side     = "x")

# Get number subjects
subject_ids <- unique(fixation_data$subject)
n_subjects <-  length(subject_ids)


# Classify fixations according to location --------------------------------

# Define the AOIS polygons
# The position number corresponds to reading order (L-R, T-B):

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

# Regardless of the starting point chosen, the order of the vertex should be
# sequential with the shape you intend to sample from.

# Left side
pair1_left_x <- c(233, 433, 433, 233) # x coordinates from vertices
pair1_left_y <- c(289, 289, 489, 489) # y coordinates from vertices

pair2_left_x <- c(530, 730, 730, 530)
pair2_left_y <- c(289, 289, 489, 489)

pair3_left_x <- c(233, 433, 433, 233)
pair3_left_y <- c(586, 586, 786, 786)

pair4_left_x <- c(530, 730, 730, 530)
pair4_left_y <- c(586, 586, 786, 786)

# Right side
pair1_right_x <- c(1193, 1393, 1393, 1193)
pair1_right_y <- c(289, 289, 489, 489)

pair2_right_x <- c(1490, 1690, 1690, 1490)
pair2_right_y <- c(289, 289, 489, 489)

pair3_right_x <- c(1193, 1393, 1393, 1193)
pair3_right_y <- c(586, 586, 786, 786)

pair4_right_x <- c(1490, 1690, 1690, 1490)
pair4_right_y <- c(586, 586, 786, 786)

# Grouped sets of coordinates to make it easier to loop
aois_x <- list(pair1_left_x, pair2_left_x,
               pair3_left_x, pair4_left_x,
               pair1_right_x, pair2_right_x,
               pair3_right_x, pair4_right_x)

aois_y <- list(pair1_left_y, pair2_left_y,
               pair3_left_y, pair4_left_y,
               pair1_right_y, pair2_right_y,
               pair3_right_y, pair4_right_y)

# Define the number of regions to sample
n_AOIs <- length(aois_x)/2

# Detect fixations that fall within any of the AOIS
for (iAOI in 1:n_AOIs) {
  
  # Check if they fall within the left AOIS
  within_left <- as.logical(point.in.polygon(fixation_data$x, 
                                             fixation_data$y, 
                                             aois_x[[iAOI]], 
                                             aois_y[[iAOI]]))
  # Check if they fall within the right AOIS
  within_right <- as.logical(point.in.polygon(fixation_data$x, 
                                              fixation_data$y, 
                                              aois_x[[iAOI + 4]], 
                                              aois_y[[iAOI + 4]]))
  
  # Label the fixations according to the AOI where they fall
  fixation_data$position[within_left]  <- iAOI
  fixation_data$position[within_right] <- iAOI + 4
  
}

# Rename variables, convert types and clean up the database ---------------
aoi_data <- fixation_data %>% 
  mutate(pair = "0",
         side = "x",
         position = as.factor(position))

# Label pairs (position in the grid, regardless of side)
aoi_data$pair[aoi_data$position == 1 | aoi_data$position == 5] <- 1
aoi_data$pair[aoi_data$position == 2 | aoi_data$position == 6] <- 2
aoi_data$pair[aoi_data$position == 3 | aoi_data$position == 7] <- 3
aoi_data$pair[aoi_data$position == 4 | aoi_data$position == 8] <- 4

aoi_data$side[aoi_data$position %in% c(1, 2, 3, 4)] <- "left"
aoi_data$side[aoi_data$position %in% c(5, 6, 7, 8)] <- "right"

# Remove non-aoi fixations, random performers and add domain rank ---------
labeled_fixations <- aoi_data %>% 
  filter(phase == 2) %>% 
  filter(subject %in% domain_rank_key$subject) %>% 
  filter(subject != 556) %>%  #subject with very low number of fixations detected
  filter(!is.na(position)) %>% 
  left_join(domain_rank_key) %>% 
  left_join(scenario_key) %>% 
  left_join(model_key_reduced) %>% 
  na.omit() %>% 
  droplevels()

# Save results ------------------------------------------------------------
save(labeled_fixations, file = file.path(folder_data, "labeled_fixations.RData"))
save(model_key_extended, file = file.path(folder_keys, "model_key_extended.RData"))
save(model_key_reduced, file = file.path(folder_keys, "model_key_reduced.RData"))

