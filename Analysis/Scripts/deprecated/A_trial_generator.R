# Trial Generator

# This script splits each block of data into single trial files.
# This is done since the I2MC algorithm for fixation detection
# requires each trial to be its own file to work appropiately.

# These new txt files will be saved in a folder per subject 
# inside the directory "Subject_files".

# During the process, we also perform some other tweaks necessary
# for the proper functioning of the algorithm. First, we convert 
# the data to the appropiate types (numerical or characters) to 
# be sure that they are loaded properly in Matlab. Then, we need
# to convert the time scale from microseconds to miliseconds. 
# Finally, we define any period where the tracker lost track of 
# the gaze as NAN, since the default behavior from the device is 
# to define those as 0's in the x and y coordinates, and can throw
# off the detection algorithm.

# Created by Eduardo Rea
# Multicue Probabilistic Project
# NLP Lab UMass AMherst
# December 2019

# Define base paths ####
library(tidyverse)
root_dir <- "C:/Users/rerr_/Google Drive/Graduate/Lab/Studies/MultiCue_Probabilistic/three_phases/INV/Analysis/Eye_tracking"
setwd(root_dir)

data_dir     <- file.path(root_dir, "Data")
messages_dir <- file.path(root_dir, "Messages")
output_dir   <- file.path(root_dir, "Subject_files")

# Get all the files to use ####
messages_list <- list.files(messages_dir)
data_list     <- list.files(data_dir)

name_list <- as.data.frame(data_list) %>% 
  separate(data_list, into = c("id", "phase", "block", "name", "file")) %>% 
  filter(as.numeric(id) > 585)

phase_list   <- unique(name_list$phase)
subject_list <- unique(name_list$id)  

n_phases   <- length(phase_list)
n_subjects <- length(subject_list)

block_sequence <- c(2, 2, 4) #EP, NP, TP

# Loop through the subjects
for (iSubject in 1:n_subjects) {
  
  subject_id <- subject_list[iSubject]
  
  subject_dir <- file.path(output_dir, subject_id)
  
  dir.create(subject_dir, showWarnings = FALSE)
  
  # Loop through the different phases
  for (iPhase in 1:n_phases) {
    
    n_blocks <- block_sequence[iPhase]
    
    # Loop through each block
    for (iBlock in 1:n_blocks) {
      
      name_block <- paste("Block", iBlock, sep = "")
      
      # Load the data from each block's file
      tracking_data <- read.delim2(file.path(data_dir, 
                                             paste(subject_id, "_", 
                                                   phase_list[iPhase],                                                  "_",
                                                   name_block, " Samples.txt",
                                                   sep = "")), 
                                   row.names = NULL, 
                                   skip = 37) %>% 
        
        # Select the columns we need and name them
        select(time = 1, 
               x_left  = 4, y_left  = 5, 
               x_right = 6, y_right = 7, 
               valid_left = 9, valid_right = 10) %>% 
        
        # Convert the values as needed
        mutate(time    = as.double(time),
               x_left  = as.character(x_left),
               x_right = as.character(x_right),
               y_left  = as.character(y_left),
               y_right = as.character(y_right))
      
      # Define the periods with no data (0's in coordinates detected)
      # as NAN, so the I2MC algorithm skips them when detecting fixations
      tracking_data$x_left[tracking_data$x_left == "0.0000"] <- "NaN"
      tracking_data$x_right[tracking_data$x_right == "0.0000"] <- "NaN"
      tracking_data$y_left[tracking_data$y_left == "0.0000"] <- "NaN"
      tracking_data$y_right[tracking_data$y_right == "0.0000"] <- "NaN"
      
      # Load the flags and messages from the tracker
      # We will use this to define when the trial started and ended (start of next trial)
      message_data <- read.delim2(file.path(messages_dir, 
                                            paste(subject_id,
                                                  "_",
                                                  phase_list[iPhase],
                                                  "_",
                                                  name_block,
                                                  " Samples.txt",
                                                  sep = "")), 
                                  row.names = NULL, 
                                  skip = 60) %>% # large number to avoid cases where the tracker was calibrated many times
        
        select(time = 1, message = 4) %>% 
        
        filter(str_detect(message, "block")) %>%
        
        mutate(id = subject_list[iSubject]) %>% 
        
        mutate(end = lead(time, 1))
      
      # Define the number of trials detected in this block
      n_trials <- dim(message_data)[1]
      
      # Loop to separate the individual trials
      for (iTrial in 1:n_trials) {
      
        # Split the data into trials according to the period defined
        # previously in the messages from the tracker
        trial_data <- tracking_data[tracking_data$time > message_data$time[iTrial] 
                                    & tracking_data$time < message_data$end[iTrial], ] %>% 
          
          mutate(time = time/1000) # Convert to miliseconds
        
        # Define the file name to save the data
        trial_file_name <- paste(subject_id, phase_list[iPhase], iBlock, iTrial,
                                 sep = "_")
        
        # Save the data to an individual tab separated file for each trial
        write.table(trial_data, 
                    file      = file.path(subject_dir, paste(trial_file_name, ".txt", sep = "")), 
                    sep       = "\t", 
                    quote     = F, 
                    qmethod   = "double",
                    row.names = F)
      
      } # End of trial loop

    } # End of block loop
    
  } # End of phase loop
  
} # End of subject loop
