# Trial Generator

# This script splits each block of data into single trial files. We do this 
# since the I2MC algorithm for fixation detection requires each trial to be
# its own file to work appropriately.

# These new .txt files will be saved in a folder per subject inside the 
# directory "Separated".

# During the process, we also perform some other tweaks necessary for the 
# proper functioning of the algorithm. 
# 
# First, we convert the data to the appropriate types (numerical or 
# characters) to be sure that they are loaded properly in MATLAB. Then, we 
# need to convert the time scale from microseconds to milliseconds. Finally, 
# we define any period where the tracker lost track of the gaze as NAN, 
# since the default behavior from the device is to define those as 0's in 
# the x and y coordinates, and can throw off the detection algorithm.

# Created by Eduardo Rea
# Multicue Probabilistic Project
# NLP Lab UMass AMherst
# December 2019


# Define base paths -------------------------------------------------------
require(tidyverse)
#require(spatialEco) #for the is.empty function

folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_data <- file.path(folder_root, "Data", "Tracker")

folder_samples  <- file.path(folder_data, "Samples")
folder_messages <- file.path(folder_data, "Messages")
folder_results  <- file.path(folder_data, "Separated")

# Create output folder
dir.create(folder_results, showWarnings = FALSE)

list_messages <- list.files(folder_messages)
list_samples  <- list.files(folder_samples)

# Get all the files to use ------------------------------------------------
name_list <- as.data.frame(list_samples) %>% 
  separate(list_samples, into = c("id", "phase", "block", "name", "file")) 

list_phases   <- unique(name_list$phase)
subject_list <- unique(name_list$id)  

n_phases   <- length(list_phases)
n_subjects <- length(subject_list)

block_sequence <- c(2, 2, 4) #EP, NP, TP


# Extract data from subjects ----------------------------------------------
for (iSubject in 1:n_subjects) {
  
  subject_id <- subject_list[iSubject]
  
  folder_subject <- file.path(folder_results, subject_id)
  
  # If the subject hasn't been processed, create its own folder to save its 
  # separated trial files
  if (!dir.exists(folder_subject)) {
    
    dir.create(folder_subject, showWarnings = FALSE)
    
    subject_samples <- list.files(folder_samples, 
                                  pattern = subject_id)
    
    subject_messages <- list.files(folder_messages, 
                                   pattern = subject_id)
    
    n_files <- length(subject_samples)
    
    
    # Loop through the files from each subject
    for (iFile in 1:n_files) {
      
      tracking_data <- read.delim2(file.path(folder_samples, 
                                             subject_samples[iFile]), 
                                   row.names = NULL, 
                                   skip = 37) %>% 
        
        select(time = 1, 
               x_left  = 4, y_left  = 5, 
               x_right = 6, y_right = 7, 
               valid_left = 9, valid_right = 10) %>% 
        
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
      # We will use these to define when the trial started and ended (start of next trial)
      message_data <- read.delim2(file.path(folder_messages, 
                                            subject_messages[iFile]),
                                  row.names = NULL, 
                                  skip = 60) %>% # large number to avoid cases where the tracker was calibrated many times
        
        select("time" = 1, "message" = 4) %>% 
        filter(str_detect(message, "block") | spatialEco::is.empty(message)) %>%
        separate(message, into = c(NA, NA, "phase", NA, "block", NA, "trial"), sep = "_| ", fill = ) %>% 
        fill(phase, block, trial) %>% 
        na.omit() %>% 
        
        group_by(phase, block, trial) %>% 
        summarise(start = min(time), end = max(time), .groups = 'drop') %>% 
        ungroup() %>%
        arrange(phase, block, start)
      
      # Define the number of trials detected in this block
      n_trials <- dim(message_data)[1]
      
      # Create base name for .txt files from each trial
      name_elements <- strsplit(strsplit(subject_samples[iFile], " ")[[1]][1], "_")
      
      name_phase <- name_elements[[1]][2]
      name_block <- strsplit(name_elements[[1]][3], "Block")[[1]][2]
      
      name_base <- paste(subject_id, name_phase, name_block, sep = "_")
      
      # Loop to separate the individual trials
      for (iTrial in 1:n_trials) {
        
        # Define the file name to save the data
        trial_file_name <- paste(name_base, iTrial, sep = "_")
         
        trial_file_path <- file.path(folder_subject, paste(trial_file_name, ".txt", sep = ""))
        
        if (!file.exists(trial_file_path)) {
          
          # Split the data into trials according to the period defined
          # previously in the messages from the tracker
          trial_data <- tracking_data[tracking_data$time > message_data$start[iTrial] 
                                      & tracking_data$time < message_data$end[iTrial], ] %>% 
            
            mutate(time = time/1000) # Convert to milliseconds
          
          # Save the data to an individual tab separated file for each trial
          write.table(trial_data,
                      file      = trial_file_path,
                      sep       = "\t",
                      quote     = F,
                      qmethod   = "double",
                      row.names = F)

          } #End of check to skip if the .txt file already exists
        
        } #End of trial loop
      
      } #End of file loop
    
    } #End of check to skip processed folders
  
  } #End of subject loop
