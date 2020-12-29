# Get required libraries and tools
require(tidyverse)

# Load the set of useful functions that Kruschke coded
source("C:/Users/rerr_/Google Drive/Graduate/Lab/Studies/MyFunctions/DBDA2Eprograms/DBDA2E-utilities.R")

# Load behavior dataset
setwd("C:/Users/rerr_/Google Drive/Graduate/Lab/Studies/MultiCue_Probabilistic/three_phases/Analysis/Fixation_detection/")


raw_data <- read.csv(file = "behavior_dataset.csv", encoding = "UTF-8") %>% 
  mutate(correct = 0) %>% 
  mutate(stage   = block) %>% 
  mutate(phase = as.factor(phase)) %>% 
  mutate(block = as.factor(block))

behavior_data <- raw_data

behavior_data$correct[behavior_data$p_left < 0.5 & behavior_data$response == -1] <- 1
behavior_data$correct[behavior_data$p_left > 0.5 & behavior_data$response == 1] <- 1

behavior_data$trial <- rep(1:60, (dim(behavior_data)[1]/480))

behavior_data$stage[behavior_data$phase == 2 & behavior_data$block == 1] <- 5
behavior_data$stage[behavior_data$phase == 2 & behavior_data$block == 2] <- 6
behavior_data$stage[behavior_data$phase == 3 & behavior_data$block == 1] <- 7
behavior_data$stage[behavior_data$phase == 3 & behavior_data$block == 2] <- 8

## Do binomial test to identify random performers
binomial_data <- behavior_data %>% 
  filter(phase != 3) %>% 
  filter(p_left != 0.5) %>% 
  na.omit() %>% 
  group_by(id) %>% 
  mutate(p_binom = as.numeric(binom.test(x = sum(correct), 
                                         n = 324, #Total of non random trials
                                         p = 0.5, 
                                         alternative = "two.sided")[[3]])) %>% 
  filter(p_binom < 0.05) %>% 
  filter(id != 377) %>% # remove problematic subject
  mutate(accuracy = mean(correct)) %>% 
  select(id, p_binom, accuracy) %>% 
  unique() %>% 
  ungroup()

# Remove subjects random response subjects
behavior_data <- behavior_data %>% 
  filter(id %in% binomial_data$id) %>% 
  select(id, phase, stage, trial, sorted1, sorted2, sorted3, sorted4, soe = evidence, response, correct) %>% 
  mutate(soe_ln = soe * log(10))

subject_code <- behavior_data %>% 
  select(id) %>% 
  unique() %>% 
  mutate(code = 1:length(unique(behavior_data$id)))

modeling_data <- full_join(behavior_data, subject_code)

# Recode responses to the right as 0
modeling_data$response[modeling_data$response == -1] <- 0

n_subjects <- length(unique(modeling_data$code))
n_blocks   <- length(unique(modeling_data$stage))
n_trials   <- length(modeling_data$trial)
n_blocks   <- length(unique(modeling_data$stage))

modeling_data$id <- as.numeric(modeling_data$code)

data_list <- list(subj_id    = modeling_data$code,
                  response   = as.numeric(modeling_data$response),
                  block      = as.numeric(modeling_data$stage),
                  soe        = as.numeric(modeling_data$soe_ln),
                  n_subjects = n_subjects,
                  n_trials   = n_trials,
                  n_blocks   = n_blocks)

detectCores() #see how many cores you have
# Kruschke's utilities code sets best
#  default chain #'s and methods for
#  your machine. Variables that hold
#  those values:
nChainsDefault
runjagsMethodDefault

model_string = "
   model{
        
        for(iTrial in 1:n_trials){

          response[iTrial] ~ dbern(theta[iTrial])
          
          theta[iTrial] <- ilogit(beta0[subj_id[iTrial], block[iTrial]] + beta1[subj_id[iTrial], block[iTrial]] * soe[iTrial])

        } #End of trial loop
          

        for(iBlock in 1:n_blocks){	
          for (iSubject in 1:n_subjects) {


            beta0[iSubject, iBlock] ~ dnorm(muBeta0[iBlock], 
                                            1/sigmaBeta0[iBlock]^2)
            
            beta1[iSubject, iBlock] ~ dnorm(muBeta1[iBlock], 
                                            1/sigmaBeta1[iBlock]^2)
                    
                    
          } #End of subject loop
        
        } #End of block loop

        
        
        for(iBlock in 1:n_blocks){		

          muBeta0[iBlock] ~ dnorm(muMuBeta0, 
                                  1/sigmaMuBeta0^2)
          
          muBeta1[iBlock] ~ dnorm(muMuBeta1, 
                                  1/sigmaMuBeta1^2)
          
          
          sigmaBeta0[iBlock] ~ dnorm(muSigmaBeta0, 
                                     1/sigmaSigmaBeta0^2)T(0.1, )
          
          
          sigmaBeta1[iBlock] ~ dnorm(muSigmaBeta1, 
                                     1/sigmaSigmaBeta1^2)T(0.1, )
          
        } #End of block loop


        muMuBeta0    ~ dnorm(0, 1/1)
        sigmaMuBeta0 ~ dnorm(0.5, 1/0.5^2)T(0.1, )

        muMuBeta1    ~ dnorm(0, 1/2^2)
        sigmaMuBeta1 ~ dnorm(0.5, 1/1^2)T(0.1, )


        muSigmaBeta0    ~ dnorm(0, 1/1)T(0.1, )
        sigmaSigmaBeta0 ~ dnorm(0.5, 1/0.5^2)T(0.1, )

        muSigmaBeta1    ~ dnorm(0, 1/2^2)T(0.1, )
        sigmaSigmaBeta1 ~ dnorm(0.5, 1/1^2)T(0.1, )
}
 "


writeLines(model_string, con = "response_logistic_model.txt")


runJagsOut = run.jags(method  = runjagsMethodDefault, #"parallel" or "rjags"
                      model   = "response_logistic_model.txt",
                      monitor = c('beta0','beta1',
                                  'muBeta0', 'sigmaBeta0',
                                  'muBeta1', 'sigmaBeta1',
                                  'muMuBeta0', 'sigmaMuBeta0',
                                  'muMuBeta1', 'sigmaMuBeta1',
                                  'muSigmaBeta0', 'sigmaSigmaBeta0',
                                  'muSigmaBeta1', 'sigmaSigmaBeta1'),
                      data = data_list,  #inits=initsList,
                      n.chains = nChainsDefault,
                      adapt    = 1000,
                      burnin   = 1000,
                      sample   = 5000,
                      thin     = 3,
                      summarise = FALSE, 
                      plots     = FALSE)

coda_samples <-  as.mcmc.list(runJagsOut)

chain_stats <- summary(runJagsOut)

#Combine chains into one big matrix
all_samples <- do.call(rbind, coda_samples)

saveRDS(all_samples, file = "reareyes_all_samples.rds")
saveRDS(chain_stats, file = "reareyes_chain_stats.rds")
saveRDS(modeling_data, file = "reareyes_modeling_data.rds")
