# STATISTICAL MODELS
 
# All models estimated random effects for each participant, nested within 
# groups. Each model was fitted using 4 chains with enough iterations to 
# reach 10,000 ESS in the group level random parameters, in order to have 
# reliable estimation of the standard deviation of the posterior. 

# Given the complexity of the models, we assigned a small step size for 
# the MCMC algorithm (0.99) to get rid of divergences while exploring the 
# parameter space. Correspondingly, we also adjusted the treedepth (12-14) 
# to allow the models to continue exploring for enough time.
 
# For the performance analysis, we used a hierarchical logistic model with
# Gaussian priors for the intercept and the predictors. We centered these 
# priors above 0 (50/50 in log odds) since the participants for this 
# analysis performed different from chance level (since we filtered them
# using a binomial test in previous steps).

# For the fixation benchmarks we used hierarchical multinomial models, with
# the location of the fixations labeled as 1-4 depending on the domain
# importance. For analysis during decision scenarios, we added a predictor
# for scenario (0-3).

# All these multinomial models have priors for the random intercepts and 
# slopes are skew normal distributions centered around the -1.0987, which
# corresponds to the logodds from 25%. This is because the maximum entropy
# scenario for the allocation of fixations across domains is equal number
# of events per domain. The positive skew in the distribution allows us to 
# allocate similar probability mass above and below 25%.

# For all the priors for the covariance matrices between random intercepts
# and slopes, we used a mildly regularizing prior of 2 for the rho parameter. 
# For the variability among participants and groups, we used a mildly 
# informative half Gaussian prior centered at 0, with a sigma of 2.

# Setup -------------------------------------------------------------------
# Load the required packages
library(rstan)
library(brms) 
library(tidybayes)
library(tidyverse)
library(scales)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Define base directories and load data -----------------------------------
folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_data <- file.path(folder_root, "Results", "Datasets")
folder_fits <- file.path(folder_root, "Results", "Fits")

# Load datasets for analysis ----------------------------------------------
load(file = file.path(folder_data, "analysis_database.RData")) 

# Define priors -----------------------------------------------------------
priors_logistic <- c(
  
  prior(normal(2, 2), class = b),
  prior(normal(2, 2), class = Intercept),
  prior(lkj(2), class = cor), 
  prior(normal(0, 2), class = sd)
  )

priors_multinomial <- c(
  
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu1),
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu2),
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu3),
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu4),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu1),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu1),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu2),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu2),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu3),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu3),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu4),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu4)
  )

priors_multinomial_scenario <- c(
  
  prior(skew_normal(-1.0987, 2, 8), class = b, dpar = mu1),
  prior(skew_normal(-1.0987, 2, 8), class = b, dpar = mu2),
  prior(skew_normal(-1.0987, 2, 8), class = b, dpar = mu3),
  prior(skew_normal(-1.0987, 2, 8), class = b, dpar = mu4),
  
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu1),
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu2),
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu3),
  prior(skew_normal(-1.0987, 2, 8), class = Intercept, dpar = mu4),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu1),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu1),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu2),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu2),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu3),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu3),
  
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy, dpar = mu4),
  prior(normal(0, 2), class = sd, coef = Intercept, group = strategy:subject, dpar = mu4)
  )

# Performance -------------------------------------------------------------
performance2.1 <- brm(data   = performance,
                      family = binomial,
                      
                      formula = correct | trials(total) ~ 1 + experiment * phase + (1 + experiment * phase | strategy/subject), 
                      
                      prior = priors_logistic,
                      
                      iter    = 6000, 
                      warmup  = 1000,
                      chains  = 4, 
                      cores   = 4,  
                      control = list(adapt_delta = 0.99, max_treedepth = 12), 
                      file    = file.path(folder_fits, "performance2.1")
                      )

# First fixation ----------------------------------------------------------
first2.1 <- brm(data   = first_fixations,
                family = multinomial(refcat = NA, link   = logit),
                
                formula = y | trials(total) ~ 1 + (1 | strategy/subject), 
                
                prior = priors_multinomial,
                
                iter    = 6000, 
                warmup  = 1000,
                chains  = 4, 
                cores   = 4,  
                control = list(adapt_delta = 0.99, max_treedepth = 12), 
                file    = file.path(folder_fits, "first2.1") 
                )

# First fixation by scenario ---------------------------------------------
first_scenario2.1 <- brm(data   = first_fixations_scenario,
                         family = multinomial(refcat = NA, link   = logit),
                         
                         formula = y | trials(total) ~ 1 + scenario + 
                                                      (1 + scenario | strategy/subject), 
                         
                         prior = priors_multinomial_scenario,
                         
                         iter    = 6000, 
                         warmup  = 1000,
                         chains  = 4,  
                         cores   = 4,  
                         control = list(adapt_delta = 0.99, max_treedepth = 12), 
                         file    = file.path(folder_fits, "first_scenario2.1") 
                         )

# Proportion of fixations  ------------------------------------------------
proportion2.1<- brm(data    = proportion_fixations,
                    family  = multinomial(refcat = NA, link = logit),
                    
                    formula = y | trials(total) ~ 1 + 
                                                 (1 | strategy/subject), 
                
                    prior = priors_multinomial,
                
                    iter    = 6000, 
                    warmup  = 1000, 
                    chains  = 4, 
                    cores   = 4,  
                    control = list(adapt_delta = 0.99, max_treedepth = 13), 
                    file    = file.path(folder_fits, "proportion2.1")
                    )

# Proportion of fixations by scenario -------------------------------------
proportion_scenario2.1 <- brm(data    = proportion_fixations_scenario,
                              family  = multinomial(refcat = NA, link   = logit),
                          
                          formula = y | trials(total) ~ 1 + scenario + 
                                                       (1 + scenario | strategy/subject),
                          
                          prior = priors_multinomial_scenario,
                          
                          iter    = 6000, 
                          warmup  = 1000, 
                          chains  = 4, 
                          cores   = 4,  
                          control = list(adapt_delta = 0.99, max_treedepth = 14), 
                          file    = file.path(folder_fits, "proportion_scenario2.1")
                          )

# Last fixation -----------------------------------------------------------
last2.1 <- brm(data   = last_fixations,
               family = multinomial(refcat = NA, link   = logit),
               
               formula = y | trials(total) ~ 1 + 
                                            (1 | strategy/subject), 
               
               prior = priors_multinomial,
               
               iter    = 6000, 
               warmup  = 1000,
               chains  = 4, 
               cores   = 4,  
               control = list(adapt_delta = 0.99, max_treedepth = 12), 
               file    = file.path(folder_fits, "last2.1") 
               )

# Last fixation by Scenario -----------------------------------------------
last_scenario2.1 <- brm(data   = last_fixations_scenario,
                        family = multinomial(refcat = NA, link   = logit),
                        
                        formula = y | trials(total) ~ 1 + scenario + 
                                                     (1 + scenario | strategy/subject), 
                        
                        prior = priors_multinomial_scenario,
                         
                        iter    = 6000, 
                        warmup  = 1000,
                        chains  = 4, 
                        cores   = 4,  
                        control = list(adapt_delta = 0.99, max_treedepth = 13), 
                        file    = file.path(folder_fits, "last_scenario2.1") 
                        )


