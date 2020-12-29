# PERFORM PLANNED COMPARISONS AND CREATE SUMMARIES

# This script takes the posterior samples from the bayesian models and 
# performs planned paired comparisons between groups and levels. It uses
# 3 supplementary functions to avoid repetitions


# Setup -------------------------------------------------------------------
# Load required packages
library(rstan)
library(brms) 
library(tidybayes)
library(tidyverse)
library(scales)

# Define base directories
folder_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_fits <- file.path(folder_root, "Results", "Fits")
folder_summaries <- file.path(folder_root, "Results", "Summaries")
folder_functions <- file.path(folder_root, "Scripts", "Functions")

# Load the models to extract draws
performance <- readRDS(file.path(folder_fits, "performance2.1.rds"))

first_fixations          <- readRDS(file.path(folder_fits, "first2.1.rds"))
first_fixations_scenario <- readRDS(file.path(folder_fits, "first_scenario2.1.rds"))

last_fixations          <- readRDS(file.path(folder_fits, "last2.1.rds"))
last_fixations_scenario <- readRDS(file.path(folder_fits, "last_scenario2.1.rds"))

proportion_fixations          <- readRDS(file.path(folder_fits, "proportion2.1.rds"))
proportion_fixations_scenario <- readRDS(file.path(folder_fits, "proportion_scenario2.1.rds"))

# Load functions for extracting and cleaning posterior --------------------
source(file.path(folder_functions, "F02_clean_posterior.R"))
source(file.path(folder_functions, "F02_rope.R"))
source(file.path(folder_functions, "F02_comparisons.R"))

# Tibbles for group effects -----------------------------------------------
effect_performance <- performance$data %>% 
  select(strategy, experiment, phase) %>% 
  unique() %>% 
  mutate(total = 120)

effect_single <- first_fixations$data %>% 
  select(strategy) %>% 
  unique() %>% 
  mutate(total = 120)

effect_single_scenario <- first_fixations_scenario$data %>% 
  select(strategy, scenario) %>% 
  unique() %>% 
  mutate(total = 120)

effect_multiple <- proportion_fixations$data %>% 
  select(strategy) %>% 
  unique() %>% 
  mutate(total = 100)

effect_multiple_scenario <- proportion_fixations_scenario$data %>% 
  select(strategy, scenario) %>% 
  unique() %>% 
  mutate(total = 100)

# Tibbles to extract individual medians -----------------------------------
effect_ind_performance <- performance$data %>% 
  select(subject, strategy, experiment, phase) %>% 
  unique() %>% 
  mutate(total = 120)

effect_ind_single <- first_fixations$data %>% 
  select(subject, strategy) %>% 
  unique() %>% 
  mutate(total = 120)

effect_ind_single_scenario <- first_fixations_scenario$data %>% 
  select(subject, strategy, scenario) %>% 
  unique() %>% 
  mutate(total = 120)

effect_ind_multiple <- proportion_fixations$data %>% 
  select(subject, strategy) %>% 
  unique() %>% 
  mutate(total = 100)

effect_ind_multiple_scenario <- proportion_fixations_scenario$data %>% 
  select(subject, strategy, scenario) %>% 
  unique() %>% 
  mutate(total = 100)

# Formulas for group effects ----------------------------------------------
formula_experiment_phase <- brmsformula(correct | trials(total) ~ 1 + experiment * phase + (1 + experiment * phase | strategy))

formula_intercept <- brmsformula(y | trials(total) ~ 1 + (1 | strategy))

formula_scenario <- brmsformula(y | trials(total) ~ 1 + scenario + (1 + scenario| strategy))


# Formulas for individual effects -----------------------------------------
formula_ind_experiment_phase <- brmsformula(correct | trials(total) ~ 1 + experiment * phase + (1 + experiment * phase | strategy/subject))

formula_ind_intercept <- brmsformula(y | trials(total) ~ 1 + (1 | strategy/subject))

formula_ind_scenario <- brmsformula(y | trials(total) ~ 1 + scenario + (1 + scenario| strategy/subject))

# Performance -------------------------------------------------------------
# Group Effects
samples_performance <- tidy_posterior(posterior = performance, 
                                      effects   = effect_performance, 
                                      formula   = formula_experiment_phase)

intervals_performance <- samples_performance %>% 
  group_by(strategy, experiment, phase) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()

# Individual estimates
individual_performance <- tidy_posterior(posterior = performance, 
                                         effects   = effect_ind_performance, 
                                         formula   = formula_ind_experiment_phase) %>% 
  
  group_by(subject, strategy, experiment, phase) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

# Follow-up comparisons
comp_performance_change <- samples_performance %>% 
  mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
  select(-percent) %>% 
  pivot_wider(id_cols = c(draw, experiment, strategy), names_from = phase, values_from = estimate) %>% 
  mutate(difference = `1`-`0`) %>% 
  na.omit() %>% 
  filter_all(all_vars(!is.infinite(.)))

comp_performance_group_diff <- comp_performance_change %>% 
  select(draw, experiment, strategy, difference) %>% 
  pivot_wider(id_cols = c(draw, experiment), names_from = strategy, values_from = difference) %>% 
  mutate(first_second = `1st only` - `2nd only`,
         first_serial = `1st only` - `Serial Search`,
         first_tally = `1st only` - `Tallying`,
         first_lazy = `1st only` - `Partial Tallying`,
         second_serial = `2nd only` - `Serial Search`,
         second_tally = `2nd only` - `Tallying`,
         second_lazy = `2nd only` - `Partial Tallying`,
         serial_tally = `Serial Search` - `Tallying`,
         serial_lazy = `Serial Search` - `Partial Tallying`,
         tally_lazy = `Tallying` - `Partial Tallying`) %>% 
  select(-c(`1st only`, `2nd only`, `Serial Search`, `Tallying`, `Partial Tallying`)) %>% 
  pivot_longer(cols = -c(draw, experiment), names_to = "comparison", values_to = "difference")

comp_performance_T1 <- comp_performance_change %>% 
  select(draw, experiment, strategy, `1`) %>% 
  pivot_wider(id_cols = c(draw, experiment), names_from = strategy, values_from = `1`) %>% 
  mutate(draw = 1:length(experiment)) %>% 
  mutate(first_second = `1st only` - `2nd only`,
         first_serial = `1st only` - `Serial Search`,
         first_tally = `1st only` - `Tallying`,
         first_lazy = `1st only` - `Partial Tallying`,
         second_serial = `2nd only` - `Serial Search`,
         second_tally = `2nd only` - `Tallying`,
         second_lazy = `2nd only` - `Partial Tallying`,
         serial_tally = `Serial Search` - `Tallying`,
         serial_lazy = `Serial Search` - `Partial Tallying`,
         tally_lazy = `Tallying` - `Partial Tallying`) %>% 
  select(-c(experiment, `1st only`, `2nd only`, `Serial Search`, `Tallying`, `Partial Tallying`)) %>% 
  pivot_longer(cols = -c(draw), names_to = "comparison", values_to = "difference")

rope_performance_change <- rope_summary(comp_performance_change, c("experiment", "strategy")) 

rope_performance_group_diff <- rope_summary(comp_performance_group_diff, c("experiment", "comparison"))

rope_performance_T1 <- rope_summary(comp_performance_T1, c("comparison"))

# First fixations ---------------------------------------------------------
# Group effects
samples_first <- tidy_posterior(posterior = first_fixations, 
                                effects   = effect_single, 
                                formula   = formula_intercept)

intervals_first <- samples_first %>% 
  group_by(strategy, domain) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()


samples_first_scenario <- tidy_posterior(posterior = first_fixations_scenario, 
                                         effects   = effect_single_scenario, 
                                         formula   = formula_scenario)

intervals_first_scenario <- samples_first_scenario %>% 
  group_by(strategy, scenario, domain) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()

# Individual estimates
individual_first<- tidy_posterior(posterior = first_fixations,
                                  effects   = effect_ind_single, 
                                  formula   = formula_ind_intercept) %>%
  group_by(subject, strategy, domain) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

individual_first_scenario <- tidy_posterior(posterior = first_fixations_scenario,
                                            effects   = effect_ind_single_scenario, 
                                            formula   = formula_ind_scenario) %>%
  group_by(subject, strategy, scenario, domain) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

# Follow-up comparisons
comp_first <- planned_comparisons(samples_first, scenarios = FALSE) %>% 
  filter(strategy == "1st only" & comparison == "d1_all" |
         strategy == "2nd only" & comparison == "d2_all" |
         strategy == "Serial Search" & comparison == "d1_all" |
         strategy == "Tallying" & comparison == "d1_2" |
         strategy == "Tallying" & comparison == "d1_3" | 
         strategy == "Tallying" & comparison == "d1_4" | 
         strategy == "Tallying" & comparison == "d2_3" | 
         strategy == "Tallying" & comparison == "d2_4" | 
         strategy == "Tallying" & comparison == "d3_4" |
         strategy == "Partial Tallying" & comparison == "d12_34" |
         strategy == "Partial Tallying" & comparison == "d1_2" |   
         strategy == "Partial Tallying" & comparison == "d3_4")

rope_first <- rope_summary(comp_first, c("strategy", "comparison")) 

comp_first_scenario <- planned_comparisons(samples_first_scenario, scenarios = TRUE) %>% 
  filter(strategy == "1st only" & comparison == "d1_all" |
         strategy == "2nd only" & comparison == "d2_all" |
         strategy == "Serial Search" & comparison == "d1_all" |
         strategy == "Tallying" & comparison == "d1_2" |
         strategy == "Tallying" & comparison == "d1_3" | 
         strategy == "Tallying" & comparison == "d1_4" | 
         strategy == "Tallying" & comparison == "d2_3" | 
         strategy == "Tallying" & comparison == "d2_4" | 
         strategy == "Tallying" & comparison == "d3_4" |
         strategy == "Partial Tallying" & comparison == "d12_34" |
         strategy == "Partial Tallying" & comparison == "d1_2" |   
         strategy == "Partial Tallying" & comparison == "d3_4")

rope_first_scenario <- rope_summary(comp_first_scenario, c("strategy", "scenario", "comparison")) 

# Proportion of fixations -------------------------------------------------
samples_proportion <- tidy_posterior(posterior = proportion_fixations, 
                                     effects   = effect_multiple, 
                                     formula   = formula_intercept)

intervals_proportion <- samples_proportion %>% 
  group_by(strategy, domain) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()

samples_proportion_scenario <- tidy_posterior(posterior = proportion_fixations_scenario, 
                                              effects   = effect_multiple_scenario, 
                                              formula   = formula_scenario)

intervals_proportion_scenario <- samples_proportion_scenario %>% 
  group_by(strategy, scenario, domain) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()


# Individual estimates
individual_proportion <- tidy_posterior(posterior = proportion_fixations,
                                        effects   = effect_ind_multiple, 
                                        formula   = formula_ind_intercept) %>%
  group_by(subject, strategy, domain) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

individual_proportion_scenario <- tidy_posterior(posterior = proportion_fixations_scenario,
                                                 effects   = effect_ind_multiple_scenario, 
                                                 formula   = formula_ind_scenario) %>%
  group_by(subject, strategy, scenario, domain) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

# Follow-up comparisons
comp_proportion <- planned_comparisons(samples_proportion, scenarios = FALSE) %>% 
  filter(strategy == "1st only" & comparison == "d1_all" |
         strategy == "2nd only" & comparison == "d2_all" |
         strategy == "Serial Search" & comparison == "d1_2" |
         strategy == "Serial Search" & comparison == "d2_3" |
         strategy == "Serial Search" & comparison == "d3_4" |
         strategy == "Serial Search" & comparison == "lin1" |
         strategy == "Serial Search" & comparison == "lin2" |
         strategy == "Serial Search" & comparison == "lin3" |
         strategy == "Tallying" & comparison == "d1_2" |
         strategy == "Tallying" & comparison == "d1_3" | 
         strategy == "Tallying" & comparison == "d1_4" | 
         strategy == "Tallying" & comparison == "d2_3" | 
         strategy == "Tallying" & comparison == "d2_4" | 
         strategy == "Tallying" & comparison == "d3_4" |
         strategy == "Partial Tallying" & comparison == "d12_34" |
         strategy == "Partial Tallying" & comparison == "d1_2" |   
         strategy == "Partial Tallying" & comparison == "d3_4")

rope_proportion <- rope_summary(comp_proportion, c("strategy", "comparison")) 

comp_proportion_scenario <- planned_comparisons(samples_proportion_scenario, scenarios = TRUE) %>% 
  filter(strategy == "1st only" & comparison == "d1_all" |
         strategy == "2nd only" & comparison == "d2_all" |
         strategy == "Serial Search" & scenario == "0" & comparison == "d1_all" |
         strategy == "Serial Search" & scenario == "1" & comparison == "d1_2" |
         strategy == "Serial Search" & scenario == "1" & comparison == "d12_34" |
         strategy == "Serial Search" & scenario == "2" & comparison == "d1_2" |
         strategy == "Serial Search" & scenario == "2" & comparison == "d1_3" |
         strategy == "Serial Search" & scenario == "2" & comparison == "d2_3" |  
         strategy == "Serial Search" & scenario == "2" & comparison == "d3_4" |
         strategy == "Serial Search" & scenario == "3" & comparison == "d1_2" |
         strategy == "Serial Search" & scenario == "3" & comparison == "d1_3" |
         strategy == "Serial Search" & scenario == "3" & comparison == "d2_3" |  
         strategy == "Serial Search" & scenario == "3" & comparison == "d3_4" |
         strategy == "Tallying" & comparison == "d1_2" |
         strategy == "Tallying" & comparison == "d1_3" | 
         strategy == "Tallying" & comparison == "d1_4" | 
         strategy == "Tallying" & comparison == "d2_3" | 
         strategy == "Tallying" & comparison == "d2_4" | 
         strategy == "Tallying" & comparison == "d3_4" |
         strategy == "Partial Tallying" & comparison == "d12_34" |
         strategy == "Partial Tallying" & comparison == "d1_2" |   
         strategy == "Partial Tallying" & comparison == "d3_4")

rope_proportion_scenario <- rope_summary(comp_proportion_scenario, c("strategy", "scenario", "comparison")) 

# Last fixations ----------------------------------------------------------
samples_last <- tidy_posterior(posterior = last_fixations, 
                               effects   = effect_single, 
                               formula   = formula_intercept)

intervals_last <- samples_last %>% 
  group_by(strategy, domain) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()


samples_last_scenario <- tidy_posterior(posterior = last_fixations_scenario, 
                                        effects   = effect_single_scenario, 
                                        formula   = formula_scenario)

intervals_last_scenario <- samples_last_scenario %>% 
  group_by(strategy, scenario, domain) %>% 
  summarise("mean"  = mean(percent),
            "upper" = mean + sd(percent),
            "lower" = mean - sd(percent)) %>% 
  ungroup()

# Individual estimates
individual_last <- tidy_posterior(posterior = last_fixations,
                                  effects   = effect_ind_single, 
                                  formula   = formula_ind_intercept) %>%
  group_by(subject, strategy, domain) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

individual_last_scenario <- tidy_posterior(posterior = last_fixations_scenario,
                                           effects   = effect_ind_single_scenario, 
                                           formula   = formula_ind_scenario) %>%
  group_by(subject, strategy, scenario, domain) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

# Follow-up comparisons
comp_last <- planned_comparisons(samples_last, scenarios = FALSE) %>% 
  filter(strategy == "1st only" & comparison == "d1_all" |
         strategy == "2nd only" & comparison == "d2_all" |
         strategy == "Serial Search" & comparison == "d1_2" |
         strategy == "Serial Search" & comparison == "d2_3" |
         strategy == "Serial Search" & comparison == "d3_4" |
         strategy == "Serial Search" & comparison == "lin1" |
         strategy == "Serial Search" & comparison == "lin2" |
         strategy == "Serial Search" & comparison == "lin3" |
         strategy == "Tallying" & comparison == "d1_2" |
         strategy == "Tallying" & comparison == "d1_3" | 
         strategy == "Tallying" & comparison == "d1_4" | 
         strategy == "Tallying" & comparison == "d2_3" | 
         strategy == "Tallying" & comparison == "d2_4" | 
         strategy == "Tallying" & comparison == "d3_4" |
         strategy == "Partial Tallying" & comparison == "d12_34" |
         strategy == "Partial Tallying" & comparison == "d1_2" |   
         strategy == "Partial Tallying" & comparison == "d3_4")

rope_last<- rope_summary(comp_last, c("strategy", "comparison")) 

comp_last_scenario <- planned_comparisons(samples_last_scenario, scenarios = TRUE) %>% 
  filter(strategy == "1st only" & comparison == "d1_all" |
         strategy == "2nd only" & comparison == "d2_all" |
         strategy == "Serial Search" & scenario == "0" & comparison == "d1_all" |
         strategy == "Serial Search" & scenario == "1" & comparison == "d2_all" |
         strategy == "Serial Search" & scenario == "2" & comparison == "d3_all" |  
         strategy == "Serial Search" & scenario == "3" & comparison == "d4_all" |
         strategy == "Tallying" & comparison == "d1_2" |
         strategy == "Tallying" & comparison == "d1_3" | 
         strategy == "Tallying" & comparison == "d1_4" | 
         strategy == "Tallying" & comparison == "d2_3" | 
         strategy == "Tallying" & comparison == "d2_4" | 
         strategy == "Tallying" & comparison == "d3_4" |
         strategy == "Partial Tallying" & comparison == "d12_34" |
         strategy == "Partial Tallying" & comparison == "d1_2" |   
         strategy == "Partial Tallying" & comparison == "d3_4")

rope_last_scenario <- rope_summary(comp_last_scenario, c("strategy", "scenario", "comparison")) 

# Save data ---------------------------------------------------------------

save(rope_performance_change, rope_first, rope_first_scenario, rope_proportion, 
     rope_proportion_scenario, rope_last, rope_last_scenario,
     rope_performance_group_diff, rope_performance_T1,
     file = file.path(folder_summaries, "bayes_rope.RData"))

save(individual_performance, individual_first, individual_first_scenario,
     individual_proportion, individual_proportion_scenario,
     individual_last, individual_last_scenario,
     file = file.path(folder_summaries, "bayes_individual_medians.RData"))

save(intervals_performance, intervals_first, intervals_first_scenario, 
     intervals_proportion, intervals_proportion_scenario, 
     intervals_last, intervals_last_scenario,
     file = file.path(folder_summaries, "bayes_intervals.RData"))

save(comp_performance_change, comp_first, comp_first_scenario, 
     comp_proportion, comp_proportion_scenario, comp_last, 
     comp_last_scenario, comp_performance_group_diff, comp_performance_T1, 
     file = file.path(folder_summaries, "bayes_comparisons.RData"))

