# Statistical Analysis
 
# This script runs statistical analysis (ANOVAS and follow up contrasts) 
# using the packages afex and emmeans. Also, it generates summaries
# corrected for within subjects designs to create plots. Finally, it uses
# the package "papaja" to generate result tables that follow APA formating.

# The within subjects correction is done with the Rmisc package. It is important
# to download it directly from github, since the version in CRAN has an error
# which returns only the normed means.

# Also, be careful in the creation of contrast tables, since by default 
# apa_table() tries to add rownames to the resulting syntax in latex.
# For this reason, we delete those, since they generate issues when knitting
# the file later on.

# We decided to keep the default behavior of afex to provide DF corrected 
# with GG, since most of our results violate the assumption of sphericity.
# We did not use Bonferroni correction on the follow up contrasts, since 
# all of them were planned to test the assumptions from TTB (but easily
# could be added by changing the "adjust" argument from the contrast from
# "none" to "bon") 

# Created by Eduardo Rea
# Multicue Probabilistic Project
# NLP Lab UMass AMherst
# December 2019

# Load and/or install all the packages we need ####
if(!"tidyverse" %in% rownames(installed.packages())) install.packages("tidyverse")
if(!"Rmisc" %in% rownames(installed.packages())) devtools::install_github("ryanhope/rmisc")

library(papaja)
library(tidyverse)
library(afex)
library(emmeans)

# Define base directories ####
folder_root <- "C:/Users/rerr_/Google Drive/Graduate/Lab/Studies/MultiCue_Probabilistic/three_phases/INV/Analysis"

folder_references   <- file.path(folder_root, "References")
folder_documents    <- file.path(folder_root, "Documents")
folder_scripts      <- file.path(folder_root, "Scripts")
folder_results      <- file.path(folder_root, "Results")
folder_behavior     <- file.path(folder_root, "Behavior")
folder_demographics <- file.path(folder_root, "Demographics_scales")
folder_figures      <- file.path(folder_documents, "Tables and Figures")

# Load measures to analyze ####
load(file.path(folder_results, "summary_datasets_group.RData"))

survey_data <- read.csv(file      = file.path(folder_demographics, "demographics.csv"), 
                        header    = T, 
                        row.names = NULL) 

wasi_data <- read.csv(file      = file.path(folder_demographics, "WASI.csv"), 
                      header    = T, 
                      row.names = NULL)

subject_ids <- unique(behavior_data$id)

age <- survey_data %>% 
  filter(ID %in% subject_ids) %>% 
  select(id = ID, gender = Gender, age = Age)

wasi <- wasi_data %>% 
  filter(ID %in% subject_ids) %>% 
  select(id = ID, score = Score)

age_descriptives <- age %>% 
  group_by(gender) %>% 
  summarise(mean_age = mean(age), sd_age = sd(age), n = n())

demo_data <- full_join(age, wasi) %>% 
  full_join(group_data)

# Define function to create within subject corrected errors for plots based on Rmisc package  ####
data_summary <- function(dataset, dependent_variable, 
                         within_variables = NULL, between_variables = NULL, id_variables = NULL){
  
  Rmisc::summarySEwithin(data          = dataset,
                         measurevar    = dependent_variable,
                         withinvars    = within_variables,
                         betweenvars   = between_variables,
                         idvar         = id_variables,
                         na.rm         = FALSE, 
                         conf.interval = 0.95) %>% 
    
    rename(mean_value = dependent_variable) %>% 
    
    full_join(dataset)
  
}

# Performance  ####
## Group
performance_transformed <- accuracy_data %>% 
  select(id, group, phase, accuracy) %>% 
  mutate(asin_trans = asin(sqrt(accuracy)))

performance_group_transformed_aov <- aov_car(asin_trans ~ phase + 
                                                          Error(id/phase), 
                                             data = performance_transformed)

apa_group_performance <- apa_print(performance_group_transformed_aov)

## Quantiles
performance_transformed_aov <- aov_car(asin_trans ~ group * phase +
                                                    Error(id/phase), 
                                       data = filter(performance_transformed, phase != 1))

apa_performance <- apa_print(performance_transformed_aov)


performance_contrasts <- performance_transformed_aov %>% 
  emmeans(specs = ~ phase | group) %>% contrast(method = "pairwise", adjust="none")

apa_performance_contrasts <- apa_print(performance_contrasts)

rownames(apa_performance_contrasts$table) <- c()

variable_labels(apa_performance_contrasts$table) <- c(group = "Quantile")


## Summaries

accuracy_group_summary <- data_summary(dataset            = accuracy_data, 
                                       dependent_variable = c("accuracy"), 
                                       within_variables   = c("phase"),
                                       id_variables       = c("id"))

accuracy_summary <- data_summary(dataset            = accuracy_data, 
                                 dependent_variable = c("accuracy"), 
                                 within_variables   = c("phase"),
                                 between_variables  = c("group"),
                                 id_variables       = c("id"))

# Demographics ####
wasi_aov <- aov_car(score ~ group +
                          Error(id), 
                    data = demo_data)

apa_wasi <- apa_print(wasi_aov)

age_aov <- aov_car(age ~ group +
                         Error(id), 
                   data = demo_data)

apa_age <- apa_print(age_aov)


## Summaries

wasi_summary <- Rmisc::summarySE(data       = demo_data, 
                                 measurevar = c("score"), 
                                 groupvars  = c("group"))

age_summary <- Rmisc::summarySE(data       = demo_data, 
                                measurevar = c("age"), 
                                groupvars  = c("group"))

# Fixation Score ####
score_aov <- aov_car(score ~ group +
                             Error(id),
                     data = fixation_score)

apa_score <- apa_print(score_aov)

score_summary <- Rmisc::summarySE(data = fixation_score,
                                  measurevar = c("score"),
                                  groupvars = c("group")) %>% 
  select(group, mean_value = score, sd, ci, se) %>% 
  
  right_join(fixation_score)

# Matching fixation with decision ####
matching_aov <- aov_car(matching_score ~ group * case +
                                         Error(id/case),
                        data = matching_data)

apa_matching <- apa_print(matching_aov)

matching_summary <- data_summary(dataset            = matching_data,
                                 dependent_variable = c("matching_score"),
                                 within_variables   = c("case"),
                                 between_variables  = c("group"),
                                 id_variables       = c("id"))

# Binomial-test by case ####
binomial_summary <- binomial_case %>% 
  mutate(non_random = as.numeric(non_random)) %>% 
  
  data_summary(dependent_variable = c("non_random"),
               within_variables   = c("case"),
               between_variables  = c("group"),
               id_variables       = c("id"))

# Subjective Weights ####
subjective_weights_aov <- aov_car(weight ~ (group * domain) + 
                                    Error(id/domain),
                                  data = filter(subjective_weights, phase == 2))

subjective_weights_contrasts <- subjective_weights_aov %>% 
  emmeans(specs = ~ domain | group) %>% contrast(method = "poly", max.degree = 1, adjust = "none")

apa_subjective_weights <- apa_print(subjective_weights_aov)

apa_subjective_weights_contrasts <- apa_print(subjective_weights_contrasts)

rownames(apa_subjective_weights_contrasts$table) <- c()

variable_labels(apa_subjective_weights_contrasts$table) <- c(group = "Quantile")

## Summaries
subjective_weights_summary <- data_summary(dataset            = subjective_weights, 
                                           dependent_variable = c("weight"), 
                                           within_variables   = c("domain", "phase"),
                                           between_variables  = c("group"),
                                           id_variables       = c("id"))

# RT across phases
rt_aov <- aov_car(phase_rt ~ group +
                             Error(id),
                  data = filter(rt_data, phase == 2))

rt_contrast <- rt_aov %>% 
  emmeans(specs = ~ group) %>% 
  contrast(method = "pairwise", adjust = "none")

apa_rt <- apa_print(rt_aov)

rt_summary <- filter(rt_data, phase == 2) %>% 
  group_by(group) %>% 
  mutate(mean_value = mean(phase_rt), 
         sd = sd(phase_rt),
         se = sd(phase_rt/sqrt(length(phase_rt))))
  
# Proportion of Fixations across Domains####
fixations_rank_transformed <- phase_data_proportion %>% 
  mutate(asin_trans = asin(sqrt(prop_fixations))) %>% 
  filter(phase == 2)

fixations_rank_transformed_aov <- aov_car(asin_trans ~ group * rank + 
                                                       Error(id/rank), 
                                          data = fixations_rank_transformed)

fixations_rank_transformed_contrasts <- fixations_rank_transformed_aov %>% 
  emmeans(specs = ~ rank | group) %>% 
  contrast(method     = "poly", 
           max.degree = 1, 
           adjust     = "none")

apa_fixations_rank <- apa_print(fixations_rank_transformed_aov)

apa_fixations_rank_contrast <- apa_print(fixations_rank_transformed_contrasts)

rownames(apa_fixations_rank_contrast$table) <- c()

variable_labels(apa_fixations_rank_contrast$table) <- c(group = "Quantile")

## Summaries
fixations_summary <- data_summary(dataset            = fixations_rank_transformed,
                                  dependent_variable = c("prop_fixations"), 
                                  within_variables   = c("rank"),
                                  between_variables  = c("group"),
                                  id_variables       = c("id"))
# First Fixation ####
first_transformed <- phase_data_first %>% 
  mutate(asin_trans = asin(sqrt(prop_trials))) %>% 
  filter(phase == 2)


first_aov <- aov_car(asin_trans ~ group * rank + 
                                  Error(id/rank),
                     data = first_transformed)

## Define contrast vector
rank1_vs_all <- c(1, -1/3, -1/3, -1/3)

first_contrast <- first_aov %>% 
  emmeans(specs   = ~ rank) %>% 
  
  contrast(method = list("Rank 1 vs All" = rank1_vs_all), 
           adjust = "none")

apa_first_rank <- apa_print(first_aov)

apa_first_rank_contrast <- apa_print(first_contrast)

rownames(apa_first_rank_contrast$table) <- c()

## Summaries
first_summary <- data_summary(dataset            = first_transformed,
                              dependent_variable = c("prop_trials"), 
                              within_variables   = c("rank"),
                              between_variables  = c("group"),
                              id_variables       = c("id"))

# Fixations by Case ####
fixations_case <- fixations_case_data %>% 
  filter(phase == 2)

n_case_aov <- aov_car(mean_fixations_case ~ group * case + 
                                            Error(id/case), 
                      data = fixations_case)

n_case_contrasts <- n_case_aov %>% 
  emmeans(specs = ~ case) %>% 
  contrast(method     = "poly", 
           max.degree = 1, 
           adjust     = "none")

apa_n_case <- apa_print(n_case_aov)

apa_n_case_contrast <- apa_print(n_case_contrasts)

rownames(apa_n_case_contrast$table) <- c()

#variable_labels(apa_n_case_contrast$table) <- c(group = "Quantile")

## Summaries
fixations_case_summary <- data_summary(dataset            = fixations_case,
                                       dependent_variable = c("mean_fixations_case"), 
                                       within_variables   = c("case"),
                                       between_variables  = c("group"),
                                       id_variables       = c("id")) 
# RT by Case ####
rt_case <- rt_case_data %>% 
  filter(phase == 2) 

rt_case_aov <- aov_car(mean_rt ~ group * case + 
                                 Error(id/case),
                       data = rt_case)

rt_case_contrasts <- rt_case_aov %>% 
  emmeans(specs = ~ case | group) %>% 
  contrast(method     = "poly", 
           max.degree = 1, 
           adjust     = "none")

apa_rt_case <- apa_print(rt_case_aov)

apa_rt_case_contrast <- apa_print(rt_case_contrasts)

rownames(apa_rt_case_contrast$table) <- c()

variable_labels(apa_rt_case_contrast$table) <- c(group = "Quantile")

## Summaries
rt_case_summary <- data_summary(dataset            = rt_case,
                                dependent_variable = c("mean_rt"), 
                                within_variables   = c("case"),
                                between_variables  = c("group"),
                                id_variables       = c("id"))
# Fixations by Inverted Case ####
fixations_inv_case <- fixations_invcase_data %>% 
  filter(phase == 2)

n_inv_case_aov <- aov_car(mean_fixations_invcase ~ group * inv_case +
                                                   Error(id/inv_case), 
                          data = fixations_inv_case)

n_inv_case_contrasts <- n_inv_case_aov %>% 
  emmeans(specs = ~ inv_case | group) %>% 
  contrast(method     = "poly", 
           max.degree = 1, 
           adjust     = "none")

apa_n_inv_case <- apa_print(n_inv_case_aov)

apa_n_inv_case_contrast <- apa_print(n_inv_case_contrasts)

rownames(apa_n_inv_case_contrast$table) <- c()

variable_labels(apa_n_inv_case_contrast$table) <- c(group = "Quantile")


## Summaries
fixations_invcase_summary <- data_summary(dataset            = fixations_inv_case,
                                          dependent_variable = c("mean_fixations_invcase"), 
                                          within_variables   = c("inv_case"),
                                          between_variables  = c("group"),
                                          id_variables       = c("id"))
# RT by Inverted Case ####
rt_inv_case <- rt_inv_case_data %>% 
  filter(phase == 2) 

rt_inv_case_aov <- aov_car(mean_rt ~ group * inv_case + 
                                     Error(id/inv_case),
                           data = rt_inv_case)

rt_inv_case_contrasts <- rt_inv_case_aov %>% 
  emmeans(specs = ~ inv_case | group) %>% 
  contrast(method     = "poly", 
           max.degree = 1,
           adjust     = "none")

apa_rt_inv_case <- apa_print(rt_inv_case_aov)

apa_rt_inv_case_contrast <- apa_print(rt_inv_case_contrasts)

rownames(apa_rt_inv_case_contrast$table) <- c()

variable_labels(apa_rt_inv_case_contrast$table) <- c(group = "Quantile")

## Summaries
rt_inv_case_summary <- data_summary(dataset            = rt_inv_case,
                                    dependent_variable = c("mean_rt"), 
                                    within_variables   = c("inv_case"),
                                    between_variables  = c("group"),
                                    id_variables       = c("id"))
# Last Fixation by Case ####
last_case_transformed <- case_data_last %>% 
  mutate(asin_trans = asin(sqrt(mean_prop_trials))) %>% 
  filter(phase == 2)

last_case_transformed_aov <- aov_car(asin_trans ~ group * case * rank + 
                                                  Error(id/case * rank),
                                     data = last_case_transformed)

## Define contrast vectors to compare the fixations to each domain against the others
rank1_vs_all <- c(1, -1/3, -1/3, -1/3)

rank2_vs_all <- c(-1/3, 1, -1/3, -1/3)

rank3_vs_all <- c(-1/3, -1/3, 1, -1/3)

rank4_vs_all <- c(-1/3, -1/3, -1/3, 1)

## Apply contrasts to the model
last_case_A_contrast <- last_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | case | group, 
          at    = list(case = c("X1"))) %>% 
  
  contrast(method = list("Rank 1 vs All" = rank1_vs_all), 
           adjust = "none")

last_case_B_contrast <- last_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | case | group, 
          at    = list(case = c("X2"))) %>% 
  
  contrast(method = list("Rank 2 vs All" = rank2_vs_all), 
           adjust = "none")

last_case_C_contrast <- last_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | case | group, 
          at    = list(case = c("X3"))) %>% 
  
  contrast(method = list("Rank 3 vs All" = rank3_vs_all), 
           adjust = "none")

last_case_D_contrast <- last_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | case | group, 
          at    = list(case = c("X4"))) %>% 
  
  contrast(method = list("Rank 4 vs All" = rank4_vs_all), 
           adjust = "none")

## Create results in pretty format
apa_last_case <- apa_print(last_case_transformed_aov)

apa_last_case_A <- apa_print(last_case_A_contrast)
apa_last_case_B <- apa_print(last_case_B_contrast)
apa_last_case_C <- apa_print(last_case_C_contrast)
apa_last_case_D <- apa_print(last_case_D_contrast)

rownames(apa_last_case_A$table) <- c()
variable_labels(apa_last_case_A$table) <- c(group = "Quantile")

rownames(apa_last_case_B$table) <- c()
variable_labels(apa_last_case_B$table) <- c(group = "Quantile")

rownames(apa_last_case_C$table) <- c()
variable_labels(apa_last_case_C$table) <- c(group = "Quantile")

rownames(apa_last_case_D$table) <- c()
variable_labels(apa_last_case_D$table) <- c(group = "Quantile")

## Summaries
last_case_summary <- data_summary(dataset            = last_case_transformed,
                                  dependent_variable = c("mean_prop_trials"), 
                                  within_variables   = c("case", "rank"),
                                  between_variables  = c("group"),
                                  id_variables       = c("id")) 

# Last Fixation by Inverted Case ####
last_inv_case_transformed <- invcase_data_last %>% 
  mutate(asin_trans = asin(sqrt(mean_prop_trials))) %>% 
  filter(phase == 2)

last_inv_case_transformed_aov <- aov_car(asin_trans ~ group * inv_case * rank + 
                                                      Error(id/inv_case * rank),
                                         data = last_inv_case_transformed)

## Define contrast vectors
rank1_vs_all <- c(1, -1/3, -1/3, -1/3)

rank2_vs_all <- c(-1/3, 1, -1/3, -1/3)

rank3_vs_all <- c(-1/3, -1/3, 1, -1/3)

rank4_vs_all <- c(-1/3, -1/3, -1/3, 1)

## Apply contrasts to the model
last_inv_case_A_contrast <- last_inv_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | inv_case | group, 
          at    = list(inv_case = c("X1"))) %>% 
  
  contrast(method = list("Rank 1 vs All" = rank1_vs_all), 
           adjust = "none")

last_inv_case_B_contrast <- last_inv_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | inv_case | group, 
          at    = list(inv_case = c("X2"))) %>% 
  
  contrast(method = list("Rank 2 vs All" = rank2_vs_all), 
           adjust = "none")

last_inv_case_C_contrast <- last_inv_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | inv_case | group, 
          at    = list(inv_case = c("X3"))) %>% 
  
  contrast(method = list("Rank 3 vs All" = rank3_vs_all), 
           adjust = "none")

last_inv_case_D_contrast <- last_inv_case_transformed_aov %>% 
  emmeans(specs =  ~ rank | inv_case | group, 
          at    = list(inv_case = c("X4"))) %>% 
  
  contrast(method = list("Rank 4 vs All" = rank4_vs_all), 
           adjust = "none")

apa_last_inv_case <- apa_print(last_inv_case_transformed_aov)

apa_last_inv_case_A <- apa_print(last_inv_case_A_contrast)
apa_last_inv_case_B <- apa_print(last_inv_case_B_contrast)
apa_last_inv_case_C <- apa_print(last_inv_case_C_contrast)
apa_last_inv_case_D <- apa_print(last_inv_case_D_contrast)

rownames(apa_last_inv_case_A$table) <- c()
variable_labels(apa_last_inv_case_A$table) <- c(group = "Quantile")

rownames(apa_last_inv_case_B$table) <- c()
variable_labels(apa_last_case_B$table) <- c(group = "Quantile")

rownames(apa_last_inv_case_C$table) <- c()
variable_labels(apa_last_inv_case_C$table) <- c(group = "Quantile")

rownames(apa_last_inv_case_D$table) <- c()
variable_labels(apa_last_inv_case_D$table) <- c(group = "Quantile")

## Summaries
last_inv_case_summary <- data_summary(dataset            = last_inv_case_transformed,
                                      dependent_variable = c("mean_prop_trials"), 
                                      within_variables   = c("inv_case", "rank"),
                                      between_variables  = c("group"),
                                      id_variables       = c("id")) 
# Transitions ####
## Cue
transitions_cue_transformed <- phase_transitions_cue %>% 
  mutate(asin_trans = asin(sqrt(prop_trans_cue))) %>% 
  filter(phase == 2)

transitions_cue_aov <- aov_car(asin_trans ~ group * trans_cue + 
                                            Error(id/trans_cue), 
                               data = transitions_cue_transformed)

transitions_cue_contrast <- transitions_cue_aov %>% 
  emmeans(specs = pairwise ~ trans_cue | group) 

apa_transitions_cue <- apa_print(transitions_cue_aov)

apa_transitions_cue_contrast <- apa_print(transitions_cue_contrast$contrasts)

rownames(apa_transitions_cue_contrast$table) <- c()
variable_labels(apa_transitions_cue_contrast$table) <- c(group = "Quantile")

## Summary
transitions_cue_summary <- data_summary(dataset            = transitions_cue_transformed,
                                        dependent_variable = c("prop_trans_cue"), 
                                        within_variables   = c("trans_cue"),
                                        between_variables  = c("group"),
                                        id_variables       = c("id")) 


## Type
transitions_type_transformed <- phase_transitions_cue_type %>% 
  mutate(asin_trans = asin(sqrt(prop_trans_cue_type))) %>% 
  filter(phase == 2)

transitions_type_aov <- aov_car(asin_trans ~ group * trans_cue * type_trans + 
                                             Error(id/trans_cue * type_trans), 
                                data = transitions_type_transformed)

transitions_type_contrasts <- transitions_type_aov %>% 
  emmeans(specs = ~ type_trans | group) %>% 
  contrast(method = "poly", 
           adjust = "none") 

apa_transitions_type <- apa_print(transitions_type_aov)

apa_transitions_type_contrasts <- apa_print(transitions_type_contrasts)

rownames(apa_transitions_type_contrasts$table) <- c()
variable_labels(apa_transitions_type_contrasts) <- c(group = "Quantile")

### Summary
transitions_type_summary <- data_summary(dataset            = transitions_type_transformed,
                                         dependent_variable = c("prop_trans_cue_type"), 
                                         within_variables   = c("trans_cue", "type_trans"),
                                         between_variables  = c("group"),
                                         id_variables       = c("id")) 


# Save the tables and summaries ####
save(accuracy_group_summary, accuracy_summary,
     rt_summary, score_summary, matching_summary, binomial_summary,
     wasi_summary, age_summary, age_descriptives,
     subjective_weights_summary,
     fixations_summary,
     first_summary,
     fixations_case_summary,
     rt_case_summary,
     fixations_invcase_summary,
     rt_inv_case_summary,
     last_case_summary,
     last_inv_case_summary,
     transitions_cue_summary, transitions_type_summary,
     
     file = file.path(folder_results, "plot_summaries.rds"))

save(apa_group_performance, apa_performance, apa_performance_contrasts,
     apa_rt, apa_score, apa_matching,
     apa_age, apa_wasi, 
     apa_subjective_weights, apa_subjective_weights_contrasts,
     apa_fixations_rank, apa_fixations_rank_contrast,
     apa_first_rank, apa_first_rank_contrast,
     apa_n_case, apa_n_case_contrast,
     apa_rt_case, apa_rt_case_contrast,
     apa_n_inv_case, apa_n_inv_case_contrast,
     apa_rt_inv_case, apa_rt_inv_case_contrast,
     apa_last_case, 
     apa_last_case_A, apa_last_case_B, apa_last_case_C, apa_last_case_D,
     apa_last_inv_case, 
     apa_last_inv_case_A, apa_last_inv_case_B, apa_last_inv_case_C, apa_last_inv_case_D,
     apa_transitions_cue, apa_transitions_cue_contrast,
     apa_transitions_type, apa_transitions_type_contrasts,
     
     file = file.path(folder_results, "stats_apa.rds"))


save(apa_score, apa_matching, file = file.path(folder_results, "stats_report.rds"))