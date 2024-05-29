options (scipen=3) # to prevent R from prining out too many digits
library(boot)    # Efron bootstrap
library(ggplot2) # plots
library(Hmisc)   # correlation
library(emdbook) # lseq
library(stringr)
library(dplyr)
library(gridExtra)


##------------------------------------------------------------------------------------------------------
#
# globals - directories
#
##------------------------------------------------------------------------------------------------------

experiment = 4


# TBD: replace with your directory
pwd = getwd() 
setwd(pwd)
print( paste('set working directory to: ', getwd() ))

data_directory         = paste0(pwd, "__experiment_", experiment, "/csv_files/")
data_directory_models  = paste0(pwd, "__experiment_", experiment, "/csv_files/experiment_", experiment, "_recursive/")
loglikes_directory     = paste0(pwd, "__experiment_", experiment, "/loglikes/")


sampling_model_available = TRUE
source(paste(pwd, "data_utils.R", sep = ""))


##------------------------------------------------------------------------------------------------------
#
# globals - variables
#
##------------------------------------------------------------------------------------------------------


# The smallest probability that will replace zero probabilities when fitting models
# as we can not have zero probabilities of any action under a model
threshold_prob = 0.00001 

# minimal number of subjects in a decision for it to be included in the policy
#minimal_subjects_per_state_for_policy = 0.2 
minimal_subjects_per_state_for_policy = 0.9 


# this is for colouring bars
model_names = c("PW", "EU", "DU", "PW_DU", "DU-Num", "EU-Num", "Sampling", "PW-DU")

use_priors = TRUE # this will set prior on softmax tau when fitting models


##------------------------------------------------------------------------------------------------------
#
# this is the list of all node alternatives along with what was chosen for each subject
#
##------------------------------------------------------------------------------------------------------

planvalues = read.csv( paste(data_directory,'subject_decisions_with_values.csv',  sep = ""), sep = ",", strip.white=TRUE)
planvalues = removeNodesWithOnlyOneChild(planvalues) # cleaning up rows that are not decisions 
planvalues = planvalues[,!(names(planvalues) %in% c("nodeValue", "path"))] # unused


##------------------------------------------------------------------------------------------------------
#
# subjects excluded for failing practice quiz more than twice
#
##------------------------------------------------------------------------------------------------------

if (experiment == 1) {
  
  exclude = c("S43329812", "S69282470", "S26848889")
  planvalues = subset(planvalues, !(planvalues$subject %in% exclude ))
    
} else if (experiment == 4) {
  
  # this map is missing
  planvalues = subset(planvalues, planvalues$world != "medium_library") 
  
  # more than two quiz attempts
  exclude = c("S99772349", "S5999785", "S13674761", "S51193428", "S3248008", "S25705172", "S23668923", "S8621940") 
  planvalues = subset(planvalues, !(planvalues$subject %in% exclude ))   
} 
  
# get the list of all subjects
subjects = unique(planvalues$subject)

# get the list of all mazes
worlds = unique(planvalues$world)

#-------------------------------------------------------------------------------------------------------------
#
# compute human policy - that is, how likely are people to pick each action in each state
# this policy includes only decisions visited by at least min_subjects
#
#-------------------------------------------------------------------------------------------------------------


policy = compute_policy(planvalues)

ggplot(policy) + aes(x=N) + geom_histogram() + xlab("distribution of humans per decision in human\n policy - adjust min_subjects if needed")

##------------------------------------------------------------------------------------------------------
#
# loading values from each model
#
##------------------------------------------------------------------------------------------------------

values_eu = read.csv( paste(data_directory_models,'Expected_Utility.csv',  sep = ""), strip.white=TRUE)
values_du = read.csv( paste(data_directory_models,'Discounted_Utility.csv',  sep = ""), strip.white=TRUE)
values_cells = read.csv( paste(data_directory_models,'Heuristic_Cells.csv',  sep = ""), strip.white=TRUE)
values_steps_cells = read.csv( paste(data_directory_models,'Heuristic_Steps_Cells.csv',  sep = ""), strip.white=TRUE)
values_steps = read.csv( paste(data_directory_models,'Heuristic_Steps.csv',  sep = ""), strip.white=TRUE)
values_pw = read.csv( paste(data_directory_models,'Probability_Weighted_Utility.csv',  sep = ""), strip.white=TRUE)
values_random = read.csv( paste(data_directory_models,'Random.csv',  sep = ""), strip.white=TRUE)

values_steps_cells_num = read.csv( paste(data_directory_models,'Steps_Cells_Numerosity.csv',  sep = ""), strip.white=TRUE)
values_steps_num = read.csv( paste(data_directory_models,'Steps_Numerosity.csv',  sep = ""), strip.white=TRUE)
values_cells_num = read.csv( paste(data_directory_models,'Cells_Numerosity.csv',  sep = ""), strip.white=TRUE)
values_pw_du = read.csv( paste(data_directory_models,'PW_DU.csv',  sep = ""), strip.white=TRUE)
values_du_num = read.csv( paste(data_directory_models,'DU_Numerosity.csv',  sep = ""), strip.white=TRUE)
values_eu_num = read.csv( paste(data_directory_models,'EU_Numerosity.csv',  sep = ""), strip.white=TRUE)

if (sampling_model_available) {
  values_sampling = read.csv( paste(data_directory_models,'Sampling.csv',  sep = ""), strip.white=TRUE)
}
##------------------------------------------------------------------------------------------------------
#
# clean up extra columns and rows which are now unused 
#
##------------------------------------------------------------------------------------------------------

validhash = unique(planvalues$nodeName)

values_eu = removeExtras(values_eu, validhash)
values_du = removeExtras(values_du, validhash)
values_pw = removeExtras(values_pw, validhash)

values_cells = removeExtras(values_cells, validhash)
values_steps_cells = removeExtras(values_steps_cells, validhash)
values_steps = removeExtras(values_steps, validhash)
values_random = removeExtras(values_random, validhash)

values_du_num = removeExtras(values_du_num, validhash)
values_eu_num = removeExtras(values_eu_num, validhash)
values_pw_du = removeExtras(values_pw_du, validhash)
values_steps_cells_num = removeExtras(values_steps_cells_num, validhash)
values_steps_num = removeExtras(values_steps_num, validhash)
values_cells_num = removeExtras(values_cells_num, validhash)

if (sampling_model_available) {
  values_sampling = removeExtras(values_sampling, validhash)
}


##------------------------------------------------------------------------------------------------------
#
# extract parameter grids, which will be used for fitting
#
##------------------------------------------------------------------------------------------------------

grid_eu     = extractGridParams(values_eu, c("tau", "gamma", "beta"))
grid_pw     = extractGridParams(values_pw, c("tau", "gamma", "beta"))
grid_du     = extractGridParams(values_du, c("tau", "gamma", "beta"))
grid_pw_du  = extractGridParams(values_pw_du, c("tau", "gamma", "beta"))
grid_eu_num = extractGridParams(values_eu_num, c("tau", "gamma", "beta", "bits"))
grid_du_num = extractGridParams(values_du_num, c("tau", "gamma", "beta", "bits"))

grid_steps =        extractGridParams(values_steps, c("tau", "k")) 
grid_cells =        extractGridParams(values_cells, c("tau", "k")) 
grid_steps_cells =  extractGridParams(values_steps_cells, c("tau", "k")) 
grid_steps_num =       extractGridParams(values_steps_num, c("tau", "k", "bits")) 
grid_cells_num =       extractGridParams(values_cells_num, c("tau", "k", "bits")) 
grid_steps_cells_num = extractGridParams(values_steps_cells_num, c("tau", "k", "bits")) 

if (sampling_model_available) {
  grid_sampling = extractGridParams(values_sampling, c("budget", "c"))
}

grid_eu = add_prior_on_tau(grid_eu)
grid_pw = add_prior_on_tau(grid_pw)
grid_du = add_prior_on_tau(grid_du)
grid_pw_du = add_prior_on_tau(grid_pw_du)
grid_eu_num = add_prior_on_tau(grid_eu_num)
grid_du_num = add_prior_on_tau(grid_du_num)

grid_steps = add_prior_on_tau(grid_steps)
grid_cells = add_prior_on_tau(grid_cells)
grid_steps_cells = add_prior_on_tau(grid_steps_cells)
grid_steps_num = add_prior_on_tau(grid_steps_num)
grid_cells_num = add_prior_on_tau(grid_cells_num)
grid_steps_cells_num = add_prior_on_tau(grid_steps_cells_num)

grid_random = data.frame(tau=1, prior = 0) # this is a dummy grid to use common LL computing function, random model has no parameters

if (sampling_model_available) {
  grid_sampling$prior = 0 #no priors supported, adding for consistency
}

ll_random = compute_LL_for_each_subject(data.frame(tau=1), values_random, planvalues)

# count number of decisions per subject

n = 0
for (s in subjects) {
  decisions = subset(planvalues, planvalues$subject == s ) 
  n = n+ length(unique(decisions$nodeName))
}

n = n/length(subjects)
print( paste( round(n,1), "decisions per subject" ))

