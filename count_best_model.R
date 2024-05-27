options (scipen=3) # to prevent R from prining out too many digits

library(boot)    # Efron bootstrap
library(ggplot2) # plots
library(Hmisc)   # correlation
library(emdbook) # lseq
library(stringr)
library(dplyr)
library(gridExtra)


experiment = 4

data_directory         = paste0(pwd, "__experiment_", experiment, "/csv_files/")
data_directory_models  = paste0(pwd, "__experiment_", experiment, "/csv_files/experiment_1_recursive/")
loglikes_directory     = paste0(pwd, "__experiment_", experiment, "/loglikes/")

source(paste(pwd, "data_utils.R", sep = ""))

model_names = c("PW", "EU", "DU", "PW_DU", "DU-Num", "EU-Num", "Sampling", "PW-DU")
planvalues = read.csv( paste(data_directory,'subject_decisions_with_values.csv',  sep = ""), sep = ",", strip.white=TRUE)
planvalues = removeNodesWithOnlyOneChild(planvalues) 
planvalues = planvalues[,!(names(planvalues) %in% c("nodeValue", "path"))]  

# exclude subjects who did quiz more than twise 
if (experiment == 1) {
  exclude = c("S43329812", "S69282470", "S26848889")
  planvalues = subset(planvalues, !(planvalues$subject %in% exclude ))
    
} else if (experiment == 4) {
  planvalues = subset(planvalues, planvalues$world != "medium_library") 
  exclude = c("S99772349", "S5999785", "S13674761", "S51193428", "S3248008", "S25705172", "S23668923", "S8621940")   planvalues = subset(planvalues, !(planvalues$subject %in% exclude ))  
}

subjects = unique(planvalues$subject)

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

values_sampling = read.csv( paste(data_directory_models,'Sampling.csv',  sep = ""), strip.white=TRUE)

##------------------------------------------------------------------------------------------------------
#
# marking all nodes that are the first decision in a maze
#
##------------------------------------------------------------------------------------------------------


min_sub = max(round(length(subjects)*0.9), 10) # Experiment 1
if (experiment == 4) min_sub = 107             # Experiment 2

policy_first  = compute_policy(planvalues, min_sub) 
first_nodes = unique(policy_first$node)

##------------------------------------------------------------------------------------------------------
#
# clean up unused columns and rows 
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
values_sampling = removeExtras(values_sampling, validhash)


# select the column with the smallest tau, we do not need tau for this analysis
selected_columns <- colnames(values_eu)[grep("^value_0.2_", colnames(values_eu))] 
values_eu = values_eu[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_du)[grep("^value_0.2_", colnames(values_du))] 
values_du = values_du[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_pw)[grep("^value_0.2_", colnames(values_pw))] 
values_pw = values_pw[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_pw_du)[grep("^value_0.2_", colnames(values_pw_du))] 
values_pw_du = values_pw_du[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_du_num)[grep("^value_0.2_", colnames(values_du_num))] 
values_du_num = values_du_num[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_eu_num)[grep("^value_0.2_", colnames(values_eu_num))] 
values_eu_num = values_eu_num[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_steps)[grep("^value_0.2_", colnames(values_steps))] 
values_steps = values_steps[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_steps_cells)[grep("^value_0.2_", colnames(values_steps_cells))] 
values_steps_cells = values_steps_cells[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_cells)[grep("^value_0.2_", colnames(values_cells))] 
values_cells = values_cells[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_cells_num)[grep("^value_0.2_", colnames(values_cells_num))] 
values_cells_num = values_cells_num[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_steps_num)[grep("^value_0.2_", colnames(values_steps_num))] 
values_steps_num = values_steps_num[ , c("world", "node", "child", selected_columns )]

selected_columns <- colnames(values_steps_cells_num)[grep("^value_0.2_", colnames(values_steps_cells_num))] 
values_steps_cells_num = values_steps_cells_num[ , c("world", "node", "child", selected_columns )]


##------------------------------------------------------------------------------------------------------
#
# extract parameter grids
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

grid_sampling = extractGridParams(values_sampling, c("budget", "c"))
grid_random = data.frame(tau=1) # this is a dummy grid to use common LL computing function, random model has no parameters
ll_random = compute_LL_for_each_subject(data.frame(tau=1), values_random, planvalues)

# for all subjects decisions, count how many times each model agrees with them

models_and_grid <- list(
 "Cells" = grid_cells,
 "Cells-Num" = grid_cells_num,
 "DU" = grid_du, 
 "DU-Num" = grid_du_num, 
 "EU" = grid_eu, 
 "EU-Num" = grid_eu_num, 
 "PW" = grid_pw, 
 "PW_DU" = grid_pw_du, 
 "Sampling" = grid_sampling, 
 "Steps" = grid_steps, 
 "Steps-Cells" = grid_steps_cells, 
 "Steps-Cells-Num" = grid_cells_num, 
 "Steps-Num" = grid_steps_num
)

models_and_values <- list(
 "Cells" = values_cells,
 "Cells-Num" = values_cells_num,
 "DU" = values_du, 
 "DU-Num" = values_du_num, 
 "EU" = values_eu, 
 "EU-Num" = values_eu_num, 
 "PW" = values_pw, 
 "PW_DU" = values_pw_du, 
 "Sampling" = values_sampling, 
 "Steps" = values_steps, 
 "Steps-Cells" = values_steps_cells, 
 "Steps-Cells-Num" = values_cells_num, 
 "Steps-Num" = values_steps_num
)

indifferent = function( x ) {
	# return TRUE if all values in x are the same, e.g. 0.5, 0.5, or 0.33, 0.33, 0.33
	return(length(unique(x)) == 1)
}

did_model_agree_with_chosen = function(values_table, decision, grid, grid_row_idx, showWarnings = TRUE) {
  
  params = grid[grid_row_idx, ]
  stringName = value_column_name_from_params(params)
  df = subset(values_table[, c("world", "node", "child", stringName)], values_table$node == decision$nodeName[1])
  
  if (nrow(df)==0 ) {
    print(paste("values_table entry not found for ", decision$world[1], decision$nodeName[1], decision$squaretype[1]))
    return(0)
  }
  
  chosen_child = decision[which(decision$chosen == TRUE), "availableChildName"]
  v = df[ which(df$child == chosen_child), stringName ]
  ref = max(df[[stringName]])
  
  if ( indifferent( df[[stringName]] ) ) return (FALSE)
  
  if (v < ref) return (FALSE)
  
  return (TRUE)
}

counts_total = data.frame()

for (s in subjects) {
	
	counts = data.frame(subject = s, model = names(models_and_values), count=0)
	
	for (name in names(models_and_values)) {
	
	    #print(name)
		vals = models_and_values[[name]]
		grid = models_and_grid[[name]]
		
		sub_df = grid
		sub_df$subject = s
    	sub_df$model = name
    	sub_df$count = 0
    
    	# select all subject decisions / only first decisions
    	decisions = subset(planvalues, planvalues$subject == s & planvalues$nodeName %in% first_nodes ) 
    	#decisions = subset(planvalues, planvalues$subject == s  ) 
    	
  		nodes = unique(decisions$nodeName)
  		
  		for (i in dim(grid)[1]) {
  			
  			for (n in nodes) {
  				# get the value subject assigns to the actions in this node, given parameters in grid[i]
  				decision = subset(decisions, decisions$nodeName == n)
  				if ( did_model_agree_with_chosen(vals, decision, grid, i, TRUE) ) {
  					sub_df$count[i] =  sub_df$count[i] + 1
  				}
  			}
  			 
  		}
  		
  		counts[which(counts$model == name), "count"] = max(sub_df$count)
  	}
  	
  	counts_total = rbind(counts_total, counts)
}

# Summarize the data to get mean count and 95% CI for each model
summary_counts <- counts_total %>%
  group_by(model) %>%
  summarise(
    mean_count = mean(count),
    se = sd(count) / sqrt(n()),
    ci_low = mean_count - qt(1 - 0.05 / 2, df = n() - 1) * se,
    ci_high = mean_count + qt(1 - 0.05 / 2, df = n() - 1) * se
  ) %>%
  arrange(desc(mean_count))  # Order by mean count

# Convert 'model' to a factor with levels ordered by mean count
summary_counts$model <- factor(summary_counts$model, levels = summary_counts$model)

model_names = c("PW", "EU", "DU", "PW_DU", "DU-Num", "EU-Num", "Sampling", "PW-DU")

summary_counts $modeltype = "Heuristic"
summary_counts $modeltype[which(summary_counts $model %in% model_names)] = "Planning"

# Plot the data using ggplot2
ggplot(summary_counts, aes(x = model, y = mean_count, , fill = modeltype )) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(x = "Model", y = "Mean Count", title = "Mean Count per Model with 95% CI (First Decisions, Experiment 2)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("grey", "deepskyblue")) + guides(fill = guide_legend(title = NULL))




