# ------------------------------------------------------------------------------------------------------------
#
# get reaction time data and plot density in different cells
#
# ------------------------------------------------------------------------------------------------------------

experiment = 1

rt_file         = paste0(pwd, "__experiment_", experiment, "/csv_files/SquareLabels.csv")
tree_file       = paste0(pwd, "__experiment_", experiment, "/csv_files/tree_builder.csv")
fit_models_file = paste0(pwd,"__experiment_", experiment, "/csv_files/fit_models.csv")

maze_search_rt         = read.csv(rt_file, sep = '\t')
tree        		   = read.csv(tree_file, sep = ',')
fit_models             = read.csv(fit_models_file, sep = ',')
maze_search_rt         = subset(maze_search_rt, maze_search_rt$rt > 0)

sub_mdp = read.csv(paste0(pwd,'__experiment_1/subject_mdp_scores.csv'), sep = ',')


tree = tree[ , c("world", "NID", "PARENTID", "depth", "N", "S", "blackremains", "isLeaf")]
tree$tree_prob = tree$N/(tree$N+tree$blackremains)
maze_search_rt$Cell = maze_search_rt$squaretype
maze_search_rt$Cell[which(maze_search_rt$Cell == "N")] = "Corridor"
maze_search_rt$Cell[which(maze_search_rt$Cell == "X")] = "Start"
maze_search_rt$Cell[which(maze_search_rt$Cell == "G")] = "Exit seen"
maze_search_rt$Cell[which(maze_search_rt$Cell == "D")] = "Decision"


mean(subset(maze_search_rt$rt, maze_search_rt$Cell == "Corridor"))
sd(subset(maze_search_rt$rt, maze_search_rt$Cell == "Corridor"))

mean(subset(maze_search_rt$rt, maze_search_rt$Cell == "Decision"))
sd(subset(maze_search_rt$rt, maze_search_rt$Cell == "Decision"))

mean(subset(maze_search_rt$rt, maze_search_rt$Cell == "Start"))
sd(subset(maze_search_rt$rt, maze_search_rt$Cell == "Start"))

maze_search_rt = subset(maze_search_rt, maze_search_rt$rt < 4000)
var.test(subset(maze_search_rt$rt, maze_search_rt$Cell == "Corridor"), subset(maze_search_rt$rt, maze_search_rt$Cell == "Decision"))

maze_search_rt = subset(maze_search_rt, maze_search_rt$squaretype != "G") # removing the cells from which goal was seen

# plot figure 7 in SI
ggplot( subset(maze_search_rt, maze_search_rt$rt < 4000), aes(x=rt, fill= Cell)) + geom_density(alpha = 0.5)+ scale_fill_brewer(palette="Spectral") + theme_light() + xlab("Reaction Time, ms.")




# ------------------------------------------------------------------------------------------------------------
#
# given data in sub_mdp, generate a histogram that shows the fraction of rows where direct is equal to 1 and 0 per subject, 
# with 95% confidence intervals
#
# ------------------------------------------------------------------------------------------------------------


library(reshape2)
library(Hmisc)


# Summarize the fractions per subject - in E1 this is 99% and 1%
sub_mdp_summary <- aggregate(cbind(direct_1 = sub_mdp$direct == 1, direct_0 = sub_mdp$direct == 0), 
                             by = list(subject = sub_mdp$subject), 
                             FUN = sum)
sub_mdp_summary$total <- sub_mdp_summary$direct_1 + sub_mdp_summary$direct_0
sub_mdp_summary$fraction_direct_1 <- sub_mdp_summary$direct_1 / sub_mdp_summary$total
sub_mdp_summary$fraction_direct_0 <- sub_mdp_summary$direct_0 / sub_mdp_summary$total

# Reshape the data
sub_mdp_long <- melt(sub_mdp_summary, 
                     id.vars = "subject", 
                     measure.vars = c("fraction_direct_1", "fraction_direct_0"),
                     variable.name = "direct", 
                     value.name = "fraction")

sub_mdp_long$direct <- ifelse(sub_mdp_long$direct == "fraction_direct_1", "Direct", "Indirect")

# Calculate mean and confidence intervals
summary_stats <- aggregate(fraction ~ direct, data = sub_mdp_long, function(x) {
  c(mean = mean(x), ci = binconf(x, n = length(x), method = "wilson")[, 2:3])
})

# Transform the summary_stats to a data frame
summary_stats <- do.call(data.frame, summary_stats)
colnames(summary_stats) <- c("direct", "mean_fraction", "ci_low", "ci_high")

ggplot(summary_stats, aes(x = direct, y = mean_fraction, fill = direct)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_fraction-ci_low, ymax = mean_fraction +ci_high), 
                position = position_dodge(0.9), width = 0.25) +
  labs(
    title = "Fraction of direct moves by subject",
    x = "Move Type",
    y = "Mean Fraction",
    fill = "Move"
  ) +
  theme_minimal()
  
# Plot distribution density for the rt column, showing direct and indirect moves
sub_mdp$action = "Direct"
sub_mdp$action[sub_mdp$direct == 0] = "Indirect"
ggplot(subset(sub_mdp, sub_mdp$rt < 1500), aes(x = rt, fill = action)) +
  geom_density(alpha = 0.5) +  # Use alpha for transparency
  labs(title = "Reaction time distribution by move type",
       x = "Reaction Time (rt)",
       y = "Density",
       fill = "Move Type") +  # Label the fill legend
  theme_minimal()



# ------------------------------------------------------------------------------------------------------------
#
# Label decisions with the depth of their tree
#
# ------------------------------------------------------------------------------------------------------------

maze_search_rt = subset(maze_search_rt[ , c("subject", "world", "step", "rt", "Cell")], maze_search_rt$Cell == "Decision" | maze_search_rt$Cell == "Start")

maze_search_rt$decisionNumber = 1

w = maze_search_rt$world[1]
s = maze_search_rt$subject[1]
idx = 1
for (i in 2: dim(maze_search_rt)[1]) {
	if (w != maze_search_rt$world[i] | s != maze_search_rt$subject[i] ) {
		w = maze_search_rt$world[i]
		s = maze_search_rt$subject[i]
		idx = 1
	} else {
		idx = idx + 1
		maze_search_rt$decisionNumber[i] = idx
	}
	
}

# max depth in the tree
max_depth = data.frame(worlds = unique(tree$world), depth = 1)
for (i in 1: dim(max_depth)[1]) {
	max_depth$depth[i] = max(subset(tree$depth, tree$world == max_depth$world[i] ))
}

# how deep is the child tree at this node
maze_search_rt$treeDepth = 1
for (i in 1: dim(maze_search_rt)[1]) {
	d = max_depth$depth[which(max_depth$world == maze_search_rt$world[i])]
	maze_search_rt$treeDepth[i] = d - maze_search_rt$decisionNumber[i]
}


# remove the states where only one room remains - they are not really decisions
maze_search_rt = subset(maze_search_rt, maze_search_rt$treeDepth > 0)


# ------------------------------------------------------------------------------------------------------------
#
# Analyzing Initial and Subsequent decision times based on fitted discounting
#
# ------------------------------------------------------------------------------------------------------------


maze_search_rt_s = subset(maze_search_rt, maze_search_rt$Cell == "Start")  # Initial decision times

maze_search_rt_d = subset(maze_search_rt, maze_search_rt$Cell == "Decision") # Subsequent decision times


# assuming fit_models is loaded
# regression with only one parameter
compute_LM <- function(fit_models , model_name, param_name, maze_search_rt, limit_rt) {
  f = subset(fit_models[, c(param_name, "subject")], fit_models$model == model_name)
  df = merge(maze_search_rt, f, by = "subject")
  df = subset(df, df$rt < limit_rt)
  if (param_name == "gamma") {
  	m = lm(data = df, rt ~ gamma)
  } else if (param_name == "tau") {
  	m = lm(data = df, rt ~ tau)
  } else if (param_name == "beta") {
  	df = subset(df, df$beta <=1 )
  	m = lm(data = df, rt ~ beta)
  } else if (param_name == "b")  {
  	# assume this is for the PW model, and transform beta to a parameter reflecting it's proximity to 1
  	f = subset(fit_models[, c("beta", "subject")], fit_models$model == "PW")
	f$b <- ifelse(f$beta <= 1, 1 - f$beta, f$beta - 1)
	df = merge(maze_search_rt, f, by = "subject")
  	df = subset(df, df$rt < limit_rt)
  	m = lm(data = df, rt ~ b)
  }
  #summary(m)
  return(summary(m))
}




compute_LM_DU_Num <- function(fit_models,  maze_search_rt, limit_rt) {
  f = subset(fit_models[, c("gamma", "tau", "bits", "subject")], fit_models$model == "DU-Num")
  df = merge(maze_search_rt, f, by = "subject")
  df = subset(df, df$rt < limit_rt)
  m = lm(data = df, rt ~ gamma + bits + tau)
  
  return(summary(m))
}

compute_LM_gamma_tau <- function(fit_models, maze_search_rt, limit_rt) {
  f = subset(fit_models[, c("gamma", "tau", "subject")], fit_models$model == "DU")
  df = merge(maze_search_rt, f, by = "subject")
  df = subset(df, df$rt < limit_rt)
  m = lm(data = df, rt ~ tau + gamma)
  
  return(summary(m))
}


compute_LM_PW<- function(fit_models, maze_search_rt, limit_rt) {
  f = subset(fit_models[, c("tau", "beta", "subject")], fit_models$model == "PW")
  df = merge(maze_search_rt, f, by = "subject")
  df = subset(df, df$rt < limit_rt)
  m = lm(data = df, rt ~ beta + tau)
  
  return(summary(m))
}



compute_LM_PW_DU_tau_b <- function(fit_models,  maze_search_rt, limit_rt) {
  f = subset(fit_models[, c("gamma", "beta", "tau", "subject")], fit_models$model == "PW_DU")
  f$b <- ifelse(f$beta <= 1, 1 - f$beta, f$beta - 1)
  df = merge(maze_search_rt, f, by = "subject")
  df = subset(df, df$rt < limit_rt)
  m = lm(data = df, rt ~ b + gamma + tau)
  
  return(summary(m))
}




initial_limit = 10000
sub_limit = 5000

# Does tau in the EU model explain decision times
compute_LM(fit_models, "EU", "tau", maze_search_rt_s, initial_limit)
compute_LM(fit_models, "EU", "tau", maze_search_rt_d, sub_limit)

# Does gamma and tau in the DU model explain decision times
compute_LM_gamma_tau(fit_models, maze_search_rt_d, sub_limit)
compute_LM_gamma_tau(fit_models, maze_search_rt_s, initial_limit)

# DU-Num model: tau, gamma, bits
compute_LM_DU_Num(fit_models, maze_search_rt_d, sub_limit)
compute_LM_DU_Num(fit_models, maze_search_rt_s, initial_limit)

# PW-DU
compute_LM_PW_DU_tau_b(fit_models, maze_search_rt_d, sub_limit)
compute_LM_PW_DU_tau_b(fit_models, maze_search_rt_s, initial_limit)



# ------------------------------------------------------------------------------------------------------------
#
# Distributions of rt in Initial and Subsequent decisions 
# df will contain either initial or Subsequent times, depending on which lines are commented
#
# ------------------------------------------------------------------------------------------------------------


f = subset(fit_models[, c("gamma", "subject")], fit_models$model == "DU")
breaks = c(-1, 0.5, 0.8, 0.95, 1) # E1  
#breaks = c(-1, 0.2, 0.5, 0.8, 1)
f$gamma_bin <- cut(f$gamma, breaks = breaks, labels = FALSE) # Discretize gamma values into bins 
df = merge(maze_search_rt_s, f, by = "subject")
#df = merge(maze_search_rt_d, f, by = "subject")
df = subset(df, df$rt < initial_limit) # X, E2
#df = subset(df, df$rt < sub_limit)


# plot distribution of rt
ggplot(df, aes(x = rt)) +
  #geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  #labs(x = "Distribution of Initial decision times", title = "Experiment 1", y = "Frequency")
  geom_histogram(binwidth = 100, fill = "red", color = "black") +
  labs(x = "Distribution of Subsequent decision times", title = "Experiment 1", y = "Frequency")


# ------------------------------------------------------------------------------------------------------------
#
# Checking if homoscedasticity holds - equal variance of residucals at each level of gamma
# If it does not, make sure to set a reasonable limit max rt in the distributions, as they normally have tails in the order of minutes
#
# ------------------------------------------------------------------------------------------------------------

m = lm(data = df, rt ~ gamma) # p-value: 0.04 E1, D
summary(m)
residuals <- resid(m)
predicted <- fitted(m)

# Plot residuals against predicted values
plot(predicted, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs. Predicted Values")
abline(h = 0, col = "red")


# ------------------------------------------------------------------------------------------------------------
#
# exploratory -- 
# look at different gammas interacting with different secision depths
#
# ------------------------------------------------------------------------------------------------------------

agg = aggregate(df$rt, by = list( df$treeDepth, df$gamma_bin), FUN = function(x) c(N = length(x), mean = mean(x), SE = sd(x)/sqrt(length(x)) ))
colnames(agg) <- c( "depth", "gamma_bin", "x")
agg$N = agg$x[,1]
agg$dt = agg$x[,2]
agg$SE = agg$x[,3]


  
l = c()
for (i in seq(min(f$gamma_bin), max(f$gamma_bin))) {
	bin = subset(f$gamma, f$gamma_bin == i)
	mn = min(bin)
	mx = max(bin)
	s <- paste(length(bin), ": ", mn, "-", mx, sep = "")
	print(s)
	l <- c(l, s)
}
  
ggplot(agg, aes(x = depth, y = dt, group = as.factor(gamma_bin), color = as.factor(gamma_bin))) +
  geom_line() +
  geom_errorbar(aes(ymin = dt - 2*SE, ymax = dt + 2*SE), width = 0.2, alpha = 0.8) +
  labs(title = "Experiment 2", x = "Depth", y = "Decision time, ms.", color = "γ N: min-max") +
  theme_minimal() +
  scale_color_manual(values=c("black", "blue", "red", "green", "purple"), labels = l)  + scale_x_continuous(breaks=1:8)






