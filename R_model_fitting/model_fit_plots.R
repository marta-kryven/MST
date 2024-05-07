




# match_model_and_human_probabilities_from_column = function(values, col) {
#   df = data.frame(policy[ ,c("child", "prob") ], modelProb = NA)
#   for (i in 1:dim(df)[1]) {
#     df$modelProb[i] = values[ which(values$child == df$child[i]), col]
#   }
#   return (df)
# }



# ------------------------------------------------------------------------------------------------
#
#   plot parameter distribution - this aplies to subjects and to worlds
#
# ------------------------------------------------------------------------------------------------


# EU
ggplot(subset(fit_models, fit_models$model == "EU"), aes(x=tau)) + geom_histogram() + xlab("EU-τ")

# DU
g1 = ggplot(subset(fit_models, fit_models$model == "DU"), aes(x=tau)) + geom_histogram() + xlab("DU-τ")
g2 = ggplot(subset(fit_models, fit_models$model == "DU"), aes(x=gamma)) + geom_histogram() + xlab("DU-γ")
grid.arrange(g1, g2, ncol = 2)

# PW
g1 = ggplot(subset(fit_models, fit_models$model == "PW"), aes(x=tau)) + geom_histogram() + xlab("PW-τ")
g2 = ggplot(subset(fit_models, fit_models$model == "PW"), aes(x=beta)) + geom_histogram() + xlab("PW-β")
grid.arrange(g1, g2, ncol = 2)


#steps num
g1 = ggplot(subset(fit_models, fit_models$model == "Steps-Num"), aes(x=tau)) + geom_histogram() + xlab("Steps-Num-τ")
g2 = ggplot(subset(fit_models, fit_models$model == "Steps-Num"), aes(x=bits)) + geom_histogram() + xlab("Steps-Num-bits")
grid.arrange(g1, g2, ncol = 2)

#EU-Num
g1 = ggplot(subset(fit_models, fit_models$model == "EU-Num"), aes(x=bits)) + geom_histogram() + xlab("EU-Num-bits")
g2 = ggplot(subset(fit_models, fit_models$model == "EU-Num"), aes(x=tau)) + geom_histogram() + xlab("EU-Num-τ")
grid.arrange(g1, g2, ncol = 2)

#Steps-Cells
g1 =ggplot(subset(fit_models, fit_models$model == "Steps-Cells"), aes(x=tau)) + geom_histogram() + xlab("Steps-Cells-τ")
g2 =ggplot(subset(fit_models, fit_models$model == "Steps-Cells"), aes(x=k)) + geom_histogram() + xlab("Steps-Cells-k")
grid.arrange(g1, g2, ncol = 2)

g1 =ggplot(subset(fit_models, fit_models$model == "Sampling"), aes(x=budget)) + geom_histogram() + xlab("Sampling budget")
g2 =ggplot(subset(fit_models, fit_models$model == "Sampling"), aes(x=c)) + geom_histogram() + xlab("Sampling exploration")
grid.arrange(g1, g2, ncol = 2)


# DU-Num
g1 =ggplot(subset(fit_models, fit_models$model == "DU-Num"), aes(x=bits)) + geom_histogram() + xlab("DU-Num-bits")
g2 =ggplot(subset(fit_models, fit_models$model == "DU-Num"), aes(x=gamma)) + geom_histogram() + xlab("DU-Num-γ")
g3 = ggplot(subset(fit_models, fit_models$model == "DU-Num"), aes(x=tau)) + geom_histogram() + xlab("DU-Num-τ")

grid.arrange(g1, g2, g3, ncol = 3)

#PW-DU
g1 = ggplot(subset(fit_models, fit_models$model == "PW_DU"), aes(x=beta)) + geom_histogram() + xlab("PW-DU-β")
g2 = ggplot(subset(fit_models, fit_models$model == "PW_DU"), aes(x=gamma)) + geom_histogram() + xlab("PW-DU-γ")
g3 = ggplot(subset(fit_models, fit_models$model == "PW_DU"), aes(x=tau)) + geom_histogram() + xlab("PW-DU-τ")
grid.arrange(g1, g2, g3, ncol = 3)


#Steps-Cells-Num
g1 =ggplot(subset(fit_models, fit_models$model == "Steps-Cells-Num"), aes(x=bits)) + geom_histogram() + xlab("Steps-Cells-Num-bits")
g2 =ggplot(subset(fit_models, fit_models$model == "Steps-Cells-Num"), aes(x=k)) + geom_histogram() + xlab("Steps-Cells-Num-k")
g3 = ggplot(subset(fit_models, fit_models$model == "Steps-Cells-Num"), aes(x=tau)) + geom_histogram() + xlab("Steps-Cells-Num-τ")

grid.arrange(g1, g2, g3, ncol = 3)



# what are the distributions of LL like?
# they are not exactly normal, but not too different
g1 = ggplot(subset(fit_models, fit_models$model == "DU"), aes(x=LL)) + geom_density() + xlab("Subject LLs, DU")
g2 = ggplot(subset(fit_models, fit_models$model == "DU-Num"), aes(x=LL)) + geom_density() + xlab("Subject LLs, DU-Num")
g3 = ggplot(subset(fit_models, fit_models$model == "PW_DU"), aes(x=LL)) + geom_density() + xlab("Subject LLs, PW-DU")
g4 = ggplot(subset(fit_models, fit_models$model == "Steps-Cells-Num"), aes(x=LL)) + geom_density() + xlab("Subject LLs, Steps-Cells-Num")
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow=2)


#-------------------------------------------------------------------------------------------------------------
#  LL_total over subjects
#  for each model bar plot sum of loglikes over all subjects
#  replication of Falk Lieder analysis
#      Fits to E4
#         Sampling -5148.005
#        Steps-Num -5241.219
#-------------------------------------------------------------------------------------------------------------

add_model_to_bar = function(bar, fit_df, m) {
  return(rbind(bar, data.frame(model = m, sum = sum(subset(fit_df$LL, fit_df$model == m)))))
}

bar = data.frame(model = "DU", sum = sum( subset(fit_models$LL, fit_models$model == "DU")))
bar = rbind(bar, data.frame(model = "PW-DU", sum = sum( subset(fit_models$LL, fit_models$model == "PW_DU"))))
bar = add_model_to_bar(bar, fit_models, "DU-Num")
bar = add_model_to_bar(bar, fit_models, "EU-Num")
bar = add_model_to_bar(bar, fit_models, "EU")
bar = add_model_to_bar(bar, fit_models, "PW")
bar = add_model_to_bar(bar, fit_models, "Steps")
bar = add_model_to_bar(bar, fit_models, "Steps-Num")
bar = add_model_to_bar(bar, fit_models, "Steps-Cells")
bar = add_model_to_bar(bar, fit_models, "Cells")
bar = add_model_to_bar(bar, fit_models, "Steps-Cells-Num")
bar = add_model_to_bar(bar, fit_models, "Cells-Num")
bar = add_model_to_bar(bar, fit_models, "Random")

if (sampling_model_available) {
  bar = add_model_to_bar(bar, fit_models, "Sampling")
}

bar$modeltype = "Heuristic"
bar$modeltype[which(bar$model %in% model_names)] = "Planning"

bar = bar[order(bar$sum),]
bar$model = factor(bar$model, levels = bar$model[order(bar$sum)])
g = ggplot(bar, aes(y=sum, x=model, fill=modeltype)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g = g + ylab("Model Fit") + coord_flip()#+ guides(fill="none") 
g = g + scale_fill_manual(values = c("grey", "deepskyblue", "seagreen3")) #scale_fill_brewer(palette="Set1")
g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g1=g

#-------------------------------------------------------------------------------------------------------------
#
#  for each model bar plot loglikes of all subjects with 95CI
#
#-------------------------------------------------------------------------------------------------------------

f =  fit_models[ , c("LL", "model", "modeltype")]
a = aggregate(f$LL,  by=list(f$model), 
      FUN = function(x) c(mean = mean(x), 
                          lo = qt(.025,length(x)-1)*sd(x)/sqrt(length(x)),  
                          hi = qt(.975,length(x)-1)*sd(x)/sqrt(length(x))  ))

colnames(a) <- c("model", "mean")
a$meanLL = a$mean[,1]
a$lo = a$mean[,1]+a$mean[,2]
a$hi = a$mean[,1]+a$mean[,3]
a = a[ ,c("model", "meanLL", "lo", "hi")]
a = a[order(-a$meanLL),]

a$modeltype = "Heuristic"
a$modeltype[which(a$model %in% model_names)] = "Planning"

a$model = factor(a$model, levels = a$model[order(-a$meanLL)])
g = ggplot(a, aes(y=meanLL, x=model, fill=modeltype)) + geom_bar(stat='identity') + theme_light() 
g = g + geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g + ylab("Model Fit") + coord_flip()+ guides(fill="none") + scale_fill_manual(values = c("grey", "deepskyblue", "seagreen3"))

g2 = g
#grid.arrange(g1, g2, ncol = 2)

t.test(subset(f$LL, f$model == "PW"), subset(f$LL, f$model == "Steps-Num")) #***

t.test(subset(f$LL, f$model == "Steps-Cells-Num"), subset(f$LL, f$model == "DU")) #**
t.test(subset(f$LL, f$model == "DU"), subset(f$LL, f$model == "EU"))


#-------------------------------------------------------------------------------------------------------------
#
# for each subject, select the model that fits to the subject best, count how many times each model is the best model
# replication of Falk Lieder analysis -- this is not great figure since bestmodel is often undefined, when CI overlap 
#
#-------------------------------------------------------------------------------------------------------------

f =  fit_models[ , c("LL", "model", "subject", "modeltype")]
count_best = data.frame(model = unique(f$model), count=0)

for (s in subjects) {
  x = subset(f, f$subject == s)
  m = x$model[which.max(x$LL)]
  if (length(m)>1) print("there are ties")
  c = count_best$count[which(count_best$model == m)]
  count_best$count[which(count_best$model == m)] = c+1
}

count_best$modeltype = "Heuristic"
count_best$modeltype[which(count_best$model %in% model_names)] = "Planning"
count_best = count_best[order(-count_best$count),]
count_best$model = factor(count_best$model, levels = count_best$model[order(-count_best$count)])
g = ggplot(count_best, aes(y=count, x=model, fill=modeltype)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g + ylab("N subjects")+ guides(fill="none")+ scale_fill_manual(values = c("grey", "deepskyblue", "seagreen3"))


#-------------------------------------------------------------------------------------------------------------
#
# simply show two bars, for models and heuristics 
#
#-------------------------------------------------------------------------------------------------------------


g = ggplot(count_best, aes(y=count, x=modeltype, fill=modeltype)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g + ylab("N subjects")+ guides(fill="none")+ scale_fill_manual(values = c("grey", "deepskyblue", "seagreen3"))

#-------------------------------------------------------------------------------------------------------------
#
# correlations of models with human aggregate responses - show each correlation as mean with CI
# 
# DISCLAIMER
# results may be affected by minimal number subjects needed to include a decision in the policy:
# suppose there is a decision state only reached by a minority -- this is either a highly suboptimal minority, or a highly optimal minority
# such that -- the is all subjects were placed in the same decision state, the probabilities would be different
#
# rerun the correlation analysis with several levels of min_subjects to see how it affects the results
#
# likewise, mean parameter fits are not guaranteed to maximize aggregate correlation, 
# if the distribution of a fitted parameter is not normal
#
#-------------------------------------------------------------------------------------------------------------


a = get_mean_parameter_fits(fit_models)

bootstrapped_correlation_with_ci = function(model_name, grid, values, parameter_fit, policy) {
  df = match_model_and_human_probabilities( model_name, grid, values, parameter_fit, policy)
  # r = rcorr(df$prob, df$modelProb)[1]$r[2] # this gives a correlation, however it needs to be bootstrapped with CI 
  df = df[,!(names(df) %in% c("child"))]
  colnames(df) <-(c("x", "y"))
  results = boot(df, statistic = Bcorr, R = 1000) # bootstrapped correlation
  ci = boot.ci(results, type=c("basic"))
  r = results$t0
  lo = ci$basic[4]
  hi = ci$basic[5]
  return ( data.frame(model =model_name, r=r, lo=lo, hi=hi ) )
}

correlations  = data.frame()
if (sampling_model_available) {
  correlations = rbind(correlations, bootstrapped_correlation_with_ci("Sampling", grid_sampling, values_sampling, a, policy) )
}
correlations = rbind(correlations, bootstrapped_correlation_with_ci("EU", grid_eu, values_eu, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("DU", grid_du, values_du, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("PW", grid_pw, values_pw, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("PW_DU", grid_pw_du, values_pw_du, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("DU-Num", grid_du_num, values_du_num, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("EU-Num", grid_eu_num, values_eu_num, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps", grid_steps, values_steps, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Cells", grid_cells, values_cells, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps-Num", grid_steps_num, values_steps_num, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Cells-Num", grid_cells_num, values_cells_num, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps-Cells", grid_steps_cells, values_steps_cells, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps-Cells-Num", grid_steps_cells_num, values_steps_cells_num, a, policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Random", grid_random, values_random, a, policy) )

correlations$modeltype = "Heuristic"
correlations$modeltype[which(correlations$model %in% model_names)] = "Planning"
correlations$model[which(correlations$model == "PW_DU")] = "PW-DU"
correlations = correlations[order(-correlations$r),]
correlations$model = factor(correlations$model, levels = correlations$model[order(-correlations$r)])
g = ggplot(correlations, aes(y=r, x=model, colour = modeltype)) + geom_point(stat='identity') + theme_light() 
g = g + geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g = g + ylab("Correlation with people")+ guides(colour="none")+ scale_colour_manual(values = c("grey", "deepskyblue", "seagreen3"))
g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Bdiff <- function(data, i) {  
  c1 = cor(data[i, "hp1"], data[i, "m1"], method='pearson') 
  c2 = cor(data[i, "hp2"], data[i, "m2"], method='pearson') 
  return(c1-c2)
}

bootstrap_difference_between_correlations = function(model1, grid1, values1, model2, grid2, values2, parameter_fit, policy) {
  df1 = match_model_and_human_probabilities( model1, grid1, values1, parameter_fit, policy)
  colnames(df1) <-(c("child", "hp1", "m1"))
  
  df2 = match_model_and_human_probabilities( model2, grid2, values2, parameter_fit, policy)
  colnames(df2) <-(c("child", "hp2", "m2"))
  
  results = boot(merge(df1, df2, by="child"), statistic = Bdiff, R = 1000) # bootstrapped correlation
  ci = boot.ci(results, type=c("basic"))
  lo = ci$basic[4]
  hi = ci$basic[5]
  return ( data.frame(diff=results$t0, lo=lo, hi=hi ) )
}

bootstrap_difference_between_correlations("DU", grid_du, values_du, "Steps-Cells-Num", grid_steps_cells_num, values_steps_cells_num, a, policy)

bootstrap_difference_between_correlations("DU-Num", grid_du_num, values_du_num, "Steps-Num", grid_steps_num, values_steps_num, a, policy)

# 
# E1 nogrid  diff         lo        hi
#            0.08         0.03     0.13
bootstrap_difference_between_correlations("DU-Num", grid_du_num, values_du_num, "Steps-Cells", grid_steps_cells, values_steps_cells, a, policy)
bootstrap_difference_between_correlations("DU-Num", grid_du_num, values_du_num, "PW", grid_pw, values_pw, a, policy)
bootstrap_difference_between_correlations("DU-Num", grid_du_num, values_du_num, "DU", grid_du, values_du, a, policy)
bootstrap_difference_between_correlations("DU-Num", grid_du_num, values_du_num, "PW_DU", grid_pw_du, values_pw_du, a, policy)

bootstrap_difference_between_correlations("DU-Num", grid_du_num, values_du_num, "EU", grid_eu, values_eu, a, policy)



bootstrap_difference_between_correlations("PW_DU", grid_pw_du, values_pw_du, "Steps", grid_steps, values_steps, a, policy)
bootstrap_difference_between_correlations("PW_DU", grid_pw_du, values_pw_du,  "EU", grid_eu, values_eu, a, policy)

bootstrap_difference_between_correlations("PW", grid_pw, values_pw,  "EU", grid_eu, values_eu, a, policy)



#-------------------------------------------------------------------------------------------------------------
#
# For each person select best model, and best heuristic
# Plot a scatterplot X-Best planningmodel, Y - best heuristic - 
# this might look more informative than LL bar plot, which averages over all subjects
#
#-------------------------------------------------------------------------------------------------------------


if (LL_CI_available) {
  f =  fit_models[ , c("LL", "model", "subject", "modeltype", "lo", "hi")]
} else {
  f =  fit_models[ , c("LL", "model", "subject", "modeltype")]
 
}
subject_best_model = data.frame(subject = subjects, bestModel=NA, bestModelLL=NA,  bestHeuristicLL=NA, bestHeuristic=NA,
                                bestModel_lo = NA, bestModel_hi = NA, bestHeur_lo = NA, bestHeur_hi = NA)

for (s in subjects) {
  x = subset(f, f$subject == s)
  
  y=subset(x, x$modeltype == "Planning")
  idx = which.max(y$LL)
  subject_best_model$bestModel[which(subject_best_model$subject == s)] = y$model[idx]
  subject_best_model$bestModelLL[which(subject_best_model$subject == s)] = y$LL[idx]
  if (LL_CI_available) {
    subject_best_model$bestModel_lo[which(subject_best_model$subject == s)] = y$lo[idx]
    subject_best_model$bestModel_hi[which(subject_best_model$subject == s)] = y$hi[idx]
  }
  
  y = subset(x, x$modeltype == "Heuristic")
  idx = which.max( y$LL )
  subject_best_model$bestHeuristic[which(subject_best_model$subject == s)] = y$model[idx]
  subject_best_model$bestHeuristicLL[which(subject_best_model$subject == s)] = y$LL[idx]
  
  if (LL_CI_available) {
    subject_best_model$bestHeur_lo[which(subject_best_model$subject == s)] = y$lo[idx]
    subject_best_model$bestHeur_hi[which(subject_best_model$subject == s)] = y$hi[idx]
  }
}

min = min(subject_best_model$bestHeuristicLL, subject_best_model$bestModelLL)
max = max(subject_best_model$bestHeuristicLL, subject_best_model$bestModelLL)

if (LL_CI_available) {
  subject_best_model$heuristic = "Not defined"
  subject_best_model$heuristic[subject_best_model$bestHeur_lo > subject_best_model$bestModel_hi] = "Heuristic"
  subject_best_model$heuristic[subject_best_model$bestModel_lo > subject_best_model$bestHeur_hi] = "Planning"
} else {
  subject_best_model$heuristic = subject_best_model$bestHeuristicLL > subject_best_model$bestModelLL
}

g = ggplot(subject_best_model, aes(y=bestHeuristicLL, x=bestModelLL, colour=heuristic)) + geom_point() + theme_light() 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Best Model LL")
g = g + ylab("Best Heuristic LL")+ guides(fill="none")+xlim(min, max) + ylim(min, max)
if (LL_CI_available) {
  g = g + geom_errorbar(aes(ymin= bestHeur_lo, ymax= bestHeur_hi, colour = heuristic), width=.02, alpha = 0.7) 
  g = g + geom_errorbarh(aes(xmin= bestModel_lo, xmax= bestModel_hi, colour = heuristic), height=.02, alpha = 0.7) 
}
g+geom_abline(slope=1) + scale_colour_manual(values = c("grey", "seagreen3", "deepskyblue"))


#-------------------------------------------------------------------------------------------------------------
#
# using data from above simply show three bars, for models, undefined, and heuristics 
#
#-------------------------------------------------------------------------------------------------------------

subject_best_model$count = 1
subject_best_model$fitted = "Heuristic"
subject_best_model$fitted[which(subject_best_model$heuristic == FALSE)] = "Planning" 
g = ggplot(subject_best_model, aes(y=count, x=fitted , fill=fitted)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g + ylab("N subjects")+ guides(fill="none") + scale_colour_manual(values = c("grey", "seagreen3", "deepskyblue"))


subject_best_model$count = 1
g = ggplot(subject_best_model, aes(y=count, x=heuristic , fill=heuristic)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("")
g + ylab("N subjects")+ guides(fill="none")  + scale_fill_manual(values = c("grey", "seagreen3", "deepskyblue"))

#+  scale_fill_manual(values = c( "seagreen3", "deepskyblue"))



#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#               Fitting to Worlds
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------









#-------------------------------------------------------------------------------------------------------------
#
#  for each model bar plot loglikes of all WORLDS with 95CI -- this assumes that LLs have been fitted to worlds
#
#-------------------------------------------------------------------------------------------------------------

f =  fit_models[ , c("LL", "model")]
a = aggregate(f$LL,  by=list(f$model), 
              FUN = function(x) c(mean = mean(x), 
                                  lo = qt(.025,length(x)-1)*sd(x)/sqrt(length(x)),  
                                  hi = qt(.975,length(x)-1)*sd(x)/sqrt(length(x))  ))

colnames(a) <- c("model", "mean")
a$meanLL = a$mean[,1]
a$lo = a$mean[,1]+a$mean[,2]
a$hi = a$mean[,1]+a$mean[,3]
a = a[ ,c("model", "meanLL", "lo", "hi")]
a = a[order(-a$meanLL),]

a$model = factor(a$model, levels = a$model[order(-a$meanLL)])
a$modeltype = "Heuristic"
a$modeltype[which(a$model %in% model_names)] = "Planning"


g = ggplot(a, aes(y=meanLL, x=model, fill=modeltype)) + geom_bar(stat='identity') + theme_light() 
g = g + geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("world LLs") + coord_flip()+ guides(fill="none")


#  superimpose dots on this plot - each dot a maze
f$modeltype = "Heuristic"
f$modeltype[which(f$model %in% model_names)] = "Planning"
f$model = factor(f$model, levels = a$model[order(-a$meanLL)])
g = ggplot(data=f) + geom_boxplot(mapping=aes(x=model, y=LL, colour=modeltype)) + theme_light() 
g = g + geom_jitter(f, mapping=aes(x=model, y=LL, colour=modeltype), alpha=0.5)
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("world LLs") + coord_flip()+ guides(colour="none")


# E4 fitting to mazes, compare best ft model and best fit heuristic
# t.test(subset(fit_models$LL, fit_models$model == "PW_DU"), subset(fit_models$LL, fit_models$model == "Steps-Cells-Num"))


#-------------------------------------------------------------------------------------------------------------
#
# for each world, select the model that fits to it best, count how many times each model is the best model
# this does not take into account CI
#
#-------------------------------------------------------------------------------------------------------------

best_model_for_maze = data.frame()
f =  fit_models[ , c("LL", "model", "world", "modeltype")]
count_best = data.frame(model = unique(f$model), count=0)

for (w in worlds) {
  x = subset(f, f$world == w)
  m = x$model[which.max(x$LL)]
  if (length(m)>1) {
    print("there are ties")
  } else {
    print (paste("best model for ", w, m)) 
    best_model_for_maze = rbind(best_model_for_maze, c(w,m))
  }
  c = count_best$count[which(count_best$model == m)]
  count_best$count[which(count_best$model == m)] = c+1
}

colnames(best_model_for_maze) <- c( "world", "model")
count_best$modeltype = "Heuristic"
count_best$modeltype[which(count_best$model %in% model_names)] = "Planning"

count_best = count_best[order(-count_best$count),]
count_best$model = factor(count_best$model, levels = count_best$model[order(-count_best$count)])
g = ggplot(count_best, aes(y=count, x=model, fill=modeltype)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("N worlds")

planning_mazes = best_model_for_maze$world[which(best_model_for_maze$model %in% model_names)]

#-------------------------------------------------------------------------------------------------------------
#
# for a maze show ll of all models
# 
#-------------------------------------------------------------------------------------------------------------

#e1
planning_mazes = c("4ways",  "4ways_mirror", "Equal_6vs6", "Equal_6vs6_mirror", "Equal_8vs8", "Equal_8vs8_mirror",
                   "3ways_2_mirror", "3ways_2", "Loop_1_mirror", "Loop_1")

planning_mazes_e3 = c("maze3_2_exit1", "maze1_1_exit0",      
                   "maze4_0_exit1", "maze1_0_exit0", # this one is really good
                   "test5_exit0",   "test5_exit1",         
                   "test3_exit1",   
                   "maze3_0_exit0",    "maze3_0_exit1",  # this one is really good          
                   "test4_exit0",  "test2_exit1", "test2_exit0",  "easy_1vs9_loop_exit0")

# E4
planning_mazes_e4 = c( "medium_cubicles",  "hard_hallway_0", "medium_cathedral", "easy_large_loop_2_copy", "medium_cathedral_copy", 
"easy_labyrinth", "hard_two_loops_2", "easy_spider_3_copy", "hard_two_loops_0", "easy_spider_2",         
"easy_spider_1", "easy_large_loop_1", "easy_large_loop_2" )

# planning_mazes computed just above
w =  planning_mazes[1]
f = subset(fit_models[ , c("LL", "model", "world", "modeltype", "lo", "hi")], fit_models$world == w)
f$model = factor(f$model, levels = f$model[order(-f$LL)])
g1 = ggplot(data=f, aes(y=LL, x=model, fill=modeltype)) + geom_bar(stat='identity') + 
  theme_light() + coord_flip()+xlab(w)+ guides(fill="none")+ geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 

w =  planning_mazes[2]
f = subset(fit_models[ , c("LL", "model", "world", "modeltype", "lo", "hi")], fit_models$world == w)
f$model = factor(f$model, levels = f$model[order(-f$LL)])
g2 = ggplot(f, aes(y=LL, x=model, fill=modeltype)) + geom_bar(stat='identity') + 
  theme_light() + coord_flip()+xlab(w)+ guides(fill="none")+ geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 

w = planning_mazes[3]
f = subset(fit_models[ , c("LL", "model", "world", "modeltype", "lo", "hi")], fit_models$world == w)
f$model = factor(f$model, levels = f$model[order(-f$LL)])
g3 = ggplot(f, aes(y=LL, x=model, fill=modeltype)) + geom_bar(stat='identity') + 
  theme_light() + coord_flip()+xlab(w)+ guides(fill="none")+ geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 

w = planning_mazes[3]
f = subset(fit_models[ , c("LL", "model", "world", "modeltype", "lo", "hi")], fit_models$world == w)
f$model = factor(f$model, levels = f$model[order(-f$LL)])
g4 = ggplot(f, aes(y=LL, x=model, fill=modeltype)) + geom_bar(stat='identity') +
  theme_light() + coord_flip()+xlab(w)+ guides(fill="none")+ geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 

grid.arrange(g1, g2, g3, g4, ncol = 2)

 

#-------------------------------------------------------------------------------------------------------------
#
# For each maze select best model, and best heuristic
# Plot a scatterplot X-Best planningmodel, Y - best heuristic - 
# this shows whether heuristics and models are differentiated
#
#-------------------------------------------------------------------------------------------------------------


f =  fit_models[ , c("LL", "model", "world", "modeltype", "lo", "hi")]
world_best_model = data.frame(world = worlds, bestModel=NA, bestModelLL=NA,  bestHeuristicLL=NA, bestHeuristic=NA,
                                bestModel_lo = NA, bestModel_hi = NA, bestHeur_lo = NA, bestHeur_hi = NA)

for (w in worlds) {
  x = subset(f, f$world == w)
  
  y=subset(x, x$modeltype == "Planning")
  idx = which.max(y$LL)
  world_best_model$bestModel[which(world_best_model$world == w)] = y$model[idx]
  world_best_model$bestModelLL[which(world_best_model$world == w)] = y$LL[idx]
  if (LL_CI_available) {
    world_best_model$bestModel_lo[which(world_best_model$world == w)] = y$lo[idx]
    world_best_model$bestModel_hi[which(world_best_model$world == w)] = y$hi[idx]
  }
  
  y = subset(x, x$modeltype == "Heuristic")
  idx = which.max( y$LL )
  world_best_model$bestHeuristic[which(world_best_model$world == w)] = y$model[idx]
  world_best_model$bestHeuristicLL[which(world_best_model$world == w)] = y$LL[idx]
  
  if (LL_CI_available) {
    world_best_model$bestHeur_lo[which(world_best_model$world == w)] = y$lo[idx]
    world_best_model$bestHeur_hi[which(world_best_model$world == w)] = y$hi[idx]
  }
}

min = min(world_best_model$bestHeuristicLL, world_best_model$bestModelLL)
max = max(world_best_model$bestHeuristicLL, world_best_model$bestModelLL)

if (LL_CI_available) {
  world_best_model$heuristic = "not defined"
  world_best_model$heuristic[world_best_model$bestHeur_lo > world_best_model$bestModel_hi] = "Heuristic"
  world_best_model$heuristic[world_best_model$bestModel_lo > world_best_model$bestHeur_hi] = "Planning"
} else {
  world_best_model$heuristic = world_best_model$bestHeuristicLL > world_best_model$bestModelLL
}

g = ggplot(world_best_model, aes(y=bestHeuristicLL, x=bestModelLL, colour=heuristic)) + geom_point() + theme_light() 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Best Model LL")
g = g + ylab("Best Heuristic LL")+ guides(fill="none")+xlim(min, max) + ylim(min, max)
if (LL_CI_available) {
  g = g + geom_errorbar(aes(ymin= bestHeur_lo, ymax= bestHeur_hi, colour = heuristic), width=.02, alpha = 0.3) 
  g = g + geom_errorbarh(aes(xmin= bestModel_lo, xmax= bestModel_hi, colour = heuristic), height=.02, alpha = 0.3) 
}
g+geom_abline(slope=1)


subset(world_best_model$world, world_best_model$heuristic == "Planning")


