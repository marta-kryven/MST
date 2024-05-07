# simulate and fit

# use this code to establish how an existing experiment would be fitted, if the mazes were navigated by a specific model

# for designing a new experiment, use a different pipeline:
# 1. create a folder, like __experiment_5, inside create folders: mazes, trees, pickled_data, node_values_recursive, csv_files
# 2. place mazes to be evaluated in ./mazes/, run tree_builder.py, after setting EXPERIMENT glogal variable to, for example 5 - 
# this will generate picke tree
# 3. from jupyter, generate values for all models for this tree, export values and tree to .scv
# 4. simulated subject data is generated also from jupyter
# 5. in R, load the value files and simulated subject data generated above, then use the regular fitting code like any other experiment

# (!!!) value tables are loaded limited to only contain nodes that are also present in planvalues
# if the simulation is not meant to be not limited to those nodes (to imitate random exit placement in each maze) reload value tables, 
# and modify the code below to randomly terminate at each node with a certain probability


# selecting one value column for the specific model that we will use to simulate humans

# tested with E1
# recovers DU parameters, sigificant difference from heuristic in LL and correlations, 
# but only 30% of subjects definitively fitted by a planning model
# model_probs = values_du[ , c("world", "node", "child", "value_1.07_0.7_1")]

# parameters recovered, scatter ok, correlations not
# model_probs = values_pw[ , c("world", "node", "child", "value_1.07_1_0.528")]

# parameters recovered, scatter ok, correlations not
# model_probs = values_steps_cells[ , c("world", "node", "child", "value_1.07_0.5")]

# model_probs = values_eu[ , c("world", "node", "child", "value_1.07_1_1")]

#tested with E4
# parameters recovered correctly, majority of subjects labelled as planning, heuristics/eu ruled out, no difference between models
# model_probs = values_du[ , c("world", "node", "child", "value_1.07_0.6_1")]


names(model_probs) = c("world", "node", "child", "p")
N_subjects_to_simulate = 100

fit_to_subjects = FALSE

nodes = unique(planvalues$nodeName)
worlds = unique(planvalues$world)
simulated_data = data.frame()

for (sim in 1 : N_subjects_to_simulate) {
  
  print(sim)
  for (w in worlds) {
    
    # start node
    n = unique(subset(planvalues$nodeName, planvalues$squaretype == "X" & planvalues$world == w))
    children = subset(model_probs, model_probs$node == n)
    
    while(dim(children)[1] >0){
      # each of the three rows needs to be selected with the corresponding probability
      idx_selected = sample(1:dim(children)[1], 1, replace=TRUE, prob=children$p )
      
      # record choice
      for (c in 1:dim(children)[1]){
        if (c==idx_selected) {
          simulated_data = rbind(simulated_data, data.frame(subject = sim, world = w, nodeName = n, availableChildName = children$child[c], chosen = TRUE ))
        } else {
          simulated_data = rbind(simulated_data, data.frame(subject = sim, world = w, nodeName = n, availableChildName = children$child[c], chosen = FALSE ))
        }
      }
      
      n=children$child[idx_selected]
      children = subset(model_probs, model_probs$node == n)
    }
  }
}


if (fit_to_subjects) {
  ll_eu               = compute_LL_for_each_subject(grid_eu, values_eu, simulated_data)
  ll_steps            = compute_LL_for_each_subject(grid_steps, values_steps, simulated_data)
  ll_cells            = compute_LL_for_each_subject(grid_cells, values_cells, simulated_data)
  ll_random           = compute_LL_for_each_subject(data.frame(tau=1), values_random, simulated_data) # slightly different call, since grid is a dummy variable
  
  ll_pw               = compute_LL_for_each_subject(grid_pw, values_pw, simulated_data) 
  ll_steps_cells      = compute_LL_for_each_subject(grid_steps_cells, values_steps_cells, simulated_data) 
  ll_du              = compute_LL_for_each_subject(grid_du, values_du, simulated_data) 
  ll_steps_num       = compute_LL_for_each_subject(grid_steps_num, values_steps_num, simulated_data)
  ll_cells_num       = compute_LL_for_each_subject(grid_cells_num, values_cells_num, simulated_data)
  ll_eu_num          = compute_LL_for_each_subject(grid_eu_num, values_eu_num, simulated_data) 
  
  
  if (sampling_model_available) {
    ll_sampling      = compute_LL_for_each_subject(grid_sampling, values_sampling, simulated_data) 
  }
  
  ll_steps_cells_num = compute_LL_for_each_subject(grid_steps_cells_num, values_steps_cells_num, simulated_data) # 
  ll_du_num          = compute_LL_for_each_subject(grid_du_num, values_du_num, simulated_data) 
  ll_pw_du           = compute_LL_for_each_subject(grid_pw_du, values_pw_du, simulated_data)   
  
  fit_models = mc_fit_all_subjects(unique(simulated_data$subject))
  subjects = unique(simulated_data$subject) 
  
} else {
  ll_eu               = compute_LL_for_each_world(grid_eu, values_eu, simulated_data)
  ll_steps            = compute_LL_for_each_world(grid_steps, values_steps, simulated_data)
  ll_cells            = compute_LL_for_each_world(grid_cells, values_cells, simulated_data)
  ll_random           = compute_LL_for_each_world(data.frame(tau=1), values_random, simulated_data) # slightly different call, since grid is a dummy variable
  
  ll_pw               = compute_LL_for_each_world(grid_pw, values_pw, simulated_data) 
  ll_steps_cells      = compute_LL_for_each_world(grid_steps_cells, values_steps_cells, simulated_data) 
  ll_du              = compute_LL_for_each_world(grid_du, values_du, simulated_data) 
  ll_steps_num       = compute_LL_for_each_world(grid_steps_num, values_steps_num, simulated_data)
  ll_cells_num       = compute_LL_for_each_world(grid_cells_num, values_cells_num, simulated_data)
  ll_eu_num          = compute_LL_for_each_world(grid_eu_num, values_eu_num, simulated_data) 
  
  
  if (sampling_model_available) {
    ll_sampling      = compute_LL_for_each_world(grid_sampling, values_sampling, simulated_data) 
  }
  
  ll_steps_cells_num = compute_LL_for_each_world(grid_steps_cells_num, values_steps_cells_num, simulated_data) # 
  ll_du_num          = compute_LL_for_each_world(grid_du_num, values_du_num, simulated_data) 
  ll_pw_du           = compute_LL_for_each_world(grid_pw_du, values_pw_du, simulated_data)   
  
  fit_models = mc_fit_all_worlds()
}

fit_models$modeltype = "Heuristic"
fit_models$modeltype[which(fit_models$model %in% model_names)] = "Planning"
LL_CI_available = TRUE





# loglikes

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
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("subject LLs") + coord_flip()+ guides(fill="none")


# target model vs EU
t.test(subset(f$LL, f$model == "PW"), subset(f$LL, f$model == "EU"))

# target model vs Best heuristic
t.test(subset(f$LL, f$model == "PW"), subset(f$LL, f$model == "Steps-Cells"))

# target model vs other models - PW, PW-DU not different, this is the first one that's different
t.test(subset(f$LL, f$model == "PW"), subset(f$LL, f$model == "DU"))


# correlations

simulated_policy = compute_policy(simulated_data)
a = get_mean_parameter_fits(fit_models)

correlations  = data.frame()
if (sampling_model_available) {
  correlations = rbind(correlations, bootstrapped_correlation_with_ci("Sampling", grid_sampling, values_sampling, a, simulated_policy) )
}
correlations = rbind(correlations, bootstrapped_correlation_with_ci("EU", grid_eu, values_eu, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("DU", grid_du, values_du, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("PW", grid_pw, values_pw, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("PW_DU", grid_pw_du, values_pw_du, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("DU-Num", grid_du_num, values_du_num, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("EU-Num", grid_eu_num, values_eu_num, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps", grid_steps, values_steps, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Cells", grid_cells, values_cells, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps-Num", grid_steps_num, values_steps_num, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Cells-Num", grid_cells_num, values_cells_num, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps-Cells", grid_steps_cells, values_steps_cells, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Steps-Cells-Num", grid_steps_cells_num, values_steps_cells_num, a, simulated_policy) )
correlations = rbind(correlations, bootstrapped_correlation_with_ci("Random", grid_random, values_random, a, simulated_policy) )

correlations$modeltype = "Heuristic"
correlations$modeltype[which(correlations$model %in% model_names)] = "Planning"
correlations$model[which(correlations$model == "PW_DU")] = "PW-DU"
correlations = correlations[order(-correlations$r),]
correlations$model = factor(correlations$model, levels = correlations$model[order(-correlations$r)])
g = ggplot(correlations, aes(y=r, x=model, colour = modeltype)) + geom_point(stat='identity') + theme_light() 
g = g + geom_errorbar(aes(ymin= lo, ymax= hi), width=.2) 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("Correlation")+ guides(colour="none")


bootstrap_difference_between_correlations("DU", grid_du, values_du, 
                                          "Steps-Cells", grid_steps_cells, values_steps_cells, 
                                          a, simulated_policy)



# -------------------------------------------------------------------------------------------------------------------
#
# subjects - individual scatterplot and bars
#
# -------------------------------------------------------------------------------------------------------------------

# best model, best heuristic - scatter
f =  fit_models[ , c("LL", "model", "subject", "modeltype", "lo", "hi")]
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
  subject_best_model$heuristic = "not defined"
  subject_best_model$heuristic[subject_best_model$bestHeur_lo > subject_best_model$bestModel_hi] = "Heuristic"
  subject_best_model$heuristic[subject_best_model$bestModel_lo > subject_best_model$bestHeur_hi] = "Planning"
} else {
  subject_best_model$heuristic = subject_best_model$bestHeuristicLL > subject_best_model$bestModelLL
}

g = ggplot(subject_best_model, aes(y=bestHeuristicLL, x=bestModelLL, colour=heuristic)) + geom_point() + theme_light() 
g = g + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Best Model LL")
g = g + ylab("Best Heuristic LL")+ guides(fill="none")+xlim(min, max) + ylim(min, max)
if (LL_CI_available) {
  g = g + geom_errorbar(aes(ymin= bestHeur_lo, ymax= bestHeur_hi, colour = heuristic), width=.02, alpha = 0.3) 
  g = g + geom_errorbarh(aes(xmin= bestModel_lo, xmax= bestModel_hi, colour = heuristic), height=.02, alpha = 0.3) 
}
g+geom_abline(slope=1)




# best model, best heuristic - bar
subject_best_model$count =1
g = ggplot(subject_best_model, aes(y=count, x=heuristic , fill=heuristic)) + geom_bar(stat='identity') + theme_light() 
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("N subjects")+ guides(fill="none")




# best model in absolute terms - this seems to be very good at recovering ground truth, but ignores CI
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
g = g + theme(text = element_text(size=18), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Model")
g + ylab("N subjects")+ guides(fill="none")



# -------------------------------------------------------------------------------------------------------------------
#
# worlds - bar plot LLs for a maze
#
# -------------------------------------------------------------------------------------------------------------------




