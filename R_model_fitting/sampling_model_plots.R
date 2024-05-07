#-------------------------------------------------------------------------------------------------------------
# Sampling model (mean fit -- budget=200, c=11), correlation with humans
#-------------------------------------------------------------------------------------------------------------

# correlate probability of each "child" in policy, with probability a model assigns to the same node ID given parameters
# for example, doing this for one model

#-------------------------------------------------------------------------------------------------------------
# sampling correlation with human policy -- 0.8 in Experiment 4
#-------------------------------------------------------------------------------------------------------------

df = match_model_and_human_probabilities("Sampling", grid_sampling, values_sampling, a)
rcorr(df$prob, df$modelProb)
ggplot(df) +aes(x=prob, y=modelProb) + geom_point() + geom_smooth(method ='lm')

#-------------------------------------------------------------------------------------------------------------
# does sampling approximate EU? 
# sampling correlates with EU corr=0.94, if both models parametrised with mean human fits
#-------------------------------------------------------------------------------------------------------------

eu_val = value_column_name_from_params(c(2.092, 1, 1)) # tau = 4.091, 2.925, 2.092, is just as good, tau=2.2 is fitted to humans
#sm_val = value_column_name_from_params(c(1000, 26)) # maximal correlation
sm_val = value_column_name_from_params(c(200, 11))
rcorr(values_eu[,eu_val], values_sampling[,sm_val]) #0.94
ggplot(data.frame(eu=values_eu[,eu_val], sampling=values_sampling[,sm_val])) + aes(x=eu, y=sampling) + geom_point() + 
  geom_smooth(method ='lm')+ylab("sampling (budget=200,c=11) corr = 0.94")

#-------------------------------------------------------------------------------------------------------------
# find parameters of the sampling model that are most correlated with EU, then
#
#
#-------------------------------------------------------------------------------------------------------------

max = 0
grid_sampling$corr = 0
for(i in 1:dim(grid_sampling)[1]) {
  sm_val = value_column_name_from_params(c(grid_sampling$budget[i], grid_sampling$c[i]))
  r = rcorr(values_eu[,eu_val], values_sampling[,sm_val])
  grid_sampling$corr[i] = r[1]$r[2]
  if (r[1]$r[2] > max) {
    max = r[1]$r[2]
    print(paste(grid_sampling$budget[i], grid_sampling$c[i], max))
  }
}

#  whcih combination of (budget, exploration) maximizes this correlation?
# it turns out that for each budget there is a maximal exploration beyond which the model does not perform any better
# c = 26 is the maximum for budget = 1000
ggplot( subset(grid_sampling, grid_sampling$budget ==1000) ) + aes(x = c, y =  corr)+geom_point() + ylab("correlation with eu (budget = 1000)")
grid_sampling = grid_sampling[,!(names(grid_sampling) %in% c("corr"))] 

#-------------------------------------------------------------------------------------------------------------
#
#   find at which parametrization sampling is best correlated with humans, this is not the mean fit
#
#-------------------------------------------------------------------------------------------------------------

max = 0
grid_sampling$corr = 0
for(i in 1:dim(grid_sampling)[1]) {
  
  sm_val = value_column_name_from_params(c(grid_sampling$budget[i], grid_sampling$c[i]))
  
  df = match_model_and_human_probabilities_from_column(values_sampling, sm_val)
  r = rcorr(df$prob, df$modelProb)
  grid_sampling$corr[i] = r[1]$r[2]
  if (r[1]$r[2] > max) {
    max = r[1]$r[2]
    print(paste(grid_sampling$budget[i], grid_sampling$c[i], max))
  }
}

# max correlation at budget=9, c=4, r=0.81
ggplot(df) +aes(x=prob, y=modelProb) + geom_point() + geom_smooth(method ='lm') + xlab("huamns prob")+ ylab("sampling prob, budget=9, c=4, r =0.81")

