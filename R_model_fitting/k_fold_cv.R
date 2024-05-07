#-------------------------------------------------------------------------------
#
#    k-fold cross-validation, assuming loglikes are computed
#
#-------------------------------------------------------------------------------


N_folds = 5

# if true,  fit is the sum of LL for each subject. 
# Some subjects encounter fewer decisions than others, so lucky subjects will have smaller LL
# if false, return mean, so that each subject contributes equally to the LL bar chart
return_sum = TRUE

# ------------------------------------------------------------------------------------------------
#
#  randomly subdivides sequence of total_sequence_length into N blocks, 
#  output is a data frame
# sample idx
#     1  11
#     1  16
# ...........
#     N   3
#
# ----------------------------------------------------------------------------------------------------

generate_k_fold_indexes = function (N, total_sequence_length) {
  
  idx = seq(1, total_sequence_length)
  sample_length = floor(total_sequence_length/N) # block length, the remaining decisions are added to a random block
  
  for (sample in 1:(N-1)) {
    selected_idx = sample(idx, sample_length)
    idx = setdiff(idx, selected_idx)
    
    if (sample == 1) {
      subdivision = data.frame(sample = 1, idx = selected_idx)
    } else {
      subdivision = rbind(subdivision, data.frame(sample = sample, idx = selected_idx))
    }
  }
  
  subdivision = rbind(subdivision, data.frame(sample = N, idx = idx))
  
  return( subdivision )
}


# ------------------------------------------------------------------------------------------------
#
#  Input:
#  LLs - loglike table with loglikes for this subject
#  grid - parameters for whcih LLs are calculated, including priors if any are used -- grid and LL dimensions must match 
#  split -- indexes for k folds
#  k -- this is the test fold on this iteration
#  model name - a string name of the model, this helps with plots
#
#  Returns:
#  LL - resulting loglike - this is eather sum, or average, over all test folds
#  parameters of the model
#  k - which fold was the test fold
#
#  global parameter return_sum controls whether al LL for a subject are summed up, or returned as a mean
# ------------------------------------------------------------------------------------------------

k_fold_fit_model = function (LLs, grid, split, k, model_name) {
  
  # initialoze priors, make sure that the grid extracted from values and the saved LLs match in length
  priors = rep(0, length(grid$prior))
  if (use_priors) priors = grid$prior
  if (dim(LLs)[1] != length(priors)) {
    print("grid and LLs do not match - does grid have priors?")
    return (NULL)
  }
  
  # which columns are test and train on this iteration
  testsample = subset(split, split$sample == k) 
  trainsample = subset(split, split$sample != k)
  ll_columns = grep("log_probs", colnames(LLs))
  train_columns = cbind(LLs[, ll_columns[trainsample$idx]], priors)
  test_columns  = LLs[, ll_columns[testsample$idx]] # test columns do not need priors
  
  most_likley_row = which.max(rowSums(train_columns)) # select the most likely row
  params = grid[most_likley_row, ]
  
  
  if (return_sum) {
    params$LL = sum(as.numeric(test_columns[most_likley_row ,]) ) 
  } else {
    # people could be encountering different numbers of decisions, so the sum of LL could weigh less lucky subjects more
    # taking mean so that each subject contributes equally
    # in practice this has little effect because the variation in the number of decisions is small
    params$LL = mean(as.numeric(test_columns[most_likley_row ,]) )  
  }
  
  params = params[,!(names(params) %in% c("prior"))] 
  params$fold = k
  params$model = model_name
  return( params ) 
}



# ------------------------------------------------------------------------------------------------
#
#  fitting all models -- retuns a table of fits with a row per each person
#  the fit is the sum of test LL over all folds, by default the best fitting parameters are avergaes 
#
#  subject_parameters_as_mode - if true, mode across all test folds is returned, this sharpens fitted parameter distribution, but generally has little effect
#  but could be of interest if parameter grids have uneven coverage
#  
# ------------------------------------------------------------------------------------------------

k_fold_fit_all_subjects = function(subject_parameters_as_mode = FALSE) {
  
  result = data.frame()

  # collect all possible parameter names we might need to group all models in one data frame 08
  # TBD: would be nice to iterate through them, does R support arrays of data.frames ? 
  
  if (sampling_model_available) {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num), colnames(grid_sampling)))
  } else {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num)))
  }
  
  allcolumns = allcolumns[!(allcolumns %in% c("prior"))]
  
  grid_eu = add_dummy_columns(grid_eu, allcolumns)
  grid_pw = add_dummy_columns(grid_pw, allcolumns)
  grid_du = add_dummy_columns(grid_du, allcolumns)
  grid_pw_du = add_dummy_columns(grid_pw_du, allcolumns)
  grid_eu_num = add_dummy_columns(grid_eu_num, allcolumns)
  grid_du_num = add_dummy_columns(grid_du_num, allcolumns)
  grid_steps = add_dummy_columns(grid_steps, allcolumns)
  grid_cells = add_dummy_columns(grid_cells, allcolumns)
  grid_steps_num = add_dummy_columns(grid_steps_num, allcolumns)
  grid_cells_num = add_dummy_columns(grid_cells_num, allcolumns)
  grid_steps_cells = add_dummy_columns(grid_steps_cells, allcolumns)
  grid_random = add_dummy_columns(grid_random, allcolumns)
  grid_steps_cells_num = add_dummy_columns(grid_steps_cells_num, allcolumns)
  
  if (sampling_model_available) {
    grid_sampling = add_dummy_columns(grid_sampling, allcolumns)
  }
  
  for (s in subjects) {
    
    print(s)
    k_fit = data.frame()
    split = generate_k_fold_indexes(N_folds, number_of_decisions(s)) # split the subject's decisions into folds, 
                                                                      #so that model will be fitted to a subject to the same test-train decisions
    for (k in 1:N_folds) {  
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_eu), grid_eu, split, k, "EU"))  
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_du), grid_du, split, k, "DU"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_pw), grid_pw, split, k, "PW"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_pw_du), grid_pw_du, split, k, "PW_DU"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_eu_num), grid_eu_num, split, k, "EU-Num"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_du_num), grid_du_num, split, k, "DU-Num"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_steps), grid_steps, split, k, "Steps"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_cells), grid_cells, split, k, "Cells"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_steps_cells), grid_steps_cells, split, k, "Steps-Cells"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_steps_num), grid_steps_num, split, k, "Steps-Num"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_cells_num), grid_cells_num, split, k, "Cells-Num"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_steps_cells_num), grid_steps_cells_num, split, k, "Steps-Cells-Num"))
        k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_random), grid_random, split, k, "Random"))
        
        if (sampling_model_available) {
          k_fit = rbind(k_fit, k_fold_fit_model(subject_LLs(s, ll_sampling), grid_sampling, split, k, "Sampling"))
        }
    }
    
    a = aggregate(k_fit$LL,  by = list(k_fit$model), FUN = sum)  # the fit is the sum over all test folds
    a = merge(a, aggregate(k_fit[ , allcolumns], by = list(k_fit$model), FUN = mean), by = "Group.1") # fitted parameters are avergaes over test folds
    colnames(a) <- c(c( "model", "LL"), allcolumns)
      
    # if we want to return the most frequently fitted parameter in the test folds as the best fitted parameter
    if (subject_parameters_as_mode) {
        for (m in unique(k_fit$models)) {
          a$tau[which(a$model ==m)] = getmode(subset(k_fit$tau,k_fit$model ==m) )      
          a$gamma[which(a$model ==m)] = getmode(subset(k_fit$gamma,k_fit$model ==m) )
          a$beta[which(a$model ==m)] = getmode(subset(k_fit$beta,k_fit$model ==m) )
          a$bits[which(a$model ==m)] = getmode(subset(k_fit$bits,k_fit$model ==m) )
        }
    }
      
    a$subject = s
    result = rbind(result, a)
  }
  
  return(result)
}






# ------------------------------------------------------------------------------------------------
#
#   same as fitting to subjects above, but fitting to mazes
#   the loglike table will have one row per world, with each column is a LL for one of the nodels and one of the subjects
#
# ------------------------------------------------------------------------------------------------

k_fold_fit_all_worlds = function() {
  
  result = data.frame()

  if (sampling_model_available) {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num), colnames(grid_sampling)))
  } else {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num)))
  }
  
  allcolumns = allcolumns[!(allcolumns %in% c("prior"))]
  
  grid_eu = add_dummy_columns(grid_eu, allcolumns)
  grid_pw = add_dummy_columns(grid_pw, allcolumns)
  grid_du = add_dummy_columns(grid_du, allcolumns)
  grid_pw_du = add_dummy_columns(grid_pw_du, allcolumns)
  grid_eu_num = add_dummy_columns(grid_eu_num, allcolumns)
  grid_du_num = add_dummy_columns(grid_du_num, allcolumns)
  grid_steps = add_dummy_columns(grid_steps, allcolumns)
  grid_cells = add_dummy_columns(grid_cells, allcolumns)
  grid_steps_num = add_dummy_columns(grid_steps_num, allcolumns)
  grid_cells_num = add_dummy_columns(grid_cells_num, allcolumns)
  grid_steps_cells = add_dummy_columns(grid_steps_cells, allcolumns)
  grid_random = add_dummy_columns(grid_random, allcolumns)
  grid_steps_cells_num = add_dummy_columns(grid_steps_cells_num, allcolumns)
  
  if (sampling_model_available) {
    grid_sampling = add_dummy_columns(grid_sampling, allcolumns)
  }
  
  worlds = unique(planvalues$world)
  for (w in worlds) {
    
    print(w)
    k_fit = data.frame()
    number_of_decisions = length(grep("log_probs", colnames(world_LLs(w, ll_eu))))
     
    split = generate_k_fold_indexes(N_folds, number_of_decisions) # split the subject's decisions into folds, 
    #so that model will be fitted to a subject to the same test-train decisions
    for (k in 1:N_folds) {  
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_eu), grid_eu, split, k, "EU"))  
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_du), grid_du, split, k, "DU"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_pw), grid_pw, split, k, "PW"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_pw_du), grid_pw_du, split, k, "PW_DU"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_eu_num), grid_eu_num, split, k, "EU-Num"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_du_num), grid_du_num, split, k, "DU-Num"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_steps), grid_steps, split, k, "Steps"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_cells), grid_cells, split, k, "Cells"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_steps_cells), grid_steps_cells, split, k, "Steps-Cells"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_steps_num), grid_steps_num, split, k, "Steps-Num"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_cells_num), grid_cells_num, split, k, "Cells-Num"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_steps_cells_num), grid_steps_cells_num, split, k, "Steps-Cells-Num"))
      k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_random), grid_random, split, k, "Random"))
      
      if (sampling_model_available) {
        k_fit = rbind(k_fit, k_fold_fit_model(world_LLs(w, ll_sampling), grid_sampling, split, k, "Sampling"))
      }
    }
    
    a = aggregate(k_fit$LL,  by = list(k_fit$model), FUN = sum)  # the fit is the sum over all test folds
    a = merge(a, aggregate(k_fit[ , allcolumns], by = list(k_fit$model), FUN = mean), by = "Group.1") # fitted parameters are avergaes over test folds
    colnames(a) <- c(c( "model", "LL"), allcolumns)
    a$world = w
    result = rbind(result, a)
  }
  
  return(result)
}



# ------------------------------------------------------------------------------------------------
#
# fitting all models -- the fit is the sum of test LL over all folds
# the best fitting parameters are avergaes
#
# ------------------------------------------------------------------------------------------------

fit_models = k_fold_fit_all_subjects ()
LL_CI_available = FALSE # this is true is montecarlo fitting is used 

#fit_models = k_fold_fit_all_worlds ()

fit_models$modeltype = "Heuristic"
fit_models$modeltype[which(fit_models$model %in% model_names)] = "Planning"


