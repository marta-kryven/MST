#-------------------------------------------------------------------------------
#
#    monte-carlo cross-validation, assuming loglikes are computed
#    this has an advantage over k-fold that there are CI for each LL fitted to subject
#
#-------------------------------------------------------------------------------

N_iterations = 1000


# if true,  fit is the sum of LL over all test folds. It can be a large number 100 iterations
# if false, return mean
# just an experiment to see if sum increases differences between models, it does not
return_sum = FALSE



generate_mc_indexes = function(train_fraction, N_decisions) {
  idx = seq(1, N_decisions)
  sample_length = floor(train_fraction*N_decisions)
  test_idx = sample(idx, sample_length)
  return( rbind ( data.frame(sample = "test",  idx = test_idx), 
                  data.frame(sample = "train", idx = setdiff(idx, test_idx))))
}

# currently does not support priors
mc_fit_model = function(LLs, grid, split, k, model_name)  {
  
  if (dim(LLs)[1] != dim(grid)[1]) {
    print("grid and LLs do not match")
    return (NULL)
  }
  
  testsample = subset(split, split$sample == "test") 
  trainsample = subset(split, split$sample != "test")
  ll_columns = grep("log_probs", colnames(LLs))
  
  if (use_priors) {
    train_columns = cbind(LLs[, ll_columns[trainsample$idx]], grid$prior)
  } else {
    train_columns = LLs[, ll_columns[trainsample$idx]]
  }
  test_columns  = LLs[, ll_columns[testsample$idx]] # test columns do not need priors
  
  most_likley_row = which.max(rowSums(train_columns)) # select the most likely row
  params = grid[most_likley_row, ]
  
  # people could be encountering different numbers of decisions, so the sum of LL in the test set could depend on that
  # taking mean so that each subject contributes equally 
  params$LL = mean(as.numeric(test_columns[most_likley_row ,]) )  
  params = params[,!(names(params) %in% c("prior"))] 
  params$iteration = k
  params$model = model_name
  return( params ) 
}


mc_fit_all_worlds = function() {
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
  
  for (w in worlds) {
    
    print(w)
    mc_fit = data.frame()
    
    for (k in 1:N_iterations) {  
      print(k)
      number_of_decisions = length(grep("log_probs", colnames(world_LLs(w, ll_eu))))
      split = generate_mc_indexes(0.2, number_of_decisions) # split the subject's decisions into test-train
      
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_eu), grid_eu, split, k, "EU"))  
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_du), grid_du, split, k, "DU"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_pw), grid_pw, split, k, "PW"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_eu_num), grid_eu_num, split, k, "EU-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_du_num), grid_du_num, split, k, "DU-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_steps), grid_steps, split, k, "Steps"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_cells), grid_cells, split, k, "Cells"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_steps_cells), grid_steps_cells, split, k, "Steps-Cells"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_steps_num), grid_steps_num, split, k, "Steps-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_cells_num), grid_cells_num, split, k, "Cells-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_steps_cells_num), grid_steps_cells_num, split, k, "Steps-Cells-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_random), grid_random, split, k, "Random"))
      
      if (sampling_model_available) {
        mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_sampling), grid_sampling, split, k, "Sampling"))
      }
      
      mc_fit = rbind(mc_fit, mc_fit_model(world_LLs(w, ll_pw_du), grid_pw_du, split, k, "PW_DU"))
      
    }
    
    a = aggregate(mc_fit[ , allcolumns], by = list(mc_fit$model), FUN = mean) # fitted parameters are avergaes over test folds
    colnames(a) <- c("model", allcolumns)
    a$world = w
    a$LL = NA
    a$lo = NA
    a$hi = NA
    
    for (m in unique(mc_fit$model)) {
      # this could be Bsum - sum of LL over all test folds
      if (return_sum) {
        results <- boot(data= subset(mc_fit$LL, mc_fit$model == m), statistic=Bsum, R=1000)
        ci = boot.ci(results, type=c("basic"))
        a$LL[which(a$model == m)] = results$t0
        a$lo[which(a$model == m)] = ci$basic[4]
        a$hi[which(a$model == m)] = ci$basic[5]
      } else {
        results <- boot(data= subset(mc_fit$LL, mc_fit$model == m), statistic=Bmean, R=1000)
        ci = boot.ci(results, type=c("basic"))
        a$LL[which(a$model == m)] = results$t0
        a$lo[which(a$model == m)] = ci$basic[4]
        a$hi[which(a$model == m)] = ci$basic[5]
      }
    }
    result = rbind(result, a)
  }
  
  return(result)
}

  

# loglike tables. e.g. ll_eu... must be pre-computed
mc_fit_all_subjects = function(subjects) {
  
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
  
  for (s in subjects) {
    
    print(s)
    mc_fit = data.frame()

    for (k in 1:N_iterations) {  
      split = generate_mc_indexes(0.2, number_of_decisions(s)) # split the subject's decisions into test-train
      
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_eu), grid_eu, split, k, "EU"))  
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_du), grid_du, split, k, "DU"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_pw), grid_pw, split, k, "PW"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_pw_du), grid_pw_du, split, k, "PW_DU"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_eu_num), grid_eu_num, split, k, "EU-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_du_num), grid_du_num, split, k, "DU-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_steps), grid_steps, split, k, "Steps"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_cells), grid_cells, split, k, "Cells"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_steps_cells), grid_steps_cells, split, k, "Steps-Cells"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_steps_num), grid_steps_num, split, k, "Steps-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_cells_num), grid_cells_num, split, k, "Cells-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_steps_cells_num), grid_steps_cells_num, split, k, "Steps-Cells-Num"))
      mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_random), grid_random, split, k, "Random"))
      
      if (sampling_model_available) {
        mc_fit = rbind(mc_fit, mc_fit_model(subject_LLs(s, ll_sampling), grid_sampling, split, k, "Sampling"))
      }
    }
    
    a = aggregate(mc_fit[ , allcolumns], by = list(mc_fit$model), FUN = mean) # fitted parameters are avergaes over test folds
    colnames(a) <- c("model", allcolumns)
    a$subject = s
    a$LL = NA
    a$lo = NA
    a$hi = NA
    
    for (m in unique(mc_fit$model)) {
      # this could be Bsum - sum of LL over all test folds
      if (return_sum) {
        results <- boot(data= subset(mc_fit$LL, mc_fit$model == m), statistic=Bsum, R=1000)
        ci = boot.ci(results, type=c("basic"))
        a$LL[which(a$model == m)] = results$t0
        a$lo[which(a$model == m)] = ci$basic[4]
        a$hi[which(a$model == m)] = ci$basic[5]
      } else {
        results <- boot(data= subset(mc_fit$LL, mc_fit$model == m), statistic=Bmean, R=1000)
        ci = boot.ci(results, type=c("basic"))
        a$LL[which(a$model == m)] = results$t0
        a$lo[which(a$model == m)] = ci$basic[4]
        a$hi[which(a$model == m)] = ci$basic[5]
      }
      
    }
    result = rbind(result, a)
  }
  
  return(result)
}



#fit_models = mc_fit_all_worlds()

fit_models = mc_fit_all_subjects (unique(planvalues$subject))
fit_models$modeltype = "Heuristic"
fit_models$modeltype[which(fit_models$model %in% model_names)] = "Planning"
LL_CI_available = TRUE

