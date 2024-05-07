



##------------------------------------------------------------------------------------------------------
#
# postprocessing utils
#
##------------------------------------------------------------------------------------------------------

add_dummy_columns = function(df, cols) {
  for (c in cols) {
    if (!(c %in% names(df) )) {
      df[, c] <- NA
    }
  }
  return(df)
}


removeNodesWithOnlyOneChild = function(subject_planvalues) {
  
  nodes = unique(subject_planvalues$nodeName) # nodeName is unique within experiment
  planvaluesNew = data.frame()
  removed = 0
  
  for (decisionNode in nodes) {
    v = subset(subject_planvalues, subject_planvalues$nodeName == decisionNode) 
    
    if (length(unique(v$availableChildName)) > 1) {
        planvaluesNew = rbind(planvaluesNew, v) 
    } else {
      removed = removed+1
    }
  }
  
  print ( paste('removed rows: ', removed))
  return(planvaluesNew) 
}


# remove extra columns and rows from exported value tables
removeExtras = function(df, validhash) {
  # df = df[,!(names(df) %in% c("prob"))] # 
  # df = df[,!(names(df) %in% c("value"))]
  df = df[df$node %in% validhash, ]
  
  if (experiment == 4) {
    df = subset(df, df$world != "medium_library")
    print("removing all medium_library because it is currently not in the tree")
  }
 return (df)
}


# assume the column name is of the form value_x_y_z, e.g. value_0.266_1_1
extractGridParams = function(df, gridParamNames) {
  
  prefix = 'value_'
  names = colnames(df)[grepl( prefix, colnames(df), fixed = TRUE)]
  names = substr(names, nchar(prefix)+1, max(nchar(names)))
  grid = data.frame()
  
  for (name in names) {
    idx = unlist(str_locate_all(name, "_"))
    idx = unique(idx) # this returns double of all indexes
    
    start = 1
    params = c()
    
    for (i in idx) {
      params = c(params, as.numeric(substr(name, start, i-1)))
      start = i+1
    }
    
    params = c(params, as.numeric(substr(name, start, nchar(name)))) # last one
    grid = rbind(grid, params)
  }
  
  colnames(grid) <- gridParamNames
  return (grid)
}


# this is a workaround for a bug that was fixed in python value generation
# This will never get called, unless old code for value generation is used
slow_search = function(df, grid, grid_row_idx) {
  prefix = 'value_'
  names = colnames(df)[grepl( prefix, colnames(df), fixed = TRUE)]
  names = substr(names, nchar(prefix)+1, max(nchar(names)))

  for (name in names) {
  
    idx = unlist(str_locate_all(name, "_"))
    idx = unique(idx) # this returns double of all indexes
    
    start = 1
    params = c()
    
    for (i in idx) {
      params = c(params, as.numeric(substr(name, start, i-1)))
      start = i+1
    }
    
    params = c(params, as.numeric(substr(name, start, nchar(name)))) # last one
    
    i = 1
    found = TRUE
    for (p in params) {
      if (p != grid[grid_row_idx,i]) {
        found = FALSE
        next
      }
      i = i+1
    }
    
    if (found) {
      print(paste("If possible, regenerate value .csvs so this does not happen -- slow search for", name ))
      return( paste("value_", name, sep =""))
    }
  }
  
  return ("NA")
}



# ------------------------------------------------------------------------------
# input: 
# value_table any of the exported value tables, whcih we load below
# grid_row - index of the row, or parameters of the row
# output - probability from the table of the actions chosen in the decision
#
# This function replaces zero probabilities with a small probability when fitting models, which is likely to happen for coarse softmax grids
# but can happen with large softmax as well when one of the actions in a choice set highly unlikley, 
# such as, going backward around a loop to open a room from a far entrance when another entrance to the same room is close by. 
# Using a coarser softmax grid is not always the best solution 
# however we ensure that the fit is robust to reasonable choice of threshold_prob, and softmax grid

# ------------------------------------------------------------------------------
probability_of_chosen_node_given_model = function(values_table, decision, grid, grid_row_idx, showWarnings = TRUE) {
  v = 0
  
  params = grid[grid_row_idx, ]
  stringName = value_column_name_from_params(params)
  
  # a workaround for a formatting issue that is now fixed in python 
  # where the comuln names are generated as "value_8.0_1_1", but we are searching for value_8_1_1
  if (!stringName %in% colnames(values_table)) {
    if (showWarnings) print(paste( "column not found -- ", stringName ))
    stringName = slow_search(values_table, grid, grid_row_idx)
    if (showWarnings) print(paste( "using column with name :", stringName ))
  }
  
  df = subset(values_table[, c("world", "node", "child", stringName)], values_table$node == decision$nodeName[1])
  
  if (nrow(df)==0 ) {
    print(paste("values_table entry not found for ", decision$world[1], decision$nodeName[1], decision$squaretype[1]))
    return(0)
  }
  
  chosen_child = decision[which(decision$chosen == TRUE), "availableChildName"]
  v = df[which(df$child == chosen_child), stringName]
  
  if (length(v)==0) {
    print ( paste("child not found ", chosen_child, " of ", decision$nodeName[1])) 
    return(0)
  }
  
  if (v==0) {
    v = threshold_prob 
    if (showWarnings) print( paste("zero probability replaced for ", stringName, "node: ", decision$nodeName[1], " child: ", chosen_child, decision$world[1]) )
  }
  
  return (v)
}


# ------------------------------------------------------------------------------
#
#     returns a string value_x_y_z where params=(x,y,z), this is used to access the right column in 
#     model values generated from python
#
# ------------------------------------------------------------------------------

value_column_name_from_params = function(params) {
  stringName = "value"
  for (p in params) {
    stringName  = paste(stringName, p, sep="_")
  }
  return(stringName)
}

# ------------------------------------------------------------------------------
#
#   get probability of selecting a node <lookup_child> from a given <values_table> given  params
#
# ------------------------------------------------------------------------------

probability_of_child_under_model_given_params = function(values_table, lookup_child, params) {
  
  stringName = value_column_name_from_params(params)
  
  if (!stringName %in% colnames(values_table)) {
    print(paste( "column not found -- ", stringName ))
    return (NULL)
  }
  
  return(values_table[ which(values_table$child == lookup_child), stringName])
}



# select all loglikes of subject s from LL table loglikes
subject_LLs = function(s, loglikes) {
  tmp = subset(loglikes, loglikes$subject == s)
  tmp <- tmp[ , -which(names(tmp) %in% c("world", "subject", "X", "prior"))] # remove all possible extra columns
  # tmp <- tmp[ , grepl("log_probs", names(tmp), fixed = TRUE)]  # this will remove all parameters as well, so we will have no way of making sure that the grid and LL match
  tmp <- tmp[colSums(!is.na(tmp)) > 0]
  return (tmp)
}


# this is a version that selects subject LLs from a table that has dimensions gridSize*world X loglikes
subject_LLs_2 = function(s, loglikes) {
  tmp = subset(loglikes, loglikes$subject == s)
  tmp <- tmp[ , -which(names(tmp) %in% c("subject", "X", "prior"))] # remove all possible extra columns
  
  LL = data.frame()
  # there are multiple worlds
  for (w in worlds) {
    x = subset(tmp, tmp$world == w)
    x <- x[colSums(!is.na(x)) > 0]
    x <- x[ , grepl("log_probs", names(x), fixed = TRUE)]
    LL = cbind(LL,x)
  }
  
  return (LL)
}

world_LLs_2 = function(w, loglikes) {
  tmp = subset(loglikes, loglikes$world == w)
  tmp <- tmp[ , -which(names(tmp) %in% c("world", "X", "prior"))] # remove all possible extra columns
  
  LL = data.frame()
  # there are multiple worlds
  for (s in subjects) {
    x = subset(tmp, tmp$subject == s)
    x <- x[colSums(!is.na(x)) > 0]
    x <- x[ , grepl("log_probs", names(x), fixed = TRUE)]
    LL = cbind(LL,x)
  }
  
  return (LL)
}


# select all loglikes of world w from LL table loglikes
# make sure this works
world_LLs = function(w, loglikes) {
  tmp = subset(loglikes, loglikes$world == w)
  tmp <- tmp[ , -which(names(tmp) %in% c("world", "subject", "X", "prior"))] # remove all possible extra columns
  #tmp <- tmp[ , grepl("log_probs", names(tmp), fixed = TRUE)]
  tmp <- tmp[colSums(!is.na(tmp)) > 0]
  return (tmp)
}

# number of decisions of a subject
number_of_decisions = function(s) {
  grid = subject_LLs(s, ll_eu)
  return(length(grep("log_probs", colnames(grid))))
}

# add prior on tau to parameter grid
add_prior_on_tau = function(grid) {
  grid$prior = 1/grid$tau
  s = sum(1/unique(grid$prior))
  grid$prior = log(grid$prior/s)
  return(grid)
}

#-------------------------------------------------------------------------------
#
#    Loglike computation
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#
#    Loglike computation
#
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#
#    Loglike computation
#
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#
#    computing log-likes for a subject
#
#-------------------------------------------------------------------------------

# parameters:
# subject_idx -- subject number, this is only used to generate logs
# subject -- ID for who loglikes are computed
# grid    -- parameter grid, e.g. grid_eu_num - it does not matter how many free parameters there are
# values_table -- probabilitias of choosing each node, for each combination of parameters
# returns a table with a row for each combination of parameters, and as many columns as the decisions made by this subject
subject_logLikes = function(subject_idx, s, grid, values_table, subject_planvalues, showWarnings = FALSE) {
  
  decisions = subset(subject_planvalues, subject_planvalues$subject == s ) 
  nodes = unique(decisions$nodeName)
  print( paste( subject_idx, "subject ", s, " decisions: ", length(nodes) ))
  
  out_grid = grid
  
  for (n in nodes) {
    log_probs = c()
    decision = subset(decisions, decisions$nodeName == n)
    
    # this is unnecessary, probability_of_chosen_node_given_model takes care to get the right child
    #if (decision$chosen[1] != TRUE) {
    #  print( paste("debug: reordering", n) )
    #  decision = decision[order(-decision$chosen), ]
    #}
    
    skip = FALSE
    for (i in 1:dim(out_grid)[1]) {	
      prob = probability_of_chosen_node_given_model(values_table, decision, grid, i, showWarnings)
      
      if (prob == 0) {
        # something wrong with value table generation, needs to be fixed in python 
        if (showWarnings) print( paste("skipping node", n, "is a map missing? ", decision$world[1] ))
        if (i > 1) print("ERR: zero probability returned, but NOT because of a missing map or node -- debug")
        skip = TRUE
        break
        
      } else {
        log_probs = c(log_probs, log(prob))
      }
    }
    if (skip) next
    out_grid = cbind(out_grid, log_probs) # adding a column
  }
  
  return(out_grid)
}

#-------------------------------------------------------------------------------
#
#    computing log-likes for a maze
#
#-------------------------------------------------------------------------------


world_logLikes = function(world_idx, w, grid, values_table, subject_planvalues, showWarnings = FALSE) {
  
  decisions = subset(subject_planvalues, subject_planvalues$world == w ) 
  decisions$hash = paste(decisions$nodeName, decisions$subject)
  subject_decision = unique(decisions$hash)
  print( paste( world_idx, "world ", w, " decisions: ", length(subject_decision) ))
  
  out_grid = grid
  
  for (h in subject_decision) {
    log_probs = c()
    decision = subset(decisions, decisions$hash == h)

    skip = FALSE
    for (i in 1:dim(out_grid)[1]) {	
      prob = probability_of_chosen_node_given_model(values_table, decision, grid, i, showWarnings)
      
      if (prob == 0) {
        # something wrong with value table generation, needs to be fixed in python 
        if (showWarnings) print( paste("skipping node", n, "is a map missing? ", decision$world[1] ))
        if (i > 1) print("ERR: zero probability returned, but NOT because of a missing map or node -- debug")
        skip = TRUE
        break
        
      } else {
        log_probs = c(log_probs, log(prob))
      }
    }
    if (skip) next
    out_grid = cbind(out_grid, log_probs) # adding a column
  }
  
  return(out_grid)
}

world_and_subject_logLikes = function(world_idx, w, s, grid, values, subject_planvalues)  {
  
}

#------------------------------------------------------------------------------------------------
#
# computing ll_model - tables of LLs later used for model fitting
#
#
#------------------------------------------------------------------------------------------------


compute_LL_for_each_subject = function(grid, values, subject_planvalues) {
  
  loglikes = data.frame()
  subject_idx = 1
  subjects = unique(subject_planvalues$subject)
  
  for (s in subjects) {
    if (dim(grid)[1] == 1) {
      tmp = subject_logLikes(subject_idx, s, grid, values, subject_planvalues) 
    } else {
      tmp = subject_logLikes(subject_idx, s, grid[,!(names(grid) %in% c("prior"))], values, subject_planvalues) 
    }
    
    tmp$subject = s
    
    if (dim(loglikes)[1] == 0) {
      loglikes = data.frame(tmp)
    } else {
      loglikes = dplyr::bind_rows(loglikes, data.frame(tmp))
    }
    
    subject_idx = subject_idx+1
  }
  
  return(loglikes)
}


compute_LL_for_each_world = function(grid, values, subject_planvalues) {
  
  loglikes = data.frame()
  world_idx = 1
  worlds = unique(subject_planvalues$world)
  
  for (w in worlds) {
    if (dim(grid)[1] == 1) {
      tmp = world_logLikes(world_idx, w, grid, values, subject_planvalues) 
    } else {
      tmp = world_logLikes(world_idx, w, grid[,!(names(grid) %in% c("prior"))], values, subject_planvalues) 
    }
    
    tmp$world = w
    
    if (dim(loglikes)[1] == 0) {
      loglikes = data.frame(tmp)
    } else {
      loglikes = dplyr::bind_rows(loglikes, data.frame(tmp))
    }
    
    world_idx = world_idx+1
  }
  
  return(loglikes)
}


# !! untested !!
# compute a table of LL where each subject is represented by length(worlds) rows, one row per world-subject combination
# this could be useful if we want to compute ll table once per experiment, not separately for worlds and subjects like
# it currently is

compute_LL_for_world_and_subject = function(grid, values, subject_planvalues) {
  
  loglikes = data.frame()
  world_idx = 1
  worlds = unique(subject_planvalues$world)
  
  for (w in worlds) {
    for (s in subjects) {
      tmp = world_and_subject_logLikes(world_idx, w, s, grid, values, subject_planvalues) 
      tmp$world = w
      tmp$subject = s
    
      if (dim(loglikes)[1] == 0) {
        loglikes = data.frame(tmp)
      } else {
        loglikes = dplyr::bind_rows(loglikes, data.frame(tmp))
      }
    }
    world_idx = world_idx+1
  }
  
  return(loglikes)
}





# R does not have a built in mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#------------------------------------------------------------------------------------------------
#
# bootstrapping
#
#------------------------------------------------------------------------------------------------



Bsum <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(sum(d))
}



Bmean <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
}


Bcorr <- function(data, i) {  
  cor(data[i, "x"], data[i, "y"], method='pearson') 
}




#-------------------------------------------------------------------------------------------------------------
#
# most frequent parameter fits to subjects for each model
#
# THIS CAN RETURN SKEWED PARAMETERS 
# only works if k_fold_fit_all_subjects was run with subject_parameters_as_mode=TRUE
#-------------------------------------------------------------------------------------------------------------

get_mode_parameter_fits = function(fit_models) {
  a = data.frame(models = unique(fit_models$model) ) 
  
  if (sampling_model_available) {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num), colnames(grid_sampling)))
  } else {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num)))
  }
  allcolumns = allcolumns[!(allcolumns %in% c("prior"))]
  for (c in allcolumns) {
    if (!(c %in% names(df) )) {
      a[, c] <- NA
    }
  }
  
  for (m in unique(fit_models$model)) {
    for (c in allcolumns) {
      a[c, which(a$model ==m)] = getmode(subset(fit_models[,c],fit_models$model ==m) )
    }
  }
  
  return(a)
}

#-------------------------------------------------------------------------------------------------------------
#
# mean parameter fits to subjects for each model, mapped to grid
#
#-------------------------------------------------------------------------------------------------------------

get_mean_parameter_fits = function(fit_models) {
  if (sampling_model_available) {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num), colnames(grid_sampling)))
  } else {
    allcolumns = unique(c(colnames(grid_pw_du), colnames(grid_steps_cells_num)))
  }
  
  allcolumns = allcolumns[!(allcolumns %in% c("prior"))]
  a = aggregate(fit_models[ , allcolumns], by = list(fit_models$model), FUN = mean)
  colnames(a) <- c("model", allcolumns)
  return(a)
}


#-------------------------------------------------------------------------------------------------------------
#
# p - model parameter to be matched to the closest parameter in the grid
# parameter_name - column name in the grid corresponding to this parameter
# grid - whichever model grid to be used to find the parameter
#
# this is used when p is some mean parameter value fitted to humans
#-------------------------------------------------------------------------------------------------------------


align_parameter_to_grid = function(p, parameter_name, grid) {
  
  values = unique(grid[ ,parameter_name])
  diff = Inf
  p_grid = p
  
  for (v in values) {
    if ( abs(p_grid-v) < diff) {
      p_grid = v
      diff = p-v
    }
  }
  return (p_grid)
}


# ------------------------------------------------------------------------------------------------
#   fitted_parameters -- this is a table of all model's parameters
#
#   generates a data frame where each probability in policy is matched to a probability 
#   predicted by model under fitted_parameters 
#
# ------------------------------------------------------------------------------------------------


match_model_and_human_probabilities = function(model_name, grid, values, fitted_parameters, human_policy ){
  model_params = colnames(grid) 
  model_params = model_params[!(model_params %in% c("prior"))]
  params = c()
  
  for (param in model_params) {
    grid_p = align_parameter_to_grid( fitted_parameters[which(a$model == model_name), param], param, grid)
    params = c(params, grid_p)
  }
  
  df = data.frame(human_policy[ ,c("child", "prob") ], modelProb = NA)
  for (i in 1:dim(df)[1]) {
    df$modelProb[i] = probability_of_child_under_model_given_params(values, df$child[i], params)
  }
  return (df)
}


#-------------------------------------------------------------------------------------------------------------
#
# compute human policy - how likely are people to make each decision
# can be used with simulated data
# this may be affected by min_subjects, rerun the analysis of policy with several values of min_subjects 
#
#-------------------------------------------------------------------------------------------------------------


compute_policy = function(planvalues) {
  min_subjects = max(round(length(subjects)*minimal_subjects_per_state_for_policy), 10)
  
  policy = aggregate(planvalues$chosen, by = list( planvalues$world, planvalues$nodeName, planvalues$availableChildName), 
                     FUN = function(x) c(mean = mean(x), N = length(x) ))
  colnames(policy) <- c( "world", "node", "child", "x");
  policy$N = policy$x[,2];
  policy$prob = policy$x[,1];
  policy = policy[ , c("world", "node", "child", "prob", "N")]
  policy = policy[order(policy$world, policy$node),]
  
  print(paste("total decisions visited by subjects:", dim(policy)[1]))
  policy = subset(policy, policy$N > min_subjects)
  print(paste("used to compute policy:", dim(policy)[1]))
  return(policy)
}


