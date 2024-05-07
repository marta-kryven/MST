
#-------------------------------------------------------------------------------
#
#    Loglikes for all models - world - takes a long time
#
#-------------------------------------------------------------------------------



ll_eu         = compute_LL_for_each_world(grid_eu, values_eu, planvalues)
ll_steps      = compute_LL_for_each_world(grid_steps, values_steps, planvalues)
ll_cells      = compute_LL_for_each_world(grid_cells, values_cells, planvalues)
ll_random     = compute_LL_for_each_world(data.frame(tau=1), values_random, planvalues)

# two parameter models
ll_pw              = compute_LL_for_each_world(grid_pw, values_pw, planvalues) 
ll_steps_cells     = compute_LL_for_each_world(grid_steps_cells, values_steps_cells, planvalues) 
ll_du              = compute_LL_for_each_world(grid_du, values_du, planvalues) 
ll_steps_num       = compute_LL_for_each_world(grid_steps_num, values_steps_num, planvalues)
ll_cells_num       = compute_LL_for_each_world(grid_cells_num, values_cells_num, planvalues)
ll_eu_num          = compute_LL_for_each_world(grid_eu_num, values_eu_num, planvalues) 

if (sampling_model_available) {
  ll_sampling      = compute_LL_for_each_world(grid_sampling, values_sampling, planvalues) 
}

# 3 parameter models -
ll_steps_cells_num = compute_LL_for_each_world(grid_steps_cells_num, values_steps_cells_num, planvalues) # 
ll_du_num          = compute_LL_for_each_world(grid_du_num, values_du_num, planvalues) 
ll_pw_du           = compute_LL_for_each_world(grid_pw_du, values_pw_du, planvalues)

write.csv( ll_eu, paste(loglikes_directory, 'll_eu_world.csv', sep = ""))
write.csv( ll_cells, paste(loglikes_directory, 'll_cells_world.csv', sep = ""))
write.csv( ll_steps, paste(loglikes_directory, 'll_steps_world.csv', sep = ""))

# 2 parameters
write.csv( ll_pw, paste(loglikes_directory, 'll_pw_world.csv', sep = ""))
write.csv( ll_du, paste(loglikes_directory, 'll_du_world.csv', sep = ""))
write.csv( ll_steps_cells, paste(loglikes_directory, 'll_steps_cells_world.csv', sep = ""))
write.csv( ll_sampling, paste(loglikes_directory, 'll_sampling_world.csv', sep = ""))

write.csv( ll_steps_num, paste(loglikes_directory, 'll_steps_num_world.csv', sep = ""))
write.csv( ll_cells_num, paste(loglikes_directory, 'll_cells_num_world.csv', sep = ""))
write.csv( ll_eu_num, paste(loglikes_directory, 'll_eu_num_world.csv', sep = ""))

# 3 parameters
write.csv( ll_pw_du, paste(loglikes_directory, 'll_pw_du_world.csv', sep = ""))
write.csv( ll_du_num, paste(loglikes_directory, 'll_du_num_world.csv', sep = ""))
write.csv( ll_steps_cells_num, paste(loglikes_directory, 'll_steps_cells_num_world.csv', sep = ""))

write.csv( ll_sampling, paste(loglikes_directory, 'll_sampling_world.csv', sep = ""))


# reading
ll_eu = read.csv( paste(loglikes_directory, 'll_eu_world.csv', sep = ""))
ll_eu_num = read.csv( paste(loglikes_directory, 'll_eu_num_world.csv', sep = ""))
ll_pw = read.csv(paste(loglikes_directory, 'll_pw_world.csv', sep = ""))
ll_du = read.csv( paste(loglikes_directory, 'll_du_world.csv', sep = ""))
ll_du_num = read.csv( paste(loglikes_directory, 'll_du_num_world.csv', sep = ""))
ll_pw_du = read.csv(paste(loglikes_directory, 'll_pw_du_world.csv', sep = ""))

ll_steps = read.csv( paste(loglikes_directory, 'll_steps_world.csv', sep = ""))
ll_steps_cells = read.csv( paste(loglikes_directory, 'll_steps_cells_world.csv', sep = ""))
ll_cells = read.csv( paste(loglikes_directory, 'll_cells_world.csv', sep = ""))
ll_steps_num = read.csv( paste(loglikes_directory, 'll_steps_num_world.csv', sep = ""))
ll_cells_num = read.csv( paste(loglikes_directory, 'll_cells_num_world.csv', sep = ""))
ll_steps_cells_num = read.csv( paste(loglikes_directory, 'll_steps_cells_num_world.csv', sep = ""))
ll_random     = compute_LL_for_each_world(data.frame(tau=1), values_random)

ll_sampling = read.csv( paste(loglikes_directory, 'll_sampling_num_world.csv', sep = ""))
#-------------------------------------------------------------------------------
#
#    Loglikes for all models - subject - takes a long time
#    The resulting tables have dimensions rows: one LL row per one grid row, so, one row for each combination of paramaters
#                                         columns: one column for one decision -- the columns are all named the same, we do not save
#                                         node or world information
#    Because subjects emcounter different number of decisions, there will be NAs in some columns, for subjects with fewer decisions 
#
#-------------------------------------------------------------------------------

# one paremeter models, no need to load
ll_eu               = compute_LL_for_each_subject(grid_eu, values_eu, planvalues)
ll_steps            = compute_LL_for_each_subject(grid_steps, values_steps, planvalues)
ll_cells            = compute_LL_for_each_subject(grid_cells, values_cells, planvalues)
ll_random           = compute_LL_for_each_subject(data.frame(tau=1), values_random, planvalues) # slightly different call, since grid is a dummy variable

# two parameter models
ll_pw               = compute_LL_for_each_subject(grid_pw, values_pw, planvalues) 
ll_steps_cells      = compute_LL_for_each_subject(grid_steps_cells, values_steps_cells, planvalues) 
ll_du              = compute_LL_for_each_subject(grid_du, values_du, planvalues) 
ll_steps_num       = compute_LL_for_each_subject(grid_steps_num, values_steps_num, planvalues)
ll_cells_num       = compute_LL_for_each_subject(grid_cells_num, values_cells_num, planvalues)
ll_eu_num          = compute_LL_for_each_subject(grid_eu_num, values_eu_num, planvalues) 

if (sampling_model_available) {
  ll_sampling      = compute_LL_for_each_subject(grid_sampling, values_sampling, planvalues) 
}

# 3 parameter models -
ll_steps_cells_num = compute_LL_for_each_subject(grid_steps_cells_num, values_steps_cells_num, planvalues) # 
ll_du_num          = compute_LL_for_each_subject(grid_du_num, values_du_num, planvalues) 
ll_pw_du           = compute_LL_for_each_subject(grid_pw_du, values_pw_du, planvalues)   

# 1 parameter
write.csv( ll_eu, paste(loglikes_directory, 'll_eu.csv', sep = ""))
write.csv( ll_cells, paste(loglikes_directory, 'll_cells.csv', sep = ""))
write.csv( ll_steps, paste(loglikes_directory, 'll_steps.csv', sep = ""))

# 2 parameters
write.csv( ll_pw, paste(loglikes_directory, 'll_pw.csv', sep = ""))
write.csv( ll_du, paste(loglikes_directory, 'll_du.csv', sep = ""))
write.csv( ll_steps_cells, paste(loglikes_directory, 'll_steps_cells.csv', sep = ""))

if (sampling_model_available) {
  write.csv( ll_sampling, paste(loglikes_directory, 'll_sampling.csv', sep = ""))
}

write.csv( ll_steps_num, paste(loglikes_directory, 'll_steps_num.csv', sep = ""))
write.csv( ll_cells_num, paste(loglikes_directory, 'll_cells_num.csv', sep = ""))
write.csv( ll_eu_num, paste(loglikes_directory, 'll_eu_num.csv', sep = ""))

# 3 parameters
write.csv( ll_pw_du, paste(loglikes_directory, 'll_pw_du.csv', sep = ""))
write.csv( ll_du_num, paste(loglikes_directory, 'll_du_num.csv', sep = ""))
write.csv( ll_steps_cells_num, paste(loglikes_directory, 'll_steps_cells_num.csv', sep = ""))



##------------------------------------------------------------------------------------------------------
#
#  reading saved loglikes, to save computation time
#
##------------------------------------------------------------------------------------------------------

ll_eu = read.csv( paste(loglikes_directory, 'll_eu.csv', sep = ""))
ll_eu_num = read.csv( paste(loglikes_directory, 'll_eu_num.csv', sep = ""))
ll_pw = read.csv(paste(loglikes_directory, 'll_pw.csv', sep = ""))
ll_du = read.csv( paste(loglikes_directory, 'll_du.csv', sep = ""))
ll_du_num = read.csv( paste(loglikes_directory, 'll_du_num.csv', sep = ""))
ll_pw_du = read.csv(paste(loglikes_directory, 'll_pw_du.csv', sep = ""))

ll_steps = read.csv( paste(loglikes_directory, 'll_steps.csv', sep = ""))
ll_steps_cells = read.csv( paste(loglikes_directory, 'll_steps_cells.csv', sep = ""))
ll_cells = read.csv( paste(loglikes_directory, 'll_cells.csv', sep = ""))
ll_steps_num = read.csv( paste(loglikes_directory, 'll_steps_num.csv', sep = ""))
ll_cells_num = read.csv( paste(loglikes_directory, 'll_cells_num.csv', sep = ""))
ll_steps_cells_num = read.csv( paste(loglikes_directory, 'll_steps_cells_num.csv', sep = ""))
ll_sampling = read.csv( paste(loglikes_directory, 'll_sampling.csv', sep = ""))

# this is an artifact of saving data frames
ll_eu     = ll_eu[,!(names(ll_eu) %in% c("X"))] 
ll_eu_num = ll_eu_num[,!(names(ll_eu_num) %in% c("X"))] 
ll_pw     = ll_pw[,!(names(ll_pw) %in% c("X"))] 
ll_du     = ll_du[,!(names(ll_du) %in% c("X"))] 
ll_du_num = ll_du_num[,!(names(ll_du_num) %in% c("X"))] 
ll_pw_du  = ll_pw_du[,!(names(ll_pw_du) %in% c("X"))] 

ll_steps           = ll_steps[,!(names(ll_steps) %in% c("X"))] 
ll_steps_cells     = ll_steps_cells[,!(names(ll_steps_cells) %in% c("X"))] 
ll_cells           = ll_cells[,!(names(ll_cells) %in% c("X"))] 
ll_steps_num       = ll_steps_num[,!(names(ll_steps_num) %in% c("X"))] 
ll_cells_num       = ll_cells_num[,!(names(ll_cells_num) %in% c("X"))] 
ll_steps_cells_num = ll_steps_cells_num[,!(names(ll_steps_cells_num) %in% c("X"))] 
ll_sampling        = ll_sampling[,!(names(ll_sampling) %in% c("X"))] 




