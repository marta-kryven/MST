README

Model fitting instructions


1. 
Make sure the .csv values needed for model fitting in R are available - they should be in directories      
__experiment_[n]/csv_files/
If there is no such directory, or the directories are empty, then first
use Python codebase to generate this data -- (1) generate planning trees for each maze using tree_builder.py (2) generate state-action values for each model and parameter combination using MST_models.ipynb, and (3) save values tables and trees as .csv using the functionality in MST_models.ipynb

2. To load data from the .csv's generated in step 1, open load_data.R and edit the paths to make sure they point to correct directories. Run load_data.R

3. Generate loglike tables for each model using compute_loglikes.R. To do this, find the comment
# Loglikes for all models
and run the code block following it -- compute_LL_for_each_subject 
This can take a while.
After these tables are computed, find the code block with lots of calls to write.csv - this saves computed doglike tables to file, in case if you need them again

4. Fit models. There are two methods K-fold cross validation, are monte carlo cross validation. It is only necessary to run one of them. We tried two methods to ensure that we are getting consistent results.

5. Plots are generated in model_fit_plots.R
