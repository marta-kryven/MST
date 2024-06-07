# Maze Search Task experiment and code

Contents

map_editor.py -  a grid-editing dialog tool for editing an existing maps, or drawing a new one 

1. subject_data_parser.py -- processing new data collected using web-experiment, needed for collecting new data
   
2. MST_webcode  -- Experiment implementation in HTML/CSS/PHP/JavaScript. To run the experiment this code needs to be downloaded to web-server with an external IP, with Apache and PHP installed. It also needs an empty directory called data to which Apache has writing access.

3. data: _experiment_1 is data from Experiment 1 in the paper, _experiment_4 is data from Experiment 2 in the paper

4. Modelling code in Python/Jupyter -- needs Python 3.8 (but higher likely ok!)
   4.1. tree_builder.py - converting a maze map to a decision tree based on all possible ways to traverse a maze
   4.2 number_perception_models.ipynb -- implements numerosity model, and Q_k_n.pickle is a table listing all mappings for actual-perceived number needed when fitting this model; 
   4.3 MST_models.ipynb, sample_model.py -- model implementations, generating tables of state-action values for each model and each combination of model parameters, which will be used to fit models to human data

6. Model fitting in R - Instrictions


6.1 
Make sure the .csv values needed for model fitting in R are available - they should be in directories      
__experiment_[n]/csv_files/
If there is no such directory, or the directories are empty, then first
use Python codebase to generate this data -- (1) generate planning trees for each maze using tree_builder.py (2) generate state-action values for each model and parameter combination using MST_models.ipynb, and (3) save values tables and trees as .csv using the functionality in MST_models.ipynb

6.2 To load data from the .csv's generated in step 1, open load_data.R and edit the paths to make sure they point to correct directories. Run load_data.R

6.3 Generate loglike tables for each model using compute_loglikes.R. To do this, find the comment
# Loglikes for all models
and run the code block following it -- compute_LL_for_each_subject 
This can take a while.
After these tables are computed, find the code block with lots of calls to write.csv - this saves computed doglike tables to file, in case if you need them again

6.4 Fit models. There are two methods K-fold cross validation, are monte carlo cross validation. It is only necessary to run one of them. We tried two methods to ensure that we are getting consistent results.

6.5 Plots are generated in model_fit_plots.R




