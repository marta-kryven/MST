# Maze Search Task experiment and code

Contents

1. subject_data_parser.py -- processing new data collected using web-experiment, needed for collecting new data
   
2. MST_webcode  -- Experiment implementation in HTML/CSS/PHP/JavaScript. To run the experiment this code needs to be downloaded to web-server with an external IP, with Apache and PHP installed. It also needs an empty directory called data to which Apache has writing access.

3. data: _experiment_1 is data from Experiment 1 in the paper, _experiment_4 is data from Experiment 2 in the paper

4. Modelling code in Python/Jupyter -- needs Python 3.8 (but higher likely ok!)
   4.1. tree_builder.py - converting a maze map to a decision tree based on all possible ways to traverse a maze
   4.2 number_perception_models.ipynb -- implements numerosity model, and Q_k_n.pickle is a table listing all mappings for actual-perceived number needed when fitting this model; 
   4.3 MST_models.ipynb, sample_model.py -- model implementations, generating tables of state-action values for each model and each combination of model parameters, which will be used to fit models to human data

6. Model fitting in R




