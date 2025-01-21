import os
import shutil
import numpy as np
import pickle

# from the MST_models notebook
import pprint
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import csv
import random

import pattern_editor
import utils as ut

import enum

from partition_prompt import PartitionPrompt, regenerate_pattern #, segment_map

import tree_builder
from tree_builder import maze2tree

from maze_info import maze_info_dict


# 5 is for modular planning, 6 is ordinary optimal planning
EXPERIMENT = 6

# Mazes desired
maze_names = [
    # "5_units",
    # "env17_a1"
    # "5_units_vis1",
    # "env17_b",
    # "6_units_b1",
    # "env17_c",
    # "6_units_flip",
    # "four_units_flip",
    #"6_units",
    #"four_units",
    # "6_units_vis1",
    # "four_units_vis1",
    # "big_alcoves",
    # "test11",
    # "big_alcoves_vis1",
    # "test21",
    # "binary_7x7_rotated",
    # "test",
    "binary_7x7",
    # "tiny_rooms",
    #"corridors_to_three_tiny_rooms_with_alcoves",
]

# TODO: move this somewhere it can be used commonly here and in maze2tree
class Cell(enum.Enum):
    WALL = 3 
    UNOBSERVED_EMPTY = 0
    OBSERVED_EMPTY = 6
    HIDDEN_EXIT = 2
    START = 5

# Construct each tree
for input_id in maze_names:

    # Recover necessary maze info
    subdir = maze_info_dict[input_id]["subdir"]
    fragment = maze_info_dict[input_id]["fragment"]
    copies = maze_info_dict[input_id]["copies"]
    start_row = maze_info_dict[input_id]["start_row"]
    start_col = maze_info_dict[input_id]["start_col"]
    exit_row = maze_info_dict[input_id]["exit_row"]
    exit_col = maze_info_dict[input_id]["exit_col"]

    input_map = pattern_editor.read_pattern(f'/home/cwyeth/Desktop/compositional_map_synthesis/test_patterns/{subdir}/{input_id}.txt')
    input_dims = (len(input_map), len(input_map[0]))
    str_map = ut.array_to_string(input_map)

    pp = PartitionPrompt()

    def get_errors_and_omissions(input_map, output):
        errors_and_omissions = (input_dims[0]*input_dims[1]) - np.sum(input_map == output)
        omissions = np.sum(output == 0.5)
        errors = errors_and_omissions - omissions
        return errors, omissions

    def report(output):
        print(output)
        ut.plot_pattern(output, "output")
        errors, omissions = get_errors_and_omissions(input_map, output)
        score = ut.structural_mdl_score(fragment, copies, errors, omissions)
        print(f"Score: {score}")

    print(str_map)
    ut.plot_pattern(input_map, "input")


    output = regenerate_pattern(fragment, copies, input_dims)
    print("Intended response:")
    report(output)

    # Now we'll explore the true maze guided by the output

    def convert_to_mst_format(map):
        """
        Converts a map in modular fragment format to mst expected format.
        Does not handle setting a start position, exit, or making any observations.
        """
        def convert(cell):
            if cell == 0:
                return Cell.UNOBSERVED_EMPTY.value
            elif cell == 1:
                return Cell.WALL.value
            elif cell == 2:
                return Cell.OBSERVED_EMPTY.value
            else:
                raise Exception(
                    "Case not implement, perhaps an omitted cell",
                )
        return [[convert(cell) for cell in row] for row in map]

    def set_start(map, i, j):
        if map[i][j] in [Cell.WALL.value, Cell.HIDDEN_EXIT.value]:
            raise Exception(
                "Starting inside a wall or exit is not intended.",
            )
        else:
            map[i][j] = Cell.START.value

    def set_exit(map, i, j):
        if map[i][j] in [Cell.WALL.value, Cell.START.value]:
            raise Exception(
                "Exiting inside a wall or the start location is not intended.",
            )
        else:
            map[i][j] = Cell.HIDDEN_EXIT.value   

    gt_map = convert_to_mst_format(input_map)

    set_start(gt_map,start_row,start_col)
    
    # Currently not setting exit so that exploration is full
    #set_exit(gt_map,exit_row,exit_col)

    print(ut.array_to_string(gt_map))

    segmentation = pp.segment_map(fragment, copies)
    #print(segmentation)
    print(f"Number of segmented cells: {len(segmentation.keys())}")
    fragment = convert_to_mst_format(fragment)

    print(segmentation.keys())
    if EXPERIMENT == 5:
        tree = maze2tree(gt_map, fragment, segmentation)
    else:
        tree = maze2tree(gt_map)
    print(f"Tree size: {len(tree.keys())}")
    # print(tree)

    # Dump as an individual tree
    with open(f"__experiment_{EXPERIMENT}/trees/{input_id}.pickle", 'wb') as handle:
        pickle.dump(tree, handle, protocol=pickle.HIGHEST_PROTOCOL)

    # This doubles as converting maps to the MST format which we'll want to switch to anyway:
    # includes start and exit as well. 
    with open(f"__experiment_{EXPERIMENT}/mazes/{input_id}.txt", 'w') as f:
        for row in gt_map:
            f.write("".join(map(str, row)) + "\n")

# Create a single unified tree (only needs to be done for the final maze added to the experiment)
def pickle_unified_tree():
    print('pickled from unified tree')

    subdir, dirs, maze_files = next(os.walk(f'__experiment_{EXPERIMENT}/mazes'))

    tree = {}

    for file_name in maze_files:
        print(file_name)
        maze_name = file_name.split('.')[0]

        try:
            with open(f'__experiment_{EXPERIMENT}/trees/{maze_name}.pickle', 'rb') as handle:
                tree_ = pickle.load(handle)
                print(maze_name, len(tree_))
        except FileNotFoundError:
            continue

        tree_['root'] = 0
        tree[maze_name] = tree_
        print(maze_name)

    with open(f'__experiment_{EXPERIMENT}/pickled_data/tree.pickle', 'wb') as handle:
        pickle.dump(tree, handle, protocol=pickle.HIGHEST_PROTOCOL)

pickle_unified_tree()


##########################################################################################
# PHASE 2: Construct dict of optimal plans 
##########################################################################################

p = pprint.PrettyPrinter(compact=False)

MODEL_TYPE = 'recursive' # we tested various value function, this is the final form used in the paper

# to be used as a function decorator for storing the results of expensive function calls 
# and returning the cached result when the same inputs occur again    
def memoize(function):    
    memo = {}
    def wrapper(*args):
        if args not in memo:
            memo[args] = function(*args)
        return memo[args]
    return wrapper


# generates zero probabilities if tau=0.1: 
# TAUS = [0.1000, 0.1413, 0.1996, 0.2820, 0.3984, 0.4735, 0.5628, 0.6690, 0.7952, 0.9451, 1.3353, 1.8864, 3.1678, 4.4754, 6.3227, 8.9326]

TAUS   = np.geomspace(0.2, 8, 12)
GAMMAS = [0.99, 0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.6, 0.5, 0.4, 0.2, 0] # np.linspace(0, 1, 14)
BETAS  = np.geomspace(0.1, 2, 10)
KAPPAS = np.linspace(0, 1, 11)

# parameters for the sampling MCTS model - grid coverage is similar to geomspace, but integer
BUDGET = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16, 20, 30, 40, 70, 100, 200, 500, 1000]
EXPLORE_C = [ 1, 2, 3, 4, 5, 7, 9, 11, 14, 18, 26, 35] 
            # the optimal exploration c increases with budget, for budget=1000 optimal c = 26

# bit threshold for information-theorethic model of (cheyette & piantodosi 2022)
Bs = [0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.6, 4.2, 5, 6, 10]

# comment models from this list to re-generate only modles that changed -- 
MODEL_NAMES = [
    'Expected_Utility', 
    'Discounted_Utility', 
    #'  Probability_Weighted_Utility',
    #    'PW_DU',
    'Heuristic_Steps',
    'Heuristic_Cells',
    'Heuristic_Steps_Cells',
    #    'Random', 
        #'Sampling',# # this model is generated from sample_model.py, it is very slow!
    #    'EU_Numerosity',
    #    'DU_Numerosity',
    #    'Steps_Numerosity',
    #    'Cells_Numerosity',
    #    'Steps_Cells_Numerosity',
] 

MODEL_NAMES_ABREV = {'Expected_Utility': 'EU', 
                     'Discounted_Utility': 'DU',
                     'Probability_Weighted_Utility': 'PW',
                     'PW_DU': 'PW_DU',
                     'Heuristic_Steps': 'HS',
                     'Heuristic_Cells': 'HC',
                     'Heuristic_Steps_Cells': 'HSC',
                     'Random': 'Rand',
                     'Sampling': 'SM',
                     'EU_Numerosity' : 'EU_Num', 
                     'DU_Numerosity' : 'DU_Num',
                     'Steps_Numerosity': 'HS_Num',
                     'Cells_Numerosity': 'HC_Num',
                     'Steps_Cells_Numerosity': 'HSC_Num'
                    }

# which parameter ranges should be fitted to each model
MODEL2PARAMS = {
                'Expected_Utility': [(round(tau,3), 1, 1) for tau in TAUS],
                'Discounted_Utility': [(round(tau,3), round(gamma,3), 1) for tau in TAUS for gamma in GAMMAS],
                'Probability_Weighted_Utility': [(round(tau,3), 1, round(gamma,3)) for tau in TAUS for gamma in BETAS],
                'PW_DU': [(round(tau,3), round(gamma,3), round(beta,3)) for tau in TAUS for gamma in GAMMAS for beta in BETAS],
                'Heuristic_Steps': [(round(tau,3), 1) for tau in TAUS],
                'Heuristic_Cells': [(round(tau,3), 0) for tau in TAUS],
                'Heuristic_Steps_Cells': [(round(tau,3), round(kappa,3)) for tau in TAUS for kappa in KAPPAS],
                'Random': [(1,)],
                'Sampling': [(bdgt, c) for bdgt in BUDGET for c in EXPLORE_C],
                'EU_Numerosity': [(round(tau,3), 1, 1, b) for tau in TAUS for b in Bs],
                'DU_Numerosity': [(round(tau,3), round(gamma,3), 1, b) for tau in TAUS for gamma in GAMMAS for b in Bs],
                'Steps_Numerosity': [(round(tau,3), 1, b) for tau in TAUS for b in Bs],
                'Cells_Numerosity': [(round(tau,3), 0, b) for tau in TAUS for b in Bs],
                'Steps_Cells_Numerosity': [(round(tau,3), round(kappa,3), b) for tau in TAUS for kappa in KAPPAS for b in Bs]
               }

# tree is generated from maze maps using tree_builder.py
# needs to be only generated once per experiment

d = os.getcwd() + f'/__experiment_{EXPERIMENT}/pickled_data/tree.pickle'

# this is <class 'dict'>, with entries for each maze, TREE[maze_name]
with open(d, 'rb') as handle:
    TREE = pickle.load(handle)


# which mazes are in the tree 
print("Mazes compiled into the tree:")
for maze_name in TREE:
    print(maze_name) 
    
# this is a table maps each bit threshold B and number n to an information-theoretic perceived number k
# TODO: I think this may not be a large enough table for some mazes
with open(os.getcwd() + '/Q_k_n.pickle', 'rb') as handle:
    numerosity_table = pickle.load(handle)  

def softmax(values, tau):
    """ small values are better
    large tau converges to random agent """

    numer = [np.exp(-v * (1/tau)) for v in values]
    denom = sum(numer)
    return [n/denom for n in numer]


def weight(p, beta):
    """ probability weighting function: convert probability p to weight """
    # return p**beta / (p**beta + (1-p)**beta) ** (1/beta)
    if p == 0:
        return 0 # just to avoid runtime warning np.exp(np.log(0)) still returns 0
    return np.exp( -1 * (-np.log(p))**beta )



@memoize
def node_value_plan(maze_name, nid, gamma=1, beta=1, bit_threshold = -1):
    """ compute value BEFORE softmax """
    value, p_exit, steps_to_exit, min_child_value = 0, 0, 0, 0
    # print(f"evaluating nid = {nid}")

    tree = TREE[maze_name]
    steps_to_node = tree[nid]['steps_from_par']
        
    if MODEL_TYPE == 'original_rldm':
        steps_to_node = tree[nid]['steps_from_root']
    
    # if using numerosity transformations, transform all variables that reflect numbers of cells
    if bit_threshold!= -1:
        steps_to_node = numerosity_table[bit_threshold][steps_to_node]
        
    # get steps_to_exit if nid is not root
    if tree[nid]["pid"] not in {"NA", None}:
        n_black_cells_total = tree[tree[nid]["pid"]]["remains"]
        cell_distances = tree[nid]["celldistances"]
        n_cells_open = len(cell_distances)
        # print(f"n_black_cells_total = parent's remains = {n_black_cells_total}")
        # print(f"We can newly see {n_cells_open} of them.")
        if isinstance(list(cell_distances)[0], tuple): #  tree builer
            r,c = tree[nid]["pos"]
            cell_distances = [abs(r-rr) + abs(c-cc) for rr,cc in cell_distances]
            
        
        # if using numerosity transformations, transform all variables that reflect numbers of cells
        if bit_threshold!= -1:
            n_cells_open = numerosity_table[bit_threshold][n_cells_open]
            n_black_cells_total = numerosity_table[bit_threshold][n_black_cells_total]
            cell_distances = [ numerosity_table[bit_threshold][c] for c in cell_distances]
        
        # expected number of steps from making observation to exit, if it is seen
        steps_to_exit = np.mean(cell_distances)
        # print(f"Expected steps to exit: {steps_to_exit}")
        
        # probability that the exit is in the current room is the ratio of cells in the current room
        # to total cells that remain unseen in the maze
        p_exit = n_cells_open/n_black_cells_total
        # print(f"probability we can see the exit: {p_exit}")
        

    # get min_child_value 
    if tree[nid].get("children", []):
        min_child_value = float("inf") # if nid is a leaf

        for cid in tree[nid]["children"]:
            child_value = node_value_plan(maze_name, cid, gamma, beta)
            if child_value < min_child_value:
                min_child_value = child_value

    # how the value function was originally implemented, including in RLDM and all previous papers
    if MODEL_TYPE == 'original_rldm':
        value = weight(p_exit, beta) * (steps_to_node + steps_to_exit) \
                + gamma * weight(1-p_exit, beta) * min_child_value

    # this is the improved formulation of the value function that works with sub-trees recursively
    # updated in Spring 2022
    # optimal expected utility is not affected, but it affects models with gamma < 1 and beta != 1 
    elif MODEL_TYPE == 'recursive': 
        value = steps_to_node + weight(p_exit, beta) * steps_to_exit \
                + gamma * weight(1-p_exit, beta) * min_child_value


    return value


# @memoize
def node_value_heuristic(maze_name, nid, kappa=1, bit_threshold = -1):
    ''' if 0<kappa<1 this is steps-cells heuristic, '''
    ''' if kappa=1 this is a steps heuristic '''
    ''' if kappa=0 this is a cells heuristic '''

    tree = TREE[maze_name]
    steps = tree[nid]["steps_from_par"]
    cells = len(tree[nid]["celldistances"])
    
    if bit_threshold!= -1:
        steps = numerosity_table[bit_threshold][steps]
        cells = numerosity_table[bit_threshold][cells]

    return steps*kappa - cells*(1-kappa)


def node_value_random(maze_name, nid):

    tree = TREE[maze_name]

    pid = tree[nid]["pid"]
    return 1/len(tree[pid]['children'])


# returns all values for given maze
def node_values(maze_name, parameters, value_func):
    # print(f"Computing values for {maze_name}")
    tree = TREE[maze_name]
    values_summary = {} # {nid: {(param): {cid: value, cid: value, ...}}}

    for nid in tree:
        if nid == 'root':
            continue
        
        children = tree[nid]['children']

        # ignore nid if it's not a decision node
        if len(children) <= 1:
            continue
        values_summary[nid] = {}
        # print(f"nid = {nid} has children, so we compute a value summary!")
        for tau, *params in parameters: # what is *???

            raw_values = [ value_func(maze_name, cid, *params) for cid in children ]
            # print(f"raw values are {raw_values}")
            values = softmax(raw_values, tau)
            # print(f"After softmax we obtain {values}")

            values_summary[nid][(tau, *params)] = {cid: val for cid,val in zip(children, values)}

    return values_summary


# generate values for all models and mazes
def pickle_node_values():

    if not os.path.exists(f'__experiment_{EXPERIMENT}/node_values_{MODEL_TYPE}'):
        os.makedirs(f'__experiment_{EXPERIMENT}/node_values_{MODEL_TYPE}')

    for model_name in MODEL_NAMES:

        parameters = MODEL2PARAMS[model_name]
        node_value_func = MODEL2RAWNODEVAL[model_name]

        print(f'{model_name} ....')

        if not os.path.exists(f'__experiment_{EXPERIMENT}/node_values_{MODEL_TYPE}/{model_name}'):
            os.makedirs(f'__experiment_{EXPERIMENT}/node_values_{MODEL_TYPE}/{model_name}')

        for params in parameters:

            values_summary = {} # {maze_name: {nid: {cid: val, ...}}

            for maze_name in TREE:

                if 'practice' in maze_name:
                    continue
                values_summary[maze_name] = node_values(maze_name, [params], node_value_func)

            # this saves a separate .pickle file for each combination of parameters, 
            # where each file contains all mazes and all nodes
            with open(f'__experiment_{EXPERIMENT}/node_values_{MODEL_TYPE}/{model_name}/node_values_{tuple(round(p,3) for p in params)}.pickle', 'wb') as handle:
                pickle.dump(values_summary, handle, protocol=pickle.HIGHEST_PROTOCOL)

        print(model_name, 'done!')

# which function should be called for each model
# if you see error 'name 'node_value_plan' is not defined' - look for a cell below that defines it
MODEL2RAWNODEVAL = {
                'Expected_Utility': node_value_plan,
                'Discounted_Utility': node_value_plan,
                'Probability_Weighted_Utility': node_value_plan,
                'PW_DU': node_value_plan,
                'Heuristic_Steps': node_value_heuristic,
                'Heuristic_Cells': node_value_heuristic,
                'Heuristic_Steps_Cells': node_value_heuristic,
                'Random': node_value_random,
                'EU_Numerosity': node_value_plan,
                'DU_Numerosity': node_value_plan,
                'Steps_Numerosity': node_value_heuristic,
                'Cells_Numerosity': node_value_heuristic,
                'Steps_Cells_Numerosity': node_value_heuristic
                }

print(f'generating values into pickle Experiment: {EXPERIMENT}, Model Type: {MODEL_TYPE}')
pickle_node_values()


##########################################################################################
# VISUALIZATION UTILITIES
##########################################################################################

def read_maze(maze_name, exp):

    with open(f'__experiment_{exp}/mazes/{maze_name}.txt', 'r') as f:
        # The below was a bug, appears to be outdated now - CW
        # ncols = int(f.readline())
        # nrows = int(f.readline())
        maze = f.readlines()
    
    # preprocessing of maze from text
    maze = tuple([tuple([int(cell) for cell in row.split('\n')[0]]) for row in maze])

    exit_pos = None

    for r,row in enumerate(maze):
        for c,cell in enumerate(row):
            if cell == 2:
                exit_pos = (r,c)
    
    return maze, exit_pos

def simulate_model_path(maze_name, experiment, model, params, deterministic=False) :

    with open(f'__experiment_{experiment}/node_values_recursive/{model}/node_values_{params}.pickle', 'rb') as handle:
       values_to_simulate = pickle.load(handle)

    maze_map, exit_pos = read_maze(maze_name, experiment)
    nid, path = 0, [0]

    while True:

        nid = path[-1] # set nid to the last element of path

        observations = TREE[maze_name][nid]['celldistances'] # this is a set, for example {(2, 4), (2, 2)}, or an empty set, if nid is root
        children = list(TREE[maze_name][nid]['children'])

        if exit_pos in observations or len(children) == 0:
            return path # exit reached, returning sequence of numeric node ids, for example [0, 1, 3]
            break

        elif len(children) == 1:
            _nid = next(iter(children)) # degenerate node, select immediate successor
            path.append(_nid)

        else:
            probabilities = [values_to_simulate[maze_name][nid][params][cid] for cid in children]
            #print(probabilities)
            if deterministic:
                _nid = children[np.argmax(probabilities)]
            else:
                _nid = random.choices(children, probabilities, k=1)[0]
            path.append(_nid)

def visualize_maze(maze, exit_pos=None, ax=None):

    nrows, ncols = len(maze), len(maze[0])
    #print(f"nrows {nrows}, ncols {ncols}")

    if ax is None:
        _, ax = plt.subplots(1)

    # custom color map
    cmap = colors.ListedColormap(['black', # black tiles
                                  'white', 
                                  'black', # 'red' if you want to see exit, 'black' if you want to hide it
                                  '#8B2323', # brown4, wall
                                  'white', 
                                  '#7FFFD4', #'#a1c38c', # entrance
                                  'white', # white cells
                                  'white'])
    
    boundaries = [0, 1, 2, 3, 4, 5, 6, 7]
    norm = colors.BoundaryNorm(boundaries, cmap.N, clip=False)

    # assign exit pos if we give one
    maze = [[cell for cell in row] for row in maze]
    if exit_pos:
        r,c = exit_pos
        maze[r][c] = 2

    # XXX fix orientation
    maze = [[int(cell) for cell in list(row)[:ncols]] for row in maze][::-1]

    # draw maze
    ax.pcolormesh(maze, edgecolors='lightgrey', linewidth=1, cmap=cmap, norm=norm)
    ax.set_aspect('equal')

    ax.axis('off')

import sample_model

def best_paths(maze_name, param, node_value_func, exit_pos=None):
    """ return list of all best paths with corresponding value """

    tree = TREE[maze_name]
    
    nid, nodes_paths = 0, []
    values_summary = node_values(maze_name, [param], node_value_func)

    agenda = [([0], 0)]

    while agenda:
        path, path_value = agenda.pop(0)
        nid = path[-1]

        observations = tree[nid]['celldistances']
        children = tree[nid]['children']

        if exit_pos in observations or len(children) == 0:
            nodes_paths.append((path, path_value))

        elif len(children) == 1:
            _nid = next(iter(children))
            agenda.append((path + [_nid], path_value))

        else:
            max_value = max(values_summary[nid][param].values())
            path_value += np.log(max_value)
            best_children = [cid for cid,val in values_summary[nid][param].items() if val == max_value]

            for cid in best_children:
                agenda.append((path + [cid], path_value))

    return nodes_paths


def rotate_maze(maze):
    """ modify code so for any rotation/reflection """

    # flip up-down
    # maze = maze[::-1]

    # flip wrt diagonal
    # maze = maze[::-1]
    # maze = list(zip(*maze))

    # flipt left-right
    maze = [row[::-1] for row in maze]

    print('\n'.join([''.join([str(cell) for cell in row]) for row in maze]))


def assign_random_exit(maze):

    black_positions = []

    for r,row in enumerate(maze):
        for c,cell in enumerate(row):
            black_positions += [(r,c)] if cell==0 else []

    r, c = random.choice(black_positions) # exit position
    maze[r][c] = 2

    pp.pprint(maze)



def visualize_path(maze, path, path_name=None, ax=None):
    
    nrows = len(maze)

    if ax is None:
        _, ax = plt.subplots(1)

    path = [(c,r) for r,c in path]
    x, y = zip(*[(x + 0.5, nrows - y - 0.5) for x,y in path])
    ax.plot(x, y, 'o--',  markersize=4, label=path_name, alpha=0.7)
    ax.plot(x[0], y[0], 's', markersize=5, color='purple')


# generate a figure for what one model does in a given node
def visualize_nodevalues(maze_name, pid, parameters, param_indx, node_value_func, model_name, ax=None):

    tree = TREE[maze_name]

    if ax is None:
        _, ax = plt.subplots(1)

    values_summary = node_values(maze_name, parameters, node_value_func)

    decision_summary = {nid: [] for nid in tree[pid]['children']}

    for param in parameters:
        for nid, val in values_summary[pid][param].items():

            decision_summary[nid].append(val)

    for nid, values in decision_summary.items():
        ax.plot([param[param_indx] for param in parameters], values, 'o--', markersize=3, label=nid, alpha=0.7)
        ax.set_ylim(0,1)
    
    ax.set_title(model_name)
    ax.grid()
    #ax.legend() # uncomment if you want the little plot to show coloured legen for each node in the corner 


def visualize_nodevalues_samplemodel(maze_name, pid, parameters, ax=None):

    if ax is None:
        _, ax = plt.subplots(1)

    decision_summary = {}
    explore_c = 11

    
    for budget, _ in parameters:
        with open(f"__experiment_{EXPERIMENT}/node_values_recursive/Sampling/node_values_{budget, explore_c}.pickle", "rb") as handle:
            node_values_summary = pickle.load(handle) # world: {pid: {(param,): {nid1: value, nid2: value}}}

        for nid, val in node_values_summary[maze_name][pid][budget, explore_c].items():
            decision_summary.setdefault(nid, []).append(val)

    for nid, values in decision_summary.items():
        ax.plot([param[0] for param in parameters], values, 'o--', markersize=3, label=nid, alpha=0.7)
        ax.set_ylim(0,1)
    
    ax.set_title('Sample Based')
    ax.grid()
    ax.legend()


# this generates a simulation of what each model does in a given tree node
def visualize_path_and_nodevalues(maze_name, pid):

    tree = TREE[maze_name]

    fig, axs = plt.subplots(3,3)
    axs = axs.flat

    maze, exit_pos = read_maze(maze_name, EXPERIMENT)

    if pid != 0:
        # if pid=0, then tree[pid] doesn't have a map key
        # process map to reflect what's been observed

        maze = tree[pid]['map']
        maze = tree_builder.update_map(maze, old_pos=tree[tree[pid]['pid']]['pos'], new_pos=tree[pid]['pos'])

    # draw maze and decision paths
    exit_pos = None

    visualize_maze(maze, exit_pos, axs[0])

    for nid in tree[pid]['children']:
        # path = tree[nid]['path_from_root']
        path = tree[nid]['path_from_par']
        visualize_path(maze, path, path_name=None, ax=axs[0])

    # node value plot for each model

    parameters = [(tau,1,1) for tau in TAUS]
    visualize_nodevalues(maze_name, pid, parameters, 0, node_value_plan, 'Expected Utility', axs[1])

    #DU tau = 1.0768355 gamma = 0.4664486
    parameters = [(1,gamma,1) for gamma in GAMMAS]
    visualize_nodevalues(maze_name, pid, parameters, 1, node_value_plan, 'Discounted Utility', axs[2])
    
    
    parameters = [(1,1,beta) for beta in BETAS]
    visualize_nodevalues(maze_name, pid, parameters, 2, node_value_plan, 'Probability Weighted', axs[3])

    # Sampling
    #parameters = [(bdgt, 2) for bdgt in BUDGET]
    #visualize_nodevalues_samplemodel(maze_name, pid, parameters, axs[4])
    
    #DU-Num tau = 0.8112561 gamma=0.4185981 beta=1.0000000   bits=4.919813
    parameters = [(0.8, 0.42, 1, bits) for bits in Bs]
    visualize_nodevalues(maze_name, pid, parameters, 3, node_value_plan, 'DU Numerosity', axs[4])
    
    parameters = [(1,1,1,bits) for bits in Bs] 
    visualize_nodevalues(maze_name, pid, parameters, 3, node_value_plan, 'EU Numerosity', axs[5])
   
    # k = 0.7
    parameters = [(1,k) for k in KAPPAS] 
    visualize_nodevalues(maze_name, pid, parameters, 1, node_value_heuristic, 'Steps-Cells', axs[6])

    parameters = [(1,0.7, bits ) for bits in Bs] 
    visualize_nodevalues(maze_name, pid, parameters, 2, node_value_heuristic, 'Steps-Cells-Num', axs[7])
    
    parameters = [(1.4, 0.68, beta ) for beta in BETAS] 
    visualize_nodevalues(maze_name, pid, parameters, 2, node_value_plan, 'PW-DU', axs[8])

    plt.tight_layout()
    return fig, axs



def visualize_nodes_path(maze_name, nodes_path, ax=None):

    tree = TREE[maze_name]
    maze, exit_pos = read_maze(maze_name, EXPERIMENT)

    def rand_jitter(arr):
        """ given array, jitter the values so that same values don't overlap """
        stdev = .04 * (max(arr) - min(arr))
        return arr + np.random.randn(len(arr)) * stdev

    if ax is None:
        _, ax = plt.subplots(1)

    exit_pos = None

    visualize_maze(maze, exit_pos, ax)

    full_path = []
    concise_path = []
    for nid in nodes_path:

        path = tree[nid]['path_from_par']
        if len(path) > 0: # TODO: could also include start position from first non-empty path
            concise_path.append(path[-1])
        full_path.extend(path)
        if path:
            row_vals, col_vals = zip(*path)
            path = [(r,c) for r,c in zip(rand_jitter(row_vals), rand_jitter(col_vals))]
            visualize_path(maze, path, path_name=nid, ax=ax)

    ax.legend(loc='upper left', bbox_to_anchor=(1,1))
    #print(full_path)
    return full_path, concise_path

def visualize_popular_subject_nodes_paths(maze_name):

    fig, axs = plt.subplots(2,2)
    axs = axs.flat

    with open(f'__experiment_{EXPERIMENT}/trees/{maze_name}.pickle', 'rb') as handle:
        tree = pickle.load(handle)

    with open(f"__experiment_{EXPERIMENT}/pickled_data/subject_decisions.pickle", "rb") as handle:
        decisions = pickle.load(handle)

    subject_paths_count = {} # {nodes path: count}

    # collect popular paths
    for sid in decisions:
        nodes_path = tuple(decisions[sid][maze_name]['nodes'])

        if nodes_path not in subject_paths_count:
            subject_paths_count[nodes_path] = 0

        subject_paths_count[nodes_path] += 1

    subject_paths_count = [(count, nodes_path) for nodes_path, count in subject_paths_count.items()]
    subject_paths_count.sort(reverse=True)

    for indx, (count, nodes_path) in enumerate(subject_paths_count[:4]):
        visualize_nodes_path(maze_name, nodes_path, ax=axs[indx])
        axs[indx].set_title(count)
    
    fig.suptitle(maze_name)


def visualize_all_path_by_subject(sid):

    fig, axs = plt.subplots(3,4)
    axs = axs.flat

    with open(f"__experiment_{EXPERIMENT}/pickled_data/subject_decisions.pickle", "rb") as handle:
        decisions = pickle.load(handle)

    for indx, maze_name in enumerate(decisions[sid]):
        if indx >= 12:
            break
        
        nodes_path = decisions[sid][maze_name]['nodes']
        visualize_nodes_path(maze_name, nodes_path, ax=axs[indx])
        axs[indx].set_title(maze_name)

    fig.suptitle(sid)


def visualize_all_best_paths(maze_name, model_name, param):

    fig, axs = plt.subplots(2,3,constrained_layout=True)
    axs = axs.flat

    nodes_paths = best_paths(maze_name, param, MODEL2RAWNODEVAL[model_name], exit_pos)
    #print(nodes_paths)

    best_path_list = []
    for (nodes_path, path_value), ax in zip(nodes_paths, axs):
    
        visualize_maze(maze, ax=ax)
        full_path, concise_path = visualize_nodes_path(maze_name, nodes_path, ax=ax) 
        best_path_list.append(concise_path)
        # This crowds the plot far too much
        #ax.set_title(f'{model_name} | {param} | {round(path_value,3)}')

    fig.suptitle(maze_name)
    plt.show()
    return best_path_list

##########################################################################################
# PHASE 3: Extract all optimal plans for each maze
##########################################################################################    

best_path_dict = {}
for maze_name in maze_names:
    maze, exit_pos = read_maze(maze_name, EXPERIMENT) 
    best_path_list = visualize_all_best_paths(maze_name, 'Expected_Utility', (np.float64(0.2),1,1))
    best_path_dict[maze_name] = best_path_list
p.pprint(best_path_dict)