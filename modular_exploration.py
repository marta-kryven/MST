import os
import shutil
import numpy as np
import pickle

import pattern_editor
import utils as ut

import enum

from partition_prompt import PartitionPrompt, regenerate_pattern #, segment_map

from tree_builder import maze2tree

# 5 is for modular planning, 6 is ordinary optimal planning
EXPERIMENT = 6

# TODO: move this somewhere it can be used commonly here and in maze2tree
class Cell(enum.Enum):
    WALL = 3 
    UNOBSERVED_EMPTY = 0
    OBSERVED_EMPTY = 6
    HIDDEN_EXIT = 2
    START = 5

# from compositional_map_synthesis import pattern_editor
# from compositional_map_synthesis import utils as ut

# from compositional_map_synthesis.partition_prompt import regenerate_pattern

# EXAMPLES WITH INTENDED SEGMENTATION

# four units

# four units
# subdir = "four_units"
# input_id = "four_units"
# fragment = [[0]] # TODO
# copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
# start_row = 3
# start_col = 0
# exit_row = 0
# exit_col = -1

# 4 UNIT CORRIDOR:

# 4 unit corridor 
# subdir = "4_unit_corridor"
# input_id = "4_unit_corridor"
# fragment = [
#     [0,0,0,0,0,0,0,],
#     [0,1,0,1,1,0,0,],
#     [0,1,0,1,1,0,0,],
# ]
# copies = [
#     {"top left": (0,1), "reflect": False, "rotations": 0},
#     {"top left": (0,9), "reflect": False, "rotations": 0},
#     {"top left": (6,1), "reflect": False, "rotations": 2},
#     {"top left": (6,9), "reflect": False, "rotations": 2},
# ]
# start_row = 4
# start_col = 0
# exit_row = -1
# exit_col = -1

# 4 unit corridor with visibility 1
# subdir = "4_unit_corridor"
# input_id = "4_unit_corridor_vis1"
# fragment = [
#     [2,2,2,2,2,2,2,],
#     [0,1,0,1,1,0,0,],
#     [0,1,0,1,1,0,0,],
# ]
# copies = [
#     {"top left": (0,1), "reflect": False, "rotations": 0},
#     {"top left": (0,9), "reflect": False, "rotations": 0},
#     {"top left": (6,1), "reflect": False, "rotations": 2},
#     {"top left": (6,9), "reflect": False, "rotations": 2},
# ]
# start_row = 4
# start_col = 0
# exit_row = -1
# exit_col = -1

# 6 UNITS

# 6 units
# subdir = "6_units"
# input_id = "6_units"
# fragment = [
#     [0,0,],
#     [0,0,],
# ]
# copies = [
#     {"top left": (0,1), "reflect": False, "rotations": 0},
#     {"top left": (0,4), "reflect": False, "rotations": 0},
#     {"top left": (0,7), "reflect": False, "rotations": 0},
#     {"top left": (7,1), "reflect": False, "rotations": 0},
#     {"top left": (7,4), "reflect": False, "rotations": 0},
#     {"top left": (7,7), "reflect": False, "rotations": 0},
# ]
# start_row = 4
# start_col = 0
# exit_row = 0
# exit_col = 7

# CORRIDORS To ROOMS WITH ALCOVES

# corridors to rooms with alcoves visibility level 1
# subdir = "corridors_to_rooms_with_alcoves"
# input_id = "corridors_to_rooms_with_alcoves_vis1"
# fragment = [
#     [0,0,0,0,0,0,0,0,0,],
#     [0,0,0,0,0,0,0,0,0,],
#     [2,2,2,2,2,2,2,2,2,],
#     [0,0,0,0,0,0,0,1,0,],
# ]
# copies = [
#     {"top left": (0,2), "reflect": False, "rotations": 0},
#     {"top left": (5,2), "reflect": True, "rotations": 2},
# ]
# start_row = 4
# start_col = 0
# exit_row = 3
# exit_col = -1

# corridors to rooms with alcoves constrained for efficiency, visibility level 1
# subdir = "corridors_to_rooms_with_alcoves"
# input_id = "corridors_to_rooms_with_alcoves_constrained_vis1"
# fragment = [
#     [0,0,0,0,0,0,0,1,2,],
#     [0,0,0,0,0,0,0,1,1,],
#     [2,2,2,2,2,2,2,2,2,],
#     [0,0,0,0,0,0,0,1,0,],
# ]
# copies = [
#     {"top left": (0,2), "reflect": False, "rotations": 0},
#     {"top left": (5,2), "reflect": True, "rotations": 2},
# ]
# start_row = 4
# start_col = 0
# exit_row = 3
# exit_col = -1

# corridors to small rooms with alcoves
# subdir = "corridors_to_rooms_with_alcoves"
# input_id = "corridors_to_small_rooms_with_alcoves"
# fragment = [[0]] # TODO
# copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
# start_row = 4
# start_col = 0
# exit_row = 3
# exit_col = -1

subdir = "corridors_to_rooms_with_alcoves"
input_id = "corridors_to_tiny_rooms_with_alcoves"
fragment = [[0]] # TODO
copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
start_row = 2
start_col = 0
exit_row = 3
exit_col = -1

# Bigger maze example
# input_id = "bigger_maze" #"twolines" # "test"
# fragment = [
#     [1, 0, 1]
#     [0, 0, 1],
#     [1, 0, 1],
#     [0, 0, 1],
#     [1, 0, 1],
# ]
# copies = [
#     {"top left":(0,0), "reflect": False, "rotations": 0},
#     {"top left":(0,3), "reflect": False, "rotations": 0},
#     {"top left":(6,0), "reflect": True, "rotations": 0},
#     {"top left":(6,3), "reflect": True, "rotations": 0},
# ]
# start_row = 5
# start_col = 0
# exit_row = -2
# exit_col = -1

# Two halls with closets
# input_id = "two_halls_with_closets"
# fragment = [
#     [0,0],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,0],
# ]
# copies = [
#     {"top left":(0,0), "reflect": False, "rotations": 0},
#     {"top left":(0,3), "reflect": True, "rotations": 0},
# ]
# start_row = -1
# start_col = 2
# exit_row = 0
# exit_col = 3

# Two halls with closets, start observed
# input_id = "two_halls_with_closets_obs"
# fragment = [
#     [0,0],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [0,1],
#     [2,2],
# ]
# copies = [
#     {"top left":(0,0), "reflect": False, "rotations": 0},
#     {"top left":(0,3), "reflect": True, "rotations": 0},
# ]
# start_row = -1
# start_col = 2
# exit_row = 0
# exit_col = 3

# Two small rooms with closets
# input_id = "two_small_rooms_with_closets"
# fragment = [
#     [0,0,0],
#     [0,0,1],
#     [0,0,1],
#     [0,0,1],
#     [0,0,1],
#     [0,0,1],
#     [0,0,1],
#     [0,0,1],
#     [0,0,0],
# ]
# copies = [
#     {"top left":(0,0), "reflect": False, "rotations": 0},
#     {"top left":(0,4), "reflect": True, "rotations": 0},
# ]
# start_row = -1
# start_col = 3
# exit_row = 0
# exit_col = 4

# Two rooms with closets
# input_id = "two_rooms_with_closets"
# fragment = [
#     [0,0,0,0],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,1],
#     [0,0,0,0],
# ]
# copies = [
#     {"top left":(0,0), "reflect": False, "rotations": 0},
#     {"top left":(0,5), "reflect": True, "rotations": 0},
# ]
# start_row = 9
# start_col = 4
# exit_row = 0
# exit_col = 3

# Corridors to rooms with alcoves + observed cells
# input_id = "corridors_to_rooms_with_alcoves_obs"
# fragment = [
#     [0,]*9,
#     [0,]*9,
#     [2,]*9,
#     [0,]*7 + [1,0],
# ]
# copies = [
#     {"top left":(0,2), "reflect": False, "rotations": 0},
#     {"top left":(5,2), "reflect": True, "rotations": 2},
# ]
# start_row = 5
# start_col = 0
# exit_row = 3
# exit_col = -1

# Snaking path with loops
# input_id = "snaking_path_with_loops"
# fragment = [
#     [1,1,0,0,1,1],
#     [1,0,0,0,0,1],
#     [1,0,1,1,0,1],
#     [0,0,1,1,0,0],
#     [0,0,1,1,0,0],
#     [1,0,1,1,0,1],
#     [1,0,0,0,0,1],
# ]
# copies = [
#     {"top left":(14,0), "reflect": False, "rotations": 1}, # TODO: the current code doesn't allow us to start in a fragment
#     {"top left":(9,12), "reflect": False, "rotations": 0},
#     {"top left":(6,0), "reflect": False, "rotations": 1},
#     {"top left":(1,13), "reflect": False, "rotations": 1},
# ]
# start_row = -2 # (-3,0) is more interesting but not supported 
# start_col = 8
# exit_row = 1
# exit_col = -4

# Simplified snaking path
# input_id = "simplified_snaking_path"
# fragment = [
#     [1,0,0,0,1],
#     [0,0,1,0,0],
#     [1,0,0,0,1],
# ]
# copies = [
#     {"top left":(7,0), "reflect": False, "rotations": 1},
#     {"top left":(5,6), "reflect": False, "rotations": 0},
#     {"top left":(2,2), "reflect": False, "rotations": 0},
# ]
# start_row = -3
# start_col = 3
# exit_row = 1
# exit_col = -4


# SET EXPERIMENT = 6: The following example is not meant to demonstrate map decomposition
# Three closet local gadget
# input_id = "three_closet_local_gadget"
# fragment = [
#     [],
# ]
# copies = []
# start_row = -1
# start_col = 2
# exit_row = -3
# exit_col = -1

# READ MAP AND RECONSTRUCT FROM FRAGMENTS
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
set_exit(gt_map,exit_row,exit_col)

print(ut.array_to_string(gt_map))

segmentation = pp.segment_map(fragment, copies)
print(segmentation)
print(f"Number of segmented cells: {len(segmentation.keys())}")
fragment = convert_to_mst_format(fragment)

# TODO: remove this logic and have maze2tree construct and memoize subtrees as necessary
# Compute subtrees for entering fragment from any entrance
# subtrees = {}
# def find_entrances(map):
#     entrances = []
#     height, width = len(map), len(map[0])

#     # Top and bottom
#     for j in range(width):
#         if map[0][j] == 0:
#             entrances.append((0,j))
#         if map[-1][j] == 0:
#             entrances.append((height-1,j))
#     # Left and right
#     for i in range(height):
#         if map[i][0] == 0:
#             entrances.append((i,0))
#         if map[i][-1] == 0:
#             entrances.append((i,width-1))

#     return entrances
# frag_map = convert_to_mst_format(fragment)
# entrances = find_entrances(frag_map)
# # We want subtrees rooted where observations are made.
# # I will assume that observations can't be made at entrances,
# # and that subtrees at observation locations do not depend on 
# # the path preceding that first observation - both these assumptions
# # should hold up in general.
# for entrance in entrances:
#     i,j = entrance
#     set_start(frag_map, i, j)
#     # print(frag_map)
#     subtree = maze2tree(frag_map)
#     frag_map[i][j] = 0
#     # The larger search enters this subtree where the first observation
#     # is made, NOT at the entrance
#     root = subtree[1]['pos']
#     del subtree[0]
#     for nid in subtree.keys():
#         branch = subtree[nid]
#         if nid == 1:
#             branch['path_from_par'] = []
#             branch['steps_from_par'] = 0
#             branch['path_from_root'] = []
#             branch['steps_from_root'] = 0
#         else:
#             # Delete subpath from entrance
#             while not branch['path_from_root'][0] == root:
#                 branch['path_from_root'].pop(0)
#             branch['steps_from_root'] = len(branch['path_from_root'])
#         branch['depth'] = branch['depth'] - 1
#     # Its possible that multiple entrances lead to the same
#     # subtree (for instance in bigger_maze, there are 4 entrances
#     # but only the top and bottom forks are roots for planning subtrees)
#     if root not in subtrees.keys():
#         subtrees[root] = subtree
        # print(subtree)
#print(entrances)
# print(subtrees)
# These subtrees aren't directly usable for navigation
# because of coordinate changes, but the values should be usable.

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

# Dump as a 'unified' tree - as long as we're only testing one, just a dict wrapper
# with open(f'__experiment_{EXPERIMENT}/pickled_data/tree.pickle', 'wb') as handle:
#     tree['root'] = 0 # Apparently this is necessary
#     pickle.dump({input_id: tree}, handle, protocol=pickle.HIGHEST_PROTOCOL)

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

# TODO: This seems actually incorrect, mazes here should be in tree builder's form like gt_map
# shutil.copyfile(
#     f"/home/cwyeth/Desktop/compositional_map_synthesis/test_patterns/{subdir}/{input_id}.txt", 
#     f"__experiment_{EXPERIMENT}/mazes/{input_id}.txt",
# )
# This doubles as converting maps to the MST format which we'll want to switch to anyway:
# includes start and exit as well. 
with open(f"__experiment_{EXPERIMENT}/mazes/{input_id}.txt", 'w') as f:
    for row in gt_map:
        f.write("".join(map(str, row)) + "\n")