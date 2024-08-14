import numpy as np

import pattern_editor
import utils as ut

import enum

from partition_prompt import regenerate_pattern, segment_map

from tree_builder import maze2tree

class Cell(enum.Enum):
    WALL = 3 
    UNOBSERVED_EMPTY = 0
    OBSERVED_EMPTY = 6
    HIDDEN_EXIT = 2
    START = 5

# from compositional_map_synthesis import pattern_editor
# from compositional_map_synthesis import utils as ut

# from compositional_map_synthesis.partition_prompt import regenerate_pattern

input_id = "bigger_maze" #"twolines" # "test"
input_map = pattern_editor.read_pattern(f'test_patterns/{input_id}.txt')
input_dims = (len(input_map), len(input_map[0]))
str_map = ut.array_to_string(input_map)

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

# Intended response
fragment = [
    [1, 0, 1],
    [0, 0, 1],
    [1, 0, 1],
    [0, 0, 1],
    [1, 0, 1],
]
copies = [
    {"top left":(0,0), "reflect": False, "rotations": 0},
    {"top left":(0,3), "reflect": False, "rotations": 0},
    {"top left":(6,0), "reflect": True, "rotations": 0},
    {"top left":(6,3), "reflect": True, "rotations": 0},
]
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

set_start(gt_map,5,0)
set_exit(gt_map,-2,-1)

print(gt_map)

segmentation = segment_map(fragment, copies)

# Compute subtrees for entering fragment from any entrance
subtrees = {}
def find_entrances(map):
    entrances = []
    height, width = len(map), len(map[0])

    # Top and bottom
    for j in range(width):
        if map[0][j] == 0:
            entrances.append((0,j))
        if map[-1][j] == 0:
            entrances.append((height-1,j))
    # Left and right
    for i in range(height):
        if map[i][0] == 0:
            entrances.append((i,0))
        if map[i][-1] == 0:
            entrances.append((i,width-1))

    return entrances
frag_map = convert_to_mst_format(fragment)
entrances = find_entrances(frag_map)
# We want subtrees rooted where observations are made.
# I will assume that observations can't be made at entrances,
# and that subtrees at observation locations do not depend on 
# the path preceding that first observation - both these assumptions
# should hold up in general.
for entrance in entrances:
    i,j = entrance
    set_start(frag_map, i, j)
    print(frag_map)
    subtree = maze2tree(frag_map)
    frag_map[i][j] = 0
    # The larger search enters this subtree where the first observation
    # is made, NOT at the entrance
    root = subtree[1]['pos']
    del subtree[0]
    for nid in subtree.keys():
        branch = subtree[nid]
        if nid == 1:
            branch['path_from_par'] = []
            branch['steps_from_par'] = 0
            branch['path_from_root'] = []
            branch['steps_from_root'] = 0
        else:
            # Delete subpath from entrance
            while not branch['path_from_root'][0] == root:
                branch['path_from_root'].pop(0)
            branch['steps_from_root'] = len(branch['path_from_root'])
        branch['depth'] = branch['depth'] - 1
    # Its possible that multiple entrances lead to the same
    # subtree (for instance in bigger_maze, there are 4 entrances
    # but only the top and bottom forks are roots for planning subtrees)
    if root not in subtrees.keys():
        subtrees[root] = subtree
        print(subtree)
print(entrances)
# print(subtrees)
# These subtrees aren't directly usable for navigation
# because of coordinate changes, but the values should be usable.

tree = maze2tree(gt_map, fragment, segmentation, subtrees)
print(tree)

