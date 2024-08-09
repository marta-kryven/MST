import numpy as np

import pattern_editor
import utils as ut

import enum

from partition_prompt import regenerate_pattern

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

input_id = "maze" #"twolines" # "test"
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
]
copies = [
    {"top left":(0,0), "reflect": False, "rotations": 0},
    {"top left":(0,3), "reflect": False, "rotations": 0},
    {"top left":(4,0), "reflect": False, "rotations": 0},
    {"top left":(4,3), "reflect": False, "rotations": 0},
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

print(gt_map)

set_start(gt_map,3,0)
set_exit(gt_map,-1,-2)

tree = maze2tree(gt_map)

