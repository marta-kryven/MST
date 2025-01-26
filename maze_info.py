# TODO:
    # "env17_a1"
    # "5_units_vis1",
    # "env17_b",
    # "6_units_b1",
    # "env17_c",
    # "6_units_flip",
    # "6_units_vis1",
    # "four_units_vis1",
    # "test11",
    # "big_alcoves_vis1",
    # "test21",
    # "binary_7x7_rotated",
    # "test",

# EXAMPLES WITH INTENDED SEGMENTATION
maze_info_dict = {
    "env17_a1": {
        "fragment": [
            [1,0,1],
            [0,0,0],
        ],
        "copies": [
            {"top left": (1,0), "reflect": False, "rotations": 0},
            {"top left": (5,0), "reflect": False, "rotations": 0},
            {"top left": (3,4), "reflect": False, "rotations": 0},
            {"top left": (7,4), "reflect": False, "rotations": 0},
        ],
    },
    "5_units": {
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (1,0), "reflect": False, "rotations": 0},
            {"top left": (1,3), "reflect": False, "rotations": 0},
            {"top left": (1,6), "reflect": False, "rotations": 0},
            {"top left": (1,9), "reflect": False, "rotations": 0},
            {"top left": (1,12), "reflect": False, "rotations": 0},
        ],
    },
    "5_units_vis1": {
        "fragment": [
            [2,2],
            [2,2],
        ],
        "copies": [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (5,3), "reflect": False, "rotations": 0},
            {"top left": (5,6), "reflect": False, "rotations": 0},
        ],
    },
    "env17_b": {
        "fragment": [
            [1,2,1],
            [2,2,2],
        ],
        "copies": [
            {"top left": (0,0), "reflect": False, "rotations": 0},
            {"top left": (3,0), "reflect": False, "rotations": 0},
            {"top left": (6,0), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (3,4), "reflect": False, "rotations": 0},
            {"top left": (6,4), "reflect": False, "rotations": 0},
        ],
    },
    "env17_c": {
        "fragment": [
            [1,2],
            [2,2],
            [1,2],
        ],
        "copies": [
            {"top left": (0,0), "reflect": False, "rotations": 0},
            {"top left": (0,3), "reflect": False, "rotations": 0},
            {"top left": (0,6), "reflect": False, "rotations": 0},
            {"top left": (0,9), "reflect": False, "rotations": 0},
            {"top left": (0,12), "reflect": False, "rotations": 0},
        ],
    },
    "6_units_b1": {
        "fragment": [
            [2,2],
            [2,2],
            [2,2],
        ],
        "copies": [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (0,10), "reflect": False, "rotations": 0},
            {"top left": (0,13), "reflect": False, "rotations": 0},
            {"top left": (0,16), "reflect": False, "rotations": 0},
        ],
    },
    "6_units_flip": {
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (5,2), "reflect": False, "rotations": 0},
            {"top left": (5,5), "reflect": False, "rotations": 0},
            {"top left": (5,8), "reflect": False, "rotations": 0},
        ],
    },
    "6_units_vis1": {
        "fragment": [
            [0,0],
            [0,2],
        ],
        "copies": [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (7,1), "reflect": False, "rotations": 1},
            {"top left": (7,4), "reflect": False, "rotations": 1},
            {"top left": (7,7), "reflect": False, "rotations": 1},
        ],
    },
    "four_units_vis1": {
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,3), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (5,0), "reflect": False, "rotations": 0},
            {"top left": (5,4), "reflect": False, "rotations": 0},
        ],
    },
    "test11": { # TODO has multiple fragments
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,0), "reflect": False, "rotations": 0},
        ],
    },
    "big_alcoves_vis1": {
        "fragment": [
            [0,0],
            [2,2],
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,8), "reflect": False, "rotations": 0},
            {"top left": (5,8), "reflect": False, "rotations": 2},
        ],
    },
    "test21": {
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (1,0), "reflect": False, "rotations": 0},
            {"top left": (1,3), "reflect": False, "rotations": 0},
            {"top left": (1,6), "reflect": False, "rotations": 0},
            {"top left": (1,9), "reflect": False, "rotations": 0},
            {"top left": (1,12), "reflect": False, "rotations": 0},
            {"top left": (1,15), "reflect": False, "rotations": 0},
        ],
    },
    "binary_7x7_rotated": {
        "fragment": [
            [0,0,0],
            [0,1,1],
            [0,1,1],
            [0,1,1],
            [0,1,1],
            [0,1,1],
        ],
        "copies": [
            {"top left": (0,0), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": True, "rotations": 0},
        ],
    },
    "test": {
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,0), "reflect": False, "rotations": 0},
            {"top left": (0,3), "reflect": False, "rotations": 0},
            {"top left": (0,6), "reflect": False, "rotations": 0},
            {"top left": (5,0), "reflect": False, "rotations": 0},
            {"top left": (5,3), "reflect": False, "rotations": 0},
            {"top left": (5,6), "reflect": False, "rotations": 0},
        ],
    },
    "tiny_rooms": {
        "fragment": [
            [1,1,1,1,0,1,0],
            [0,0,0,0,0,0,0],
        ],
        "copies": [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (3,1), "reflect": False, "rotations": 0},
            {"top left": (6,1), "reflect": False, "rotations": 0},
        ],
    },
    "four_units": {
        "subdir": "four_units",
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,3), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (5,0), "reflect": False, "rotations": 0},
            {"top left": (5,4), "reflect": False, "rotations": 0},
        ],
    },

    "four_units_flip": {
        "fragment": [
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,0), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (5,3), "reflect": False, "rotations": 0},
            {"top left": (5,7), "reflect": False, "rotations": 0},
        ],
    },

    "6_units" : {
        "subdir" : "6_units",
        "fragment" : [
            [0,0,],
            [0,0,],
        ],
        "copies" : [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (0,4), "reflect": False, "rotations": 0},
            {"top left": (0,7), "reflect": False, "rotations": 0},
            {"top left": (7,1), "reflect": False, "rotations": 0},
            {"top left": (7,4), "reflect": False, "rotations": 0},
            {"top left": (7,7), "reflect": False, "rotations": 0},
        ],
    },

    "two_arms_5x7" : {
        "subdir" : "two_arms",
        "fragment" : [[0]], # TODO
        "copies" : [{"top left": (0,0), "reflect": False, "rotations": 0}], # TODO
    },

    "corridors_to_three_tiny_rooms_with_alcoves": {
        "subdir": "corridors_to_rooms_with_alcoves",
        "fragment": [
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 1, 0],         
        ],
        "copies": [
            {"top left": (0,2), "reflect": False, "rotations": 0},
            {"top left": (3,2), "reflect": True, "rotations": 2},
            {"top left": (6,2), "reflect": True, "rotations": 2},
        ],
    },

    "binary_7x7" : {
        "subdir" : "binary_7x7",
        "fragment" : [
            [0, 0, 0, 0, 0, 0],
            [1, 1, 1, 1, 1, 0],
            [1, 1, 1, 1, 1, 0],
        ],
        "copies" : [
            {"top left": (0,1), "reflect": False, "rotations": 0},
            {"top left": (4,1), "reflect": True, "rotations": 2},
        ],
    },
    

    "big_alcoves": {
        "subdir": "big_alcoves",
        "fragment": [
            [0,0],
            [0,0],
            [0,0],
            [0,0],
        ],
        "copies": [
            {"top left": (0,8), "reflect": False, "rotations": 0},
            {"top left": (5,8), "reflect": False, "rotations": 0},
        ],
    },
}

    # "6_units_b1": {
    #     "fragment": [
    #         [0,0],
    #         [0,0],
    #     ],
    #     "copies": [
    #         {"top left": (1,0), "reflect": False, "rotations": 0},
    #         {"top left": (1,3), "reflect": False, "rotations": 0},
    #         {"top left": (1,6), "reflect": False, "rotations": 0},
    #         {"top left": (1,9), "reflect": False, "rotations": 0},
    #         {"top left": (1,12), "reflect": False, "rotations": 0},
    #     ],
    # },


    # ENV 15 = THREE ROOMS WITH CLOSETS

    # three small rooms with closets visibility level 1
    # subdir = "three_rooms_with_closets"
    # input_id = "three_small_rooms_with_closets_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 3
    # start_col = 5
    # exit_row = 0
    # exit_col = -3

    # ENV 16 = FOUR UNITS B (same subdir as FOUR UNITS) 

    # four units b visibility level 1
    # subdir = "four_units"
    # input_id = "four_units_b_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 4
    # start_col = 3
    # exit_row = -1
    # exit_col = -1

    # ENV 17 = ENV17

    # env17
    # subdir = "env17"
    # input_id = "env17"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 2
    # start_col = 3
    # exit_row = -1
    # exit_col = -2

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
    # subdir = 'two_with_closets'
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
    # subdir = 'two_with_closets'
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

    # four units
    # subdir = "four_units"
    # input_id = "four_units_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 3
    # start_col = 0
    # exit_row = 0
    # exit_col = -1

    # ENV 2-3 = 6 UNITS

        # 6 units visibility level 1
    # subdir = "6_units"
    # input_id = "6_units_vis1"
    # fragment = [
    #     [0,0,],
    #     [0,2,],
    # ]
    # copies = [ # TODO: this has to be edited to get rotations right for modular
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

    # ENV 4 = CORRIDORS To ROOMS WITH ALCOVES

    # corridors to rooms with alcoves visibility level 1
    # This one is way too slow
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


    # corridors to tiny rooms with alcoves
    # subdir = "corridors_to_rooms_with_alcoves"
    # input_id = "corridors_to_tiny_rooms_with_alcoves"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 2
    # start_col = 0
    # exit_row = 3
    # exit_col = -1

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


    # big alcoves
    # subdir = "big_alcoves"
    # input_id = "big_alcoves_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 4
    # start_col = 0
    # exit_row = 0
    # exit_col = -1


    # ENV 7-8 = H PAIR

    # H pair
    # subdir = "H_pair"
    # input_id = "H_pair"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = -1
    # start_col = 6
    # exit_row = 3
    # exit_col = -4

    # H pair with incentive rotated
    # subdir = "H_pair"
    # input_id = "H_pair_with_incentive_rotated"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = 6
    # start_col = -1
    # exit_row = -4
    # exit_col = 3

    # ENV 9 = LOOPS

    # clockwise loop pair visability level 1
    # subdir = "loop_pair"
    # input_id = "clockwise_loop_pair_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = -1
    # start_col = 4
    # exit_row = 0
    # exit_col = -1

    # reflected loop pair visability level 1
    # subdir = "loop_pair"
    # input_id = "reflected_loop_pair_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = -1
    # start_col = 4
    # exit_row = 0
    # exit_col = -1

    # ENV 10 = PARALLEL HALLS PAIR

    # parallel halls pair visibility level 1
    # subdir = "parallel_halls_pair"
    # input_id = "parallel_halls_pair_vis1"
    # fragment = [[0]] # TODO
    # copies = [{"top left": (0,0), "reflect": False, "rotations": 0}] # TODO
    # start_row = -1
    # start_col = 5
    # exit_row = 0
    # exit_col = -2