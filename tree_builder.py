
from collections import deque
import pprint
import pickle
import os
import csv

pp = pprint.PrettyPrinter(compact=False)

EXPERIMENT = 5


def memoize(function):
    """ ram cacher """
    memo = {}
    def wrapper(*args):
        if args not in memo:
            memo[args] = function(*args)
        return memo[args]
    return wrapper


def observation_made(maze, pos):
    "set of cells (r,c) of room"

    observations, (r,c) = set(), pos
    nrows, ncols = len(maze), len(maze[0])
    
    # 1st quadrant
    c_left = 0
    
    for r_ in range(r, -1, -1):
        columns = []
        for c_ in range(c, c_left-1, -1):

            if maze[r_][c_] == 3:
                break

            columns.append(c_)

            if maze[r_][c_] in {0, 2}:
                observations.add((r_, c_))
        
        if not columns:
            break

        c_left = columns[-1]

    # 2nd quadrant
    c_right = ncols-1

    for r_ in range(r, -1, -1):
        columns = []
        for c_ in range(c, c_right+1):

            if maze[r_][c_] == 3:
                break

            columns.append(c_)

            if maze[r_][c_] in {0, 2}:
                observations.add((r_, c_))

        if not columns:
            break

        c_right = columns[-1]

    # 3rd quadrant
    c_left = 0
    
    for r_ in range(r, nrows):
        columns = []

        for c_ in range(c, c_left-1, -1):

            if maze[r_][c_] == 3:
                break

            columns.append(c_)

            if maze[r_][c_] in {0, 2}:
                observations.add((r_, c_))
        
        if not columns:
            break

        c_left = columns[-1]

    # 4th quadrant
    c_right = ncols-1
    
    for r_ in range(r, nrows):
        columns = []
        for c_ in range(c, c_right+1):

            if maze[r_][c_] == 3:
                break

            columns.append(c_)

            if maze[r_][c_] in {0, 2}:
                observations.add((r_, c_))

        if not columns:
            break

        c_right = columns[-1]
        
    return observations


def update_map(maze, old_pos, new_pos):

    observations = observation_made(maze, new_pos)

    maze_updated = [[6 if (r,c) in observations else maze[r][c] 
                                for c in range(len(maze[0]))] 
                                for r in range(len(maze))]

    maze_updated[old_pos[0]][old_pos[1]] = 6

    maze_updated = tuple(tuple(row) for row in maze_updated)

    return maze_updated


def next_path(maze, pos):
    
    nrows, ncols = len(maze), len(maze[0])

    agenda = deque([[pos]])
    observations_seen = {} # {observation frozen set : number of times that this set has been seen}

    while agenda:

        path = agenda.popleft()

        r_, c_ = path[-1]

        for rr, cc in ((0,1), (0,-1), (1,0), (-1,0)):

            # clip within bounds
            r, c = max(min(r_+rr, nrows-1), 0), max(min(c_+cc, ncols-1), 0)

            # ignore if neigh is a wall
            if maze[r][c] == 3 or (r,c) in path:
                continue

            # if new observation is made, then we have a path
            observations = observation_made(maze, (r,c))

            if observations:

                if (r,c) in observations_seen.get(frozenset(observations), set()):
                    # if this particular set of observations has been made at position (r,c),
                    # then we don't want to consider any more paths
                    continue

                observations_seen.setdefault(frozenset(observations), set()).add((r,c))
                yield path + [(r,c)], observations
            
            else:
                agenda.append(path + [(r,c)])

    return []

def maze2tree(maze):

    # determine start position
    remains = 0
    for r, row in enumerate(maze):
        for c, val in enumerate(row):
            if val == 5:
                pos = (r,c)
            elif val in {0,2}:
                remains += 1
    
    maze = update_map(maze, pos, pos)

    tree = {0: {'pos': pos,
                'remains': remains,
                'path_from_par': [],
                'path_from_root': [],
                'steps_from_par': 0,
                'steps_from_root': 0,
                'celldistances': set(),
                'children': set(),
                'pid': None,
                'depth': 0
                }}

    agenda = [(0, maze)]

    while agenda: # in each loop, find and append children

        node, updated_map = agenda.pop(0)
        pos = tree[node]['pos']
        node_depth = tree[node]['depth']

        # print('-------------------------------------------')
        # print(node, pos)        
        # print()

        for path, observation in next_path(updated_map, pos):

            # print(path)
            # print(observation, tree[node]['remains']-len(observation))
            # pp.pprint(updated_map)
            # print()

            branch = {  'pos': path[-1],
                        'remains': tree[node]['remains']-len(observation),
                        'path_from_par': path,
                        'path_from_root': tree[node]['path_from_root'][:-1] + path,
                        'steps_from_par': len(path) - 1,
                        'steps_from_root': tree[node]['steps_from_root'] + len(path) - 1,
                        'celldistances': observation,
                        'children': set(),
                        'pid': node,
                        'depth': node_depth + 1,
                        'map': updated_map, # map where nid started from!
                    }

            new_node = max(tree)+1
            agenda.append((new_node, update_map(updated_map, path[0], path[-1])))

            tree[node]['children'].add(new_node)
            tree[new_node] = branch

            if new_node % 200 == 0:
                print('at node:', new_node)

    return tree

# generates a tree with only one maze
def read_maze(maze_name, exp):

    with open(f'__experiment_{exp}/mazes/{maze_name}.txt', 'r') as f:
        ncols = int(f.readline())
        nrows = int(f.readline())
        maze = f.readlines()
    
    # preprocessing of maze from text
    maze = tuple([tuple([int(cell) for cell in row.split('\n')[0]]) for row in maze])

    exit_pos = None

    for r,row in enumerate(maze):
        for c,cell in enumerate(row):
            if cell == 2:
                exit_pos = (r,c)
    
    return maze, exit_pos



def pickle_individual_tree():

    subdir, dirs, maze_files = next(os.walk(f'__experiment_{EXPERIMENT}/mazes'))

    for file_name in maze_files:

        print(file_name)
        maze_name = file_name.split('.')[0]

        #if maze_name != 'hard_two_big_rooms_1':
        #    continue

        # if maze_name in {'hard_two_big_rooms_1', 'hard_hallway_1'}:
        #     continue

        # if 'x_' in maze_name:
        #     continue

        # if os.path.exists(f"__experiment_{EXPERIMENT}/trees/{maze_name}.pickle"):
        #     continue

        print(maze_name)

        maze, exit_pos = read_maze(maze_name, EXPERIMENT)
        tree = maze2tree(maze)

        with open(f"__experiment_{EXPERIMENT}/trees/{maze_name}.pickle", 'wb') as handle:
            pickle.dump(tree, handle, protocol=pickle.HIGHEST_PROTOCOL)


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

    pp.pprint(len(tree))

    with open(f'__experiment_{EXPERIMENT}/pickled_data/tree.pickle', 'wb') as handle:
        pickle.dump(tree, handle, protocol=pickle.HIGHEST_PROTOCOL)


if __name__ == '__main__':

    pickle_individual_tree()
    pickle_unified_tree()
