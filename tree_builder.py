
from collections import deque
import pprint
import pickle
import os
import csv

from partition_prompt import fragment_to_map_coords

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

def maze2tree(maze, fragment = None, segmentation = {}):

    subtrees = {}

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
                'depth': 0,
                'copies_explored': [],
                }}

    agenda = [(0, maze)]

    while agenda: # in each loop, find and append children

        node, updated_map = agenda.pop(0)
        pos = tree[node]['pos']
        node_depth = tree[node]['depth']

        # print('-------------------------------------------')
        # print(node, pos)        
        # print()

        # if pos is segmented and this hasn't been done yet,
        # recursively add, correct, and complete fragment subtree
        # by adding leaves to agenda - then skip adding paths
        #print("testing whether pos is in segmentation")
        #print(pos)
        if pos in segmentation.keys():
            #print("pos is in segmentation")
            copy, base_i, base_j = segmentation[pos]
            if copy['top left'] not in tree[node]['copies_explored']: # also need to check if this subtree has already been explored...
                #print("tree not yet explored")
                if (base_i,base_j) in subtrees.keys():
                    subtree = subtrees[(base_i,base_j)]
                else:
                    fragment[base_i][base_j] = 5 # start
                    subtree = maze2tree(fragment)
                    fragment[base_i][base_j] = 0
                    subtrees[(base_i,base_j)] = subtree
                frag_to_map = fragment_to_map_coords(fragment, copy)
                # This is the branch of the global tree that the subtree will be stiched too
                branch = tree[node]
                # Now we actually stitch the subtree in
                def stitch(nid, pid=None):
                    """
                    Given the local nid within a subtree and its global pid recursively stitch a new node into the global tree.
                    """
                    subtree_branch = subtree[nid]
                    if nid == 0:
                        # The current node has already been added and is
                        # identified with nid 0.
                        # TODO: assuming for now the subtrees are nontrivial
                        for child in subtree_branch['children']:
                            stitch(child, node)
                    else:
                        new_node = max(tree) + 1
                        pos = frag_to_map[subtree_branch['pos']]
                        parent_map = tree[pid]['map']
                        # Note that the parent's map does not include its own observations, which must be added here TODO: what if we start in a fragment?
                        starting_map = update_map(parent_map, tree[pid]['path_from_par'][0], tree[pid]['path_from_par'][-1])
                        observation = observation_made(starting_map, pos)
                        stitched_branch = {
                            'pos': pos, 
                            'remains': tree[pid]['remains'] - len(observation),
                            'path_from_par': [frag_to_map[step] for step in subtree_branch['path_from_par']], # only nid=0 would be wrong
                            'path_from_root': branch['path_from_root'][:-1] + [frag_to_map[step] for step in subtree_branch['path_from_root']],
                            'celldistances': observation,
                            'children': set(),
                            'pid': pid,
                            'depth': tree[pid]['depth'] + 1,
                            'map': starting_map, # the map this node started with before its own observations
                            'copies_explored': [corners for corners in tree[pid]['copies_explored']],
                        }
                        stitched_branch['steps_from_par'] = len(stitched_branch['path_from_par'])
                        stitched_branch['steps_from_root'] = len(stitched_branch['path_from_root'])
                        tree[pid]['children'].add(new_node)
                        # print(f"adding child {new_node} to {pid} in global tree (recursive stitching).")
                        tree[new_node] = stitched_branch
                        if not subtree_branch['children']:
                            # the copy is fully explored, proceed with an ordinary (global) search
                            stitched_branch['copies_explored'].append(copy['top left'])
                            # when popped from the global searches queue, will need map updated with observations. 
                            updated_map = update_map(starting_map, tree[pid]['pos'], pos) 
                            agenda.append(
                                (new_node, updated_map),
                            )
                        else:
                            # exploration continues within the subtree
                            for child in subtree_branch['children']:
                                stitch(child, new_node)
                stitch(0)
                continue # global exploration continues not from this node, but from the leaves of the subtree which have been added to the agenda.
            else:
                # This case should only occur if we start in a fragment.
                # Then it is probably reasonable to plan normally,
                # because the true map may not match the fragment exactly.
                pass
        # else:
        #     print(f"{pos} not in segmentation")
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
                        'copies_explored': [corner for corner in tree[node]['copies_explored']]
                    }

            new_node = max(tree)+1
            agenda.append((new_node, update_map(updated_map, path[0], path[-1])))

            tree[node]['children'].add(new_node)
            # print(f"adding child {new_node} to {node} in global tree (global search).")
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
