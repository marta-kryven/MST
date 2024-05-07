import pprint
import time
import random
import numpy as np
import pickle
import matplotlib.pyplot as plt


EXPERIMENT = 1

with open(f'__experiment_{EXPERIMENT}/pickled_data/tree.pickle', 'rb') as handle:
    TREE = pickle.load(handle)


with open(f'__experiment_{EXPERIMENT}/pickled_data/node_values_Humans.pickle', 'rb') as handle:
    # {(world, nodes): {ratio:_, count:_}}
    NODE_STATS = pickle.load(handle)


# parameters for the sampling MCTS model - grid coverage is similar to geomspace, but manual because we need integers
BUDGET = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16, 20, 30, 40, 70, 100, 200, 500, 1000]
EXPLORE_C = [ 1, 2, 3, 4, 5, 7, 9, 11, 14, 18, 26, 35]

Model_Parameters  = [(bdgt, c) for bdgt in BUDGET for c in EXPLORE_C]

def softmax(values, tau):
    """ small values are better, tau=1 is optimal
    large tau converges to random agent """

    numer = [np.exp(-v * (1/tau)) for v in values]
    denom = sum(numer)
    return [n/denom for n in numer]


class MonteCarloTreeSearch:

    def __init__(self, world, root):

        self.env = TREE[world]
        self.root = root
        self.mcts_tree = {root: {'n_visited': 1,
                                 'reward': 0,
                                 'normalized_reward': 0, # reward/n_visited
                                 'parent': None,
                                 'children':[],}}

    def node_reward(self,node):
        """ return reward at state (sum of steps-from-root and exp steps-to-exit) """

        # root state has 0 reward
        if self.env[node]['pid'] == 'NA': 
            return 0

        reward = -self.env[node]['steps_from_root']

        cell_dist = self.env[node]['celldistances']
        curr_r, curr_c = self.env[node]['pos']

        cell_dist = [ abs(rr-curr_r) + abs(cc - curr_c) for  rr, cc in cell_dist]

        reward -= sum(cell_dist)/len(cell_dist)

        return reward

    def traverse(self,node,c): 
        """ recursively travel down the mcts tree """

        # NOTE p=1 when state is terminal
        # NOTE reached root node -> must expand from here
        if node == self.root:
            p = 0
        else:

            observed = len(self.env[node]['celldistances'])
            blacks = self.env[self.env[node]['pid']]['remains']

            p = observed/blacks

        # terminate = 1 (= YES, terminate) with probability p
        terminate = np.random.binomial(1,p)

        if 1 - terminate: # don't terminate early

            if self.mcts_tree[node]['children']: # children exist in mcts tree
                node = self.best_leaf(node,c)
                return self.traverse(node,c)

        return node # children does not exist in mcts tree or terminate early

    def rollout(self,node):
        """ random rollout and return reward from final node """

        episode = []

        while True:

            # NOTE p=1 when state is terminal
            observed = len(self.env[node]['celldistances'])
            blacks = self.env[self.env[node]['pid']]['remains']

            p = observed/blacks

            # terminate = 1 (= YES, terminate) with probability p
            terminate = np.random.binomial(1,p)

            episode.append((node,p,terminate))
 
            if terminate:
                return episode, self.node_reward(node)

            node = random.choice(list(self.env[node]["children"]))
            
    def expand(self,node):
        """ add children of node to mcts tree """

        for leaf in self.env[node]["children"]:

            if leaf != node:
                self.mcts_tree[node]['children'].append(leaf)
                self.mcts_tree[leaf] = {'n_visited': 0,
                                        'reward': 0,
                                        'parent': node,
                                        'children':[], }

    def backprop(self,node,reward):
        """ recursively update reward and n_visited by travelling up the mcts tree """
        
        self.mcts_tree[node]['reward'] += reward
        self.mcts_tree[node]['n_visited'] += 1
        self.mcts_tree[node]['normalized_reward'] = self.mcts_tree[node]['reward'] / self.mcts_tree[node]['n_visited']
        node = self.mcts_tree[node]['parent']

        if node is not None: # root node as parent None
            self.backprop(node,reward)

    def ucb1(self,ri,ni,n,c):
        return ri/ni + c*(np.log(n)/ni)**0.5 if ni else float("inf")

    def best_leaf(self,node,c):
        """ returns best child of node, break tie randomly """

        n = self.mcts_tree[node]['n_visited']
        children = self.mcts_tree[node]['children']
        
        if not children:
            return node

        # dict summary of ucb1 value to list of children with that ucb1 value 
        # ... {ucb1 value : list of children}
        ucb_to_child = {}

        for child in children:

            ri = self.mcts_tree[child]['reward']
            ni = self.mcts_tree[child]['n_visited']

            ucb_to_child.setdefault(self.ucb1(ri,ni,n,c), []).append(child)
        
        return random.choice(ucb_to_child[max(ucb_to_child)])

    def run(self,budget,c):

        # self.summary = {} # {epoch: {nid: value}}

        # step 0: initiate root
        # NOTE for some reason expanding from self.root at the beginning is critical
        # NOTE use combination of self.expand(self.root) and n_visited=1
        self.expand(self.root)

        for i in range(1,budget+1):

            # traverse mcts_tree to find leaf node
            leaf = self.traverse(self.root,c)
            reward = self.node_reward(leaf)

            # expand if the leaf node has been visited once
            if self.mcts_tree[leaf]['n_visited']==1:
                self.expand(leaf)
                leaf = self.best_leaf(leaf, c) # picks randomly from newly expanded leaves

                # rollout if the leaf node has NOT been visited
                _, reward = self.rollout(leaf)

            # step 4: backprop reward from rollout
            self.backprop(leaf,reward)

            # self.summary[i] = self.mcts_tree
            yield i, self.mcts_tree


# corr coeff ~ 0.5
def node_value_for_one_node_ratio(world, nid, budget, c):

    mcts = MonteCarloTreeSearch(world, nid)
    children = TREE[world][nid]['children']

    summary = {} # {budget: {(world, cid): value}}
    
    for bdgt, mcts_tree in mcts.run(budget):

        for cid in children:

            # choice probability
            value = mcts_tree[cid]['n_visited']/(mcts_tree[nid]['n_visited']-1)
            summary.setdefault(bdgt, {}).update({(world,cid): value})
    
    return summary[bdgt]


# corr coeff ~ 0.7
def node_value_for_one_node_epochs(world, nid, budget, c):

    # mcts = MonteCarloTreeSearch(world, nid)
    children = TREE[world][nid]['children']

    summary = {bdgt: {(world, cid):0 for cid in children} for bdgt in range(1, budget+1)} # {budget: {(world, cid): value}}

    for _ in range(300):

        mcts = MonteCarloTreeSearch(world, nid)
    
        for bdgt, mcts_tree in mcts.run(budget,c):

            best_cid = mcts.best_leaf(nid, c)
            summary[bdgt][world, best_cid] += 1/200
    
    return summary[bdgt]


# corr coeff ~ 0.6
def node_value_for_one_node_softmax(world, nid, budget, c, tau):

    mcts = MonteCarloTreeSearch(world, nid)
    children = TREE[world][nid]['children']

    summary = {} # {budget: {(world, cid): value}}
    
    for bdgt, mcts_tree in mcts.run(budget):

        children_values = [(cid, -mcts_tree[cid].get('normalized_reward', 0)) for cid in children]
        children, values = zip(*children_values)

        values = softmax(values, tau)

        summary[bdgt] = {(world,cid): val for cid, val in zip(children, values)}

    return summary[bdgt]


# corr coeff ~ 0.5
def node_value_for_one_node_avgvalue(world, nid, budget, c, tau):

    mcts = MonteCarloTreeSearch(world, nid)
    children = TREE[world][nid]['children']

    summary = {bdgt: {(world, cid):0 for cid in children} for bdgt in range(1, budget+1)} # {budget: {(world, cid): value}}

    for _ in range(200):
    
        for bdgt, mcts_tree in mcts.run(budget):

            best_cid = mcts.best_leaf(nid)
            summary[bdgt][world, best_cid] += mcts_tree[best_cid].get('normalized_reward', 0)/200

    for bdgt in summary:

        children_values = [(cid, summary[bdgt][world, cid]) for cid in children]
        children, values = zip(*children_values)
        values = softmax(values, tau)
        summary[bdgt] = {(world, cid): val for cid, val in zip(children, values)}
    
    return summary[bdgt]


def node_values_summary(budget, explore_c):

    summary = {} # {nid: {(param): {cid: value, cid: value, ...}}}

    for world in TREE:
        print(world)
        for nid in TREE[world]:

            if nid == 'root':
                continue

            children = TREE[world][nid]['children']

            if len(children) <= 1:
                continue 

            summary[world, nid] = {(bdgt, explore_c): {cid: 0 for cid in children} for bdgt in range(1,budget+1)}

            for _ in range(200):

                mcts = MonteCarloTreeSearch(world, nid)
            
                for bdgt, mcts_tree in mcts.run(budget, explore_c):

                    best_cid = mcts.best_leaf(nid, explore_c)
                    summary[world, nid][bdgt, explore_c][best_cid] += 1/200
    
    return summary


def pickle_node_value_summaries():

    for bdgt, c in Model_Parameters:
        print(bdgt, c)

        # tbd what is going on inside node_values_summary? the code below passes the highest budget?
        summary = node_values_summary(bdgt, c) # {nid: {(param): {cid: value, cid: value, ...}}}
        _summary = {}

        for world, nid in summary:
                _summary.setdefault(world, {})
                _summary[world].update({nid: {(bdgt, c): {}}})

                for cid in summary[world, nid][bdgt, c]:
                    _summary[world][nid][bdgt, c][cid] = summary[world, nid][bdgt, c][cid]

        with open(f'__experiment_{EXPERIMENT}/node_values_recursive/Sampling/node_values_{bdgt, c}.pickle', 'wb') as handle:
                pickle.dump(_summary, handle, protocol=pickle.HIGHEST_PROTOCOL)
    

def plot_human_sample_correlation(budget, explore_c):

    values_summary = node_values_summary(budget, explore_c)

    # human vs epochs
    value_humans, value_epoch = [], []

    for world, nid in NODE_STATS:

        if NODE_STATS[world, nid]['count'] < 20:
            continue

        if len(TREE[world][nid]['children']) <= 1:
            continue

        children = TREE[world][nid]['children']

        if sum(NODE_STATS.get((world, cid), {}).get('ratio', 0) for cid in children) == 0:
            continue

        print('-----------------------------------')
        print(NODE_STATS[world, nid])

        # summary_epochs = node_value_for_one_node_epochs(world, nid, budget, explore_c)

        for cid in children:
            value_humans.append(NODE_STATS.get((world, cid), {}).get('ratio', 0))
            # value_epoch.append(summary_epochs[world, cid])
            value_epoch.append(values_summary[world,nid][budget, explore_c][cid])


    corr_coeff = round(np.corrcoef(value_humans, value_epoch)[0][1], 3)
    plt.plot(value_humans, value_epoch, 'o', markersize=3, alpha=0.7)
    plt.title(f'corr coeff = {corr_coeff}, budget = {budget}, c = {explore_c}')
    plt.xlabel('humans')
    plt.ylabel('sample based')
    plt.gca().set_aspect('equal')
    plt.show()



if __name__ == "__main__":
    pp = pprint.PrettyPrinter(compact=False, width=80)
    
    # BUDGET, EXPLORE_C = 8, 5
    # BUDGET, EXPLORE_C = 70, 10

    # plot_human_sample_correlation(BUDGET, EXPLORE_C)
    # node_values_summary(BUDGET, EXPLORE_C)
    pickle_node_value_summaries()
