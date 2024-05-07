
import numpy as np
import pickle
import csv
import os
import pprint
import datetime

import tree_builder

pp = pprint.PrettyPrinter(compact=False, width=90)


def path_to_node(path, maze_name):
    """ 
    given path (list of tuples of x,y), covert to list of tree nodes
    """

    maze, exit_pos = tree_builder.read_maze(maze_name)

    tree_ = TREE[maze_name]
    nodes = [TREE[maze_name]['root']]

    temp = [] # collect sub paths

    for pos in path:
        temp.append(pos)

        for cid in tree_[nodes[-1]]["children"]:

            # this will be compared against temp
            path_from_par = tree_[cid]["path_from_par"]

            # only check first and last node of temp with parth_from_par
            if temp[0]==path_from_par[0] and temp[-1]==path_from_par[-1]:
                nodes.append(cid)
                temp = [temp[-1]]
                break # once we find a node, we can ignore the rest of cid's
        
        # exit was visible at this node
        if exit_pos in tree_builder.observation_made(maze, pos):
            break

    return nodes


def get_subjects():

    subjects = []

    if EXPERIMENT == 1:

        # collect subjects ids
        return [file.split('.txt')[0] for file in os.listdir(f'__experiment_{EXPERIMENT}/att-test')
                                   if 'sequence' not in file]

    elif EXPERIMENT == 2:

        with open(f'__experiment_{EXPERIMENT}/att-test.csv') as csvfile:

            csv_reader = csv.DictReader(csvfile, delimiter='\t')

            return {row['subject'] for row in csv_reader}

    elif EXPERIMENT == 3:

        # mturk batch names
        batch_names = {'maze_game_21', 'maze_game_22', 'maze_game_23', 'maze_game_24', 'maze_game_24_', 
                 'maze_game_25', 'maze_game_26'}

        for batch_name in batch_names:
            result_summary = csv.DictReader(open(f"__experiment_{EXPERIMENT}/mturk_csv/{batch_name}.csv", 'r'))
            # subjects += [(row['Answer.surveycode'], row['WorkerId']) for row in result_summary]
            subjects += [row['Answer.surveycode'] for row in result_summary]
            
        return subjects
        
    elif EXPERIMENT == 4:

        # # mturk batch names
        # batch_names = {'maze_game_25', 'maze_game_26'} # old
        batch_names = {'maze_game_32', 'maze_game_34', 'maze_game_35', 'maze_game_36'}

        for batch_name in batch_names:
            result_summary = csv.DictReader(open(f"__experiment_{EXPERIMENT}/mturk_csv/{batch_name}.csv", 'r'))
            subjects += [row['Answer.surveycode'] for row in result_summary]
            
        return subjects


def subject_decisions_from_txt_log():

    subjects = get_subjects()

    summary = {} # {sid: {practice:_, dur:_, age:_, gender:_, crt_score:_, comments:_, steps:_}}
    decisions = {} # {sid: {world: {path:_, nodes: includes root and leaf nodes}}}
    trial_sequence = {} # {sid: [sequence of trials]}

    for sid in subjects:

        summary[sid] = {}
        decisions[sid] = {}

        with open(f'__experiment_{EXPERIMENT}/att-test/{sid}.txt', "r") as log:
            lines = list(log.readlines())

        # ugh someone (S60093941) forgot to answer everything..
        if "Age" not in lines[-1] and sid not in {'S60093941', 'S45348992'}:
            print('omg?', sid)
            del summary[sid]
            del decisions[sid]
            continue

        # read first line for start timestamp
        _, date, time, _, sid, _, steps, path, _  = lines[0].split()

        start_I = datetime.datetime.strptime( f"{date} {time}", "%d/%B/%Y %I:%M:%S" )  # start time
        start_H = datetime.datetime.strptime( f"{date} {time}", "%d/%B/%Y %H:%M:%S" )  # start time

        summary[sid]["practice"] = 0

        for line in lines:

            if "solvingquiz" in line:
                # NOTE not practice key if subject did not do practice
                _, date, time, _, _, task, ans  = line.split()
                summary[sid]["practice"] += 1

            elif "webfile/easy" in line or "webfile/medium" in line or "webfile/hard" in line:
                # ip, date, time, _, sid, world, steps, path, _
                _, date, time, _, sid, world, steps, path, _  = line.split()

                world = world.split('/')[-1].split('.txt')[0]

                if world in trial_sequence.get(sid, []):
                    continue

                # XXX
                if world not in TREE:
                    print('here??', world)
                    continue

                trial_sequence.setdefault(sid, []).append(world)

                path = [eval(pos) for pos in path.replace(';','').split('p')[1:]]
                # NOTE XXX att-test log texts record position with (col, row), so need to switch it to (row,col)
                path = [(row,col) for col,row in path]

                nodes = path_to_node(path, world)
                decisions[sid][world] = {"path":path, "nodes":nodes}

            elif "decision2" in line:
                _, date, time, _, _, task, *ans  = line.split()
                summary[sid]["comment"] = ' '.join(ans)

            elif "CRT1" in line:
                _, date, time, _, _, task, *ans  = line.split()
                summary[sid]["crt_score"] = 1 if set(ans[:2]) & {"6", "6hours"} else 0
                summary[sid].setdefault("crt_ans", []).append(ans)
            
            elif "CRT2" in line:
                _, date, time, _, _, task, *ans  = line.split()
                summary[sid]["crt_score"] += 1 if set(ans[:2]) & {"10", "10hours", "10.00"} else 0
                summary[sid].setdefault("crt_ans", []).append(ans)

            elif "CRT3" in line:
                _, date, time, _, _, task, *ans  = line.split()
                summary[sid]["crt_score"] += 1 if set(ans[:2]) & {"1", "1dollar", "$1", "1.00"} else 0
                summary[sid].setdefault("crt_ans", []).append(ans)

            elif "Age" in line:
                _, date, time, _, _, age, _, gender  = line.split()
                summary[sid].update({"age":int(age), "gender":gender[0].upper()})

        summary[sid]["steps"] = eval(steps)

        end_I = datetime.datetime.strptime( f"{date} {time}", "%d/%B/%Y %I:%M:%S" ) # end time
        end_H = datetime.datetime.strptime( f"{date} {time}", "%d/%B/%Y %H:%M:%S" ) # end time

        # I need to do this because because AM PM isn't specified
        dur = min(round((end_I-start_I).seconds / 60, 3), round((end_H-start_H).seconds / 60, 3))
        summary[sid]["duration"] = dur # duration in minutes
    
    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_stats.pickle', 'wb') as handle:
        pickle.dump(summary, handle, protocol=pickle.HIGHEST_PROTOCOL)

    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_decisions.pickle', 'wb') as handle:
        pickle.dump(decisions, handle, protocol=pickle.HIGHEST_PROTOCOL)

    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_trial_sequence.pickle', 'wb') as handle:
        pickle.dump(trial_sequence, handle, protocol=pickle.HIGHEST_PROTOCOL)    


def subject_decisions_from_csv_log():

    subjects = get_subjects()

    summary = {} # {sid: {practice:_, dur:_, age:_, gender:_, crt_score:_, comments:_, steps:_}}
    decisions = {} # {sid: {world: {path:_, nodes: includes root and leaf nodes}}}
    trial_sequence = {} # {sid: [sequence of trials]}

    for sid in subjects:

        summary[sid] = {}
        decisions[sid] = {}

        summary[sid]["practice"] = 1
        summary[sid]["steps"] = 0
        
        for key in ['age', 'dur', 'gender', 'crt_score']:
            summary[sid][key] = None # not in data that Marta shared
    
    with open(f'__experiment_{EXPERIMENT}/att-test.csv') as csvfile:

        csv_reader = csv.DictReader(csvfile, delimiter='\t')

        for row in csv_reader:

            sid = row['subject']
            maze_name = row['maze']

            if maze_name in trial_sequence.get(sid, []):
                continue

            trial_sequence.setdefault(sid, []).append(maze_name)

            # NOTE XXX att-test log texts record position with (col, row), so need to switch it to (row,col)
            path = row['path']

            path = [eval(pos) for pos in path.replace(';','').split('p')[1:]]
            path = [(row,col) for col,row in path]

            nodes = path_to_node(path, maze_name)
            decisions[sid][maze_name] = {"path":path, "nodes":nodes}

            summary[sid]["steps"] += eval(row['steps'])


    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_stats.pickle', 'wb') as handle:
        pickle.dump(summary, handle, protocol=pickle.HIGHEST_PROTOCOL)

    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_decisions.pickle', 'wb') as handle:
        pickle.dump(decisions, handle, protocol=pickle.HIGHEST_PROTOCOL)

    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_trial_sequence.pickle', 'wb') as handle:
        pickle.dump(trial_sequence, handle, protocol=pickle.HIGHEST_PROTOCOL) 


def subject_decisions():

    # raw att-test exists
    if os.path.isdir(f'__experiment_{EXPERIMENT}/att-test'):
        subject_decisions_from_txt_log()

    # log file in csv file (Marta doesn't have raw logs anymore)
    elif os.path.isfile(f'__experiment_{EXPERIMENT}/att-test.csv'):
        subject_decisions_from_csv_log() 


def node_stats():
    """
    read from subject_decisions and collect count and ratio
    note that only nodes that were chosen at least by subjects is recorded here
    """

    summary = {} # { (world, nid): {"count":_, "ratio":_} }
    subjects = get_subjects()

    # collect count
    for sid in subjects:
        for maze_name in DECISIONS[sid]:
            for nid in DECISIONS[sid][maze_name]["nodes"]:
                summary.setdefault((maze_name, nid), {}).setdefault('count',0)
                summary[(maze_name,nid)]['count'] += 1

    # collect ratio
    for maze_name, nid in summary:

        pid = TREE[maze_name][nid]['pid']

        if pid in {None, 'NA'}: # instead of 'NA'
            summary[(maze_name,nid)]["ratio"] = 1
        else:
            summary[(maze_name,nid)]["ratio"] = round(summary[(maze_name,nid)]["count"]/summary[(maze_name,pid)]["count"], 2)

    with open(f"__experiment_{EXPERIMENT}/pickled_data/node_values_Humans.pickle", "wb") as handle:
        pickle.dump(summary, handle, protocol=pickle.HIGHEST_PROTOCOL)


def assign_bonuses():

    # subject statistics

    steps_data = [SUBJECT_STATS[sid]['steps'] for sid in SUBJECT_STATS]

    print(min(steps_data), np.percentile(steps_data, 20), np.percentile(steps_data, 50), np.percentile(steps_data, 70), max(steps_data))

    for sid in SUBJECT_STATS:

        steps = SUBJECT_STATS[sid]['steps']
        bonus = 3 if steps <= 340 else 2 if steps <= 390 else 1 if steps <= 450 else 0
        print(f"subject: {sid}, bonus: {bonus}, practice: {SUBJECT_STATS[sid]['practice']}")


def subject_stats_summary():

    import matplotlib.pyplot as plt
    tick_font_size = 13
    label_font_size = 11

    steps, duration, age, gender, practices = [], [], [], {'F': 0, 'M': 0, '': 0}, {}

    for info in SUBJECT_STATS.values():

        steps.append(info['steps'])
        duration.append(info['duration'])
        age.append(info.get('age', 0))
        gender[info.get('gender', '')] += 1

        if info['practice'] not in practices:
            practices[info['practice']] = 0
        practices[info['practice']] += 1
    
    fig, axs = plt.subplots(2,2)
    axs = axs.flat
    
    axs[0].hist(age, edgecolor = 'white', bins = 12, color='cornflowerblue')
    axs[0].set_title(f'Age Distribution (M={np.median(age)}, SD={round(np.std(age))})')
    axs[0].set_xlabel('Age', fontsize=label_font_size)
    axs[0].set_ylabel('Number of Subjects', fontsize=label_font_size)
    axs[0].xaxis.set_tick_params(labelsize=tick_font_size)
    axs[0].yaxis.set_tick_params(labelsize=tick_font_size)

    x_values = list(practices)
    heights = [practices[val] for val in x_values]
    axs[1].bar(x_values, heights, width=0.5)
    axs[1].set_title(f'Practice')
    axs[1].set_xlabel('Number of Practice Rounds', fontsize=label_font_size)
    axs[1].set_ylabel('Number of Subjects', fontsize=label_font_size)
    axs[1].xaxis.set_tick_params(labelsize=tick_font_size)
    axs[1].yaxis.set_tick_params(labelsize=tick_font_size)

    # axs[2].bar(['FEMALE', 'MALE'], [gender['F'], gender['M']], width=0.3)
    # axs[2].set_title(f'Gender')

    axs[2].grid(True, zorder=0, color = "grey", linewidth = "0.5", linestyle = "--")
    axs[2].hist(duration, edgecolor = 'white', bins = 12, zorder=3)
    axs[2].set_title(f'Experiment Time (M={round(np.median(duration))}, SD={round(np.std(duration))})')
    axs[2].set_xlabel('Duration (minutes)', fontsize=label_font_size)
    axs[2].set_ylabel('Number of Subjects', fontsize=label_font_size)
    axs[2].xaxis.set_tick_params(labelsize=tick_font_size)
    axs[2].yaxis.set_tick_params(labelsize=tick_font_size)

    axs[3].hist(steps, edgecolor = 'white', bins = 12, color='cornflowerblue')
    axs[3].set_title(f'Steps Distribution (M={np.median(steps)}, SD={round(np.std(steps))})')
    axs[3].set_xlabel('Number of Steps', fontsize=label_font_size)
    axs[3].set_ylabel('Number of Subjects', fontsize=label_font_size)
    axs[3].xaxis.set_tick_params(labelsize=tick_font_size)
    axs[3].yaxis.set_tick_params(labelsize=tick_font_size)

    print(gender['F'], gender['M'])

    plt.tight_layout()
    plt.show()



if __name__ == "__main__":

    EXPERIMENT = 4

    # print(len(get_subjects()))

    #with open(f'__experiment_{EXPERIMENT}/pickled_data/tree.pickle', 'rb') as handle:
    #    TREE = pickle.load(handle)
    
    # subject_decisions()

    # with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_decisions.pickle', 'rb') as handle:
    #     DECISIONS = pickle.load(handle)

    with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_stats.pickle', 'rb') as handle:
         SUBJECT_STATS = pickle.load(handle)

         subject_stats_summary()

    # with open(f'__experiment_{EXPERIMENT}/pickled_data/subject_trial_sequence.pickle', 'rb') as handle:
    #     MAZE_SEQUENCE = pickle.load(handle)
        
    # node_stats()

    # subject_stats_summary()
    # assign_bonuses()

    #with open(f'__experiment_{EXPERIMENT}/pickled_data/node_values_Humans.pickle', 'rb') as handle:
    #    NODESTATS = pickle.load(handle)
