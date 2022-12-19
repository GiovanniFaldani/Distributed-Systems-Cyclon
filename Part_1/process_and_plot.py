import numpy as np
import matplotlib.pyplot as plt
import re

File = open("./data/results1000_100_50.csv")

Filestring = File.read()


# Average discovery proportion
DiscProps = Filestring.split('[')[1].split(']')[0].split(',')
DiscProps = [float(i) for i in DiscProps]
AvgDiscProp = np.mean(DiscProps)
print('Average Discovery Proportion: ', AvgDiscProp,'\n')

# Average exit node churn resilience

# get the PIDs of every node
ChildrenPIDs = Filestring.split('[')[2].split(']')[0].split(',')
for i in range(len(ChildrenPIDs)):
    ChildrenPIDs[i] = ChildrenPIDs[i].strip()
#print('Children PIDs:', ChildrenPIDs, '\n')

N = len(ChildrenPIDs)

#get the list of turns when a node exits
TurnSinceInactive = Filestring.split('[')[3].split(']')[0].split(',')
TurnSinceInactive = [int(i) for i in TurnSinceInactive]
#print('Turn since exiting nodes have been inactive:', TurnSinceInactive, '\n')

# get the list of views per turn for every node
BigList = Filestring.split('[[[')[1].split(']]]')[0].split(',')
ViewsList = []
SingleView = []
NodeViews = []
for i in range(len(BigList)):
    BigList[i] = BigList[i].strip()
    BigList[i] = BigList[i].strip('[')
    BigList[i] = BigList[i].strip('[[')

    if ']]' in BigList[i] or i == len(BigList)-1: #contains ]]
        BigList[i] = BigList[i].strip(']]')
        SingleView.append(BigList[i])
        NodeViews.append(SingleView)
        ViewsList.append(NodeViews)
        SingleView = []
        NodeViews = []

    elif ']' in BigList[i]: #contains ]
        BigList[i] = BigList[i].strip(']')
        SingleView.append(BigList[i])
        NodeViews.append(SingleView)
        SingleView = []
        
    else:
        SingleView.append(BigList[i])

#print('List of views of the first node: ', ViewsList[0], '\n')

# ViewsList[node][turn] = View of that node at that turn

# compute average exit node churn resilience

ChurnRes = []
for i in range(len(TurnSinceInactive)):
    if TurnSinceInactive[i] != -1:
        Turn = TurnSinceInactive[i]-1
        PID = ChildrenPIDs[i]
        hasExited = True
        while(Turn < 10):
            counter = 0
            for j in range(len(ViewsList)):
                if Turn < len(ViewsList[j]):
                    if PID in ViewsList[j][Turn]:
                        counter += 1

            if counter <= 0.25 * N: # node is only in less than 25% of views
                ChurnRes.append((PID,Turn))
                Turn = 10
            else:
                Turn += 1

#print("Churn Resilience of every exiting node: ", ChurnRes, "\n")

AvgChurnRes = np.mean([Tuple[1] for Tuple in ChurnRes])

print("Average Churn Resilience: ", AvgChurnRes, "\n")