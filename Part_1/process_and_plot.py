import numpy as np
import matplotlib.pyplot as plt
import re

File = open("./data/results100_10_5.csv")

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
print('Children PIDs:', ChildrenPIDs, '\n')

# get the list of views per turn for every node
BigList = Filestring.split('[[[')[1].split(']]]')[0].split(',')
ViewsList = []
for i in range(len(BigList)):
    BigList[i] = BigList[i].strip()
    print(BigList[i])
    ViewsOfNode = BigList[i]
    ViewsOfNode = ViewsOfNode.split(']')[0].split(',')
    print(ViewsOfNode)
    ViewsListOfNode = []
    for j in range(len(ViewsOfNode)):
        ViewsOfNode[j] = ViewsOfNode[j].strip()
        CurrentView = ViewsOfNode[j]
        #CurrentView = CurrentView.split('[')[1].split(']')[0].split(',')
        for k in range(len(CurrentView)):
            CurrentView[k] = CurrentView[k].strip()

        ViewsListOfNode.append(CurrentView)

    ViewsList.append(ViewsListOfNode)

print(ViewsList[0])