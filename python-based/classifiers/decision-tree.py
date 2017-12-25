import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn import tree # Decision tree classifier
import graphviz
import numpy as np
import pandas as pd
import seaborn as sns
import os

# Set seed
np.random.seed(100)

data_file = pd.read_csv("IPLD.csv")
data = pd.DataFrame(data_file)

# Rename columns
data.columns = ['age', 'sex', 'tb', 'db', 'aap', 'sgpaa', 'sgoaa', 'tp', 'alb', 'ag', 'label']

target = data.label

d = data
 
 # Change sex to boolean so we have all numerical attributes
for i in range(0, len(d)):
    if d.iloc[i,1] == 'Male':
        d.iloc[i,1] = np.int32(1)
    else:
        d.iloc[i,1] = np.int32(0)
        

# Convert sex field from type object to type int8 for DecisionTreeClassifier()
d.iloc[:,1] = d.iloc[:,1].astype('int8')

# Split into test and train data 80/20 split
test_data, train_data = train_test_split(d,test_size=0.2)

# verify that the test and train data are not mixed at all

s1 = set(list(test_data.index))
s2 = set(list(train_data.index))
intersection = s1.intersection(s2)

if len(intersection) != 0:
    print("Error training and test data mixed")
else:
    print("Completed 80/20 split of training and test data")
    print("Data preprocessing complete")


test_data_label = test_data.label
train_data_label = train_data.label

td = train_data.iloc[:,1:10]

classifier = tree.DecisionTreeClassifier()
classifier.fit(td, train_data_label)

dot_data = tree.export_graphviz(classifier, out_file=None) 
graph = graphviz.Source(dot_data) 
graph.render("ipld.gv")
print(graph)
