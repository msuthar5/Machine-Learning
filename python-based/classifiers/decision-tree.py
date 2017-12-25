import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn import tree# Decision tree classifier
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

# Split into training and test data with 80 : 20 split
train_data, test_data = train_test_split(data,test_size=0.2)
train_label = train_data.label
test_label = test_data.label

# verify that the test and train data are not mixed at all

s1 = set(list(test_data.index))
s2 = set(list(train_data.index))
intersection = s1.intersection(s2)

if len(intersection) != 0:
    print("Error training and test data mixed")
else:
    print("Completed 80/20 split of training and test data")
    print("Data preprocessing complete")

d = data

 # Change sex to boolean so we have all numerical attributes

for i in range(0, len(d)):
    if d.iloc[i,1] == 'Male':
        d.iloc[i,1] = np.int32(1)
    else:
        d.iloc[i,1] = np.int32(0)
d.iloc[:,1] = d.iloc[:,1].astype('int8')

test_data, train_data = train_test_split(d,test_size=0.2)

test_data_label = test_data.label
train_data_label = train_data.label

# Define classifier
classifier = tree.DecisionTreeClassifier()
classifier.fit(train_data, train_data_label)

dot_data = tree.export_graphviz(classifier, out_file=None)
graph = graphviz.Source(dot_data)
graph.render("ipld")
print(graph)
