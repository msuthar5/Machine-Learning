"""basic.py: This simple project is useful for getting acquaited with
   python based data analysis. Utilizing the college.csv file, we perform
   some simple data frame manipulations and produce scatterplots, 
   point plots, line plots, and histograms"""

__author__      = "Manish Suthar"
__copyright__   = "InteliSON LLC 2017, Data-Science with Python"

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

data_file = pd.read_csv('college.csv')

college_data = pd.DataFrame(data_file)
row_names = college_data['Unnamed: 0']

x = college_data.drop('Unnamed: 0',1)

data = pd.DataFrame(x)
data = data.rename(index=row_names)


# Create scatterplot matrix of first 10 columns
sns.set_style('ticks')
sns.pairplot(pd.DataFrame(data.iloc[:,1:10]))
plt.show()

private = data.iloc[:, 0]
out_state = data.iloc[:,8]
in_state = np.arange(778)

private = pd.Series.as_matrix(private)
out_state = pd.Series.as_matrix(out_state)
out_state = np.array(out_state, dtype='int32')
private = np.array(private)

'''sns.boxplot(x=private,
            y=out_state,
            hue=private,
            palette=sns.color_palette("muted"), orient="v"
           )

sns.set_style("whitegrid")
plt.show()'''

# Boxplot of private schools and their tuition
bp = sns.boxplot(data=data, x="Private", y='Outstate', order=["No", "Yes"])
bp.set(xlabel="Private School", ylabel="Out of State Tuition",)
plt.title("Private vs Public Tuition")
bp.axes.set_ylim(5000, 25000, 3000)
plt.show()

# create en elite field binning the top10perc attribute
# where schools with >50 top10perc students are considered elite
elite = np.array(data.iloc[:,4] > 50)
data['Elite'] = elite
data.loc[:,'Elite'].describe()

elite_outstate_bp = sns.boxplot(data=data,
                                x="Elite",
                                y="Outstate")
elite_outstate_bp.set(xlabel="Elite School",
                      ylabel="Out Of State Tuition",)
plt.title("Elite School vs Non-Elite School Tuition")

plt.show()

#Split up frame to private schools and non private schools
private_schools = data[data.Private == 'Yes']

# CAN ALSO DO: private_schools = data[data['Private'] == 'Yes']

non_private = data[data.Private == 'No']

# Histogram: Private School Apps recieved
priv_apps = sns.distplot(private_schools.iloc[:,1])
priv_apps.set(xlabel="Number of applications recieved for private schools")
priv_apps.axes.set_ylim(0,0.00009)
plt.show()

# Private school apps received vs accepted

priv_apps_rec_vs_accept = sns.pointplot(data=private_schools,
                                        x='Accept', y='Enroll')
priv_apps_rec_vs_accept.set(xlabel="# Applications Accepted",
                            ylabel="# Applications Recieved")
plt.title("Private School # Apps Received vs # Apps Accepted")
plt.show()

# Line plot: Non-private school apps received vs accepted

non_priv_apps_rec_vs_accept = sns.lmplot(x='Accept', y='Enroll',
                                           data=non_private)
non_priv_apps_rec_vs_accept.set(xlabel="# Applications Accepted",
                            ylabel="# Applications Recieved")

plt.title("Public School # Apps Received vs # Apps Accepted")
plt.show()