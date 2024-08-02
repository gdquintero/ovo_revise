import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import models

def plot_solutions(ind,df_seropositives,df_sol,sero_outliers,noutliers):
    t = np.linspace(0,70,1000)
    disease = [r"Measles",r"Mumps",r"Rubella"]
    plt.rcParams.update({'font.size': 12})
    plt.rc('text', usetex=True)
    plt.rc('font', family='serif')
    # plt.xticks(fontsize=18)
    # plt.yticks(fontsize=18)
    plt.ylim([0,1.05])
    plt.plot(t,models.F(t,*df_sol.iloc[0].values))
    plt.plot(sero_outliers[0],sero_outliers[1],'ro',mfc='none',ms=10)

    for i in range(noutliers):
        point1 = [sero_outliers[0,i],models.F(sero_outliers[0,i],*df_sol.iloc[0].values)]
        point2 = [sero_outliers[0,i],sero_outliers[1,i]]
        x_values = [point1[0], point2[0]]
        y_values = [point1[1], point2[1]]
        plt.plot(x_values, y_values, 'k', linestyle="--")

    l = plt.plot(df_seropositives[0].values,df_seropositives[ind].values,"ko")
    plt.setp(l, 'markersize', 6)
    # plt.title(disease[ind-1],fontsize = 18)
    plt.savefig(disease[ind-1]+".pdf",bbox_inches = "tight")
    plt.show()

df_seropositives = pd.read_table("output/seropositives.txt",delimiter=" ",header=None,skiprows=1)
df_mixed_measles = pd.read_table("output/solutions_mixed_measles.txt",delimiter=" ",header=None,skiprows=0)
df_mixed_mumps   = pd.read_table("output/solutions_mixed_mumps.txt",delimiter=" ",header=None,skiprows=0)
df_mixed_rubella = pd.read_table("output/solutions_mixed_rubella.txt",delimiter=" ",header=None,skiprows=0)

with open("output/outliers.txt") as f:
    lines = f.readlines()
    xdata = [line.split()[0] for line in lines]

noutliers = int(xdata[0])

outliers = np.empty(3*noutliers,dtype=int)

for i in range(3*noutliers):
    outliers[i] = int(xdata[i+1])

measles_outliers = np.empty((2,noutliers))
mumps_outliers   = np.empty((2,noutliers))
rubella_outliers = np.empty((2,noutliers))

for i in range(noutliers):
    measles_outliers[0,i] = df_seropositives[0].values[outliers[i]-1]
    measles_outliers[1,i] = df_seropositives[1].values[outliers[i]-1]

    mumps_outliers[0,i] = df_seropositives[0].values[outliers[noutliers+i]-1]
    mumps_outliers[1,i] = df_seropositives[2].values[outliers[noutliers+i]-1]

    rubella_outliers[0,i] = df_seropositives[0].values[outliers[2*noutliers+i]-1]
    rubella_outliers[1,i] = df_seropositives[3].values[outliers[2*noutliers+i]-1]



# Plotamos las soluciones 1:Measles, 2:Mumps, 3:Rubella
plot_solutions(1,df_seropositives,df_mixed_measles,measles_outliers,noutliers)
plot_solutions(2,df_seropositives,df_mixed_mumps,mumps_outliers,noutliers)
plot_solutions(3,df_seropositives,df_mixed_rubella,rubella_outliers,noutliers)



