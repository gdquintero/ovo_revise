import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import models
import os

cwd = os.getcwd()
parent =  os.path.abspath(os.path.join(cwd,os.pardir))

size_img = 0.6
# plt.rcParams.update({'font.size': 11})
plt.rcParams['figure.figsize'] = [size_img * 6.4,size_img * 4.8]
plt.rc('text', usetex=True)
plt.rc('font', family='serif')

# df_data = pd.read_table(parent+"/data/andreani100.txt",delimiter=" ",header=None,skiprows=1,skipinitialspace=True)
# df_sol = pd.read_table(parent+"/output/solution_andreani_scaled.txt",delimiter=" ",header=None,skiprows=0,skipinitialspace=True)
# df_outliers = pd.read_table(parent+"/output/outliers_andreani_scaled.txt",delimiter=" ",header=None,skiprows=0,skipinitialspace=True)

# t = np.linspace(df_data[0].values[0],df_data[0].values[-1],1000)
# noutliers = df_outliers[0].values[0]
# outliers = np.empty((2,noutliers))

# for i in range(noutliers):
#     outliers[0,i] = df_data[0].values[df_outliers[0].values[i+1]-1]
#     outliers[1,i] = df_data[1].values[df_outliers[0].values[i+1]-1]
#     # print(outliers[1][i])

# plt.plot(df_data[0].values,df_data[1].values,"ko",ms=1)
# plt.plot(t,models.andreani(t,*df_sol.values[0]),lw=1)
# plt.plot(outliers[0],outliers[1],'ro',mfc='none',ms=4,mew=0.5)
# plt.savefig(parent+"/images/andreani_fitting.pdf",bbox_inches = "tight")
# plt.show()

def plot_log(n):

    fig,ax = plt.subplots(1, 1)
    if n == 100: 
        file = "andreani_scaled_curve_100.txt"
    elif n == 1000:
        file = "andreani_scaled_curve_1000.txt"

    df_data = pd.read_table(file,delimiter=" ",header=None,skipinitialspace=True)
    ax.tick_params(axis='both',direction='in',which='both')
    ax.loglog(df_data[1].values,df_data[2].values,"-",color="darkgreen",lw=1)
    ax.set_xscale('linear')

    if n == 100:
        plt.xticks(np.arange(0,16.1,5))
    elif n == 1000:
        plt.xticks(np.arange(0,150.1,50))

    # ax.set_ylim(min(np.log10(y)),max(np.log10(y))+10)
    plt.xlabel("Number of outliers $o$")
    plt.ylabel("$f(x^*)$ (log scale)")
    plt.savefig(parent+"/images/log10.pdf",bbox_inches="tight")

plot_log(100)
# plot_log(1000)