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

def plot_fit(n):
    if n == 100: 
        file = parent+"/data/andreani100.txt"
    elif n == 1000:
        file = parent+"/data/andreani1000.txt"
    elif n == 10000:
        file = parent+"/data/andreani10000.txt"
    elif n == 100000:
        file = parent+"/data/andreani100000.txt"
    else:
        file = parent+"/data/andreani1000000.txt"

    df_data = pd.read_table(file,delimiter=" ",header=None,skiprows=1,skipinitialspace=True)
    df_sol = pd.read_table(parent+"/output/solution_andreani_scaled.txt",delimiter=" ",header=None,skiprows=0,skipinitialspace=True)
    df_outliers = pd.read_table(parent+"/output/outliers_andreani_scaled.txt",delimiter=" ",header=None,skiprows=0,skipinitialspace=True)

    t = np.linspace(df_data[0].values[0],df_data[0].values[-1],1000)
    noutliers = df_outliers[0].values[0]
    outliers = np.empty((2,noutliers))

    for i in range(noutliers):
        outliers[0,i] = df_data[0].values[df_outliers[0].values[i+1]-1]
        outliers[1,i] = df_data[1].values[df_outliers[0].values[i+1]-1]
        # print(outliers[1][i])

    plt.plot(df_data[0].values,df_data[1].values,"ko",ms=0.1)
    plt.plot(outliers[0],outliers[1],'ro',mfc='none',ms=1,mew=0.1)
    plt.plot(t,models.andreani(t,*df_sol.values[0]),lw=1)
    plt.tick_params(axis='both',direction='in')
    plt.xticks(np.arange(-1,3.1,1))
    plt.yticks(np.arange(-6,12.1,6))
    plt.ylim(-6.5,13)
    plt.xlim(-1.1,3.6)
    plt.savefig(parent+"/images/andreani_fitting"+str(n)+".pdf",bbox_inches = "tight")
    plt.close()

def plot_log(n):

    # fig,ax = plt.subplots(1, 1)
    if n == 100: 
        file = "andreani_scaled_log_100.txt"
    elif n == 1000:
        file = "andreani_scaled_log_1000.txt"
    elif n == 10000:
        file = "andreani_scaled_log_10000.txt"
    elif n == 100000:
        file = "andreani_scaled_log_100000.txt"
    else:
        file = "andreani_scaled_log_1000000.txt"

    df_data = pd.read_table(file,delimiter=" ",header=None,skipinitialspace=True)
    # ax.tick_params(axis='both',direction='in',which='both')
    # ax.loglog(df_data[1].values,df_data[2].values,"-o",color="darkgreen",lw=0.5,ms=1)
    # ax.set_xscale('linear')

    plt.plot(df_data[1].values,df_data[2].values,"-o",color="darkgreen",lw=0.5,ms=1)




    if n == 100:
        plt.xticks(np.arange(5,15.1,2))
        plt.yticks(np.arange(0,5.1,1))
        plt.ylim(-0.5,5)
    elif n == 1000:
        plt.xticks(np.arange(50,150.1,20))
        plt.yticks(np.arange(0,10.1,2))
        plt.ylim(-0.5,10)
    elif n == 10000:
        plt.xticks(np.arange(750,1250.1,100))
        plt.yticks(np.arange(0,6.1,1))
        plt.ylim(-0.5,6)
    elif n == 100000:
        plt.xticks(np.arange(5000,15000.1,2000))
        plt.yticks(np.arange(0,12.1,2))
        plt.ylim(-0.5,12)
    else:
        plt.xticks(np.arange(50000,150000.1,20000))
        plt.yticks(np.arange(0,18.1,3))
        plt.ylim(-0.5,18)


    # plt.yscale("log")
    # plt.yticks(np.arange(10**(-1),10,4))
    plt.xlabel("Number of outliers $o$")
    plt.ylabel("$f(x^*)$")
    plt.tick_params(axis='both',direction='in')
    plt.savefig(parent+"/images/andreani_scaled"+str(n)+".pdf",bbox_inches="tight")
    # plt.show()
    plt.close()


# for n in [100,1000,10000,100000,1000000]:
#     plot_log(n)

plot_fit(1000000)
# plot_log(1000000)
