import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import models
import os

cwd = os.getcwd()
parent =  os.path.abspath(os.path.join(cwd,os.pardir))

df_data = pd.read_table(parent+"/data/osborne2.txt",delimiter=" ",header=None,skiprows=1,skipinitialspace=True)
df_sol = pd.read_table(parent+"/output/solution_osborne2.txt",delimiter=" ",header=None,skiprows=0,skipinitialspace=True)

t = np.linspace(df_data[0].values[0],df_data[0].values[-1],1000)

# print(df_sol.values[0])

plt.plot(df_data[0].values,df_data[1].values,"ro")
plt.plot(t,models.osborne2(df_sol.values[0],t))
plt.show()