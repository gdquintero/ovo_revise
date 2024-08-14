import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
from sklearn.metrics import mean_squared_error
import models
import os

cwd = os.getcwd()
parent =  os.path.abspath(os.path.join(cwd,os.pardir))

def main(problem):
    if problem == 1:
        df = pd.read_table(parent+"/data/seropositives.txt",delimiter=" ",header=None,skiprows=1)

        popt_measles, pcov_measles  = curve_fit(models.F,df[0].values,df[1].values,p0=np.ones(3),bounds=(np.zeros(3),np.ones(3)))
        popt_mumps, pcov_mumps      = curve_fit(models.F,df[0].values,df[2].values,p0=np.ones(3),bounds=(np.zeros(3),np.ones(3)))
        popt_rubella, pcov_rubella  = curve_fit(models.F,df[0].values,df[3].values,p0=np.ones(3),bounds=(np.zeros(3),np.ones(3)))

        with open(parent+"/output/sol_ls_farrington.txt","w") as f:
            f.write("%f %f %f\n" % (popt_measles[0],popt_measles[1],popt_measles[2]))
            f.write("%f %f %f\n" % (popt_mumps[0],popt_mumps[1],popt_mumps[2]))
            f.write("%f %f %f\n" % (popt_rubella[0],popt_rubella[1],popt_rubella[2])) 

        y_pred = np.empty((3,29))

        y_pred[0,:] = models.F(df[0].values,*popt_measles)
        y_pred[1,:] = models.F(df[0].values,*popt_mumps)
        y_pred[2,:] = models.F(df[0].values,*popt_rubella)

        error_measles =  mean_squared_error(df[1].values,y_pred[0,:])
        error_mumps   =  mean_squared_error(df[2].values,y_pred[1,:])
        error_rubella =  mean_squared_error(df[3].values,y_pred[2,:])
            
        print("Mean squared error for Measles:","{:.3e}".format(error_measles))
        print("Mean squared error for Mumps:","{:.3e}".format(error_mumps))
        print("Mean squared error for Rubella:","{:.3e}".format(error_rubella))

    else:
        df = pd.read_table(parent+"/data/osborne2.txt",delimiter=" ",header=None,skiprows=1)
        popt, pcov = curve_fit(models.osborne2,df[0].values,df[1].values,p0=np.ones(11),method='trf')
        
        with open(parent+"/output/sol_ls_osborne2.txt","w") as f:
            for i in range(11):
                f.write("%f\n" % popt[i])
    

main(2)
