import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import models
import random

cwd = os.getcwd()
parent =  os.path.abspath(os.path.join(cwd,os.pardir))

def gen_data(m):
    t = np.linspace(-1,3.5,m)
    y = models.andreani(t,*xsol)

    random.seed(123456)

    outliers = 0

    for i in range(m):
        y[i] = y[i] + 0.5 * (2 * random.random() - 1)

        if random.random() <= 0.1:
            outliers += 1
            y[i] = 5 + 7.5 * random.random()


    with open(parent+"/data/andreani"+str(m)+".txt","w") as f:
        f.write("%i\n" % m)
        for i in range(m):
            f.write("%f %f\n" % (t[i],y[i]))

    print(outliers)


xsol = np.array([0,2,-3,1])

# gen_data(100)
gen_data(1000)
# gen_data(10000)
# gen_data(100000)
# gen_data(1000000)



# with open(parent+"/data/andreani.txt","w") as f:
#     f.write("%i\n" % m)
#     for i in range(m):
#         f.write("%f %f\n" % (t[i],y[i]))

# plt.plot(t,y,"o")
# plt.show()