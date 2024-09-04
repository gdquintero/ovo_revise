import numpy as np
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

    noutliers = 0

    for i in range(m):
        y[i] = y[i] + 0.8 * (2 * random.random() - 1)

        if random.random() <= 0.1:
            noutliers += 1
            # y[i] = 5 + 7.5 * random.random()
            operacion = np.random.choice([0,1],p=[0.2, 0.8])
            r = np.random.uniform(5, 10)

            if operacion == 1:
                y[i] += r
            else:
                y[i] -= r


    with open(parent+"/data/andreani"+str(m)+".txt","w") as f:
        f.write("%i\n" % m)
        for i in range(m):
            f.write("%f %f\n" % (t[i],y[i]))

    print(noutliers)
    plt.plot(t,y,"o",color="darkgreen",ms=2)
    plt.show()


xsol = np.array([0,2,-3,1])

gen_data(100)
# gen_data(1000)
# gen_data(10000)
# gen_data(100000)
# gen_data(1000000)

