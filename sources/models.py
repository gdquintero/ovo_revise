import numpy as np

l = lambda t, a, b, c: (a * t - c) * np.exp(-b * t) + c

F = lambda t, a, b, c: 1.0 - np.exp((a/b) * t * np.exp(-b * t) + (1.0/b) * ((a/b) - c) * \
    (np.exp(-b * t) - 1.0) - c * t)

def osborne2(x,t):
    y = x[0] * np.exp(-t * x[4]) + x[1] * np.exp(-x[5] * (t - x[8])**2) + \
        x[2] * np.exp(-x[6] * (t - x[9])**2) + x[3] * np.exp(-x[7] * (t - x[10])**2)
    
    return y