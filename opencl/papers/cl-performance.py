import random
import numpy as np
import pylab as pl

'''from scikits.learn import svm, datasets'''
'''from scikits.learn.metrics import roc, auc'''

import matplotlib.pyplot as plt
import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.cm as cm
import matplotlib.lines as ln

npts = 8

x      = np.zeros(npts)
fdp    = np.zeros(npts)
floops = np.zeros(npts)
cl     = np.zeros(npts)

#x = [16.,32.,64.,128.,256.,512.,1024.,1280.,2048.,4096.]

#fdp    = [.025, .086, .2, .76, 3.02, 12.1, 49.5, 77.7, 199.1, 794.7]
#floops = [.022, .087, .19, .71, 2.82, 11.2, 45.0, 70.1, 178.7, 714.1]
#cl     = [.017, .020, .020, .036, .092, .32, 1.22, 1.89, 4.82, 19.29]

x = [64.,128.,256.,512.,1024.,1280.,2048.,4096.]

fdp    = [.2  , .76 , 3.02, 12.1, 49.5, 77.7, 199.1, 794.7]
floops = [.19 , .71 , 2.82, 11.2, 45.0, 70.1, 178.7, 714.1]
cl     = [.020, .036, .092, .32 , 1.22, 1.89, 4.82 , 19.29]

for i in range(npts):
   fdp[i]    = 1.e6 * fdp[i]   /(x[i]*x[i])
   floops[i] = 1.e6 * floops[i]/(x[i]*x[i])
   cl[i]     = 1.e6 * cl[i]    /(x[i]*x[i])

fig = plt.figure()
ax = fig.add_subplot(111)

ax.plot(x, fdp,    '-o', color='k', linewidth=3.0, ms=10.0)
ax.plot(x, floops, '^' , color='k', linewidth=3.0, ms=10.0)
ax.plot(x, cl,     '+' , color='k', linewidth=3.0, ms=10.0)

ax.set_xlabel('Array Width/Height (# of Elements)')
ax.set_ylabel('Avgerage Time per Element (micro sec)')
ax.set_title('')
ax.grid(True)

plt.show()

