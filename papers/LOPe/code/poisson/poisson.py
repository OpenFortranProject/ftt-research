import math
import numpy as np
import matplotlib.pyplot as plt
from scipy import linalg

def collocation_points(L, dz):
   z = dz * np.arange(L)
   return z
#end collocation_points

def sources(L, k, b, x):
   '''
   Assumes the spatial domain is [0,2*pi)
   '''

   y = np.zeros(len(x))

   # cos functions
   for j in range(0,L/2):
      y += b[j] * np.cos(k[j]*x)
      #print "cosines: ", j, b[j], k[j], np.cos(k[j]*x)

   # sin functions
   for j in range(L/2,L-1):
      y += b[j] * np.sin(k[j]*x)
      #print "sines: ", j, b[j], k[j], np.sin(k[j]*x)

   return y
#end sources

def pt_source(L, k, a, x):

   y = 0.0

   # cos functions
   for j in range(0,L/2):
      y += a[j] * np.cos(k[j]*x)
      #print "cosines: ", j, a[j], k[j], np.cos(k[j]*x)

   # sin functions
   for j in range(L/2,L-1):
      y += a[j] * np.sin(k[j]*x)
      #print "sines: ", j, a[j], k[j], np.sin(k[j]*x)

   return y
#end source

def collocation_matrix(L, k, z):
   n = len(k)
   a = np.ones(n)
   A = np.zeros((n,n))

   for row in range(len(k)):
      for col in range(0,L/2):
         A[row,col] = np.cos(k[col]*z[row])
         #print "cosine:", col, k[col], z[col], np.cos(k[col]*z[row])
      for col in range(L/2,L-1):
         A[row,col] = np.sin(k[col]*z[row])
         #print "sine:  ", col, k[col], z[col], np.sin(k[col]*z[row])

   return A
#end collocation_matrix

'''
This is run only if called directly by python
and not imported as a module.
'''
if __name__ == "__main__":
   '''
   Assumes the spatial domain is [0,1]
   '''

   pi = math.pi

   L = 8
   dz = 2*pi/L

   N = 64
   dx = 2*pi/N

   # coefficients for source function
   #
   b = np.zeros(L-1)
   b[0] = 0.5
   b[3] = 0.5

   # coefficients for solution approximation
   #
   a = np.zeros(len(b))

   # wavenumbers
   #
   k = 1 + np.arange(L-1)
   for j in range(L/2,L-1):   # upper half repeated for sin functions
      k[j] = k[j-L/2]

   z = collocation_points(len(a), dz)

   A = collocation_matrix(L, k, z)

   f = sources(L, k, b, z)
   a = linalg.solve(A,f)

   N = 64
   dx = 2*pi/N
   x = np.arange(N+1) * dx

   # plot the solution
   #
   y = sources(L, k, a, x)
   plt.plot(x/(2*pi), y, "bo")

   # plot other stuff (forcing function sometimes)
   #
   b[0] = 1
   b[3] = 0
   y = sources(L, k, b, x)
   plt.plot(x/(2*pi), y, "ro")

   plt.show()

'''
   plt.title("MPI Scaling")
   plt.xlabel("Number of Processes")
   plt.ylabel("Time to Completion")
   plt.axis([0,12, -1,11])
   plt.plot(x,y, "ro")
   plt.show()
'''
