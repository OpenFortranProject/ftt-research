#!/usr/bin/env python
"""
Example: simple line plot.
Show how to make and save a simple line plot with labels, title and grid
"""
import numpy
import pylab
import re
import string

x = []
e = []
f = open('error_time.dat', 'r')
for line in f:
   words = re.split(' *', string.strip(line))
   x.append(float(words[0]))
   e.append(float(words[1]))
f.close()
pylab.plot(x, e)

pylab.xlabel('time')
pylab.ylabel('max error')
pylab.title('Error')
pylab.grid(True)
pylab.savefig('poisson.png')

pylab.show()
