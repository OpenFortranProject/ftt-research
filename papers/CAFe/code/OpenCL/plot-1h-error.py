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
v = []
f = open('output_1h_0.dat', 'r')
for line in f:
   words = re.split(' *', string.strip(line))
   x.append(float(words[1]))
   v.append(float(words[2]))
f.close()
pylab.plot(x, v)

x = []
v = []
f = open('output_1h_mid.dat', 'r')
for line in f:
   words = re.split(' *', string.strip(line))
   x.append(float(words[1]))
   v.append(float(words[2]))
f.close()
pylab.plot(x, v)

x = []
v = []
f = open('output_1h_end.dat', 'r')
for line in f:
   words = re.split(' *', string.strip(line))
   x.append(float(words[1]))
   v.append(float(words[2]))
f.close()
pylab.plot(x, v)

pylab.xlabel('x')
pylab.xlim([0, 1])
pylab.ylabel('v')
pylab.ylim([-1, 1])
pylab.title('1h Error')
pylab.grid(True)
pylab.savefig('poisson.png')

pylab.show()
