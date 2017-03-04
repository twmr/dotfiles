#!/usr/bin/env python
# code from https://stackoverflow.com/questions/287871
from __future__ import print_function

def print_format_table():
    """
    prints table of formatted text format options
    """
    for style in range(8):
        for fg in range(30,38):
            s1 = ''
            for bg in range(40,48):
                format = ';'.join([str(style), str(fg), str(bg)])
                s1 += '\x1b[%sm %s \x1b[0m' % (format, format)
            print(s1)
        print('\n')

print_format_table()

print()
colors = []
for i in range(120):
  if i and i % 5 == 0:
      print(' '.join(colors))
      colors = []
      
  colors.append("\33["+str(i)+"m\\33["+str(i)+"m\033[0m ")
  #print(colors)
  #x=x+

print("\nfrom termcolor package")
import os
try:
    import termcolor
    os.system("python -m termcolor")
except ImportError:
    print("termcolor not installed")
