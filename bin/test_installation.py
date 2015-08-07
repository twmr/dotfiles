#!/usr/bin/env python

import sys

print("python version {}".format(sys.version))

import ffc
print("ffc version {}".format(ffc.__version__))
import dolfin
print("dolfin version {}".format(dolfin.__version__))
import mshr
# mshr does not have a version attr
# print("mshr version {}".format(mshr.__version__))
print('mshr installed')

import petsc4py
print("petsc version {}".format(petsc4py.__version__))
import slepc4py
print("slepc version {}".format(slepc4py.__version__))

import bempp
print('bempp installed')
print("success")
