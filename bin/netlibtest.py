#!/usr/bin/env python

import os
from os.path import join

SBDIR=os.getenv("SANDBOXDIR")

NLDIR = join(SBDIR, "NetLib")
WCUDIR = join(SBDIR, "wcu_manager_p0")

print "using sandbox: ", SBDIR
from _NETLIB import NETLIB
import _NETLIB.ProtoImporter


for p in [ '/usr/local/include', join(NLDIR, "src/api"), join(WCUDIR, "pb2") ]:
    print "REGISTERING PROTO PATH:", p
    NETLIB.ProtoImporter.registerProtoPath(p)
# or set PROTOPATH in env

import _NETLIB.ClientConnector
import _NETLIB.InterfaceManager
import _NETLIB.ClientConnectorPool as ccp

# import wcu_manager_p0.IStage

# clc = NETLIB.ClientConnector("tmwc", "tcp://127.0.0.1:30401", 100)

class Mydict(dict):
    def __getitem__(self, b):
        c = super(Mydict,self).__getitem__(b)
        print "GET", c
        if len(c) > 1:
            raise RuntimeError('more than one clc for the same object name')
        return c[0]



# ports = [28008, 28006, 28011, 28001, 28000]
# ports.sort()
ports = [26999, 27999, 28000, 28001, 28002, 28003, 28004, 28005, 28006, 28007, 28008, 28009, 28010, 28011, 28012, 28013, 28014, 28015, 30400, 49152]

names = list()


pool = ccp.ClientConnectorPool(100)

mymap = Mydict()
for port in ports:
    clc = pool.getClc("localhost", port)
    #clc = NETLIB.ClientConnector("localhost:%d" %port ,
    #                             "tcp://127.0.0.1:%d" % port, 100)
    for obj in clc:
        name = clc[obj].name
        names.append(name)
        print "NAME", name
        mymap[name] = [clc]

    # print dir(clc)
    # stage = clc.POC_Stage
    # print stage.getSymbolicPositions()

for key, val in mymap.iteritems():
    print "KEY", key, "value", val

names.sort()
print names
#print mymap
