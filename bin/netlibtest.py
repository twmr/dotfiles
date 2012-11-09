#!/usr/bin/env python

import os
from os.path import join
import re

SBDIR=os.getenv("SANDBOXDIR")

NLDIR = join(SBDIR, "NetLib")
WCUDIR = join(SBDIR, "wcu_manager_p0")

print "using sandbox: ", SBDIR
# from ipynetlib._NETLIB import NETLIB

# for p in [ '/usr/local/include', join(NLDIR, "build/arch-pc/src/api"),
#            join(WCUDIR, "build/arch-pc/pb2") ]:
#     print "REGISTERING PROTO PATH:", p
#     NETLIB.ProtoImporter.registerProtoPath(p)
# or set PROTOPATH in env

import ipynetlib._NETLIB.ClientConnector
# import ipynetlib._NETLIB.InterfaceManager
import ipynetlib._NETLIB.ClientConnectorPool as ccp
# import eds.IEds

#if we want to talk with the psu we need to import the psu interface stuff
import hwctrl.IPsu


# clc = NETLIB.ClientConnector("tmwc", "tcp://127.0.0.1:27999", 100)
# eds = clc.POC_ExposureDataStreamer
# print eds.getState()

# import wcu_manager_p0.IStage

# clc = NETLIB.ClientConnector("tmwc", "tcp://127.0.0.1:30401", 100)

class Mydict(dict):
    def __getitem__(self, b):
        c = super(Mydict,self).__getitem__(b)
        print "GET", c
        if len(c) > 1:
            raise RuntimeError('more than one clc for the same object name')
        return c[0]


LOGDIR=join(SBDIR,'hws-log')
ports=list()

#TODO use glob module
for r,d,f in os.walk(LOGDIR):
    for file in f:
        if "stderr" in file:
            continue
        # print "FILE", file
        for line in open(join(LOGDIR, file), 'r').readlines():
            if re.match(".*tcp://", line):
                uri = line[line.find('tcp'):].strip()
                port = int(uri[-5:])
                ports.append(port)
                print "uri", uri, "port", port

ports.sort()

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

psuclc = mymap['PSU_HVREF']
psu = psuclc.PSU_HVREF # or psuclc['PSU_HVREF']
print psu

print dir(psu)

# import ipdb; ipdb.set_trace()
