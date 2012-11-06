#!/usr/bin/env python

import os
from os.path import join

SBDIR=os.getenv("SANDBOXDIR")

NLDIR = join(SBDIR, "NetLib")
WCUDIR = join(SBDIR, "wcu_manager_p0")

print "using sandbox: ", SBDIR
from _NETLIB import NETLIB
import _NETLIB.ProtoImporter


NETLIB.ProtoImporter.registerProtoPath("/usr/local/include")
# NETLIB.ProtoImporter.registerProtoPath(join(NLDIR, "build/arch-pc/box/usr/lib64/ims-apis"))
# NETLIB.ProtoImporter.registerProtoPath(join(WCUDIR, "build/arch-pc/box/usr/lib64/ims-apis"))
NETLIB.ProtoImporter.registerProtoPath(join(NLDIR, "src/api"))
NETLIB.ProtoImporter.registerProtoPath(join(WCUDIR, "pb2"))
# or set PROTOPATH in env

import _NETLIB.ClientConnector
import _NETLIB.InterfaceManager
# import wcu_manager_p0.IStage

# clc = NETLIB.ClientConnector("tmwc", "tcp://127.0.0.1:30401", 100)
clc = NETLIB.ClientConnector("ml", "tcp://127.0.0.1:27999", 100)
print dir(clc)
# stage = clc.POC_Stage
# print stage.getSymbolicPositions()
