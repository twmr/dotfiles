#!/usr/bin/env python

import os
from os.path import join
import re

SBDIR=os.getenv("SANDBOXDIR")
LOGDIR=join(SBDIR,'hws-log', 'poc')
ports=list()

#TODO use glob module
import glob
for file in glob.glob(join(LOGDIR, '*stdout.log')):
    print "logf name", file
    for line in open(file, 'r').readlines():
        if re.match(".*tcp://", line):
            uri = line[line.find('tcp'):].strip()
            port = int(uri[-5:])
            ports.append(port)
            print "uri", uri, "port", port

ports.sort()
servers = [('localhost', str(x)) for x in ports]
