#!/usr/bin/env python
from ipynetlib._NETLIB import NETLIB
import ipynetlib._NETLIB.ClientConnector
from ipyutils._PB2.Protobuf import fromProtobuf
from pprint import pprint

redqueenport=50000
clc = NETLIB.ClientConnector("rq", "tcp://localhost:%d" %redqueenport, 100)

for c in clc:
    print c

topman = clc['TopologyManager']

query = ['SCANNER1', 'MP7']
print "TopologyManager Query:", query
reply = topman.findObjects(query)
print reply
parsedreply = fromProtobuf(reply)

pprint(parsedreply)
# import ipdb; ipdb.set_trace()
