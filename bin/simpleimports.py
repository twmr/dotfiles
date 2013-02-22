import hwlib

from core import SessionManager
from core import ClientConnectorBase as Base

from mathpack.misc import polar
import quantities as pq

psu = hwlib.PowerSupply('PSU_HVREF', verbose=True)

ramper = hwlib.Ramper('RAMPER_PSYS_C', verbose=True)

mp2 = hwlib.Multipole('MP2', verbose=True)

scanner = hwlib.Scanner('SCANNER1', verbose=True)
scanner2 = hwlib.Scanner('SCANNER2', verbose=True)

cup = hwlib.Cup('PAM_FC1',verbose=True)

session = SessionManager('SessionManager', verbose=True)

mp5 = Base('MP5', verbose=True)

exp = Base('POC_Exposure', verbose=True)

print "ramper, cup, psu, scanner, scanner2, session, mp2 and mp5 objects initialized"
