from _hwlib import hwlib
import _hwlib.multipole
import _hwlib.powersupply
import _hwlib.ramper
import _hwlib.scanner
import _hwlib.cup
import _hwlib.ramper

from core import SessionManager
from core import ClientConnectorBase as Base

from mathpack.misc import polar

psu = hwlib.PowerSupply('PSU_HVREF', verbose=True)

ramper = hwlib.Ramper('RAMPER_PSYS_C', verbose=True)

mp2 = hwlib.Multipole('MP2', verbose=True)

scanner = hwlib.Scanner('SCANNER1', verbose=True)

cup = hwlib.Cup('PAM_FC1',verbose=True)

session = SessionManager('SessionManager', verbose=True)

mp5 = Base('MP5', verbose=True)

print "ramper, cup, psu, scanner, session, mp2 and mp5 objects initialized"
