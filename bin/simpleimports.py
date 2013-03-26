import poclib.hwlib as hw

from poclib.core import SessionManager
from poclib.core import ClientConnectorBase as Base

from poclib.mathpack.misc import polar
import quantities as pq

psu = hw.PowerSupply('PSU_HVREF', verbose=True)

ramper = hw.Ramper('RAMPER_PSYS_C', verbose=True)

mp2 = hw.Multipole('MP2', verbose=True)

scanner = hw.Scanner('SCANNER1', verbose=True)
scanner2 = hw.Scanner('SCANNER2', verbose=True)

cup = hw.Cup('PAM_FC1',verbose=True)

session = SessionManager('SessionManager', verbose=True)

mp5 = Base('MP5', verbose=True)

exp = Base('POC_Exposure', verbose=True)

mpshutter = Base('MP_SHUTTER')

print "ramper, cup, psu, scanner, scanner2, session, mp2 and mp5 objects initialized"
