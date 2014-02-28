import poclib.hwlib as hw

from poclib.core import SessionManager
from poclib.core import ClientConnectorBase as Base

from poclib.mathpack.misc import polar
import quantities as pq


psu, mp2, cup, mpshutter, stage, session = map(
    Base, ['PSU_HVREF', 'MP2', 'PAM_FC1', 'MP_SHUTTER', 'JEOL_STAGE',
           'SessionManager'])

print "ramper, cup, psu, scanner, scanner2, session, mp2 and mp5 objects " \
    "initialized"
