from _hwlib import hwlib
from _hwlib import pluginmanager #sonst funktioniert hwlib.loadTree nicht !!
import _hwlib.multipole as MP
import _hwlib.powersupply as PS
import _hwlib.ramper as RP
import _hwlib.scanner as SC
import _hwlib.cup as CP

print "loading ObjectTree"
otree = hwlib.loadTree(hwlib.servers)
print "Done"

psu = PS.PowerSupply('PSU_HVREF')

from _hwlib import ramper as RP
ramper = RP.Ramper('RAMPER_PSYS_C')

mp2 = MP.Multipole('MP2')

scanner = SC.TestScanner('ScannerNew')

cup  = CP('PAM_FC1')

print "ramper, cup, psu, scanner and mp2 objects initialized"
