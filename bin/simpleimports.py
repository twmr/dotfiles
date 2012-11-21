from _hwlib import hwlib
import _hwlib .pluginmanager #sonst funktioniert hwlib.loadTree nicht !!
import _hwlib.multipole
import _hwlib.powersupply
import _hwlib.ramper
import _hwlib.scanner
import _hwlib.cup
import _hwlib.ramper

print "loading ObjectTree"
otree = hwlib.loadTree(hwlib.servers)
print "Done"

psu = hwlib.PowerSupply('PSU_HVREF')

ramper = hwlib.Ramper('RAMPER_PSYS_C')

mp2 = hwlib.Multipole('MP2')

scanner = hwlib.TestScanner('ScannerNew')

cup  = hwlib.Cup('PAM_FC1')

print "ramper, cup, psu, scanner and mp2 objects initialized"
