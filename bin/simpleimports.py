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

psu = hwlib.PowerSupply('PSU_HVREF', verbose=True)

ramper = hwlib.Ramper('RAMPER_PSYS_C', verbose=True)

mp2 = hwlib.Multipole('MP2', verbose=True)

scanner = hwlib.TestScanner('SCANNER1', verbose=True)

cup  = hwlib.Cup('PAM_FC1',verbose=True)

print "ramper, cup, psu, scanner and mp2 objects initialized"
