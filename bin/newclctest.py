"""
clientconnectorwrapper.py
"""
__author__ = 'thomas.hisch@ims.co.at'
__copyright__ = 'Copyright (C) IMS Nanofabrication AG'

from ipyutils._PB2.Protobuf import fromProtobuf
from ipyutils._TFL import TFL
import ipyutils._TFL._Meta.Object
import ipyutils._TFL._Meta.M_Class

from core import RQPluginManager

def  setter_wrap_func(instance, name):
    def sf(instance, *args, **kw):
        if len(args) >= 1:
            value = args[0]
            if isinstance(value, dict):
                kw, args = value, ()
                args = ()
            else:
                kw, args = {}, (value,)
        return getattr(instance._plugin, name)(*args, **kw)
    return sf

def getter_wrap_func(instance, name):
    def gf(instance, *args, **kw):
        attr = getattr(instance._plugin, name)(*args, **kw)
        msgdict = fromProtobuf(attr)
        if len(msgdict) == 1:
            return msgdict.itervalues().next()
        else:
            return msgdict
    return gf

#TODO is it possible implement access to enums (mycup.enum.xxx) without this
# class ?
class _enum:
    pass

class ClientConnectorMeta(TFL.Meta.M_Class):
    """
    this metaclass implements a factory pattern and a singleton pattern
    """

    _instances = {} #TODO description
    _typemap = {} #TODO description
    _otree = RQPluginManager()

    def __new__(meta, name, bases, dct):
        """
        is called at import time by the python interpreter
        """
        print "CCM __new__ called with ", meta, name
        result = super(ClientConnectorMeta, meta).__new__(meta, name, bases, dct)
        print "adding ", result, "to typemap:", dct.get("_PLUGIN_CAT", " ")
        meta._typemap [dct.get("_PLUGIN_CAT", "")] = result
        return result

    def __init__(cls, name, bases, dct):
        super(ClientConnectorMeta, cls).__init__(name, bases, dct)

    def __call__(cls, name, *args, **kw):
        if not cls._instances.has_key(name):
            plugin = cls._otree[name]

            #determine the class from which we want to instantiate an object/instance
            real_cls = cls._typemap[plugin.cat]
            # import ipdb; ipdb.set_trace()

            inst = super(ClientConnectorMeta, real_cls).__call__(name, *args, **kw)
            inst._plugin = plugin
            inst._servlets = map(lambda s: s, inst._plugin)

            #store the created instance (Singleton)
            cls._instances[name] = inst

            for s in inst._servlets:
                if hasattr(cls, s) or hasattr(cls, s[3:]):
                    # don't overwrite already implemented functions
                    continue

                # read/write property: there are get and set methods for a
                # physical entity in the interface
                if s.startswith("get") and \
                        s.replace("get" , "set", 1) in inst._servlets:
                    setattr(cls, s[3:],
                            property(getter_wrap_func(inst, s),
                                     setter_wrap_func(inst, \
                                                          s.replace("get", "set", 1))))
                # read only property
                elif s.startswith("get"):
                    setattr(cls, s[3:], property(getter_wrap_func(inst, s)))

                # don't handle read/write twice
                elif s.startswith("set") and \
                        s.replace("set", "get", 1) in inst._servlets:
                    pass

                # write only property
                else:
                    setattr(cls, s, setter_wrap_func(inst, s))

            # enums in the Protofiles can be accessed with
            # mycup = Cup('PAM_FC1'); print mycup.enum.Status.ST_IDLE
            # if there is a _ENUMS_ attribute in the a subclass of HWCore
            if hasattr(inst,'_ENUMS_'):
                assert isinstance(inst._ENUMS_, dict)
                setattr(inst,'enum', _enum())
                for k, v in inst._ENUMS_.iteritems():
                    setattr(inst.enum, k, v)

        return cls._instances[name]


class ClientConnectorBase(TFL.Meta.Object):

    __metaclass__ = ClientConnectorMeta

    def __init__(self, name, *args, **kw):
        self.name = name
        if 'verbose' in kw:
            self.verbose = kw['verbose']
        else:
            self.verbose = False

class Multipole (ClientConnectorBase) :
    _PLUGIN_CAT = "MPole"

    def __init__(self, name, *args, **kw) :
        print "MPole init called with ", name
        self.__super.__init__(name, *args, **kw)
        # print "Hello Multipole %s: %s" % (self.name, self.plugin)

class Switch (ClientConnectorBase) :
    _PLUGIN_CAT = "Switch"

    def __init__(self, name, *args, **kw) :
        print "Switch init called with ", name
        self.__super.__init__(name, *args, **kw)
        # print "Hello Switch %s: %s" % (self.name, self.plugin)



#obj1 = ClientConnectorBase("MP7")
#obj2 = ClientConnectorBase("EN_MP7")
