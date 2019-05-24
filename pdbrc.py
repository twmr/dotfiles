import pdb
# Command line history:
import readline
histfile = "~/.pdb-pyhist"
try:
    readline.read_history_file(histfile)
except IOError:
    pass
import atexit
atexit.register(readline.write_history_file, histfile)
del histfile
readline.set_history_length(200)
del atexit
del readline


class Config(pdb.DefaultConfig):
    highlight = True
    use_pygments = True
    prompt = '# '  # because + has a special meaning in the regexp
    editor = 'emacs'
    stdin_paste = 'epaste'
    disable_pytest_capturing = False
    #pygments_formatter_class = "pygments.formatters.TerminalTrueColorFormatter"
    pygments_formatter_class = "pygments.formatters.Terminal256Formatter"
    pygments_formatter_kwargs = {"style": "solarized-dark"}
