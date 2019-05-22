import pdb

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
