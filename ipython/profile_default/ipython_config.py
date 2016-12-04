c = get_config()
c.TerminalIPythonApp.display_banner = False
c.TerminalIPythonApp.profile = u'default'
c.TerminalInteractiveShell.history_length = 10000
c.TerminalInteractiveShell.autoindent = True
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.color_info = True
c.TerminalInteractiveShell.colors = 'NoColor'

from IPython.terminal.prompts import Token
c.TerminalInteractiveShell.highlighting_style_overrides = {
            Token.Prompt: '#ansiblue', Token.PromptNum: '#ansiblue bold'}

