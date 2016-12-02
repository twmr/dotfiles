# sample ipython_config.py
c = get_config()

c.Global.display_banner = True
c.Global.log_level = 20
c.Global.extensions = [
    'myextension'
]
c.Global.exec_lines = [
    'import numpy',
    'import scipy'
]
c.Global.exec_files = [
    'mycode.py',
    'fancy.ipy'
]
c.InteractiveShell.autoindent = True
c.InteractiveShell.colors = 'LightBG'
c.InteractiveShell.confirm_exit = False
c.InteractiveShell.deep_reload = True
c.InteractiveShell.editor = 'nano'
c.InteractiveShell.prompt_in1 = 'In [\#]: '
c.InteractiveShell.prompt_in2 = '   .\D.: '
c.InteractiveShell.prompt_out = 'Out[\#]: '
c.InteractiveShell.prompts_pad_left = True
c.InteractiveShell.xmode = 'Context'

c.PrefilterManager.multi_line_specials = True

c.AliasManager.user_aliases = [
 ('la', 'ls -al')
]


from IPython.terminal.prompts import Token
c.TerminalInteractiveShell.highlighting_style_overrides = {
            Token.Prompt: '#ansiblue', Token.PromptNum: '#ansiblue bold'}
