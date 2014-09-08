#!/usr/bin/env python
import sys
from IPython.nbformat.current import read
from IPython.utils.text import strip_ansi

fname = sys.argv[1]
#with open(fname, encoding='utf-8') as f:
with open(fname) as f:
    nb = read(f, 'ipynb')

banners = {
'heading':  'Heading %d ------------------',
'markdown': 'Markdown cell ---------------',
'code':     'Code cell -------------------',
'raw':      'Raw cell --------------------',
'output':   'Output ----------------------',
}

for cell in nb.worksheets[0].cells:
    if cell.cell_type == 'heading':
        print(banners['heading'] % cell.level)
    else:
        print(banners[cell.cell_type])

    if cell.cell_type == 'code':
        source = cell.input
    else:
        source = cell.source
    
    print(source)
    if not source.endswith('\n'):
        print()
    
    if cell.cell_type == 'code':
        if cell.outputs:
            print(banners['output'])
            for output in cell.outputs:
                if 'text' in output:
                    print(strip_ansi(output.text))
                elif 'traceback' in output:
                    print(strip_ansi('\n'.join(output.traceback)))
                else:
                    print("(Non-plaintext output)")
        print()
    
