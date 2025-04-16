#/bin/bash

# this is only for emacs, since it is not possible to use an
# executable that consists of multiple arguements -> I should file a
# flycheck bug report maybe
pixi run pyright $*
