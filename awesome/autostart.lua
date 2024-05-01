-- Standard awesome library
local spawn = require("awful.spawn")

-- Startup programs
-- Programs starting before spawn_with_shell
--

os.execute("terminator &")
os.execute("flameshot &")

-- Keyboard configuration
--
-- US keyboard
os.execute("setxkbmap us altgr-intl -option ctrl:nocaps")

os.execute("laptop-settings.sh")

-- Programs starting with spawn_with_shell
-- Gnome components
-- spawn.with_shell("/usr/libexec/gsd-xsettings")
--testing spawn.with_shell("gnome-keyring-daemon")

-- Network applet
spawn.with_shell("sleep 3; nm-applet")

-- Power management
-- spawn.with_shell("xfce4-power-manager")

-- Volume icon
-- spawn.with_shell("sleep 3; volumeicon")

-- Deactivate keyboard while typing macbook pro
-- spawn.with_shell("rm ~/.dispad.pid; dispad -F")

-- Start Conky
-- spawn.with_shell("conky -c /home/$USER/.config/awesome/conky/conky_simple/conkyrc")

-- Start Dropbox Daemon
-- spawn.with_shell("dropbox start")

-- Set wallpaper and screensaver
-- spawn.with_shell("feh --bg-fill /home/$USER/.config/awesome/wallpaper/funnysystem.jpg")
spawn.with_shell("xscreensaver -no-splash")

-- Configuration scripts and HiDPI specifics macbook pro
-- spawn.with_shell("/home/$USER/.config/awesome/scripts/xmodmapawesome")
-- spawn.with_shell("sleep 3; xrdb -merge /home/$USER/.config/awesome/scripts/Xdefaults.hidpi")
--spawn.with_shell("sh ~/.config/awesome/scripts/monitorretina")
