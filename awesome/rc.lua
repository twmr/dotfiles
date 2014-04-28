-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers

local home     = os.getenv("HOME")
local hostname = os.getenv("HOSTNAME")
local browser  = os.getenv("BROWSER")
local exec     = awful.util.spawn
local sexec    = awful.util.spawn_with_shell

local altkey = "Mod1" -- Alt_L
local modkey = "Mod4"

-- Beautiful theme
--beautiful.init(awful.util.getdir("config") .. "/zenburn.lua")
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")
beautiful.init(awful.util.getdir("config") .. "/theme.lua")

settings = {}
settings.term = 'urxvt256c-ml'
-- settings.term = 'gnome-terminal'
settings.browser1 = browser
settings.browser2 = 'chromium-browser'
settings.editor = 'emacsclient -n -c'
settings.editor2 = 'gvim'
settings.filemanager = 'nemo ' .. home
settings.music = 'rhythmbox'

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.floating,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.

if hostname == "pc-52-rh" then
   monitors = {
      prim = 1,
      second = 2
   }
   tags = {
      namessecond  = { "1main","2emacs","3web","4scripts","5jeol",
                       "6remote","7vm","8keller","9brunn", "10tmp",
                       "11nemo", "12tmp", "13tmp" },
      names  = { 1,2,3,4,5,6,7,8,9 }
   }
   lockcmd = "xscreensaver-command -lock"
   defaultlayoutidmon2 = 1 --4
   defaultlayoutidmon1 = 1
else
   monitors = {
      prim = 2,
      second = 1
   }
   tags = {
      names  = { "doc","vsc","web","qtc", "mail", "nonuni",7,8,9 },
      namessecond  = { "vsc",2,3,4,5,6,7,8,9 }
   }
   lockcmd = "gnome-screensaver-command -l"
   defaultlayoutidmon2 = 1
   defaultlayoutidmon1 = 1
end

if screen.count() == 1 then
   monitors.prim = 1
   monitors.seond = 1
   tags = {
      namessecond  = { "doc","vsc","web","qtc", "nonuni",6,7,8,9}
   }
end

for s = 1, screen.count() do
   if s > 1 then
     tags[s] = awful.tag(tags.names, s, layouts[defaultlayoutidmon2])
   else
     tags[s] = awful.tag(tags.namessecond, s, layouts[defaultlayoutidmon1])
   end
   awful.tag.setproperty(tags[s][5], "mwfact", 0.13)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", settings.term .. " -e man awesome" },
   { "edit config", settings.editor .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "lock screen", lockcmd },
   { "quit",  function ()
      awesome.quit()
      exec("gnome-session-quit --logout")
      end  }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", settings.term }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(mytextclock)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    awful.key({ modkey }, "]", function () awful.screen.focus(monitors.second) end),
    awful.key({ modkey }, "[", function () awful.screen.focus(monitors.prim) end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () exec(settings.term)       end),
    awful.key({ modkey            }, "e",      function () exec(settings.editor)     end),
    awful.key({ modkey            }, "t",      function () exec(settings.filemanager)end),
    awful.key({ modkey            }, "w",      function () exec(settings.browser2)   end),
    awful.key({ modkey, "Shift"   }, "w",      function () exec(settings.browser1)   end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    awful.key({ "Control", altkey }, "l",     function () exec(lockcmd)                 end),

    -- Prompt
    awful.key({ modkey },            "r",     function () exec("gnome-do")           end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
              function (c)
                 -- the client currently has the input focus, so it cannot be
                 -- minimized, since minimized clients can't have the focus.
                 c.minimized = true
    end),
    awful.key({ modkey,           }, "m",
              function (c)
                 c.maximized_horizontal = not c.maximized_horizontal
                 c.maximized_vertical   = not c.maximized_vertical
    end),

    awful.key({ modkey }, "`",
              function (c)
                 if awful.client.floating.get(c) then
                    awful.client.floating.delete(c)
                    -- awful.titlebar.remove(c)
                 else
                    awful.client.floating.set(c, true)
                    -- awful.titlebar.add(c)
                 end
    end)
)

-- {{{ Key bindings
--
-- {{{ Global keys
-- globalkeys = awful.util.table.join(
--     -- {{{ Applications
--     awful.key({ modkey }, "e", function () exec(settings.editor) end),
--     awful.key({ modkey }, "t", function () exec(settings.filemanager) end),
--     awful.key({ modkey }, "w", function () exec(settings.browser2) end),
--     awful.key({ modkey, "Shift" }, "w", function () exec(settings.browser1) end),
--     awful.key({ modkey }, "Return",  function () exec(settings.term) end),
--     awful.key({ modkey }, "x",  function () exec(settings.term) end),
--     awful.key({ modkey }, "g", function () exec(settings.music) end),
--     awful.key({ modkey }, "q", function () exec(settings.term .. " -T MEINSERVER -e ssh thomas@opentech.meinserver.at") end),
--     awful.key({ modkey }, "a", function () exec(settings.term .. " -T OPENTECHMAIL -e ssh thomas@opentech.at") end),
-- --    awful.key({ modkey }, "q", function () exec("emacsclient --eval '(make-remember-frame)'") end),
--     -- }}}

--     -- {{{ Prompt menus

--  --   awful.key({ modkey }, "s", function () menubar.show() end),
    
--     -- run or raise applications with dmenu
--     awful.key({ modkey }, "r", function () exec("gnome-do") end),

-- --    awful.key({ modkey }, "F3", function ()
-- --        awful.prompt.run({ prompt = "Dictionary: " }, promptbox[mouse.screen].widget,
-- --            function (words)
-- --                sexec("crodict "..words.." | ".."xmessage -timeout 10 -file -")
-- --            end)
-- --    end),
-- --    awful.key({ altkey }, "F4", function ()
-- --        awful.prompt.run({ prompt = "Run Lua code: " }, promptbox[mouse.screen].widget,
-- --        awful.util.eval, nil, awful.util.getdir("cache") .. "/history_eval")
-- --    end),
--     -- }}}

--     -- {{{ Awesome controls
--     awful.key({ modkey }, "b", function ()
--         wibox[mouse.screen].visible = not wibox[mouse.screen].visible
--     end),
--     awful.key({ modkey, "Shift" }, "q", function ()
--         awesome.quit()
--         exec("gnome-session-save --logout")
--       end),
--     awful.key({ modkey, "Control" }, "r", function ()
--         promptbox[mouse.screen].text = awful.util.escape(awful.util.restart())
--     end),
--     -- }}}

--     -- {{{ Tag browsing
--     awful.key({ modkey }, "Right",   awful.tag.viewnext),
--     awful.key({ modkey }, "Left",   awful.tag.viewprev),
--     -- awful.key({ altkey , "Control" }, "Right",   awful.tag.viewnext),
--     -- awful.key({ altkey , "Control" }, "Left",   awful.tag.viewprev),
--     awful.key({ modkey }, "Escape", awful.tag.history.restore),
--     awful.key({ modkey }, "v", awful.tag.history.restore),
--     -- }}}

--     -- {{{ Layout manipulation
--     awful.key({ modkey }, "l",          function () awful.tag.incmwfact(0.05) end),
--     awful.key({ modkey }, "h",          function () awful.tag.incmwfact(-0.05) end),
--     awful.key({ modkey, "Shift" }, "l", function () awful.client.incwfact(-0.05) end),
--     awful.key({ modkey, "Shift" }, "h", function () awful.client.incwfact(0.05) end),
--     awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end),
--     awful.key({ modkey },          "space", function () awful.layout.inc(layouts, 1) end),
--     -- }}}

--     -- {{{ Focus controls
--     --awful.key({ modkey }, "p", function () awful.screen.focus_relative(1) end),
--     awful.key({ modkey }, "]", function () awful.screen.focus(monitors.second) end),
--     awful.key({ modkey }, "[", function () awful.screen.focus(monitors.prim) end),
--     awful.key({ modkey }, "'", function () awful.tag.viewprev() end),
--     awful.key({ modkey }, "\\", function () awful.tag.viewnext() end),
--     awful.key({ modkey }, "s", function () scratchpad.toggle() end),
--     awful.key({ modkey }, "u", awful.client.urgent.jumpto),

--     -- awful.key({ altkey }, "Tab", function ()
--     --     awful.client.focus.byidx(1)
--     --     if client.focus then client.focus:raise() end
--     -- end),
--     awful.key({ modkey }, "j", function ()
--         awful.client.focus.byidx(1)
--         if client.focus then client.focus:raise() end
--     end),
--     awful.key({ modkey }, "k", function ()
--         awful.client.focus.byidx(-1)
--         if client.focus then client.focus:raise() end
--     end),
--     awful.key({ modkey }, "Tab", function ()
--         awful.client.focus.history.previous()
--         if client.focus then client.focus:raise() end
--     end),
-- --    awful.key({ altkey }, "Escape", function () mouse.coords({x=525, y=330}, true)
-- --        awful.menu.menu_keys.down = { "Down", "Alt_L" }
-- --        local cmenu = awful.menu.clients({ width = 230 }, true)
-- --    end),
--     awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1) end),
--     awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end)

--     -- awful.key({ modkey }, "<",
--     --    function (c)
--     --        if c.titlebar then
--     --            awful.titlebar.remove(c)
--     --            debug_notify(c.name .. "\ntitlebar " .. colored_off)
--     --        else
--     --            awful.titlebar.add(c, { altkey = "Mod1" })
--     --            debug_notify(c.name .. "\ntitlebar " .. colored_on)
--     --        end
--     --    end)
--     -- }}}
-- )
-- }}}

-- {{{ Client manipulation
-- clientkeys = awful.util.table.join(
--     awful.key({ modkey, "Shift" }, "c", function (c) c:kill() end),
--     awful.key({ modkey }, "d", function (c) scratchpad.set(c, 0.60, 0.60, true) end),
--     awful.key({ modkey }, "f",
--               function (c)
--                  -- if c.titlebar then TODO
--                  awful.titlebar.remove(c)
--                  c.fullscreen           = not c.fullscreen
--               end),
--     awful.key({ modkey }, "m", function (c)
--         c.maximized_horizontal = not c.maximized_horizontal
--         c.maximized_vertical   = not c.maximized_vertical
-- 		 -- for k, c in pairs(client.get()) do
-- 		 --    local class=string.lower(c.class)
-- 		 --    if string.match(class, lower_command) then
-- 		 --       for i, v in ipairs(c:tags()) do
-- 		 --    awful.tag.viewonly(v)
-- 		 --    c:raise()
-- 		 --    c.minimized = false
-- 		 --    return
-- 		 --       end
-- 		 --    end
-- 		 -- end
--     end),
--     awful.key({ modkey }, "o",     awful.client.movetoscreen),
--     awful.key({ modkey }, "Next",  function () awful.client.moveresize(20, 20, -40, -40) end),
--     awful.key({ modkey }, "Prior", function () awful.client.moveresize(-20, -20, 40, 40) end),
--     awful.key({ modkey }, "Down",  function () awful.client.moveresize(0, 20, 0, 0) end),
--     awful.key({ modkey }, "Up",    function () awful.client.moveresize(0, -20, 0, 0) end),
-- --    awful.key({ modkey }, "Left",  function () awful.client.moveresize(-20, 0, 0, 0) end),
-- --    awful.key({ modkey }, "Right", function () awful.client.moveresize(20, 0, 0, 0) end),
-- --    awful.key({ modkey, "Control"},"r", function (c) c:redraw() end),
--     awful.key({ modkey, "Shift" }, "0", function (c) c.sticky = not c.sticky end),
--     awful.key({ modkey, "Shift" }, "m", function (c) c:swap(awful.client.getmaster()) end),
--     awful.key({ modkey, "Shift" }, "c", function (c) exec("kill -CONT " .. c.pid) end),
--     awful.key({ modkey, "Shift" }, "s", function (c) exec("kill -STOP " .. c.pid) end),
--     awful.key({ modkey, "Shift" }, "t", function (c)
--         if   c.titlebar then awful.titlebar.remove(c)
--         else awful.titlebar.add(c, { modkey = modkey }) end
--     end),
--     awful.key({ modkey }, "`",
--               function (c) if awful.client.floating.get(c)
--                  then awful.client.floating.delete(c);    awful.titlebar.remove(c)
--                  else awful.client.floating.set(c, true); awful.titlebar.add(c) end
--               end)
-- )
-- }}}


-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.movetotag(tag)
                     end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      if client.focus and tag then
                          awful.client.toggletag(tag)
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Keyboard digits
-- local keynumber = 0
-- for s = 1, screen.count() do
--    keynumber = math.min(9, math.max(#tags[s], keynumber));
-- end
-- }}}

-- {{{ Tag controls
-- for i = 1, keynumber do
--     globalkeys = awful.util.table.join( globalkeys,
--         awful.key({ modkey }, "#" .. i + 9, function ()
--             local screen = mouse.screen
--             if tags[screen][i] then awful.tag.viewonly(tags[screen][i]) end
--         end),
--         awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
--             local screen = mouse.screen
--             if tags[screen][i] then awful.tag.viewtoggle(tags[screen][i]) end
--         end),
--         awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
--             if client.focus and tags[client.focus.screen][i] then
--                 awful.client.movetotag(tags[client.focus.screen][i])
--             end
--         end),
--         awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
--             if client.focus and tags[client.focus.screen][i] then
--                 awful.client.toggletag(tags[client.focus.screen][i])
--             end
--         end) )
-- end
-- }}}


-- {{{ Rules
awful.rules.rules = {
    -- all clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    -- { rule = { class = "mplayer" },
    --   properties = { floating = true } },
    -- { rule = { class = "pinentry" },
    --   properties = { floating = true } },
    -- { rule = { class = "gimp" },
    --   properties = { floating = true } },
   --  { rule = { class = "Firefox", instance = "Navigator" },
   --    properties = { tag = tags[monitors.prim][3] }
   --  },
    -- { rule = { instance = "chromium-browser" },
      -- properties = { tag = tags[monitors.prim][3] } },
    -- { rule = { instance = "google-chrome" },
      -- properties = { tag = tags[monitors.prim][3] } },
   --  { rule = { class = "Skype", instance = "skype" },
   --    properties = { tag = tags[monitors.prim][5], split = 250, target = "master" }
   --  },
   --  { rule = { class = "URxvt", instance = "urxvt" },
   --    callback = awful.client.setslave
   --  },
   --  -- Skype
   --  { rule = { name = ".*Chat.*" },
   --    properties = { tag = tags[monitors.prim][5] },
   --    callback = awful.client.setslave
   --  },
   --  { rule = { name = ".*Gnuplot.*" },
   --    properties = { floating = true },
   --    callback = awful.titlebar.add
   --  },
   --  -- { rule = { name = "Command Window" },
   --  --   properties = { tag = tags[0][1] },
   --  --   callback = awful.client.setmaster
   --  -- },

    -- matlab/matplotlib
    -- { rule = { name = "Figure .*" },
    --  properties = { floating = true },
    --  callback = awful.titlebar.add
    -- },
   --  -- { rule = { class = "Gnome-Control-Center" instance = "gnome-control-center" },
   --  --  properties = { floating = true }, callback = awful.titlebar.add
   --  --},
   --  { rule = { class = "Virt-manager", instance = "virt-manager" },
   --    properties = { floating = true }
   --  },
   -- { rule = { class = "Thunderbird", instance = "Mail" },
   --   properties = { tag = tags[monitors.second][1] }
   -- },
    -- { rule = { class = "Emacs", instance = "emacs" },
    --   properties = { tag = tags[monitors.second][2] }
    -- },
   --  { rule = { class = "Emacs", instance = "_Remember_" },
   --    properties = { floating = true }, callback = awful.titlebar.add
   --  },
   --  { rule = { class = "Xmessage", instance = "xmessage" },
   --    properties = { floating = true }, callback = awful.titlebar.add
   --  },
   --  { rule = { class = "Pidgin", instance = "Pidgin"},
   --    properties = { tag = tags[monitors.prim][5] }
   --  },
   --  { rule = { class = "Pidgin", role = "buddy_list"},
   --    properties = { tag = tags[monitors.prim][5] }
   -- -- properties = { floating = true, target = "master", split = 250} },
   --  },
   --  { rule = { class = "MPlayer" },
   --    properties = { tag = tags[monitors.prim][9] },
   --    callback = awful.titlebar.add
   --  },
   --  { rule = { class = "Gnote", instance = "gnote"},
   --    --properties = { split = 10 },
   --    properties = { floating = true },
   --    callback = awful.titlebar.add
   --  },
   --  { rule = { class = "Gtg", instance = "gtg"},
   --    --properties = { split = 10 },
   --    properties = { floating = true },
   --    callback = awful.titlebar.add
   --  },
   --  { rule = { class = "Toplevel" },
   --    --properties = { split = 10 },
   --    properties = { floating = true },
   --    callback = awful.titlebar.add
   --  },
   --  { rule = { class = "TkFDialog"},
   --    --properties = { split = 10 },
   --    properties = { floating = true },
   --    callback = awful.titlebar.add
   --  },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus",
                      function(c)
                         c.border_color = beautiful.border_focus
                      end)
client.connect_signal("unfocus",
                      function(c)
                         c.border_color = beautiful.border_normal
                      end)
-- }}}


-- }}}


-- }}}
