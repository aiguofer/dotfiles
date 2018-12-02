-- debug_print command does only print anything to stdout
-- if devilspie2 is run using the --debug option

app_name = string.lower(get_window_name())
start_xchat, last = string.find(app_name, 'hexchat')
start_slack, last = string.find(app_name, 'franz')

debug_print(app_name)
debug_print(string.find(app_name, 'hexchat'))
debug_print(string.find(app_name, 'slack'))
debug_print(start_slack == nil)

if (start_xchat or start_slack) then
   x, y, width, height = get_window_geometry()
   debug_print(get_window_geometry())
   unmaximize()
   pin_window()

   if (start_xchat == 1) then
      set_window_geometry(960, 712, 960, 1080)
   end

   if (start_slack == 1) then
      set_window_geometry(0, 712, 960, 1080)
   end

   maximize_vertically()
   set_skip_tasklist(true)
   set_skip_pager(true)
   -- debug_print("Window Name: " .. get_window_name())
   -- debug_print("Application name: " .. get_application_name())
end
