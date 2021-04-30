-- Devilspie Apps
-- Description: Move Apps to Workspace X

apps = {
   [1] = {"Emacs"},
   [2] = {"Atril Document Viewer"},
   [3] = {"Terminal"},
   [4] = {"Firefox"},
   [5] = {"TelegramDesktop", "Clementine", "Pragha Music Player"},
}

function move_it_to (name, wk)
   if (get_application_name() == name) then
      set_window_workspace(wk)
      maximize()
   end
end

-- * run
for wk,apps in pairs(apps) do
   for _, app in ipairs(apps) do
      move_it_to(app, wk)
   end
end
