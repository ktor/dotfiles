Welcome to my home directory configuration.

# XMonad keyboard shortcuts
* _Mute, Volume Up, Volume Down_ - mapped to amixer commands
* ``Ctrl-``` - toggle copyq, seachable clipboard manager
* `Ctrl-Win-S` - secure lock and suspend
* `Ctrl-Win-L` - secure lock
* `Ctrl-Win-V` - keepass auto-type TODO: doesn't work currently
* `Print Screen` - capture selected fragment of a screen

# Features
* Backlight controls
* Fixed Java GUI apps loading with setWMName "LG3D"
* _xmobar_ and _wallpaper_ run on startup, cool colorful keyboard layout status
* notifications
* xcape - use CTRL as ESC
* unclutter - hide mouse cursor when not in use

# Shell features
* excellent bash history extension [hsrt][1]
** history: ignore duplicates, million entries, ignore: ls,bg,df,history, save
  timestamps
* _ls, dir, vdir, grep, fgrep, egrep_ are colorfull
* bash
** bash completion - autocompletes command parameters
** prompt: styled with utf icons, shows branch of git repo
** `Esc` - vi mode for bash
* syntax highlighting in less, more and cat
* ghci convenience setup
* git config
* npm config
* ssh config

## Scripts and aliases
* `$ alert` - notifies when long running command has ended syntax: log_running_command; alert
* `$ tmux` uses vi bindings for copy mode Ctrl-B [, paste in any tmux window with Ctrl-B ]
* `$ clean_maven_local_repository.sh` cleans leftover files in maven repository that sometimes block library updates
* `$ patch_gradle.sh` patches gradle libnative-platform.so for all gradle
  versions installed with wrapper
* `$ patch_node.sh` patches `node` executables in recursively for current
  directory and all descendats, used when local node executable is installed with gradle/maven
* `$ catcolor` adds code highlighting for cat output
* `$ curljson` adds code prettyfier and highlighting for curl json output
* `$ commit-merge` calls git commit with default merge commit message as a
  template that you can edit before commiting
* `$ wallpaper` - command that downloads randomly one of 10 latest bing backgrounds
  and sets it as a desktop wallpaper (without actually saving a file on disk)
* `$ monitor-*.sh` - commands that configure xinerama for different
  configurations of external monitors/beamers, location based
* `daily-home-backup.sh` - incremental full system backup to external disk
* `gw` - great scripts that runs nearest gradle wrapper
* `menu.sh` - include PATH modified in bashrc in launcher environment
* `mime-type-setup.sh` - normalize default apps for various mime types
* `vimurl` - read web url in vim
* `weather.sh` - great script that shows current weather forecast in your location within terminal
* `z` - fuzzy jump to directory
### Inactive scripts, reference code
* `patch-*` - modifies downloaded linux binaries to run on nixos
* `enable-elasticsearch-in-docker.sh` - solved in nixos config, fixes: https://www.elastic.co/guide/en/elasticsearch/reference/current/vm-max-map-count.html

[1]:https://github.com/dvorka/hstr
