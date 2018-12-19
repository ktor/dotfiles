# dotfiles
my home directory configuration

# X features
* xmonad configuration
## X Cheatsheet
* `$ terminator` - great terminal with great coding font that supports vim powerline and solarized theme for eye peace, infinite output buffer for convenient review of long running tail
* `$ wallpaper` - command that downloads randomly one of 10 latest bing backgrounds
  and sets it as a desktop wallpaper (without actually saving a file on disk)
* `$ idea` - command that starts up intellij idea
* `$ monitor-*.sh` - commands that configure xinerama for different
  configurations of external monitors/beamers, location based

# xmonad features
* _Mute, Volume Up, Volume Down_ - mapped to amixer commands
* Backlight controls
* Fixed Java GUI apps loading with setWMName "LG3D"
* _xmobar_ and _wallpaper_ run on startup, cool colorful keyboard layout status
* better borders management
* notifications
* xcape - use CTRL as ESC
* unclutter - hide mouse cursor when not in use
## xmonad Cheatsheet
* ``Ctrl-``` - toggle copyq, seachable clipboard manager
* `Ctrl-Win-S` - secure lock and suspend
* `Ctrl-Win-L` - secure lock
* `Ctrl-Win-V` - keepass auto-type TODO: doesn't work currently
* `Print Screen` - capture selected fragment of a screen

# Shell features
* _ls, dir, vdir, grep, fgrep, egrep_ are colorfull
* bash completion - autocompletes command parameters
* syntax highlighting in less, more and cat
* ghci convenience setup
* git config
* npm config
* ssh config
* history: ignore duplicates, million entries, ignore: ls,bg,df,history, save
  timestamps
* prompt: show branch of git repo
## shell Cheatsheet
* `Esc` - vi mode for bash
* `$ alert` - notifies when long running command has ended syntax: log_running_command; alert
* `$ tmux` uses vi bindings for copy mode Ctrl-B [, paste in any tmux window with Ctrl-B ]
* `$ clean_maven_local_repository.sh` cleans leftover files in maven repository that sometimes block library updates
* `$ dotfiles-pull.sh` installs dotfiles in current directory
* `$ patch_gradle.sh` patches gradle libnative-platform.so for all gradle
  versions installed with wrapper
* `$ patch_node.sh` patches `node` executables in recursively for current
  directory and all descendats, used when local node executable is installed with gradle/maven
* `$ catcolor` adds code highlighting for cat output
* `$ curljson` adds code prettyfier and highlighting for curl json output
* `$ commit-merge` calls git commit with default merge commit message as a
  template that you can edit before commiting

