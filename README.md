# dotfiles
my home directory configuration

# X features
* xmonad configuration
## Cheatsheet
* `$ terminator` - great terminal with great coding font that supports vim
  powerline and solarized theme for eye peace
* `$ wallpaper` - command that downloads randomly one of 10 latest bing backgrounds
  and sets it as a desktop wallpaper (without actually saving a file on disk)
* `$ idea` - command that starts up intellij idea
* `$ doma-setup.sh` - command that configures xinerama with notebook screen on
  the left side of external monitor

# xmonad features
* _Mute, Volume Up, Volume Down_ - mapped to amixer commands
* Backlight controls
* Fixed Java GUI apps loading with setWMName "LG3D"
* _xmobar_ and _wallpaper_ run on startup
* better borders management TODO: remove border if windows is shown alone on
  current WS even in xinerama
## Cheatsheet
* `Ctrl-Alt-S` - secure lock and suspend
* `Ctrl-Alt-L` - secure lock
* `Print Screen` - capture selected fragment of a screen with flameshot

# Shell features
* _ls, dir, vdir, grep, fgrep, egrep_ are colorfull
* bash completion - autocompletes command parameters
* history: ignore duplicates, million entries, ignore: ls,bg,df,history, save
  timestamps
* prompt: show branch of git repo
* runs ssh-agent by default
## Cheatsheet
* `Esc` - vi mode for bash
* `$ alert` - notifies when long running command has ended syntax: log_running_command; alert
* `$ tmux` uses vi bindings for copy mode Ctrl-B [, paste in any tmux window with
  Ctrl-B ]
