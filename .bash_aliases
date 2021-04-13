# shorten bash path
PROMPT_DIRTRIM=1

# Linux Helpers
alias setkeyrepeat='
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15
gsettings set org.gnome.desktop.peripherals.keyboard delay 180
'

alias reload-conky='
killall conky
conky & disown
'

alias update='sudo apt-get update && sudo apt-get dist-upgrade'

# General
alias c='clear'


alias edit-hosts='
sudo vim /ets/hosts
'

alias reload='. ~/.bashrc'

# SSH
alias ssh-config='cat ~/.ssh/config'
alias edit-ssh-config='vim ~/.ssh/config'

# Security
alias murder='history -c'

# Images
alias resize-jpg='mogrify -format jpg -quality 85 -resize 1920 -strip *'
alias resize-png='mogrify -format png -quality 85 -resize 1920 -strip *'

# Filepermissions
alias reset-permissions='
find . -type f -exec chmod 644 {} \;
find . -type d -exec chmod 755 {} \;
'

alias clean-project='
find . -name ".DS_Store" -type f -delete
find . -name ".idea" -type d -exec rm -r {} \;
find . -name "node_modules" -type d -prune
find . -name "node_modules" -type d -prune -exec rm -rf "{}" +
find . -name "*.pyc" -exec rm -f {} \;
'

alias clean-project-dry='
find . -name ".DS_Store" -type f -prune
find . -name ".idea" -type d -prune
find . -name "node_modules" -type d -prune
find . -name "*.pyc" -type f -prune
'

# Backups
alias backmeup='
mkdir -p /mnt/development_drive/backups/
rsync -a --delete --progress ~/.bash_aliases /mnt/development_drive/backups/me/
rsync -a --delete --progress ~/.ctags /mnt/development_drive/backups/me/
rsync -a --delete --progress ~/.tmux.conf /mnt/development_drive/backups/me/
rsync -a --delete --progress ~/.vimrc /mnt/development_drive/backups/home/
rsync -a --delete --progress ~/.gitconfig /mnt/development_drive/backups/home/
rsync -a --delete --progress ~/.gitignore_global /mnt/development_drive/backups/home/
rsync -a --delete --progress ~/.ssh /mnt/development_drive/backups/home/
'

# Tmux

# Attaches tmux to a session (example: ta portal)
alias ta='tmux attach'

# Creates a new session
alias tn='tmux new-session -s'

# Lists all ongoing sessions
alias tl='tmux list-sessions'

# Kill a session.
alias tk='tmux kill-session -t'

# Docker

# stop all
alias dsa='docker stop $(docker ps -a -q)'

# Remove Server Aliases

llias cdpr='cd /mnt/development_drive/projects/'
