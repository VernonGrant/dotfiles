# ========================================
# ALIASES
# ========================================

# bash profile
alias reload='. ~/.zprofile && echo "reloaded!"'
alias edit-profile='vim ~/.bash_profile'

# general
alias c='clear'
alias ll='ls -la'

# ssh
alias ssh-config='cat ~/.ssh/config'
alias edit-ssh-config='vim ~/.ssh/config'

# security
alias murder='history -c'

# images
alias resize-jpg-hd='mogrify -format jpg -quality 95 -resize 1920 -strip *'
alias resize-png-hd='mogrify -format png -quality 95 -resize 1920 -strip *'

# docker
alias docker-stop-all='docker stop $(docker ps -aq)'
alias docker-restart-no='docker update --restart=no $(docker ps -aq)'

# filepermissions
alias reset-permissions='
find . -type f -exec chmod 644 {} \;
find . -type d -exec chmod 755 {} \;
'

# Edit and update hosts file.
alias edit-hosts='
sudo vim /etc/hosts
sudo dscacheutil -flushcache
'

# ========================================
# Backups
# ========================================

alias backmeup='
mkdir -p /Volumes/Backups/Home/.config/filezilla
rsync -a --delete --progress ~/.config/filezilla /Volumes/Backups/Home/.config/
rsync -a --delete --progress ~/.bash_profile /Volumes/Backups/Home/
rsync -a --delete --progress ~/.ctags /Volumes/Backups/Home/
rsync -a --delete --progress ~/.gitconfig /Volumes/Backups/Home/
rsync -a --delete --progress ~/.gitignore_global /Volumes/Backups/Home/
rsync -a --delete --progress ~/.NERDTreeBookmarks /Volumes/Backups/Home/
rsync -a --delete --progress ~/.ssh /Volumes/Backups/Home/
rsync -a --delete --progress ~/.viminfo /Volumes/Backups/Home/
rsync -a --delete --progress ~/.vimrc /Volumes/Backups/Home/
rsync -a --delete --progress ~/.vimrc_legacy /Volumes/Backups/Home/
rsync -a --delete --progress ~/.vim /Volumes/Backups/Home/
rsync -a --delete --progress ~/.vim-project-vimrcs /Volumes/Backups/Home/
rsync -a --delete --progress ~/vim-snippets /Volumes/Backups/Home/
rsync -a --delete --progress ~/.zprofile /Volumes/Backups/Home/
rsync -a --delete --progress ~/.zsh_history /Volumes/Backups/Home/
rsync -a --delete --progress ~/.zshrc /Volumes/Backups/Home/
rsync -a --delete --progress ~/notes /Volumes/Backups/Home/
rsync -a --delete --progress /Volumes/Socrates/Jackie /Volumes/Backups/
rsync -a --delete --progress /Volumes/Socrates/Photos /Volumes/Backups/
rsync -a --delete --progress /Volumes/Socrates/Projects /Volumes/Backups/
rsync -a --delete --progress /Volumes/Socrates/ProjectAssets /Volumes/Backups/
'

# ========================================
# Adobe Creative Cloud
#
# Stop adobe creative cloud agents from
# starting up when not needed.
# /Library/LaunchAgents
# /Library/LaunchDaemons
# /user/Library/LaunchAgents
# ========================================

alias kill-adobe='
rm ~/Library/LaunchAgents/com.adobe.*.plist &&
sudo rm /Library/LaunchAgents/com.adobe.*.plist
sudo rm /Library/LaunchDaemons/com.adobe.*.plist
'

# ========================================
# TOOLS
# ========================================

# composer
PATH=$HOME/.composer/vendor/bin:$PATH

export PATH

# ========================================
# SETUP SCRIPTS
# ========================================

# Help put mac to sleep when the lis is closed.
# https://github.com/pirj/noclamshell
# brew install pirj/homebrew-noclamshell/noclamshell
# brew services start noclamshell

# Add the following to .zprofile
# source ~/.bash_profile

# Enable emacs bindings in terminal by adding the following to .zshrc
# bindkey -e

# Disable press and hold function.
# defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Set key repeat rates.
# defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
# defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)

# ========================================
# Projects
#
# find . -name "node_modules" -type d -prune | xargs du -chs
# find . -name "node_modules" -type d -prune -exec rm -rf '{}' +
# ========================================
