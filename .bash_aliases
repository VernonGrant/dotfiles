#######################################################################
#                              VARIABLES                              #
#######################################################################

# shorten bash path
PROMPT_DIRTRIM=1

alias reload='. ~/.bashrc'
alias c='clear'

# Linux helpers
alias set-key-repeat='
xset r rate 150 60
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 15
gsettings set org.gnome.desktop.peripherals.keyboard delay 150
'

alias stress-test='
stress --cpu 12 --vm 14 --vm-bytes 1024M --timeout 120
'

alias reload-conky='
killall conky
conky & disown
'

alias update='
sudo apt-get update
sudo apt-get dist-upgrade
'

alias today='
watch -n 1 cat ~/Devenv/notes/today.md
'

alias block-social='
sudo echo "" >> /etc/hosts
echo "0.0.0.0 facebook.com" >> /etc/hosts
echo "0.0.0.0 www.facebook.com" >> /etc/hosts
echo "0.0.0.0 youtube.com" >> /etc/hosts
echo "0.0.0.0 www.youtube.com" >> /etc/hosts
echo "0.0.0.0 twitter.com" >> /etc/hosts
echo "0.0.0.0 www.twitter.com" >> /etc/hosts
echo "0.0.0.0 instagram.com" >> /etc/hosts
echo "0.0.0.0 www.instagram.com" >> /etc/hosts
cat /etc/hosts
'

alias empty-trash='
rm -rf ~/.local/share/Trash/*
'

# General
alias edit-hosts='sudo vim /etc/hosts'

# SSH
alias ssh-config='cat ~/.ssh/config'
alias edit-ssh-config='vim ~/.ssh/config'

# Security
alias murder='history -c'

# Images
alias resize-jpg='mogrify -format jpg -quality 85 -resize 1920 -strip *'
alias resize-png='mogrify -format png -quality 85 -resize 1920 -strip *'

# File Permissions
alias reset-permissions='
find . -type f -exec chmod 644 {} \;
find . -type d -exec chmod 755 {} \;
'

alias clean-project='
find . -name ".DS_Store" -type f -delete
find . -name ".idea" -type d -exec rm -r {} \;
find . -name ".vscode" -type d -exec rm -r {} \;
find . -name "node_modules" -type d -exec rm -r {} \;
find . -name ".source-completions" -type d -exec rm -r {} \;
find . -name "*.pyc" -exec rm -f {} \;
'

alias clean-project-dry='
find . -name ".DS_Store" -type f -prune
find . -name ".idea" -type d -prune
find . -name ".vscode" -type d -prune
find . -name "node_modules" -type d -prune
find . -name ".source-completions" -type d -prune
find . -name "*.pyc" -type f -prune
'

# Backup my local machine to external drives.
alias back-me-up='rsync -av --delete --exclude="node_modules" --exclude=".cache" ~/ /media/vernon/Storage/Backups/'

# Tmux
# Attaches tmux to a session (example: ta portal)
alias ta='tmux attach'

# Creates a new session
alias tn='tmux new-session -s'

# Lists all ongoing sessions
alias tl='tmux list-sessions'

# Kill a session.
alias tk='tmux kill-session -t'

# Kill all sessions.
alias tka='tmux kill-server'

# Setup tmux sessions.
alias tmux-setup=". ~/.scripts/tmux-setup.sh"

# Docker stop all.
alias docker-stop-all='docker stop $(docker ps -a -q)'

# Source Completions (lsp-mode and CTags).
alias source-completions-update='. ~/.scripts/source-completions-update.sh'
alias source-completions-list='ls -la ~/Devenv/source-completions/'

#######################################################################
#                          HELPER FUNCTIONS                           #
#######################################################################

# Cutting MP3 files example.
# sudo aptitude install poc-streamer
# mp3cut -o output.mp3 -t 00:00:00+000-00:02:05+000 source.mp3

# Convert the pages of a PDF file to images.
function pdf-to-jpg() {
    pdftoppm -jpeg -rx 200 -ry 200 $1 page
}

# Mark business invoices as paid.
function pdf-mark-paid() {
    filename="${1%.*}"
    qpdf $1 --overlay ~/.scripts/paid-marker.pdf --to=z -- $filename-paid.pdf

    gs \
	-sOutputFile=$filename-paid-signed-grayed.pdf \
	-sDEVICE=pdfwrite \
	-sColorConversionStrategy=Gray \
	-dProcessColorModel=/DeviceGray \
	-dCompatibilityLevel=1.4 \
	-dNOPAUSE \
	-dBATCH \
	$filename-paid-decrypt.pdf
}


#######################################################################
#                          GLOBAL VARIABLES                           #
#######################################################################

LOCAL_PROJECTS_PATH='/home/vernon/Devenv/projects'

#######################################################################
#                   GLOBAL ALIASES, LOCAL & REMOTE                    #
#######################################################################

# Add source completions to a project. For example calling
# 'add-source-completions wordpress woocommerce' will add all the
# source code for that fromwork inside the current project. This is
# then used by Emacs's lsp-mode.
function source-completions-add() {
    for var in "$@"
    do
	mkdir -p ./.source-completions/$var
	cp -R ~/Devenv/source-completions/$var/* ./.source-completions/$var
    done
}

# Download files from a remote server.
# $1, the ssh connection string.
# $2, the path that should be pulled.
# $3, the local destination.
function pullRemoteFiles() {
    rsync -auv $1:$2 $3
}

# Upload local files to a remote server.
# $1, the path that should be pushed.
# $2, the ssh connection string.
# $3, the remote destination.
function pushLocalFiles() {
    rsync -auv $1 $2:$3
}

# Make me the owner of all the files under the given path.
# $1, the folder path.
function setDefaultOwnership() {
    cd $1
    echo 'secret' | sudo -S chown -R vernon:vernon ./
    cd -
}

# Uploades the database to a running mysql container.
# $1, the mysql container name, id.
# $2, the mysql database path.
function uploadDatabaseToDockerContainer() {
    local loc_mysql_user="root"
    local loc_mysql_password="my-secret-pw"
    local loc_mysql_database="exampledb"

    # avoid asking for sudo password.
    echo 'secret' | sudo -S ls ./

    # upload database to local running container.
    cat $2 | sudo docker exec -i $1 /usr/bin/mysql --user="$loc_mysql_user" --password="$loc_mysql_password" $loc_mysql_database
}

# Changes the URL of a running WordPress container.
# $1, the wordpress container id.
# $2, the current url.
# $3, the replacement url.
function changeDockerContainerWordPressURL() {
    # avoid asking for sudo password.
    echo 'secret' | sudo -S ls ./

    # install wordpress cli.
    sudo docker exec -i $1 sh -c "cd /var/www/html && curl -O https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar"
    sudo docker exec -i $1 sh -c "cd /var/www/html && chmod +x wp-cli.phar"
    sudo docker exec -i $1 sh -c "cd /var/www/html && mv wp-cli.phar /usr/local/bin/wp"

    # replace wordpress url.
    sudo docker exec -i $1 sh -c "cd /var/www/html && wp search-replace --all-tables '$2' '$3' --allow-root"
}

# Pulls a mysql database from my remote development server.
# $1, the ssh connection string.
# $2, the mysql user.
# $3, the mysql user password.
# $4, the mysql database name.
function getRemoteDatabase() {
    # dump database on server.
    ssh $1 "mkdir -p ~/temp && mysqldump --user=\"$2\" --password=\"$3\" $4 > ~/temp/$4.sql && ls -la ~/temp/"

    # rsync it down.
    rsync -a --delete $1:~/temp/$4.sql ./
}

# Replace match with provided string. Uses vim like patterns.
# $1, path of file.
# $2, match.
# $3, replacement.
function replaceInFile() {
    sed -i "s/$2/$3/g" $1
}
