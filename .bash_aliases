#######################################################################
#                              VARIABLES                              #
#######################################################################

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

alias update='
sudo apt-get update
sudo apt-get dist-upgrade
'

# General
alias c='clear'


alias edit-hosts='
sudo vim /etc/hosts
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

# Remove Server Aliases
alias cdpr='cd /mnt/development_drive/projects/'

# Docker stop all
alias dsa='docker stop $(docker ps -a -q)'


#######################################################################
#                   GLOBAL ALIASES, LOCAL & REMOTE                    #
#######################################################################

REMOTE_PROJECTS_PATH='/mnt/development_drive/projects'
LOCAL_PROJECTS_PATH='/home/vernon/Devenv/projects'

# make sure the remote permissions are correct.
# $1, the folder path of the project from within the projects folder.
function setRemoteProjectPermissions() {
	ssh Dev "cd $REMOTE_PROJECTS_PATH && cd $1 && find . -name "mysql" -type d -exec chown -R systemd-coredump:root {} \;"
	ssh Dev "cd $REMOTE_PROJECTS_PATH && cd $1 && find . -name "uploads" -type d -exec chown -R www-data:www-data {} \;"
	ssh Dev "cd $REMOTE_PROJECTS_PATH && cd $1 && find . -name "plugins" -type d -exec chown -R www-data:www-data {} \;"
}

# Pulls a mysql database from my remote development server.
# $1, the ssh connection string.
# $2, the mysql container name, id.
function getRemoteDockerDatabase() {
	local loc_mysql_user="root"
	local loc_mysql_password="my-secret-pw"
	local loc_mysql_database="exampledb"

	# dump database on server.
	ssh $1 "docker exec -i $2 mysqldump --user=\"$loc_mysql_user\" --password=\"$loc_mysql_password\" $loc_mysql_database > ~/temp/$2.sql"
	ssh $1 "ls -la ~/temp/"

	# rsync it down.
	rm -f ./$2.sql
	rsync -a --delete --progress $1:~/temp/$2.sql ./
}


# should be used in wordpress project root only.
# $1, the ssh connection string.
# $2, the path where the wordpress uploads, plugins are.
function syncRemoteWordPressData() {
	rsync -a --delete --progress $1:$REMOTE_PROJECTS_PATH$2/uploads ./docker-data/
	rsync -au --progress $1:$REMOTE_PROJECTS_PATH$2/plugins ./docker-data/
}

# make sure the local permissions are correct.
# $1, the root path.
function setLocalProjectPermissions() {
	cd $1
	echo 'secret' | sudo -S find . -name "mysql" -type d -exec chown -R systemd-coredump:vernon {} \;
	sudo find . -name "uploads" -type d -exec chown -R www-data:www-data {} \;
	sudo find . -name "plugins" -type d -exec chown -R www-data:www-data {} \;
	cd -
}

# Uploades the database to a running mysql container.
# $1, the mysql container name, id.
# $2, the mysql database path.
function replaceLocalDockerDatabase() {
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
function replaceLocalDockerWordPressURL() {
	# avoid asking for sudo password.
	echo 'secret' | sudo -S ls ./

	# install wordpress cli.
	sudo docker exec -i $1 sh -c "cd /var/www/html && curl -O https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar"
	sudo docker exec -i $1 sh -c "cd /var/www/html && chmod +x wp-cli.phar"
	sudo docker exec -i $1 sh -c "cd /var/www/html && mv wp-cli.phar /usr/local/bin/wp"

	# replace wordpress url.
	sudo docker exec -i $1 sh -c "cd /var/www/html && wp search-replace --all-tables '$2' '$3' --allow-root"
}

# replace match with provided string. Uses vim like patterns.
# $1, path of file.
# $2, match.
# $3, replacement.
function replaceInFile() {
	sed -i "s/$2/$3/g" $1
}
