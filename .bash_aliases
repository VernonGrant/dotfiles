#######################################################################
#                              VARIABLES                              #
#######################################################################

# shorten bash path
PROMPT_DIRTRIM=1

# Linux Helpers
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
alias back-me-up='. ~/.scripts/backup-local.sh'

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

# setup tmux sessions.
alias tmux-setup=". ~/.scripts/tmux-setup.sh"

# Docker stop all
alias docker-sa='docker stop $(docker ps -a -q)'

#######################################################################
#                          HELPER FUNCTIONS                           #
#######################################################################

# Pulls a mysql database from my remote development server.
# sudo apt install poppler-utils
# $1, pdf file
function pdf-to-jpg() {
	pdftoppm -jpeg $1 page
}

# Overlays a signed and stamped PDF ontop the provided PDF's last page.
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

# $1, the ssh connection string.
# $2, the path that should be pulled.
# $3, the local destination.
function pullRemoteFiles() {
	rsync -auv $1:$2 $3
}

# $1, the path that should be pushed.
# $2, the ssh connection string.
# $3, the remote destination.
function pushLocalFiles() {
	rsync -auv $1 $2:$3
}

#
# $1, the folder path.
function setDefaultOwnership() {
	cd $1
	echo 'secret' | sudo -S chown -R vernon:vernon ./
	cd -
}

#
# $1, the folder path.
function setDockerDataOwnership() {
	cd $1
	#echo 'secret' | sudo -S find . -name "mysql" -type d -exec chown -R systemd-coredump:vernon {} \;
	echo 'secret' | sudo -S find . -name "uploads" -type d -exec chown -R www-data:www-data {} \;
	sudo find . -name "plugins" -type d -exec chown -R www-data:www-data {} \;
	cd -
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

# replace match with provided string. Uses vim like patterns.
# $1, path of file.
# $2, match.
# $3, replacement.
function replaceInFile() {
	sed -i "s/$2/$3/g" $1
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

# Old Functions Below.

# TODO: remove.
# still used by, aap, miogyn, ecsaf.
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


# TODO: remove.
# still used by, aap, ecsaf.
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
