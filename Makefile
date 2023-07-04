# Start emacs using the local init.el file
# This will:
# - bootstrap org-babel
# - convert readme.org to a pure elisp file called readme.el, and
# - execute readme.el
#
# .. which completes the emacs startup initialization process
test:
	emacs -q -l init.el

# Copy the init.el, readme.org, jim.org and steph.org  files into $HOME/.emacs.d
copy:
	@./scripts/copy.sh

# Create a timestamped copy of the $HOME/.emacs.d folder
backup:
	@./scripts/backup.sh

# Create a temporary copy of $HOME/.emacs.d, and
# create a new $HOME/.emacs.d based on the contents of this repo
install:
	@./scripts/install.sh

# *If a backup emacs folder exists* then
# delete the current .emacs.d folder, and
# restore the .emacs.d backup
uninstall:
	@./scripts/uninstall.sh

# List all emacs related files in the home folder and
# show an abbreviated listing of the .emacs.d folder
status:
	ls -al ~ | grep emacs
	echo ""
	ls -lt ~/.emacs.d | head -n 5

# Show a tree-based listing of $HOME/.emacs.d
# (down to a depth of three levels)
#
# This can give some indication of what bootstrapping
# activities have occurred.
watch:
	watch -n 0.5 'tree -L 3 ~/.emacs.d'

# Create a backup of $HOME/.emacs.d, then delete it.
destroy: backup
	rm -rf ~/.emacs.d
