#!/bin/bash

# Abort if emacs is running
if pgrep emacs > /dev/null; then 
	echo "️🛑 Cannot install $HOME/.emacs.d directory while emacs is running"; 
	exit 1
fi

# Backup .emacs.d if it exists
if [ -d "$HOME/.emacs.d" ]; then 
	echo "📂 Found existing $HOME/.emacs.d"

	TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
	echo "⏩️️️️ Copying $HOME/.emacs.d to $HOME/.emacs.d.bak-${TIMESTAMP}"
	cp -r $HOME/.emacs.d "$HOME/.emacs.d.bak-${TIMESTAMP}"
	echo "🥳 Backed up!"
	ls -al $HOME | grep emacs
else
	echo "👻 No existing $HOME/.emacs.d"
	exit 1
fi 
