#!/bin/bash

# Abort if emacs is running
if pgrep emacs > /dev/null; then 
	echo "️🛑 Cannot copy development files into $HOME/.emacs.d directory while emacs is running"; 
	exit 1
fi

if [ -d "$HOME/.emacs.d" ]; then 
	echo "📂 Found existing $HOME/.emacs.d"
	echo "⏩️️️️ ️Copying development files into $HOME/.emacs.d"
	cp init.el readme.org $HOME/.emacs.d
else
	echo "👻 No existing $HOME/.emacs.d"
fi 

echo "️️️️️️️🤘 Done!"
echo ""
ls -l $HOME/.emacs.d; 
