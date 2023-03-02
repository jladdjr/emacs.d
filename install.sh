#!/bin/bash

# Abort if emacs is running
if pgrep emacs > /dev/null; then 
	echo "️🛑 Cannot install .emacs.d directory while emacs is running"; 
	exit 1
fi

# Backup .emacs.d if it exists
if [ -d "$HOME/.emacs.d" ]; then 
	echo "📂 Found existing $HOME/.emacs.d"
	if [ -d "$HOME/.emacs.d.bak" ]; then 
		echo "❌️️️ $HOME/.emacs.d.bak already exists. Aborting.."; 
		exit 2
	fi
	# Moving existing .emacs.d to .emacs.d.bak
	echo "⏩️️️️ Moving $HOME/.emacs.d to $HOME/.emacs.d.bak"
	mv $HOME/.emacs.d $HOME/.emacs.d.bak;
else
	echo "👻 No existing $HOME/.emacs.d"
fi 

# Install development version of .emacs.d
echo "️️️️️️️🤘 Creating $HOME/.emacs.d with development files"
mkdir $HOME/.emacs.d; 
cp init.el readme.org $HOME/.emacs.d; 

echo "🎉 Installed!"; 
echo ""
ls -l $HOME/.emacs.d; 
