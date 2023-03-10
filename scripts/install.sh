#!/bin/bash

# Abort if emacs is running
if pgrep emacs > /dev/null; then 
	echo "ī¸đ Cannot install $HOME/.emacs.d directory while emacs is running"; 
	exit 1
fi

# Backup .emacs.d if it exists
if [ -d "$HOME/.emacs.d" ]; then 
	echo "đ Found existing $HOME/.emacs.d"
	if [ -d "$HOME/.emacs.d.bak" ]; then 
		echo "âī¸ī¸ī¸ $HOME/.emacs.d.bak already exists. Aborting.."; 
		exit 2
	fi
	# Moving existing .emacs.d to .emacs.d.bak
	echo "âŠī¸ī¸ī¸ī¸ Moving $HOME/.emacs.d to $HOME/.emacs.d.bak"
	mv $HOME/.emacs.d $HOME/.emacs.d.bak
else
	echo "đģ No existing $HOME/.emacs.d"
fi 

# Install development version of .emacs.d
echo "ī¸ī¸ī¸ī¸ī¸ī¸ī¸đ¤ Creating $HOME/.emacs.d with development files"
mkdir $HOME/.emacs.d; 
cp init.el readme.org $HOME/.emacs.d; 

echo "đ Installed!"; 
echo ""
ls -l $HOME/.emacs.d; 
