#!/bin/bash

# Abort if emacs is running
if pgrep emacs > /dev/null; then 
	echo "️🛑 Cannot restore $HOME/.emacs.d.bak directory while emacs is running"; 
	exit 1
fi

# Restore .emacs.d.bak if it exists
if [ -d "$HOME/.emacs.d.bak" ]; then 
	echo "📂 Found $HOME/.emacs.d.bak"
	if [ -d "$HOME/.emacs.d" ]; then 
		echo "🔥 Deleting $HOME/.emacs.d"
		rm -rf $HOME/.emacs.d
	fi
	# Moving .emacs.d.bak to .emacs.d
	echo "🔄 Moving $HOME/.emacs.d.bak to $HOME/.emacs.d"
	mv $HOME/.emacs.d.bak $HOME/.emacs.d
	
	echo "⏰ Restored!"
	echo ""
	ls -l $HOME/.emacs.d; 
else
	echo "🤷 No existing $HOME/.emacs.d.bak"
	exit 1
fi 
