test:
	emacs -q -l init.el

install:
	@./install.sh

uninstall:
	@./uninstall.sh

status:
	ls -al ~ | grep emacs
	echo ""
	ls -l ~/.emacs.d
