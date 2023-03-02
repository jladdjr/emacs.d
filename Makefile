test:
	emacs -q -l init.el

copy:
	@./copy.sh

backup:
	@./backup.sh

install:
	@./install.sh

uninstall:
	@./uninstall.sh

status:
	ls -al ~ | grep emacs
	echo ""
	ls -l ~/.emacs.d

watch:
	watch -n 0.5 'ls -l ~/.emacs.d'

destory:
	rm -rf ~/.emacs.d
