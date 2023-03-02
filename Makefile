test:
	emacs -q -l init.el

copy:
	@./scripts/copy.sh

backup:
	@./scripts/backup.sh

install:
	@./scripts/install.sh

uninstall:
	@./scripts/uninstall.sh

status:
	ls -al ~ | grep emacs
	echo ""
	ls -lt ~/.emacs.d | head -n 5

watch:
	watch -n 0.5 'tree -L 3 ~/.emacs.d'

destory:
	rm -rf ~/.emacs.d
