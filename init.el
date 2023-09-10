;;; Retrieved from
;;; https://orgmode.org/worg/org-contrib/babel/intro.html#emacs-initialization
;;; on 2023.01.05

;;; init.el --- Where all the magic begins
;;
;; Before starting emacs, must copy source of org-mode to ~/.emacs.d/src
;; and build the source.
;;
;; cd ~/.emacs.d
;; mkdir src
;; cd src
;; git clone https://git.savannah.gnu.org/git/emacs/org-mode.git org
;; cd org; make autoloads
;;
;; This file loads Org and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir)))))
       (add-to-list 'load-path org-dir)
       (add-to-list 'load-path org-contrib-dir)
  ;; load up Org and Org-babel
  (require 'org)
  (require 'ob-tangle))

;;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

;;; load up all literate org-mode files in the private directory
(setq private-dotfiles-dir (expand-file-name "private/" dotfiles-dir))

(if (file-directory-p private-dotfiles-dir)
	(mapc #'org-babel-load-file (directory-files private-dotfiles-dir t "\\.org$")))

;;; init.el ends here
