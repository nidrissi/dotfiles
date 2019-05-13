;; init.el

;; I have a lot of RAM.
(setq gc-cons-threshold 100000000)

(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "emacs.org"
                   user-emacs-directory))
