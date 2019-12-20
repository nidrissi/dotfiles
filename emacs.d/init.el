;; init.el

;; I have a lot of RAM.
(setq gc-cons-threshold 100000000)

;; Very annoying
;; https://stackoverflow.com/a/17422623
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'org)

(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "emacs.org"
                   user-emacs-directory))
