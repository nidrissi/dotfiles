;; init.el

(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file
 (expand-file-name "emacs.org"
                   user-emacs-directory))
