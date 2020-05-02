;; init.el

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Very annoying
;; https://stackoverflow.com/a/17422623
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'org)

;; to tangle or not tangle, that is the question
(defun my/tangle? () (if window-system "yes" "no"))

(setq vc-follow-symlinks t)
(if (eq system-type 'windows-nt)
    (delete-file (expand-file-name "emacs.el" user-emacs-directory)))
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))
