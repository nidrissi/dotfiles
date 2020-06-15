;;; init.el

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'org)                  ; https://stackoverflow.com/a/17422623
(setq vc-follow-symlinks t)
;; use file-truename to avoid symlink being "younger" than the tangled file
(org-babel-load-file (file-truename (expand-file-name "emacs.org" user-emacs-directory)))
