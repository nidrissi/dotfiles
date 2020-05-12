;; init.el

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Very annoying
;; https://stackoverflow.com/a/17422623
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'org)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; to tangle or not tangle, that is the question
(defun my/tangle-p () (if window-system "yes" "no"))

(setq vc-follow-symlinks t)
(if (eq system-type 'windows-nt)
    (delete-file (expand-file-name "emacs.el" user-emacs-directory)))
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))
