;;; emacs.el -- My init file

;;; Commentary:
;;; This is my init file.

;;; Code:
;;; Unicode!
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Init
(fset 'yes-or-no-p 'y-or-n-p)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(cd (getenv "HOME"))
(server-start)

;; Must be near the beginning
(setq grep-program "\"c:/Program Files/Git/usr/bin/grep.exe\""
      find-program "\"c:/Program Files/Git/usr/bin/find.exe\"")

;; use-package
(require 'use-package)

;; Interface
(setq ring-bell-function 'ignore)
(openwith-mode)
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))
(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

(add-hook 'after-init-hook 'global-visual-line-mode)
(diminish 'visual-line-mode)

(use-package uniquify)
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (add-hook 'after-init-hook 'global-undo-tree-mode))

;; dired
(use-package dired
  :commands dired
  :bind
  (:map dired-mode-map
        ("K" . dired-k)
        ("g" . dired-k))
  :init
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook 'dired-k-no-revert))
(use-package dired-k
 :ensure t
 :commands dired-k)
(use-package dired-x
  :commands dired-omit-mode
  :init
  (add-hook 'dired-mode-hook 'dired-omit-mode))


(cua-mode t)

(use-package ace-window
  :ensure t
  :bind ("C-$" . ace-window))
(use-package hippie-exp :bind ("M-/" . hippie-expand))
(use-package eshell :bind ("C-c e" . eshell))
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode))

;; Disable stupid stuff
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f9>"))
(setq disabled-command-function nil)

;; Helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("C-!" . helm-mini)
   ("M-s o" . helm-occur)))

(use-package helm-projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p p" . helm-projectile-switch-project)
   ("C-c p h" . helm-projectile))
  :config
  (use-package projectile :ensure t)
  (projectile-mode)
  (helm-projectile-on))

;; Recentf
(use-package recentf :config (recentf-mode 1))

;; Flyspell
(use-package flyspell-correct-helm
  :ensure t
  :bind ("M-*" . flyspell-correct-previous-word-generic))

;; Typescript
(defun setup-tide-mode ()
  "Setup tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
(use-package tide
  :ensure t
  :commands tide-setup
  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil)))
(use-package typescript-mode
  :ensure t
  :mode "\\.ts'"
  :init
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'setup-tide-mode))

;;; Misc prog
(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :interpreter ("perl" "perl5" "miniperl"))
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown?\\'" "\\.md?\\'"))
(use-package web-mode
  :ensure t
  :mode ("\\.\\([tT][tT]\\)\\'" ; template toolkit
         "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))
(use-package sass-mode
  :ensure t
  :mode "\\.scss?\\'")
(use-package jade-mode
  :ensure t
  :mode "\\.jade\\'")
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; smartparens
(use-package smartparens-config
  :ensure smartparens
  :init
  (smartparens-global-mode))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status)
  :config
  (global-magit-file-mode)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setenv "SSH_ASKPASS" "git-gui--askpass"))
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode))

;; AUCTeX
(use-package reftex
  :ensure t
  :defer t
  :config
  (add-to-list 'reftex-bibliography-commands "addbibresource"))

(use-package latex
  :ensure auctex
  :mode ("\\.tex'" . latex-mode)
  :init
  (setq ispell-tex-skip-alists
        (list
         (append
          (car ispell-tex-skip-alists)
          '(("\\\\cref" ispell-tex-arg-end)
            ("\\\\Cref" ispell-tex-arg-end)
            ("\\\\import" ispell-tex-arg-end 2)
            ("\\\\textcite" ispell-tex-arg-end)))
         (cadr ispell-tex-skip-alists)))

  ;; hooks
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-default "LaTeXmk"
                    ;; I don't know why AUCTeX devs think they know better...
                    company-minimum-prefix-length 3)))

  :config
  (add-to-list 'LaTeX-font-list '(11 "" "" "\\mathfrak{" "}"))

  ;; Fold
  (add-to-list 'LaTeX-fold-macro-spec-list '("[r]" ("cref" "Cref")))
  (add-to-list 'LaTeX-fold-macro-spec-list '("[c]" ("textcite")))
  (add-to-list 'LaTeX-fold-macro-spec-list '("[f]" ("tablefootnote")))
  (add-to-list 'LaTeX-fold-macro-spec-list '("[n]" ("nomenclature")))
  (add-to-list 'LaTeX-fold-math-spec-list '("[" ("lbrack")))
  (add-to-list 'LaTeX-fold-math-spec-list '("]" ("rbrack")))
  (add-to-list 'LaTeX-fold-math-spec-list '("\u00ab" ("og")))
  (add-to-list 'LaTeX-fold-math-spec-list '("\u00bb" ("fg")))
  (add-to-list 'tex--prettify-symbols-alist '("\\varphi" . 966))
  (add-to-list 'tex--prettify-symbols-alist '("\\coloneqq" . 8788))
  (add-to-list 'tex--prettify-symbols-alist '("\\vartheta" . 977))
  (add-to-list 'tex--prettify-symbols-alist '("\\varnothing" . 8709))

  ;; reftex
  (TeX-add-style-hook
   "cleveref"
   (lambda ()
     (if (boundp 'reftex-ref-style-alist)
         (add-to-list
          'reftex-ref-style-alist
          '("Cleveref" "cleveref"
            (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
     (reftex-ref-style-activate "Cleveref")
     (TeX-add-symbols
      '("cref" TeX-arg-ref)
      '("Cref" TeX-arg-ref)
      '("cpageref" TeX-arg-ref)
      '("Cpageref" TeX-arg-ref))))

  ;; LaTeXmk
  (use-package auctex-latexmk :ensure t)
  (auctex-latexmk-setup))

;;; Fonts (used for folding)
(dolist (range '((#x2200 . #x23ff) (#x27c0 . #x27ef) (#x2980 . #x2bff) (#x1d400 . #x1d7ff)))
  (set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs (car range)) (decode-char 'ucs (cdr range)))
   "STIX"))

;;; emacs.el ends here
