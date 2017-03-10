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
  :config
  (load-theme 'solarized-dark t))
(use-package smart-mode-line
  :config
  (sml/setup))

(global-visual-line-mode)
(diminish 'visual-line-mode)

(use-package uniquify)
(use-package company
  :diminish company-mode
  :config
  (global-company-mode))
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; dired
(use-package dired
  :commands dired
  :bind
  (:map dired-mode-map
        ("K" . dired-k)
        ("g" . dired-k))
  :init
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))
(use-package dired-k :commands dired-k)

(cua-mode t)

(use-package ace-window :bind ("C-$" . ace-window))
(use-package hippie-exp :bind ("M-/" . hippie-expand))
(use-package eshell :bind ("C-c e" . eshell))
(use-package ace-jump-mode :bind ("C-c SPC" . ace-jump-mode))

;; Disable stupid stuff
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f9>"))
(setq disabled-command-function nil)

;; Helm
(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("C-!" . helm-mini)
   ("M-s o" . helm-occur)))
(use-package projectile
  :bind ("C-c p p" . helm-projectile-switch-project)
  :diminish projectile-mode
  :config
  (require 'helm-config)
  (projectile-mode)
  (helm-projectile-on))

;; Recentf
(use-package recentf
  :config
  (recentf-mode 1))

;; Programming
;;; Typescript
(defun setup-tide-mode ()
  "Setup tide-mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
(use-package tide
  :commands tide-setup
  :config
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))
(use-package typsecript-mode
  :mode "\\.ts'"
  :init
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'setup-tide-mode))

;;; Misc prog
(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :interpreter ("perl" "perl5" "miniperl"))
(use-package markdown-mode :mode ("\\.markdown?\\'" "\\.md?\\'"))
(use-package web-mode
  :mode ("\\.\\([tT][tT]\\)\\'" ; template toolkit
         "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"))
(use-package sass-mode :mode "\\.scss?\\'")
(use-package jade-mode :mode "\\.jade\\'")
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Magit
(use-package magit
  :bind ("C-c m" . magit-status)
  :config
  (use-package ssh-agency)
  (global-magit-file-mode)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; AUCTeX
(use-package reftex
  :defer t
  :config
  (add-to-list 'reftex-bibliography-commands "addbibresource"))

(use-package latex
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
  (add-to-list
   'TeX-command-list
   '("LaTeXmk" "latexmk %(-PDF) %(-xelatex) %s" TeX-run-TeX nil t
     :help "Run Latexmk on file"))
  (add-to-list
   'TeX-expand-list
   '("%(-PDF)"
     (lambda ()
       (if (or TeX-PDF-mode TeX-DVI-via-PDFTeX)
           "-pdf" ""))))
  (add-to-list
   'TeX-expand-list
   '("%(-xelatex)"
     (lambda ()
       (if (eq TeX-engine 'xetex)
           "-e \"$pdflatex =~ s/pdflatex/xelatex/\"" ""))))

  ;; Windows
  (if (eq system-type 'windows-nt)
      (progn
        (setq preview-gs-command "gswin64c.exe")
        (add-to-list 'TeX-view-program-list
                     '("Sumatra PDF"
                       ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                        (mode-io-correlate " -forward-search %b %n") " %o")))
        (assq-delete-all 'output-pdf TeX-view-program-selection)
        (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))))

;;; Fonts (used for folding)
(dolist (range '((#x2200 . #x23ff) (#x27c0 . #x27ef) (#x2980 . #x2bff) (#x1d400 . #x1d7ff)))
  (set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs (car range)) (decode-char 'ucs (cdr range)))
   "STIX"))

;;; emacs.el ends here
