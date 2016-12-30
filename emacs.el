;;; Unicode!
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init
(fset 'yes-or-no-p 'y-or-n-p)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Horrible hack, has to be maintained manually
(setq my-packages '(ace-window ace-jump-mode auctex company diminish flycheck git-commit haskell-mode helm helm-projectile magit markdown-mode openwith powerline powershell projectile rainbow-delimiters sass-mode smart-mode-line smart-mode-line-powerline-theme ssh-agency tide tuareg typescript-mode undo-tree visual-fill-column web-mode with-editor yaml-mode))
(defun install-my-packages ()
  (interactive)
  (mapc #'package-install my-packages))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))
(cd (getenv "HOME"))
(server-start)

(setq grep-program "\"c:/Program Files/Git/usr/bin/grep.exe\""
      find-program "\"c:/Program Files/Git/usr/bin/find.exe\"")

;; Interface
(setq ring-bell-function 'ignore)
(openwith-mode)
(sml/setup)
(setq visual-fill-column-width 80)
(global-visual-line-mode)
(require 'uniquify)
(global-company-mode)
(global-undo-tree-mode)

;; Keybindings
(require 'my-mode)
(cua-mode t)                            ; order matters!
(require 'diminish)                     ; hide from mode line
(diminish 'my-mode)

(define-key my-mode-map (kbd "C-;") #'ace-window)
(define-key my-mode-map (kbd "M-/") #'hippie-expand)
(define-key my-mode-map (kbd "C-c e") #'eshell)
(define-key my-mode-map (kbd "C-x C-b") #'ibuffer)
(define-key my-mode-map (kbd "C-c m") #'magit-status)
(define-key my-mode-map (kbd "C-c SPC") #'ace-jump-mode)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f9>"))
(setq disabled-command-function nil)

;; Helm
(require 'helm-config)
(helm-mode 1)
(define-key my-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key my-mode-map (kbd "C-x C-b") 'helm-buffers-list)
(define-key my-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key my-mode-map (kbd "C-,") 'helm-mini)
(define-key my-mode-map (kbd "M-s o") 'helm-occur)
(projectile-mode)
(helm-projectile-on)

;; Programming
;;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;;; Misc prog
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.markdown?\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md?\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.\\([tT][tT]\\)\\'" . web-mode)) ; template toolkit
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Magit
(require 'ssh-agency)
(global-magit-file-mode)
(autoload 'magit-status "magit")
(setq magit-last-seen-setup-instructions "1.4.0")
(setenv "SSH_ASKPASS" "git-gui--askpass")

;; AUCTeX
(setq ispell-tex-skip-alists
      (list
       (append
        (car ispell-tex-skip-alists)
        '(("\\\\cref" ispell-tex-arg-end)
          ("\\\\Cref" ispell-tex-arg-end)
          ("\\\\import" ispell-tex-arg-end 2)
          ("\\\\textcite" ispell-tex-arg-end)))
       (cadr ispell-tex-skip-alists)))

(eval-after-load "reftex"
  '(progn
     (add-to-list 'reftex-bibliography-commands "addbibresource")))

(eval-after-load "latex"
  '(progn
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
     (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
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
     (add-hook 'LaTeX-mode-hook
               (lambda () (setq TeX-command-default "LaTeXmk")))

     ;; Windows
     (if (eq system-type 'windows-nt)
         (progn
           (setq preview-gs-command "gswin64c.exe")
           (add-to-list 'TeX-view-program-list
                        '("Sumatra PDF"
                          ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                           (mode-io-correlate " -forward-search %b %n") " %o")))
           (assq-delete-all 'output-pdf TeX-view-program-selection)
           (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

     ;; Okular forward PDF search requires absolute path.
     (add-to-list
      'TeX-expand-list
      '("%a" (lambda nil (expand-file-name (buffer-file-name)))))))

;;; Fonts (used for folding)
(dolist (x '((#x2200 . #x23ff) (#x27c0 . #x27ef) (#x2980 . #x2bff) (#x1d400 . #x1d7ff)))
  (set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs (car x)) (decode-char 'ucs (cdr x)))
   "STIX"))
