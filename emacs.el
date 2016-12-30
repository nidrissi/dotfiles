;;; Unicode!
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; "²" = "\u00b2"
;; Customize
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:height 1.2 :family "Consolas")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -file-line-error")
 '(LaTeX-fill-break-at-separators (quote (\\\[ \\\])))
 '(LaTeX-math-abbrev-prefix "\u00b2")
 '(LaTeX-math-list
   (quote
    ((111 "circ" "Ring operator" 8728)
     (75 "Bbbk" "Blackboard bold k" 120156)
     (224 "otimes" "Circled times" 8855))))
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(TeX-quote-language-alist (quote (("french" "\\og{}" "\\fg{}" nil))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   (quote
    ((output-pdf "Sumatra PDF")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-html "xdg-open"))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program
   "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(completion-ignored-extensions
   (quote
    (".hi" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".fls" ".fdb_latexmk" ".run.xml" ".synctex.gz" "-blx.bib" ".nav" ".out" ".snm" ".log" ".bcf")))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(cperl-close-paren-offset -4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cua-mode t nil (cua-base))
 '(cua-prefix-override-inhibit-delay 0.4)
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" default)))
 '(default-frame-alist
    (quote
     ((background-color . "black")
      (foreground-color . "white")
      (font . "DejaVu Sans Mono-12")
      (fullscreen . maximized)
      (vertical-scroll-bars . right))))
 '(delete-old-versions t)
 '(display-time-mode t)
 '(display-time-string-forms (quote (24-hours ":" minutes)))
 '(echo-keystrokes 0.1)
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(gc-cons-threshold 20000000)
 '(global-company-mode t)
 '(global-magit-file-mode t)
 '(helm-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-program-name "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
 '(magit-diff-refine-hunk (quote all))
 '(markdown-enable-math t)
 '(menu-bar-mode nil)
 '(openwith-associations
   (quote
    (("\\.\\(?:pdf\\|ps\\)\\'" "c:/Program Files (x86)/SumatraPDF/SumatraPDF"
      (file))
     ("\\.\\(?:png\\|jpg\\|jpeg\\)\\'" "start"
      (file)))))
 '(org-format-latex-header
   "\\documentclass{scrartcl}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove")
 '(org-latex-default-packages-alist
(quote
 (("AUTO" "inputenc" t)
  ("T1" "fontenc" t)
  ("" "graphicx" t)
  ("" "grffile" t)
  ("" "longtable" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "amssymb" t)
  ("" "capt-of" nil)
  ("" "hyperref" nil))))
 '(org-latex-packages-alist (quote (("" "microtype" nil) ("" "mathtools" nil))))
'(package-selected-packages
(quote
 (ace-window yaml-mode web-mode visual-fill-column unicode-fonts ssh-agency solarized-theme smart-mode-line-powerline-theme sass-mode org openwith markdown-mode magit jade-mode haskell-mode flycheck flx-ido cperl-mode color-theme browse-kill-ring auctex)))
'(preview-gs-options
(quote
 ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-section-prefixes (quote ((0 . "part.") (1 . "cha.") (t . "sec."))))
'(safe-local-variable-values
(quote
 ((eval add-to-list
        (quote LaTeX-fold-math-spec-list)
        (quote
         ("▿"
          ("ez"))))
  (eval add-to-list
        (quote LaTeX-fold-math-spec-list)
        (quote
         ("𝕜"
          ("K"))))
  (eval add-to-list
        (quote LaTeX-fold-math-spec-list)
        (quote
         ("ш"
          ("shuffle"))))
  (eval add-to-list
        (quote LaTeX-fold-math-spec-list)
        (quote
         ("∂_μ"
          ("dm"))))
  (eval add-to-list
        (quote LaTeX-fold-math-spec-list)
        (quote
         ("∂_λ"
          ("dl"))))
  (eval make-local-variable
        (quote LaTeX-fold-math-spec-list)))))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow))))

;; Init
(fset 'yes-or-no-p 'y-or-n-p)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))
(cd (getenv "HOME"))
(server-start)

;; Interface
(setq ring-bell-function 'ignore)
(openwith-mode)
(setq sml/theme 'powerline)
(sml/setup)
(setq visual-fill-column-width 80)
(global-visual-line-mode)
(require 'uniquify)

;; Keybindings
(require 'my-mode)
(cua-mode t) ; order matters!
(require 'diminish)                     ; Hide from mode line
(diminish 'my-mode)

(define-key my-mode-map (kbd "C-;") #'ace-window)
(define-key my-mode-map (kbd "M-/") #'hippie-expand)
(define-key my-mode-map (kbd "C-c e") #'eshell)
(define-key my-mode-map (kbd "C-x C-b") #'ibuffer)
(define-key my-mode-map (kbd "C-c m") #'magit-status)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f9>"))
(setq disabled-command-function nil)

;; Helm
(require 'helm-config)
(define-key my-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key my-mode-map (kbd "C-x C-b") 'helm-buffers-list)
(define-key my-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key my-mode-map (kbd "C-,") 'helm-mini)

;; Programming
;;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;;; Misc prog
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Ouaibe
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

;; Markdown
(add-to-list 'auto-mode-alist '("\\.markdown?\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md?\\'" . markdown-mode))

;; Magit
(require 'ssh-agency)
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
                          ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
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
