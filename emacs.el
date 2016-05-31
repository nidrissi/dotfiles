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
    ("a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" default)))
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
 '(flx-ido-mode t)
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(gc-cons-threshold 20000000)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-window-height 1)
 '(ido-mode (quote both) nil (ido))
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
    (("\\.(?:pdf|ps)\\'" "c:/Program Files (x86)/SumatraPDF/SumatraPDF"
      (file))
     ("\\.mp3\\'" "xmms"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\)\\'" "display"
      (file)))))
 '(openwith-mode t)
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-section-prefixes (quote ((0 . "part.") (1 . "cha.") (t . "sec."))))
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

;; Powerline
(setq sml/theme 'powerline)
(sml/setup)

;;; Visual fill mode
(setq visual-fill-column-width 80)
(global-visual-line-mode)

;;; Distinguish buffer names with path
(require 'uniquify)

;;; Ibuffer, a buffer list
(setq ibuffer-saved-filter-groups
      '(("default"
         ("latex" (or (mode . latex-mode)
                      (mode . plain-tex-mode)
                      (mode . tex-output-mode)))
         ("web" (or (mode . html-mode)
                    (mode . js-mode)
                    (mode . css-mode)
                    (mode . sass-mode)
                    (mode . web-mode)
                    (mode . markdown-mode)
                    (mode . jade-mode)))
         ("git" (or (name . "*magit")
                    (name . ".gitignore")))
         ("emacs" (or (name . "*Messages*")
                      (name . "*scratch*")
                      (name . "*Help*"))))))
(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Editing
;;; Keybindings
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c w") 'woman)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c c") 'calendar)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f9>"))
(setq disabled-command-function nil)
(browse-kill-ring-default-keybindings)

;; Programming
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
(global-set-key (kbd "C-c m") 'magit-status)
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
                           (mode-io-correlate " -forward-search %b %n") " %O")))
           (assq-delete-all 'output-pdf TeX-view-program-selection)
           (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

     ;; Okular forward PDF search requires absolute path.
     (add-to-list
      'TeX-expand-list
      '("%a" (lambda nil (expand-file-name (buffer-file-name)))))

     ;; Output directory: build
     (add-to-list
      'TeX-expand-list
      '("%O" (lambda ()
               (concat "\"build/"
                       (replace-regexp-in-string
                        "\"" ""
                        (funcall file (TeX-output-extension) t))
                       "\""))))
     (setq TeX-view-program-list-builtin
           '(("Okular"
              ("okular --unique %O"
               (mode-io-correlate "#src:%n%a")))))
     (add-hook
      'LaTeX-mode-hook
      (lambda ()
        (defun TeX-view ()
          (interactive)
          (TeX-command "View" 'TeX-active-master 0))))))

;;; Fonts (used for folding)
(dolist (x '((#x2200 . #x23ff) (#x27c0 . #x27ef) (#x2980 . #x2bff) (#x1d400 . #x1d7ff)))
  (set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs (car x)) (decode-char 'ucs (cdr x)))
   "STIX"))
