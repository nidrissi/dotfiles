;; Init
(fset 'yes-or-no-p 'y-or-n-p)
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))
(cd (getenv "HOME"))
(setq enable-local-eval t)
(server-start)

;; Interface
;;; Remove superfluous elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-message t
      initial-scratch-message nil)

;;; Add essentials ones
(setq default-frame-alist
      '((background-color . "black")
        (foreground-color . "white")
        (font . "DejaVu Sans Mono-12")))
(set-scroll-bar-mode 'right)
(column-number-mode t)
(line-number-mode t)
(display-time-mode t)
(show-paren-mode t)
(setq display-time-string-forms '(24-hours ":" minutes)
      echo-keystrokes 0.1)
(setq-default indicate-empty-lines t)

;;; Unicode!
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; Distinguish buffer names with path
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Ibuffer, a buffer list
(setq ibuffer-saved-filter-groups
      '(("default"
         ("latex" (mode . latex-mode))
         ("perl" (mode . perl-mode))
         ("web" (or (mode . html-mode)
                    (mode . js-mode)
                    (mode . css-mode)
                    (mode . web-mode)))
         ("git" (name . "*magit"))
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

(setq cua-enable-cursor-indications t
      cua-normal-cursor-color "gray"
      cua-remap-control-v nil
      cua-remap-control-z nil
      cua-prefix-override-inhibit-delay 0.4)
(cua-mode t)

;;; Ido
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t
      ido-max-window-height 1
      ido-auto-merge-work-directories-length -1)
(setq gc-cons-threshold 20000000)       ; gc
(projectile-global-mode)

;;; Backup
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      version-control t)

;; Programming
;;; Misc prog
(setq standard-indent 4)
(setq-default indent-tabs-mode nil
              tab-width 4)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(setq cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4)
(add-to-list 'auto-mode-alist '("\\.\\([tT][tT]\\)\\'" . html-mode)) ; template toolkit

;; Haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; Magit
(require 'ssh-agency)
(autoload 'magit-status "magit")
(global-set-key (kbd "C-c m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
(setenv "SSH_ASKPASS" "git-gui--askpass")


;; AUCTeX
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-electric-sub-and-superscript t
      reftex-plug-into-AUCTeX t
      ;; Sinon TeX-next-error bug
      LaTeX-command "latex -file-line-error"
      TeX-quote-language-alist '(("french" "\\og{}" "\\fg{}"))
      preview-gs-options
      '("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4"
        "-dGraphicsAlphaBits=4")
      TeX-view-program-selection
      '(((output-dvi style-pstricks) "dvips and gv")
        (output-dvi "xdvi")
        (output-pdf "Okular")
        (output-html "xdg-open"))
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server t
      TeX-source-correlate-mode t
      reftex-section-prefixes '((0 . "part.") (1 . "cha.") (t . "sec."))
      ispell-tex-skip-alists
      (list
       (append
        (car ispell-tex-skip-alists)
        '(("\\\\cref" ispell-tex-arg-end)
          ("\\\\Cref" ispell-tex-arg-end)
          ("\\\\import" ispell-tex-arg-end 2)
          ("\\\\textcite" ispell-tex-arg-end)))
       (cadr ispell-tex-skip-alists))
      flyspell-tex-command-regexp "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
(setq-default TeX-PDF-mode t)

;;; Math
(setq LaTeX-math-menu-unicode t
      LaTeX-math-list
      '((?o "circ" "Ring operator" 8728)
        (?K "Bbbk" "Blackboard bold k" 120156))
      LaTeX-fill-break-at-separators '(\\\[ \\\]))

(eval-after-load "reftex"
  '(progn
     (add-to-list 'reftex-bibliography-commands "addbibresource")))

(eval-after-load "latex"
  '(progn
     (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
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

;; Org mode
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq
 org-log-done t
 org-agenda-files
 (list (if (file-exists-p "C:/Users/Najib/OneDrive")
           "C:/Users/Najib/OneDrive/agenda.org"
         "E:/SkyDrive/agenda.org")))

;; Misc
(setq woman-fill-column 80
      line-move-visual nil
      ;; Calendar
      calendar-week-start-day 1
      calendar-latitude 50.6
      calendar-longitude 3.1
      ;; Browser
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (cond ((eq system-type 'windows-nt) "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
            (t "google-chrome")))

;; Customize
;; "²" = "\u00b2"
(custom-set-faces)
(custom-set-variables
 '(LaTeX-math-abbrev-prefix "\u00b2")
 '(ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe"))
