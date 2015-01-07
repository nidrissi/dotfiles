;; Init
(fset 'yes-or-no-p 'y-or-n-p)
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))
(setq enable-local-eval t)
(setq my-home
      (cond ((eq system-type 'windows-nt) "C:/Users/Najib/")
            (t "~")))
(cd my-home)
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
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t
      ido-max-window-height 1
      ido-auto-merge-work-directories-length -1
      ido-use-faces nil)                ; flx-ido
(setq gc-cons-threshold 20000000)       ; gc
(projectile-global-mode)

;;; Icomplete
(require 'icomplete+)
(icomplete-mode t)
(setq icomplete-prospects-height 1)

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


;; Magit
(autoload 'magit-status "magit")
(global-set-key (kbd "C-c m") 'magit-status)


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
      TeX-source-correlate-mode t)
(setq-default TeX-PDF-mode t)

;;; Math
(setq LaTeX-math-menu-unicode t
      LaTeX-math-list
      '((?o "circ" "Ring operator" 8728)
        (?K "Bbbk" "Blackboard bold k" 120156)))

(require 'latex)
(require 'reftex)
(require 'tex-fold)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-to-list 'reftex-bibliography-commands "addbibresource")
(setq reftex-section-prefixes '((0 . "part.") (1 . "cha.") (t . "sec.")))
(add-to-list 'LaTeX-font-list '(11 "" "" "\\mathfrak{" "}"))

(setq ispell-tex-skip-alists
      (list
       (append
        (car ispell-tex-skip-alists)
        '(("\\\\cref" ispell-tex-arg-end)
          ("\\\\textcite" ispell-tex-arg-end)))
       (cadr ispell-tex-skip-alists)))
(setq flyspell-tex-command-regexp "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")

;;; Fold
(add-to-list 'TeX-fold-macro-spec-list
             '("[r]" ("cref" "Cref")))
(add-to-list 'TeX-fold-macro-spec-list
             '("[c]" ("textcite")))
(add-to-list 'TeX-fold-macro-spec-list
             '("[f]" ("tablefootnote")))
(add-to-list 'TeX-fold-macro-spec-list
             '("[n]" ("nomenclature")))
(add-to-list 'LaTeX-fold-math-spec-list
             '("[" ("lbrack")))
(add-to-list 'LaTeX-fold-math-spec-list
             '("]" ("rbrack")))

;;; LaTeXmk
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

;;; Windows
(if (eq system-type 'windows-nt)
    (progn
      (setq preview-gs-command "gswin64c.exe")
      (add-to-list 'TeX-view-program-list
                   '("Sumatra PDF"
                     ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                      (mode-io-correlate " -forward-search %b %n") " %O")))
      (assq-delete-all 'output-pdf TeX-view-program-selection)
      (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

;;; Okular forward PDF search requires absolute path.
(add-to-list
 'TeX-expand-list
 '("%a" (lambda nil (expand-file-name (buffer-file-name)))))

;;; Output directory: build
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
     (TeX-command "View" 'TeX-active-master 0))))



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
      (cond ((eq system-type 'windows-nt) "C:/Users/Najib/AppData/Local/Google/Chrome/Application/chrome.exe")
            (t "google-chrome")))

;; Diary
(require 'epa-file)
(epa-file-enable)
(setq my-diary-file
      (if (file-exists-p "C:/Users/Najib/OneDrive/journal.gpg")
          "C:/Users/Najib/OneDrive/journal.gpg"
        "e:/SkyDrive/journal.gpg"))
(defun my-new-diary-entry ()
  (interactive)
  (find-file my-diary-file)
  (org-mode)
  (let ((date (format-time-string "* %d/%m/%y")))
    (unless (search-forward-regexp date nil t)
      (insert date "\n"))
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (insert "** " (format-time-string "%R") "\n")))
(global-set-key (kbd "C-c j") 'my-new-diary-entry)
      

;; Customize
;; "²" = "\u00b2"
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "\u00b2")
 '(ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
 '(magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"))
;; Init
(fset 'yes-or-no-p 'y-or-n-p)
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))
(setq enable-local-eval t)
(setq my-home
      (cond ((eq system-type 'windows-nt) "C:/Users/Najib/")
            (t "~")))
(cd my-home)
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
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c e") 'eshell)
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
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t
      ido-max-window-height 1
      ido-auto-merge-work-directories-length -1
      ido-use-faces nil)                ; flx-ido
(setq gc-cons-threshold 20000000)       ; gc
(projectile-global-mode)

;;; Icomplete
(require 'icomplete+)
(icomplete-mode t)
(setq icomplete-prospects-height 1)

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


;; Magit
(autoload 'magit-status "magit")
(global-set-key (kbd "C-c m") 'magit-status)


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
      TeX-source-correlate-mode t)
(setq-default TeX-PDF-mode t)

;;; Math
(setq LaTeX-math-menu-unicode t
      LaTeX-math-list
      '((?o "circ" "Ring operator" 8728)
        (?K "Bbbk" "Blackboard bold k" 120156)))

(require 'latex)
(require 'reftex)
(require 'tex-fold)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-to-list 'reftex-bibliography-commands "addbibresource")
(setq reftex-section-prefixes '((0 . "part.") (1 . "cha.") (t . "sec.")))
(add-to-list 'LaTeX-font-list '(11 "" "" "\\mathfrak{" "}"))

(setq ispell-tex-skip-alists
      (list
       (append
        (car ispell-tex-skip-alists)
        '(("\\\\cref" ispell-tex-arg-end)
          ("\\\\textcite" ispell-tex-arg-end)))
       (cadr ispell-tex-skip-alists)))
(setq flyspell-tex-command-regexp "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")

;;; Fold
(add-to-list 'TeX-fold-macro-spec-list
             '("[r]" ("cref" "Cref")))
(add-to-list 'TeX-fold-macro-spec-list
             '("[c]" ("textcite")))
(add-to-list 'TeX-fold-macro-spec-list
             '("[f]" ("tablefootnote")))
(add-to-list 'TeX-fold-macro-spec-list
             '("[n]" ("nomenclature")))
(add-to-list 'LaTeX-fold-math-spec-list
             '("[" ("lbrack")))
(add-to-list 'LaTeX-fold-math-spec-list
             '("]" ("rbrack")))

;;; LaTeXmk
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

;;; Windows
(if (eq system-type 'windows-nt)
    (progn
      (setq preview-gs-command "gswin64c.exe")
      (add-to-list 'TeX-view-program-list
                   '("Sumatra PDF"
                     ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                      (mode-io-correlate " -forward-search %b %n") " %O")))
      (assq-delete-all 'output-pdf TeX-view-program-selection)
      (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

;;; Okular forward PDF search requires absolute path.
(add-to-list
 'TeX-expand-list
 '("%a" (lambda nil (expand-file-name (buffer-file-name)))))

;;; Output directory: build
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
     (TeX-command "View" 'TeX-active-master 0))))



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
      (cond ((eq system-type 'windows-nt) "C:/Users/Najib/AppData/Local/Google/Chrome/Application/chrome.exe")
            (t "google-chrome")))


;; Customize
;; "²" = "\u00b2"
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "\u00b2")
 '(ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
 '(magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"))
