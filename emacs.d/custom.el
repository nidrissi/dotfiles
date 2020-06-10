;;; custom.el

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit aw-mode-line-face :height 3.0))))
 '(highlight ((t (:background "#0a4b5b"))))
 '(org-archived ((t (:foreground "dim gray" :weight normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -file-line-error")
 '(LaTeX-fill-break-at-separators '(\\\[ \\\]))
 '(LaTeX-font-list
   '((11 "" "" "\\mathfrak{" "}")
     (1 "" "" "\\mathcal{" "}")
     (2 "\\textbf{" "}" "\\mathbf{" "}")
     (3 "\\textsc{" "}")
     (5 "\\emph{" "}")
     (6 "\\textsf{" "}" "\\mathsf{" "}")
     (9 "\\textit{" "}" "\\mathit{" "}")
     (12 "\\textulc{" "}")
     (13 "\\textmd{" "}")
     (14 "\\textnormal{" "}" "\\mathnormal{" "}")
     (18 "\\textrm{" "}" "\\mathrm{" "}")
     (19 "\\textsl{" "}" "\\mathbb{" "}")
     (20 "\\texttt{" "}" "\\mathtt{" "}")
     (21 "\\textup{" "}")
     (23 "\\textsw{" "}")
     (4 "" "" t)))
 '(LaTeX-math-abbrev-prefix "²")
 '(LaTeX-math-list
   '((111 "circ" "Ring operator" 8728)
     (75 "Bbbk" "Blackboard bold k" 120156)
     (224 "otimes" "Circled times" 8855)))
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-complete-expert-commands t)
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(TeX-quote-language-alist '(("french" "\\og{}" "\\fg{}" nil)))
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   '(("Sumatra PDF"
      ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
       (mode-io-correlate " -forward-search \"%b\" %n")
       " %o"))))
 '(TeX-view-program-selection
   '((output-pdf "Okular")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-html "xdg-open")))
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
 '(aw-keys '(38 233 34 39 40 45 232 95 231 224))
 '(backup-by-copying t)
 '(bibtex-autokey-name-case-convert-function 'identity)
 '(bibtex-autokey-names 'infty)
 '(bibtex-autokey-titleword-length 0)
 '(bibtex-autokey-titleword-separator "")
 '(bibtex-autokey-titlewords 0)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "")
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(calendar-week-start-day 1)
 '(cdlatex-math-symbol-prefix 178)
 '(column-number-mode t)
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-align-annotations t)
 '(completion-ignored-extensions
   '(".hi" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".fls" ".fdb_latexmk" ".run.xml" ".synctex.gz" "-blx.bib" ".nav" ".out" ".snm" ".log" ".bcf" ".ilg" ".ind" ".tdo"))
 '(confirm-kill-emacs 'yes-or-no-p)
 '(cperl-close-paren-offset -4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" default))
 '(default-frame-alist
    '((font . "DejaVu Sans Mono-11")
      (vertical-scroll-bars . right)))
 '(delete-old-versions t)
 '(dired-async-mode t)
 '(display-time-mode t)
 '(display-time-string-forms '(24-hours ":" minutes))
 '(ebib-allow-identical-fields t)
 '(ebib-bibtex-dialect 'biblatex)
 '(ebib-file-associations
   '(("pdf" . "setsid xdg-open %s")
     ("ps" . "setsid xdg-open %s")
     ("djvu" . "setsid xdg-open %s")))
 '(ebib-index-columns
   '(("Entry Key" 20 t)
     ("Year" 6 nil)
     ("Author/Editor" 40 t)
     ("Title" 60 t)
     ("journaltitle" 50 nil)))
 '(ebib-keywords-field-keep-sorted t)
 '(ebib-keywords-file "ebib-keywords.txt")
 '(ebib-preload-bib-files '("mainbib.bib"))
 '(ebib-reading-list-file "~/org/ebib-list.org")
 '(ebib-reading-list-template "** %M %T
   :PROPERTIES:
   %K
   :END:
")
 '(ebib-timestamp-format "%Y.%m.%d")
 '(ebib-uniquify-keys t)
 '(ebib-use-timestamp t)
 '(echo-keystrokes 0.1)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(flyspell-use-meta-tab nil)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-dictionary "en_US")
 '(ispell-local-dictionary-alist
   '((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t
          ("-d" "en_US")
          nil utf-8)
     ("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t
      ("-d" "en_US")
      nil utf-8)
     ("fr_FR" "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[-']" t
      ("-d" "fr_FR")
      nil utf-8)))
 '(ispell-program-name "hunspell")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(load-prefer-newer t)
 '(lsp-keymap-prefix "i")
 '(magit-diff-refine-hunk 'all)
 '(magit-section-initial-visibility-alist nil)
 '(markdown-enable-math t)
 '(menu-bar-mode nil)
 '(midnight-mode t)
 '(openwith-associations
   '(("\\.\\(?:pdf\\|ps\\|png\\|jpg\\|jpeg\\|svgz?\\)\\'" "setsid -w xdg-open"
      (file))) nil nil "Customized with use-package openwith")
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (todo "TODO" nil))
      nil)))
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files
   '("~/org/rapports.org" "~/org/cours.org" "~/org/general.org" "~/org/AAP.org" "~/org/articles.org" "~/org/idees.org" "~/org/mission.org" "~/org/refile.org" "~/org/seminaire.org"))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-todo-ignore-deadlines 'far)
 '(org-agenda-todo-ignore-scheduled 7)
 '(org-agenda-todo-list-sublevels nil)
 '(org-attach-use-inheritance t)
 '(org-capture-templates
   '(("t" "Tâches" entry
      (file+headline "refile.org" "Tâches")
      "* TODO %?
  %t
  %i
  %a")
     ("i" "Idées" entry
      (file "idees.org")
      "* %?
  %t")
     ("o" "Orateur" entry
      (file "~/org/seminaire.org")
      "* PREVU %? %^g
  %^{Date prévue ?}t
** TODO Annonce
** TODO Demander la carte
** TODO État de frais" :time-prompt t)))
 '(org-clock-idle-time 10)
 '(org-default-notes-file "~/org/refile.org")
 '(org-display-custom-times t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-leading-stars t)
 '(org-latex-classes
   '(("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("scrartcl" "\\documentclass{scrartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
 '(org-latex-pdf-process
   '("latexmk -pdf -pdflatex=%latex --synctex=1 -interaction=nonstopmode  -file-line-error -shell-escape %f"))
 '(org-log-done 'time)
 '(org-lowest-priority 68)
 '(org-modules '(ol-bibtex ol-docview ol-eww ol-info))
 '(org-preview-latex-default-process 'imagemagick)
 '(org-priority-faces
   '((65 :foreground "dark orange" :weight bold)
     (66 :foreground "yellow" :weight normal)
     (67 :foreground "dark cyan" :weigth normal)
     (68 . "(:foreground \"grey\" :weigth normal)")))
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(org-special-ctrl-a/e t)
 '(org-startup-truncated nil)
 '(org-time-stamp-custom-formats '("<%a %d/%m/%y>" . "<%a %d/%m/%y %H:%M>"))
 '(org-todo-keyword-faces
   '(("WIP" . org-todo)
     ("PRE" . org-todo)
     ("REV" . org-todo)
     ("SUB" . "cyan")
     ("PUB" . org-done)
     ("IDÉE" . "yellow")
     ("CONTACT" . "orange")
     ("PRÉVU" . "purple")))
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(reftex htmlize typescript-mode which-key lsp-ivy lsp-python-ms esup adaptive-wrap lsp-treemacs flycheck lsp-ui lsp-mode all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-dired diredfl uniquify dired-x smartparens-mode org rust-mode elpy lua-mode glsl-mode julia-repl tuareg julia-mode ebib graphviz-dot-mode ivy smart-mode-line json-mode go-mode ssh-agency tup-mode openwith-mode sass-mode web-mode tide multiple-cursors diff-hl move-text gitconfig-mode gitignore-mode ivy-hydra counsel-projectile counsel anzu volatile-highlights-mode volatile-highlights smartparens company-emoji auctex-latexmk projectile-mode use-package zenburn-theme yaml-mode visual-fill-column unicode-fonts undo-tree smart-mode-line-powerline-theme rainbow-delimiters openwith markdown-mode magit diminish cperl-mode company color-theme auctex ace-window))
 '(prettify-symbols-unprettify-at-point t)
 '(preview-gs-command "gswin64c.exe")
 '(preview-gs-options
   '("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))
 '(projectile-completion-system 'ivy)
 '(projectile-globally-ignored-file-suffixes
   '(".hi" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".fls" ".fdb_latexmk" ".run.xml" ".synctex.gz" "-blx.bib" ".nav" ".out" ".snm" ".log" ".bcf"))
 '(python-shell-interpreter "python3")
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python3")
 '(reftex-derive-label-parameters
   '(3 20 t 1 "-"
       ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to" "et" "le" "la" "les" "un" "une" "dessous")
       t))
 '(reftex-insert-label-flags '("s" "asft"))
 '(reftex-label-alist
   '(("theorem" 97 "thm:" "~\\ref{%s}" t
      ("theorem"))
     ("theoremintro" 97 "thm:" "~\\ref{%s}" t
      ("theorem"))
     ("proposition" 97 "prop:" "~\\ref{%s}" t
      ("proposition"))
     ("corollary" 97 "cor:" "~\\ref{%s}" t
      ("corollary"))
     ("lemma" 97 "lem:" "~\\ref{%s}" t
      ("lemma"))
     ("definition" 97 "def:" "~\\ref{%s}" t
      ("definition"))
     ("example" 97 "exa:" "~\\ref{%s}" t
      ("example"))
     ("remark" 97 "rmk:" "~\\ref{%s}" t
      ("remark"))
     ("conjecture" 97 "conj:" nil t
      ("conjecture"))))
 '(reftex-label-ignored-macros-and-environments '("tikzpicture" "tikzcd"))
 '(reftex-plug-into-AUCTeX t)
 '(set-mark-command-repeat-pop t)
 '(sml/theme 'powerline)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(vc-follow-symlinks t)
 '(version-control t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(x-underline-at-descent-line t))
