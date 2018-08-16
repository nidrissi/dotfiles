;;; custom --- My custom options

;;; Commentary:
;;; Options for customize.

;;; Code:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "#0a4b5b")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-amsmath-label "eq.")
 '(LaTeX-command "latex -file-line-error")
 '(LaTeX-equation-label "eq.")
 '(LaTeX-figure-label "fig.")
 '(LaTeX-fill-break-at-separators (quote (\\\[ \\\])))
 '(LaTeX-math-abbrev-prefix "²")
 '(LaTeX-math-list
   (quote
    ((111 "circ" "Ring operator" 8728)
     (75 "Bbbk" "Blackboard bold k" 120156)
     (224 "otimes" "Circled times" 8855))))
 '(LaTeX-table-label "tab.")
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-complete-expert-commands t)
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(TeX-quote-language-alist (quote (("french" "\\og{}" "\\fg{}" nil))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Sumatra PDF"
      ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
       (mode-io-correlate " -forward-search %b %n")
       " %o")))))
 '(TeX-view-program-selection
   (quote
    ((output-pdf "Sumatra PDF")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-html "xdg-open"))))
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/saves"))))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-tooltip-align-annotations t)
 '(completion-ignored-extensions
   (quote
    (".hi" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".fls" ".fdb_latexmk" ".run.xml" ".synctex.gz" "-blx.bib" ".nav" ".out" ".snm" ".log" ".bcf" ".ilg" ".ind" ".tdo")))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(counsel-ag-base-command "C:/msys64/mingw64/bin/ag --vimgrep --nocolor --nogroup %s")
 '(cperl-close-paren-offset -4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" default)))
 '(default-frame-alist
    (quote
     ((font . "DejaVu Sans Mono-11")
      (vertical-scroll-bars . right))))
 '(delete-old-versions t)
 '(display-time-mode t)
 '(display-time-string-forms (quote (24-hours ":" minutes)))
 '(echo-keystrokes 0.1)
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(flyspell-use-meta-tab nil)
 '(gc-cons-threshold 20000000)
 '(git-gutter:modified-sign "#")
 '(helm-ag-base-command "c:/cygwin64/bin/ag --vimgrep --nocolor --nogroup")
 '(helm-ff-skip-boring-files t)
 '(helm-mode-fuzzy-match t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-dictionary "american")
 '(ispell-local-dictionary-alist
   (quote
    ((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t
          ("-d" "en_US" "-p" "C:\\users\\najib\\hunspell\\personal.en")
          nil iso-8859-1)
     (#("american" 0 1
        (idx 0))
      "[[:alpha:]]" "[^[:alpha:]]" "[']" t
      ("-d" "en_US" "-p" "C:\\users\\najib\\hunspell\\personal.en")
      nil iso-8859-1)
     (#("fr-moderne" 0 1
        (idx 2))
      "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[-']" t
      ("-d" "fr-moderne" "-p" "C:\\users\\najib\\hunspell\\personal.fr")
      nil utf-8))))
 '(ispell-program-name "c:/msys64/mingw64/bin/hunspell.exe")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk (quote all))
 '(markdown-enable-math t)
 '(menu-bar-mode nil)
 '(openwith-associations
   (quote
    (("\\.\\(?:pdf\\|ps\\)\\'" "c:/Program Files/SumatraPDF/SumatraPDF"
      (file))
     ("\\.\\(?:png\\|jpg\\|jpeg\\)\\'" "start"
      (file))
     ("\\.svgz?" "start"
      (file)))))
 '(org-latex-classes
   (quote
    (("beamer" "\\documentclass[presentation]{beamer}"
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
     ("scrartcl" "\\documentclass{srcartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
 '(org-latex-pdf-process
   (quote
    ("latexmk -pdf --synctex=1 -interaction=nonstopmode  -file-line-error -shell-escape %f")))
 '(package-selected-packages
   (quote
    (go-mode ssh-agency tup-mode openwith-mode jade-mode sass-mode web-mode tide multiple-cursors diff-hl move-text gitconfig-mode gitignore-mode ivy-hydra counsel-projectile counsel anzu volatile-highlights-mode volatile-highlights smartparens company-emoji auctex-latexmk projectile-mode ace-jump-mode esup use-package zenburn-theme yaml-mode visual-fill-column unicode-fonts undo-tree smart-mode-line-powerline-theme rainbow-delimiters org openwith markdown-mode magit diminish cperl-mode company color-theme browse-kill-ring auctex ace-window)))
 '(prettify-symbols-unprettify-at-point t)
 '(preview-gs-command "gswin64c.exe")
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(projectile-completion-system (quote ivy))
 '(projectile-globally-ignored-file-suffixes
   (quote
    (".hi" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".fls" ".fdb_latexmk" ".run.xml" ".synctex.gz" "-blx.bib" ".nav" ".out" ".snm" ".log" ".bcf")))
 '(projectile-indexing-method (quote alien))
 '(reftex-default-bibliography
   (quote
    ("c:/Users/Najib/Work/math/texmf/bibtex/bib/mainbib_bib/mainbib.bib")))
 '(reftex-insert-label-flags (quote ("s" "asft")))
 '(reftex-label-alist
   (quote
    (("theorem" 97 "thm." "~\\ref{%s}" t
      ("theorem"))
     ("theoremintro" 97 "thm." "~\\ref{%s}" t
      ("theorem"))
     ("proposition" 97 "prop." "~\\ref{%s}" t
      ("proposition"))
     ("corollary" 97 "cor." "~\\ref{%s}" t
      ("corollary"))
     ("lemma" 97 "lem." "~\\ref{%s}" t
      ("lemma"))
     ("definition" 97 "def." "~\\ref{%s}" t
      ("definition"))
     ("example" 97 "exa." "~\\ref{%s}" t
      ("example"))
     ("remark" 97 "rmk." "~\\ref{%s}" t
      ("remark"))
     ("conjecture" 97 "conj." nil t
      ("conjecture")))))
 '(reftex-label-ignored-macros-and-environments (quote ("tikzpicture" "tikzcd")))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   (quote
    ((TeX-command-extra-options . "-shell-escape")
     (eval add-to-list
           (quote TeX-fold-macro-spec-list)
           (quote
            ("[n{1}]"
             ("nomnom"))))
     (eval add-to-list
           (quote LaTeX-fold-math-spec-list)
           (quote
            ("¡"
             ("ashk"))))
     (eval add-to-list
           (quote LaTeX-fold-math-spec-list)
           (quote
            ("d"
             ("dd"))))
     (eval web-mode-set-engine "django")
     (eval add-to-list
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
 '(set-mark-command-repeat-pop t)
 '(sml/replacer-regexp-list
   (quote
    (("^~/\\.emacs\\.d/elpa/" ":ELPA:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^~/Documents/" ":Doc:")
     ("^~/Google Drive/" ":GDrive:")
     ("^~/Work/" ":Work:"))))
 '(standard-indent 2)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(x-underline-at-descent-line t))
