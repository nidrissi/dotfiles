;;; custom --- My custom options

;;; Commentary:
;;; Options for customize.

;;; Code:
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
 '(LaTeX-math-abbrev-prefix "²")
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
 '(backup-directory-alist (quote (("." . "~/.emacs.d/saves"))))
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program
   "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-tooltip-align-annotations t)
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
      (vertical-scroll-bars . right))))
 '(delete-old-versions t)
 '(display-time-mode t)
 '(display-time-string-forms (quote (24-hours ":" minutes)))
 '(echo-keystrokes 0.1)
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|textcite\\|label\\|c?ref\\|eqref\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(gc-cons-threshold 20000000)
 '(helm-ff-skip-boring-files t)
 '(helm-mode-fuzzy-match t)
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
    (("\\.\\(?:pdf\\|ps\\)\\'" "c:/Program Files/SumatraPDF/SumatraPDF"
      (file))
     ("\\.\\(?:png\\|jpg\\|jpeg\\)\\'" "start"
      (file)))))
 '(package-selected-packages
   (quote
    (yaml-mode web-mode visual-fill-column unicode-fonts undo-tree tuareg tide ssh-agency smart-mode-line-powerline-theme sass-mode rainbow-delimiters powershell org openwith markdown-mode magit helm-projectile haskell-mode flx-ido diminish cperl-mode company color-theme browse-kill-ring auctex ace-window)))
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(projectile-completion-system (quote helm))
 '(projectile-globally-ignored-file-suffixes
   (quote
    (".hi" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".fls" ".fdb_latexmk" ".run.xml" ".synctex.gz" "-blx.bib" ".nav" ".out" ".snm" ".log" ".bcf")))
 '(projectile-indexing-method (quote alien))
 '(reftex-default-bibliography
   (quote
    ("c:/Users/Najib/Work/phd/texmf/bibtex/bib/main_bib/math.bib")))
 '(reftex-label-alist
   (quote
    (("theorem" 97 "thm." "~\\ref{%s}" t
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
 '(reftex-plug-into-AUCTeX t)
 '(reftex-section-prefixes (quote ((0 . "part.") (1 . "cha.") (t . "sec."))))
 '(safe-local-variable-values
   (quote
    ((eval web-mode-set-engine "django")
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
 '(show-paren-mode t)
 '(sml/theme (quote powerline))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow))))