;;; custom.el --- Custom variables for Emacs
;;; Commentary: These variables should be set through the customization UI
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 3)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux"))))
 '(company-backends
   (quote
    ((company-yasnippet company-web-jade company-web-html company-tern company-semantic company-bbdb company-nxml company-css company-eclim company-clang company-xcode company-cmake company-dabbrev-code company-gtags company-etags)
     company-files company-capf company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-delay 0.5)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 30)
 '(company-transformers
   (quote
    (company-sort-by-backend-importance company-sort-by-occurrence company-sort-by-statistics)))
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-indent 2)
 '(ecb-windows-width 0.2)
 '(ein:use-auto-complete t)
 '(ein:use-auto-complete-superpack t)
 '(elpy-mode-hook (quote (subword-mode)))
 '(elpy-modules
   (quote
    (elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-timeout 4)
 '(helm-ag-use-agignore t)
 '(helm-follow-mode-persistent t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot 1)
 '(js-indent-level 2)
 '(js2-global-externs (quote ("angular")))
 '(js2-include-node-externs t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(line-number-mode t)
 '(magit-diff-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff")))
 '(magit-pull-arguments (quote ("--rebase")))
 '(markdown-command "/usr/bin/pandoc")
 '(mode-require-final-newline (quote visit-save))
 '(powerline-default-separator (quote utf-8))
 '(projectile-completion-system (quote helm))
 '(projectile-mode-line nil)
 '(python-indent-offset 4)
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython" "jupyter")))
 '(python-shell-prompt-detect-failure-warning nil)
 '(require-final-newline (quote visit-save))
 '(rm-blacklist (quote (" Wrap" " Helm" " hl-p")))
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-local-symbol-highlight-mode)))
 '(send-mail-function (quote mailclient-send-it))
 '(sml/extra-filler -6)
 '(sml/mode-width (quote right))
 '(sml/name-width 5)
 '(sml/shorten-mode-string "")
 '(sml/theme (quote dark))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 3)
 '(web-mode-css-indent-offset 3)
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-enable-sql-detection t)
 '(web-mode-markup-indent-offset 3)
 '(web-mode-script-padding 0)
 '(web-mode-sql-indent-offset 3)
 '(web-mode-style-padding 0))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#454e51"))))
 '(company-scrollbar-fg ((t (:background "#394143"))))
 '(company-tooltip ((t (:inherit default :background "#32393b"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "firebrick"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(js2-object-property ((t (:inherit default :foreground "gray"))))
 '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "white" :weight normal)))))
;;; custom.el ends here
