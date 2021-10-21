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
   '((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux")))
 '(clean-buffer-list-delay-general 1)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-delay 0.5)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 30)
 '(company-transformers
   '(company-sort-by-backend-importance company-sort-by-occurrence company-sort-by-statistics))
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(elpy-formatter 'black)
 '(flycheck-disabled-checkers '(python-mypy python-pylint))
 '(flycheck-python-flake8-executable "~/.local/bin/flake8")
 '(helm-ag-use-agignore t)
 '(history-delete-duplicates t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot 1)
 '(js-indent-level 2)
 '(js2-global-externs '("angular"))
 '(js2-include-node-externs t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(line-number-mode t)
 '(magit-diff-arguments
   '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff"))
 '(magit-pull-arguments '("--rebase"))
 '(markdown-command "/usr/bin/pandoc")
 '(mode-require-final-newline 'visit-save)
 '(powerline-gui-use-vcs-glyph t)
 '(projectile-completion-system 'helm)
 '(projectile-mode-line nil)
 '(python-indent-offset 4)
 '(python-shell-completion-native-disabled-interpreters '("pypy" "ipython" "jupyter"))
 '(python-shell-prompt-detect-failure-warning nil)
 '(require-final-newline 'visit-save)
 '(semantic-default-submodes
   '(global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-local-symbol-highlight-mode))
 '(send-mail-function 'mailclient-send-it)
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
 '(pyenv-active-python-face ((t (:foreground "goldenrod" :weight bold))))
 '(sml/filename ((t (:inherit sml/global :background "black" :foreground "gold")))))
;;; custom.el ends here
