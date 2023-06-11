;;; custom.el --- Custom variables for Emacs
;;; Commentary: These variables should be set through the customization UI
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-general 1)
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("5e62fe2e73f4d646f37a1d04e92e61bc3d808bbf5032718aba06c44b0638878c" default))
 '(debug-on-error nil)
 '(history-delete-duplicates t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(semantic-default-submodes
   '(global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-local-symbol-highlight-mode))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#454e51"))) t)
 '(company-scrollbar-fg ((t (:background "#394143"))) t)
 '(company-tooltip ((t (:inherit default :background "#32393b"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "firebrick"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(js2-object-property ((t (:inherit default :foreground "gray"))))
 '(pyenv-active-python-face ((t (:foreground "goldenrod" :weight bold))))
 '(sml/filename ((t (:inherit sml/global :background "black" :foreground "gold")))))
;;; custom.el ends here
