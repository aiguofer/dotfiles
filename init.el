;; .emacs --- My emacs config
;;; Commentary:
;;; Code:
(setq load-prefer-newer t)

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Use UTF8 everywhere, see https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;; Use bash.. zsh causes slowness in projectile: https://github.com/syl20bnr/spacemacs/issues/4207
(setq shell-file-name "/bin/bash")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Move customizations to different file
(use-package cus-edit
  :init
  (setq custom-file (concat user-emacs-directory "custom.el"))
  :config
  (load custom-file))

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe))

(use-package subword
  :ensure t
  :diminish subword-mode
  :init
  (global-subword-mode))

(use-package semantic
  :ensure t
  :init
  (semantic-mode 1))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD$")

(use-package sh-mode
  :mode
  (("bashrc$" . sh-mode)
   ("bash_profile$" . sh-mode)
   ("bash_aliases$" . sh-mode)
   ("bash_local$" . sh-mode)
   ("bash_completion$" . sh-mode)
   ("\\.zsh" . sh-mode)
   ("runcoms/[a-zA-Z]+$" . sh-mode)))

(use-package vmd-mode
  :ensure t
  :commands (vmd-mode)
  :init
  (add-hook 'markdown-mode-hook 'vmd-mode))

(use-package coffee-mode
  :ensure t)

(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(use-package systemd
  :ensure t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  inferior-js-mode-hook))
    (add-hook hook
              (lambda ()
                (tern-mode)

                (add-to-list (make-local-variable 'company-backends)
                             '(company-tern company-yasnippet))

                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
  )

(use-package pyenv
  :load-path "pyenv.el"
  :config
  (global-pyenv-mode)
  (dolist (hook '(prog-mode-hook js2-mode-hook))
    (add-hook hook #'pyenv-use-corresponding)))

(use-package json-reformat
  :ensure t)

(use-package json-snatcher
  :ensure t
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  (add-hook 'js-mode-hook 'js-mode-bindings)
  (add-hook 'js2-mode-hook 'js-mode-bindings))

(use-package magit
  :ensure t
  :commands (magit-status magit-log)
  :init
  (global-magit-file-mode))

(use-package magit-filenotify
  :ensure t
  :commands (magit-filenotify-mode)
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(use-package sudo-edit
  :ensure t)

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package js-doc
  :ensure t
  :commands (js-doc-insert-function-doc js-doc-insert-tag)
  :config
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                (define-key js2-mode-map "@" 'js-doc-insert-tag)
                )))

(use-package web-completion-data
  :ensure t)

(use-package ac-html-bootstrap
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.tpl" . web-mode)
   ("\\.php" . web-mode)
   ("\\.[agj]sp" . web-mode)
   ("\\.as[cp]x" . web-mode)
   ("\\.erb" . web-mode)
   ("\\.mustache" . web-mode)
   ("\\.ejs" . web-mode)
   ("html?$" . web-mode)
   ("\\.template?" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("django"    . "segmentation.*\\.html")
          ("django"    . "klink.*\\.html")
          ("ctemplate"  . "\\.template")
          ("angular"  . "tunecakes.*\\.ejs"))
        )

  (add-hook 'web-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends
                           '(company-nxml company-web-html company-tern
                                          company-yasnippet company-css))
              (company-web-bootstrap+)
              (add-hook 'before-save-hook 'web-beautify-html-buffer t t)
              ;; (add-hook 'after-save-hook 'web-mode-reload t t)
              ))


  (add-hook 'css-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-css-buffer t t)
              ))
  )

(use-package web-beautify
  :ensure t
  :commands (web-beautify-css-buffer web-beautify-html-buffer web-beautify-js-buffer))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config

  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt")

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(elpy-company-backend))))
  (add-hook 'inferior-python-mode-hook
          (lambda ()
            (push
             'comint-watch-for-password-prompt comint-output-filter-functions)))
)

(use-package session
  :ensure t
  :init
  (session-initialize))

(use-package yasnippet
  :ensure t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil))
  :config
  (yas-global-mode)

  (defun expand-for-web-mode ()
    (when (equal mode-name "Web")
      (make-local-variable 'yas-extra-modes)
      (setq yas-extra-modes
            (let ((web-lang (web-mode-language-at-pos)))
              (cond
               ((equal web-lang "html")       '(html-mode))
               ((equal web-lang "css")        '(css-mode))
               ((equal web-lang "javascript") '(javascript-mode))
               )))))

  (add-hook 'yas-before-expand-snippet-hook 'expand-for-web-mode)
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  :config
  (setq projectile-globally-ignored-files
        (append '("*.txt" "*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#")
                projectile-globally-ignored-files))

  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package helm-projectile
  :ensure t
  :bind
  (("C-x C-f" . proj-open-file))
  :init
  (defun proj-open-file ()
    "Open file using projectile if in project"
    (interactive)
    (if (projectile-project-p)
        (helm-projectile)
      (helm-for-files)))
  :config
  (helm-projectile-on))

(use-package keyfreq
  :init
  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          forward-char
          backward-char
          previous-line
          next-line
          helm-next-line
          helm-previous-line
          helm-M-x
          newline
          proj-open-file
          save-buffer
          yank))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package python-docstring
  :ensure t
  :init
  (add-hook 'python-mode-hook 'python-docstring-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (setq show-paren-style 'mixed)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (show-paren-mode 1)
  (electric-pair-mode 1))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package helm-ag
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-c h o" . helm-occur))
  :config
  (helm-mode)
  (helm-adaptive-mode))

(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  :config
  (with-eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(use-package simple
  :diminish visual-line-mode
  :config
  (global-visual-line-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package linum-off
  :ensure t
  :config
  (global-linum-mode 1)

  (defun my-find-file-check-make-large-file-read-only-hook ()
    "If a file is over a given size, turn off nlinum and font-lock-mode."
    (if (> (buffer-size) (* 1024 1024))
        (progn (linum-mode -1)
               (font-lock-mode -1))))

  (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :commands (powerline-default-theme))

;; This doesn't seem to work right in use-package
(add-hook 'after-init-hook (lambda ()
                             (load-theme 'tango-dark t)
                             (sml/setup)
                             (powerline-default-theme)))

(use-package tern
  :ensure t
  :commands (tern-mode))

(use-package cc-mode
  :config
  (defun make-CR-do-indent ()
    (define-key c-mode-base-map "\C-m" 'c-context-line-break))

  (add-hook 'c-initialization-hook 'make-CR-do-indent)

  (defun c-mode-common-hook ()
    (c-toggle-auto-hungry-state 1))

  (add-hook 'c-mode-common-hook 'c-mode-common-hook))

(use-package minibuffer
  :config
  ;; lower garbage collect thresholds in minibuffer
  ;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)		; completion-at-point-functions
          (company-abbrev company-dabbrev)
          ))

  (use-package company-statistics
    :ensure t
    :init
    (company-statistics-mode))

  (use-package company-web
    :ensure t
    :commands (company-web-bootstrap+ company-web-html))

  (use-package company-try-hard
    :ensure t
    :bind
    (("C-<tab>" . company-try-hard)
     :map company-active-map
     ("C-<tab>" . company-try-hard)))

  (use-package company-tern
    :ensure t
    :config
    ;; Enable JavaScript completion between <script>...</script> etc.
    (defadvice company-tern (before web-mode-set-up-ac-sources activate)
      "Set `tern-mode' based on current language before running `company-tern'."
      (if (equal major-mode 'web-mode)
          (let ((web-mode-cur-language (web-mode-language-at-pos)))
            (if (or (string= web-mode-cur-language "javascript")
                    (string= web-mode-cur-language "jsx"))
                (unless tern-mode (tern-mode))
              (if tern-mode (tern-mode -1)))))))

  )

(use-package ignoramus
  :ensure t
  :init
  (ignoramus-setup))


(use-package window
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window)
   ))

(use-package files
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

;;; init.el ends here
