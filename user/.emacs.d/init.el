;;; .emacs --- My emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'default-frame-alist
             '(font . "Hack Nerd Font 18"))
(set-face-attribute 'mode-line nil :font "Hack Nerd Font 14")

;; Move to top to fix package-selected-package
;; see https://github.com/jwiegley/use-package/issues/397
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Use UTF8 everywhere, see https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Don't ask about killing process buffers on shutdown
;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Use bash.. zsh causes slowness in projectile: https://github.com/syl20bnr/spacemacs/issues/4207
;; (setq shell-file-name "/bin/bash")

;; Show filename in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; allow typing y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; enable up/down case region key shortcuts
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Download and set up use-package
(straight-use-package 'use-package)

;; Allow usage of :bind in use-package
(use-package bind-key
  :straight t)

;; Allow usage of :diminish in use-package
(use-package diminish
  :straight t)

(use-package eldoc
  :custom (eldoc-minor-mode-string nil))

(use-package window
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window)))

(use-package windmove
  :bind
  (("M-j" . windmove-left)
   ("M-i" . windmove-up)
   ("M-k" . windmove-down)
   ("M-l" . windmove-right)))

;; Eventually replace tree-sitter with built-in treesit
;; TODO: enable
;; (use-package treesit
;;   :config
;;   (add-to-list 'treesit-extra-load-path
;;                (substitute-env-in-file-name "$HOME/personal/tree-sitter-module/dist")))

;; Use treesit to smartly move around coude based on syntax tree
;; TODO: enable when I switch to treesit
;; (use-package combobulate
;;   :preface
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c o")

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook ((python-ts-mode . combobulate-mode)
;;          (js-ts-mode . combobulate-mode)
;;          (css-ts-mode . combobulate-mode)
;;          (yaml-ts-mode . combobulate-mode)
;;          (typescript-ts-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode))
;;   ;; Amend this to the directory where you keep Combobulate's source
;;   ;; code.
;;   :load-path ("path-to-git-checkout-of-combobulate"))

(use-package session
  :straight t
  :demand t
  :hook (after-save . session-save-session)
  :config
  (session-initialize))

(use-package simple
  :diminish visual-line-mode
  :hook (before-save . delete-trailing-whitespace)
  :config
  ;; use this instead of linum
  (global-display-line-numbers-mode)
  ;; visual-line-mode is a mode for wrapping lines
  (global-visual-line-mode))

;; clean up unusued buffers at midnight
(use-package midnight
  :config
  (midnight-mode))

;; Make M-f/b move between hyphens or camel-case changes
(use-package subword
  :straight t
  :diminish subword-mode
  ;; need to load after diminish so it gets diminished
  :after diminish
  :init
  (global-subword-mode))

;; Auto load changes to file made from another program
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package markdown-mode
  :straight t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package pkgbuild-mode
  :straight t
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

;; Respect .editorconfig files
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package vmd-mode
  :straight (:host github :repo "aiguofer/vmd-mode")
  :commands (vmd-mode))

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :interpreter "node")

(use-package json-mode
  :straight t)

;; coffeeScript
(use-package coffee-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'")

;; Search for word within buffer
(use-package swiper
  :straight t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

(use-package systemd
  :straight t)

(use-package magit
  :straight t
  :commands (magit-status magit-log))

;; (use-package emacsql-sqlite3
;;   :straight t
;;   :config
;;   (setq org-roam-database-connector 'sqlite3))

;; (use-package forge
;;   :straight t
;;   :after magit)

;; (use-package code-review
;;   :straight t
;;   :after forge
;;   :bind (:map forge-topic-mode-map
;;               ("C-c r" . 'code-review-forge-pr-at-point)
;;               :map code-review-feedback-section-map
;;               ("C-k" . 'code-review-section-delete-comment)
;;               :map code-review-local-comment-section-map
;;               ("C-k" . 'code-review-section-delete-comment)
;;               :map code-review-reply-comment-section-map
;;               ("C-k" . 'code-review-section-delete-comment)
;;               :map code-review-mode-map
;;               ("C-c C-n" . 'code-review-comment-jump-next)
;;               ("C-c C-p" . 'code-review-comment-jump-previous))
;;   :custom
;;   (code-review-auth-login-marker 'forge))

(use-package sudo-edit
  :straight t)

(use-package js-doc
  :straight t
  :commands (js-doc-insert-function-doc js-doc-insert-tag))

(use-package web-completion-data
  :straight t)

;; (use-package django-mode
;;   :straight t
;;   :mode
;;   (("\\.djhtml$" . django-html-mode)))

(use-package web-mode
  :straight t
  :mode
  (("\\.tpl" . web-mode)
   ("\\.php" . web-mode)
   ("\\.[agj]sp" . web-mode)
   ("\\.as[cp]x" . web-mode)
   ("\\.erb" . web-mode)
   ("\\.mustache" . web-mode)
   ("\\.ejs" . web-mode)
   ("\\.html?$" . web-mode)
   ("\\.template?" . web-mode))
  :hook (web-mode . setup-web-mode)
  :custom
  (web-mode-engines-alist '(("django"    . "textmyjournal.*\\.html")
                            ("ctemplate"  . "\\.template")
                            ("angular"  . "tunecakes.*\\.ejs")))
  :config
  (defun setup-web-mode ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends
                 '(company-yasnippet))
    (add-hook 'before-save-hook 'web-beautify-html-buffer t t)
    ))

(use-package web-beautify
  :straight t
  :commands (web-beautify-html-buffer))

(use-package prettier-js
  :straight t
  :hook
  ((json-mode js2-mode inferior-js-mode typescript-mode css-mode) . prettier-js-mode))

(use-package org
  :straight t
  :custom
  ;; set maximum indentation for description lists
  (org-list-description-max-indent 5)
  ;; prevent demoting heading also shifting text inside sections
  (org-adapt-indentation nil))

(use-package ox-pandoc
  :straight t
  :after (org exec-path-from-shell))

(use-package python
  :hook (inferior-python-mode . fix-python-password-entry)
  :custom
  (python-shell-interpreter "jupyter-console")
  (python-shell-interpreter-args "--simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter-console")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

  (defun fix-python-password-entry ()
    (push
     'comint-watch-for-password-prompt comint-output-filter-functions))

  (defun my-setup-python (orig-fun &rest args)
    "Use corresponding kernel for current Pyenv version"
    (let* ((curr-python (car (split-string (shim-version) ":")))
           (python-shell-buffer-name (concat "Python-" curr-python))
           (python-shell-interpreter-args (if (bound-and-true-p djangonaut-mode)
                                              "shell_plus -- --simple-prompt"
                                            (concat "--simple-prompt --kernel=pyenv_" curr-python)))
           (python-shell-interpreter (if (bound-and-true-p djangonaut-mode)
                                         "django-admin"
                                       python-shell-interpreter)))
      (apply orig-fun args)))

  (advice-add 'python-shell-get-process-name :around #'my-setup-python)
  (advice-add 'python-shell-calculate-command :around #'my-setup-python))

(use-package cython-mode
  :straight t)

;; Highlight and reformat docstrings in python
(use-package python-docstring
  :straight t
  :hook (python-mode . python-docstring-mode))

(use-package poetry
  :straight t)

;; Needed dependency for py-cmd-buffer modes
(use-package buftra
  :straight (:host github :repo "humitos/buftra.el"))

(use-package py-pyment
  :straight (:host github :repo "aiguofer/py-cmd-buffer.el")
  :after buftra
  :config
  (setq py-pyment-options '("--output=numpydoc")))

;; TODO: enable through LSP
;; (use-package py-isort
;;   :straight (:host github :repo "aiguofer/py-cmd-buffer.el")
;;   :hook (python-mode . py-isort-enable-on-save)
;;   ;; :config
;;   ;; (setq py-isort-options '("-l=88" "--profile=black"))
;;   )

;; Auto-remove
;; (use-package py-autoflake
;;   :straight (:host github :repo "humitos/py-cmd-buffer.el")
;;   :hook (python-mode . py-autoflake-enable-on-save)
;;   :config
;;   (setq py-autoflake-options '("--expand-star-imports")))

;; Reformat docstrings
;; (use-package py-docformatter
;;   :straight (:host github :repo "humitos/py-cmd-buffer.el")
;;   :hook (python-mode . py-docformatter-enable-on-save)
;;   :config
;;   (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))

;; (use-package djangonaut
;;   :straight t
;;   :config
;;   (setq pythonic-interpreter "python")
;;   (global-djangonaut-mode))

;; (use-package elpy
;;   :straight t
;;   :bind (:map elpy-mode-map
;;               ("C-M-n" . elpy-nav-forward-block)
;;               ("C-M-p" . elpy-nav-backward-block))
;;   :hook ((elpy-mode . (lambda ()
;;                         (add-hook 'before-save-hook
;;                                   'elpy-format-code nil t)))
;;          (elpy-mode . flycheck-mode)
;;          (elpy-mode . (lambda ()
;;                         (set (make-local-variable 'company-backends)
;;                              '((elpy-company-backend :with company-yasnippet))))))
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (setq elpy-shell-echo-output nil) ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
;;   (setq elpy-rpc-python-command "python3")
;;   (setq elpy-rpc-timeout 2))

(use-package eglot
  ;; :straight t
  :hook
  ((kotlin-mode
     go-mode
     python-mode) . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
    (lambda (&rest args)
      (let ((venv-directory (string-trim (shell-command-to-string "pyenv prefix"))))
        ;; pylsp otpions https://gist.github.com/doolio/8c1768ebf33c483e6d26e5205896217f
        `((:pylsp .
                  (:plugins
                   (:jedi_completion (:include_params t
                                      :fuzzy t)
                    :jedi (:environment ,venv-directory)
                    ;; :flake8 (:enabled t)
                    :black (:enabled t)
                    :rope_autoimport (:enabled t
                                      :python_path ,venv-directory)))))))))

;; (use-package lsp-mode
;;   :straight t
;;   :config
;;   (setq lsp-pylsp-plugins-jedi-use-pyenv-environment t)
;;   ;; make sure we have lsp-imenu everywhere we have LSP
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

;; ;; completion backends
;; (use-package company-lsp
;;   :straight t
;;   :config
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (add-to-list (make-local-variable 'company-backends)
;;                            '(company-lsp))))
;;   )
;; )

;; (use-package lsp-java
;;   :straight t
;;   :hook (java-mode . lsp))

;; (use-package lsp-ui
;;   :straight t
;;   :after lsp-mode
;;   :hook lsp-mode
;;   :config
;;   (setq lsp-ui-sideline-ignore-duplicate t)

;;   (define-key lsp-ui-mode-map [remap xref-find-definitions]
;;     #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references]
;;     #'lsp-ui-peek-find-references))

;; (use-package dap-mode
;;   :straight t
;;   :after lsp-mode
;;   :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
;;   :init
;;   (dap-auto-configure-mode)
;;   :config

;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   (defun dap-python--pyenv-executable-find (command)
;;     (with-venv (executable-find "python")))

;;   (add-hook 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra))))

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil))
  :hook (yas-before-expand-snippet . expand-for-web-mode)
  :init
  (yas-global-mode)
  :config
  (defun expand-for-web-mode ()
    (when (equal mode-name "Web")
      (make-local-variable 'yas-extra-modes)
      (setq yas--extra-modes
            (let ((web-lang (web-mode-language-at-pos)))
              (cond
               ((equal web-lang "html")       '(html-mode))
               ((equal web-lang "css")        '(css-mode))
               ((equal web-lang "javascript") '(javascript-mode))
               ))))))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :init
  ;; this must be done before :config so we can't use :bind
  (define-key global-map (kbd "C-c p") 'projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-globally-ignored-files
        (append '("*.txt" "*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#")
                projectile-globally-ignored-files)))

(use-package keyfreq
  :straight t
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

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (show-paren-mode 1)
  (electric-pair-mode 1))

(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

(use-package sudo-edit
  :straight t)

(use-package helm
  :straight t
  :diminish helm-mode
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-c h o" . helm-occur))
  :config
  (helm-mode)
  (helm-adaptive-mode))

(use-package helm-ag
  :after helm
  :straight t)

(use-package helm-rg
  :after helm
  :straight t)

(use-package ripgrep
  :after helm
  :straight t)

(use-package helm-projectile
  :straight t
  :demand t
  :after (helm projectile)
  :bind
  (("C-x C-f" . proj-open-file))
  :custom
  (projectile-completion-system 'helm)
  :init
  (defun proj-open-file ()
    "Open file using projectile if in project"
    (interactive)
    (if (projectile-project-p)
        (helm-projectile)
      (helm-for-files)))
  :config
  (helm-projectile-on))

(use-package helm-pydoc
  :straight t
  :bind
  (:map python-mode-map
        ("C-c C-d" . helm-pydoc)))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  :config
  (with-eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(use-package nord-theme
  :straight t
  :custom
  (nord-region-highlight "frost")
  :config
  ;; fix from https://github.com/arcticicestudio/nord-emacs/issues/59#issuecomment-414882071
  ;; hopefully won't need this forever
  (if (daemonp)
      (cl-labels ((load-nord (frame)
                             (with-selected-frame frame
                               (load-theme 'nord t))
                             (remove-hook
                              'after-make-frame-functions
                              #'load-nord)))
        (add-hook 'after-make-frame-functions #'load-nord))
    (load-theme 'nord t)))

(use-package smart-mode-line-powerline-theme
  :straight t
  :after (nord-theme)
  :custom
  (sml/mule-info nil)
  (sml/no-confirm-load-theme t)

  :config
  (sml/setup)
  (powerline-default-theme)

  (add-to-list 'sml/replacer-regexp-list
               '("^~/.pyenv/versions/\\([a-zA-Z0-9_-]+\\)/"
                 (lambda (s) (concat ":PE:" (match-string 1 s) ":")))
               t))

(use-package tide
  :straight t
  :hook ((js2-mode inferior-js-mode typescript-mode) . setup-tide-mode)
  :custom
  (company-tooltip-align-annotations t)
  :config
  (defun setup-tide-mode ()
    "Set up Tide mode."
    (interactive)
    (tide-setup)
    ;; (flycheck-mode +1)
    ;; (setq flycheck-check-syntax-automatically '(save-mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (add-to-list (make-local-variable 'company-backends)
                 '(company-tide company-yasnippet)))



  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tide (before web-mode-set-up-ac-sources activate)
    "Set `tide-mode' based on current language before running `company-tide'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tide-mode (tide-mode))
            (if tide-mode (tide-mode -1))))))

  )

(use-package cc-mode
  :hook ((c-initialization . make-CR-do-indent)
         (c-mode-common . c-mode-common-hook))
  :config
  (defun make-CR-do-indent ()
    (define-key c-mode-base-map "\C-m" 'c-context-line-break))

  (defun c-mode-common-hook ()
    (c-toggle-auto-hungry-state 1))
  )

(use-package minibuffer
  :hook   ((minibuffer-setup-hook . my-minibuffer-setup-hook)
           (minibuffer-exit-hook . my-minibuffer-exit-hook))
  :config
  ;; lower garbage collect thresholds in minibuffer
  ;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))
  )

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-fontification-alist
        (append '((1 (eq major-mode 'python-mode) font-lock-string-face)
                  (1 (eq major-mode 'fundamental-mode) green-face)
                  (1 (member major-mode '(shell-mode sh-mode shell-script-mode))
                     font-lock-function-name-face))
                ibuffer-fontification-alist)))

(use-package company
  :straight t
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
  )

;; Keep commonly used completions near the top
(use-package company-statistics
  :straight t
  :after company
  :init
  (company-statistics-mode))

(use-package company-web
  :straight t
  :after company)

(use-package company-try-hard
  :straight t
  :after company
  :bind
  (("C-<tab>" . company-try-hard)
   :map company-active-map
   ("C-<tab>" . company-try-hard)))

(use-package company-quickhelp
  :straight t
  :after company
  :config
  (company-quickhelp-mode))

;; a nicer way to show company completions with icons and doc popup where available (lsp etc.)
;; Also doesn't clutter up the screen with super-big multiline truncated lines
(use-package company-box
  :straight t
  :after company
  :if (display-graphic-p)
  :custom
  (company-box-frame-behavior 'point)
  (company-box-show-single-candidate t)
  (company-box-doc-delay 1)

  :hook
  (company-mode . company-box-mode))

;; little hack function to make company box frame bigger
(defun themkat/company-box-fix-size ()
  (interactive)
  (let* ((box-frame (company-box--get-frame)))
    (when (not (null box-frame))
      (set-face-attribute 'default
                          box-frame
                          :height 180))))

(use-package ignoramus
  :straight t
  :init
  (ignoramus-setup))

(use-package files
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package switch-buffer-functions
  :straight t)

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package package-lint
  :straight t)

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package highlight-symbol
  :straight t)

(use-package applescript-mode
  :straight t)

(use-package undo-tree
  :diminish undo-tree-mode
  :straight t
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package gradle-mode
  :diminish gradle-mode
  :straight (:host github :repo "aiguofer/emacs-gradle-mode")
  :hook
  ((kotlin-mode . gradle-mode)
   (java-mode . gradle-mode)))

(use-package kotlin-mode
  :diminish kotlin-mode
  :straight t
  ;; :after (lsp-mode dap-mode)
  :config
  ;; (require 'dap-kotlin)
  ;; ;; should probably have been in dap-kotlin instead of lsp-kotlin
  ;; (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  )

;; Replace kotlin-mode with kotlin-ts-mode when treesit support
;; (use-package kotlin-ts-mode
;;   :straight (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
;;   ;; :mode "\\.kt\\'" ; if you want this mode to be auto-enabled
;;   )

(use-package groovy-mode
  :straight t)

(use-package go-mode
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :straight t
  :after tree-sitter-langs
  :diminish tree-sitter-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; shim applies pyenv, jenv, goenv, nodenv, etc
(use-package shim
  :straight (:host github :repo "twlz0ne/shim.el")
  :demand t
  :hook
  ((java-mode
    kotlin-mode
    python-mode
    go-mode
    ruby-mode
    tide-mode) . shim-mode)
  :config
  (shim-init-ruby)
  (shim-init-python)
  (shim-init-node)
  (shim-init-java)
  (shim-init-go)
  (shim-register-mode 'java 'kotlin-mode)
  (shim-register-mode 'node 'js2-mode)
  (shim-register-mode 'python 'python-ts-mode))

(use-package terraform-mode
  :straight t
  :hook (terraform-mode . outline-minor-mode))

;;; init.el ends here

;; DISABLED: Replaced by shim
;; (use-package pyenv
;;   :straight (:host github :repo "aiguofer/pyenv.el")
;;   :config
;;   (setq pyenv-use-alias 't)
;;   (setq pyenv-modestring-prefix " ")
;;   (setq pyenv-modestring-postfix nil)
;;   (setq pyenv-set-path nil)

;;   (global-pyenv-mode)
;;   (defun pyenv-update-on-buffer-switch (prev curr)
;;     (if (and (string-equal "Python" (format-mode-line mode-name nil nil curr))
;;              (not (cl-search ".pyenv/versions" (buffer-file-name))))
;;         (progn
;;           (pyenv-use-corresponding)
;;           (pyvenv-activate (concat (pyenv--prefix) "/"))
;;           )))
;;   (add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))

;; DISABLED: on large repos, switching branches causes huge slowdowns
;; (use-package magit-filenotify
;;   :straight t
;;   :commands (magit-filenotify-mode)
;;   :hook (magit-status-mode . magit-filenotify-mode))

;; DISABLED: was causing issues with go-to-def for certain languages, useful
;; when using elpy
;; (use-package smart-jump
;;   :straight t
;;   :config
;;   (smart-jump-setup-default-registers))

;; DISABLED: I don't think I use this
;; ;; Parse certain language files for various uses
;; (use-package semantic
;;   :straight t
;;   :init
;;   (semantic-mode 1))

;; DISABLED: seems flymake has caught up to flycheck
;; (use-package flycheck
;;   :straight t
;;   :init
;;   (global-flycheck-mode))

;; (use-package flycheck-tip
;;   :straight t
;;   :bind
;;   (("C-c C-n" . error-tip-cycle-dwim)
;;    ("C-c C-p" . error-tip-cycle-dwim-reverse))
;;   )

;; DISABLED: shim handles using correct venv, no current need for this
;; (use-package with-venv
;;   :straight t)

;; DISABLED: haven't used in a while
;; (use-package jupyter
;;   :straight t
;;   :hook
;;   (jupyter-repl-mode . (lambda ()
;;                          (setq company-backends '(company-capf))))
;;   :bind (:map jupyter-repl-mode-map
;;               ("C-M-n" . jupyter-repl-history-next)
;;               ("C-M-p" . jupyter-repl-history-previous)
;;               ("M-n" . jupyter-repl-forward-cell)
;;               ("M-p" . jupyter-repl-backward-cell)
;;               :map jupyter-repl-interaction-mode-map
;;               ("M-i" . nil)
;;               ("C-?" . jupyter-inspect-at-point)))
