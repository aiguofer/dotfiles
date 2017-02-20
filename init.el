;; .emacs --- My emacs config
;;; Commentary:
;;; Code:
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (auto-package-update-maybe))

(use-package subword
  :ensure t
  :diminish subword-mode
  :config
  (global-subword-mode))

(use-package semantic
  :ensure t
  :config
  (semantic-mode 1))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package flycheck
  :ensure t
  :config
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
   ("zshrc$" . sh-mode)
   ("\\.zsh" . sh-mode)))

(use-package vmd-mode
  :ensure t
  :config
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
                (tern-mode t)

                (add-to-list (make-local-variable 'company-backends)
                             '(company-tern company-yasnippet))

                (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
  )

;; (use-package pyenv-mode-auto
;;   :ensure t
;;   :init

;;   (defun projectile-pyenv-mode-set ()
;;   "Set pyenv version matching project name."
;;   (let ((project (projectile-project-name)))
;;     (if (member project (pyenv-mode-versions))
;;         (pyenv-mode-set project)
;;       (pyenv-mode-unset))))

;;   (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

(use-package pyenv
  :load-path "pyenv.el"
  :init
  (dolist (hook '(prog-mode-hook js2-mode-hook))
    (add-hook hook #'pyenv-use-corresponding))
  :config
  (global-pyenv-mode))

(use-package magit
  :ensure t)

(use-package magit-filenotify
  :ensure t
  :init
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(use-package sudo-edit
  :ensure t)

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

(use-package js-doc
  :config
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                (define-key js2-mode-map "@" 'js-doc-insert-tag)
                )))

(use-package web-completion-data
  :ensure t)

(use-package company-web
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
  :init
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
  :defer t
  :config
  (elpy-enable)
  (elpy-use-ipython)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt --pprint")
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(elpy-company-backend)) ;; removed company-yasnippet, might want to add back
              )))

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
  (projectile-global-mode))

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

(use-package nlinum
  :ensure t
  :bind
  (("M-n" . nlinum-mode))
  :config
  (global-nlinum-mode)

  (defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, turn off nlinum and font-lock-mode."
  (if (> (buffer-size) (* 1024 1024))
      (progn (nlinum-mode -1)
             (font-lock-mode -1))))

  (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :commands (powerline-default-theme))

(add-hook 'after-init-hook (lambda ()
                             (load-theme 'tango-dark t)
                             (sml/setup)
                             (powerline-default-theme)))

(use-package tern
  :ensure t)

(use-package cc-mode
  :init
  (defun make-CR-do-indent ()
    (define-key c-mode-base-map "\C-m" 'c-context-line-break))

  (add-hook 'c-initialization-hook 'make-CR-do-indent)

  (defun c-mode-common-hook ()
    (c-toggle-auto-hungry-state 1))

  (add-hook 'c-mode-common-hook 'c-mode-common-hook))

(use-package minibuffer
  :init
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
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)		; completion-at-point-functions
          (company-abbrev company-dabbrev)
          ))
  :config
  (global-company-mode))

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

(use-package window
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window)
   ))

(setq-default indent-tabs-mode nil)
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
    ((elpy-company-backend company-yasnippet company-web-jade company-web-html company-tern company-semantic company-bbdb company-nxml company-css company-eclim company-clang company-xcode company-cmake company-dabbrev-code company-gtags company-etags)
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
 '(custom-safe-themes
   (quote
    ("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8e5dd88c42089566d5f8e1a23d3017c213eeccd94a7b9e1a58a2dc3e08cb26d5" "8cf56691a70156f611ac86d0bbcbc7dee7673df195de5918f34bfdc6814ffd39" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "65320d86c52e9019347ed725f2a7c07705be5acb38bc83b92064e2489f6c3edc" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "a233249cc6f90098e13e555f5f5bf6f8461563a8043c7502fb0474be02affeea" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" default)))
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
    (elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-timeout 2)
 '(helm-follow-mode-persistent t)
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot 1)
 '(js-indent-level 3)
 '(js2-global-externs (quote ("angular")))
 '(js2-include-node-externs t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js3-auto-indent-p t)
 '(js3-boring-indentation t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-indent-dots t)
 '(js3-indent-level 3)
 '(js3-indent-on-enter-key t)
 '(js3-lazy-dots t)
 '(json-reformat:indent-width 3)
 '(json-reformat:pretty-string\? t)
 '(line-number-mode t)
 '(magit-diff-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff")))
 '(magit-pull-arguments (quote ("--rebase")))
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   (quote
    (use-package company django-mode find-file-in-project git-commit helm helm-company jedi-core magit magit-rockstar markdown-mode pos-tip powerline projectile string-inflection swiper tern typescript-mode vmd-mode web-beautify web-completion-data with-editor yaml-mode yasnippet counsel-projectile coffee-mode docker dockerfile-mode smart-mode-line imenu-anywhere flycheck flycheck-tip sudo-edit flymake-yaml jedi json-mode systemd smart-mode-line-powerline-theme helm-projectile xkcd web-mode tramp-term tramp-hdfs tj-mode tern-django swiper-helm spacegray-theme smart-tab session rainbow-delimiters python-docstring pig-snippets pig-mode memoize melpa-upstream-visit magit-tramp magit-filenotify lusty-explorer linum-off keyfreq js2-refactor helm-fuzzy-find helm-dired-recent-dirs helm-ag geeknote elpy edit-color-stamp django-snippets direx diminish company-web company-try-hard company-jedi company-anaconda bash-completion auto-package-update angular-snippets ac-html-csswatcher ac-html-bootstrap)))
 '(powerline-default-separator (quote utf-8))
 '(projectile-completion-system (quote helm))
 '(projectile-mode-line nil)
 '(python-indent-offset 4)
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython")))
 '(rm-blacklist (quote (" Wrap" " Helm" " hl-p")))
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-local-symbol-highlight-mode)))
 '(session-use-package t nil (session))
 '(sml/extra-filler -6)
 '(sml/mode-width (quote right))
 '(sml/name-width 5)
 '(sml/shorten-mode-string "")
 '(sml/theme (quote dark))
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

;;; .emacs ends here
