;; .emacs --- My emacs config
;;; Commentary:
;;; Code:

;; Move to top to fix package-selected-package
;; see https://github.com/jwiegley/use-package/issues/397
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Use UTF8 everywhere, see https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Don't ask about killing process buffers on shutdown
;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Use bash.. zsh causes slowness in projectile: https://github.com/syl20bnr/spacemacs/issues/4207
(setq shell-file-name "/bin/bash")

;; Show filename in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; allow typing y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

(use-package windmove
  :bind
  (("M-j" . windmove-left)
  ("M-i" . windmove-up)
  ("M-k" . windmove-down)
  ("M-l" . windmove-right))
)


(use-package subword
  :straight t
  :diminish subword-mode
  :init
  (global-subword-mode))

(use-package semantic
  :straight t
  :init
  (semantic-mode 1))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))

(use-package flycheck-tip
  :straight t
  :init
  (define-key global-map (kbd "C-c C-n") 'error-tip-cycle-dwim)
  (define-key global-map (kbd "C-c C-p") 'error-tip-cycle-dwim-reverse))

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

(use-package vmd-mode
  :straight t
  :commands (vmd-mode)
  :init
  (add-hook 'markdown-mode-hook 'vmd-mode))

(use-package coffee-mode
  :straight t)

(use-package swiper
  :straight t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'")

(use-package systemd
  :straight t)

(use-package json-mode
  :straight t
  :mode "\\.json\\'")

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  inferior-js-mode-hook))
    (add-hook hook
              (lambda ()
                (tide-mode)

                (add-to-list (make-local-variable 'company-backends)
                             '(company-tide company-yasnippet))
                )))
  )

(use-package json-snatcher
  :straight t
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  (add-hook 'js-mode-hook 'js-mode-bindings)
  (add-hook 'js2-mode-hook 'js-mode-bindings))

(use-package magit
  :straight t
  :commands (magit-status magit-log)
  :init
  (global-magit-file-mode))

(use-package magit-filenotify
  :straight t
  :commands (magit-filenotify-mode)
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(use-package sudo-edit
  :straight t)

(use-package diminish
  :straight t)

(use-package bind-key
  :straight t)

(use-package js-doc
  :straight t
  :commands (js-doc-insert-function-doc js-doc-insert-tag)
  :config
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                (define-key js2-mode-map "@" 'js-doc-insert-tag)
                )))

(use-package web-completion-data
  :straight t)

;; (use-package ac-html-bootstrap
;;   :straight t)

(use-package ac-html-csswatcher
  :straight t)

(use-package django-mode
  :straight t
  :mode
  (("\\.djhtml$" . django-html-mode)))

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
                           '(company-nxml company-web-html company-tide
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
  :straight t
  :commands (web-beautify-css-buffer web-beautify-html-buffer web-beautify-js-buffer))

(use-package python
  :bind
  (("C-c C-2" . run-python2)
   ("C-c C-3" . run-python3))
  :config

  (use-package jupyter
    :straight t
    :hook
    (jupyter-repl-mode . (lambda ()
                           (setq company-backends '(company-capf))))
    :bind
    (:map jupyter-repl-mode-map
          ("C-M-n" . jupyter-repl-history-next)
          ("C-M-p" . jupyter-repl-history-previous)
          ("M-n" . jupyter-repl-forward-cell)
          ("M-p" . jupyter-repl-backward-cell)
          :map jupyter-repl-interaction-mode-map
          ("M-i" . nil)
          ("C-?" . jupyter-inspect-at-point)
          )
    )


  (setq python-shell-interpreter "jupyter-console"
        python-shell-interpreter-args "--simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter-console")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (push
               'comint-watch-for-password-prompt comint-output-filter-functions)))

  (defun run-python2 ()
    (interactive)
    "Run IPython with Python 2."
    (let ((python-shell-buffer-name "Python 2")
          (python-shell-interpreter-args "--simple-prompt --kernel=python2"))
      (run-python nil nil t)))

  (defun run-python3 ()
    "Run IPython with Python 3."
    (interactive)
    (let ((python-shell-buffer-name "Python 3")
          (python-shell-interpreter-args "--simple-prompt --kernel=python3")
          )
      (run-python nil nil t)))

  (use-package pyenv
    :straight (:host github :repo "aiguofer/pyenv.el")
    :config
    (global-pyenv-mode))

  (use-package buftra
    :straight (:host github :repo "humitos/buftra.el"))

  (use-package py-pyment
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :config
    (setq py-pyment-options '("--output=numpydoc")))

  (use-package py-isort
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :config
    (setq py-isort-options '("--lines=88" "-m=3" "-tc" "-fgw=0" "-ca"))
    (add-hook 'python-mode-hook 'py-isort-enable-on-save)
    )

  (use-package blacken
    :straight t
    :config
    (setq blacken-line-length '88)
    (add-hook 'python-mode-hook 'blacken-mode))

  (use-package python-docstring
    :straight t
    :init
    (add-hook 'python-mode-hook 'python-docstring-mode))

  (use-package elpy
    :straight t
    :bind
    (:map elpy-mode-map
          ("C-M-n" . elpy-nav-forward-block)
          ("C-M-p" . elpy-nav-backward-block))
    :init
    (elpy-enable)
    :config

    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))

    (add-hook 'python-mode-hook
              (lambda ()
                (add-to-list (make-local-variable 'company-backends)
                             '(elpy-company-backend))))
    )

  ;; (use-package lsp-mode
  ;;   :straight t
  ;;   :config

  ;;   ;; make sure we have lsp-imenu everywhere we have LSP
  ;;   (require 'lsp-imenu)
  ;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;;   ;; get lsp-python-enable defined
  ;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
  ;;   ;;     or any other function that can be used to find the root directory of a project
  ;;   (lsp-define-stdio-client lsp-python "python"
  ;;                            #'projectile-project-root
  ;;                            '("pyls"))

  ;;   ;; make sure this is activated when python-mode is activated
  ;;   ;; lsp-python-enable is created by macro above
  ;;   (add-hook 'python-mode-hook
  ;;             (lambda ()
  ;;               (lsp-python-enable)))

  ;;   ;; lsp extras
  ;;   (use-package lsp-ui
  ;;     :straight t
  ;;     :config
  ;;     (setq lsp-ui-sideline-ignore-duplicate t)
  ;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;;     (define-key lsp-ui-mode-map [remap xref-find-definitions]
  ;;       #'lsp-ui-peek-find-definitions)
  ;;     (define-key lsp-ui-mode-map [remap xref-find-references]
  ;;       #'lsp-ui-peek-find-references))

  ;;   (use-package company-lsp
  ;;     :straight t
  ;;     :config
  ;;     (add-hook 'python-mode-hook
  ;;               (lambda ()
  ;;                 (add-to-list (make-local-variable 'company-backends)
  ;;                              '(company-lsp))))
  ;;     )

  ;;   ;; NB: only required if you prefer flake8 instead of the default
  ;;   ;; send pyls config via lsp-after-initialize-hook -- harmless for
  ;;   ;; other servers due to pyls key, but would prefer only sending this
  ;;   ;; when pyls gets initialised (:initialize function in
  ;;   ;; lsp-define-stdio-client is invoked too early (before server
  ;;   ;; start)) -- cpbotha
  ;;   (defun lsp-set-cfg ()
  ;;     (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
  ;;       ;; TODO: check lsp--cur-workspace here to decide per server / project
  ;;       (lsp--set-configuration lsp-cfg)))

  ;;   (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))
  )


(use-package session
  :straight t
  :init
  (session-initialize))


(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package yasnippet
  :straight t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil))
  :config
  (yas-global-mode)

  (defun expand-for-web-mode ()
    (when (equal mode-name "Web")
      (make-local-variable 'yas-extra-modes)
      (setq yas--extra-modes
            (let ((web-lang (web-mode-language-at-pos)))
              (cond
               ((equal web-lang "html")       '(html-mode))
               ((equal web-lang "css")        '(css-mode))
               ((equal web-lang "javascript") '(javascript-mode))
               )))))

  (add-hook 'yas-before-expand-snippet-hook 'expand-for-web-mode)
  )

(use-package projectile
  :straight t
  :diminish projectile-mode
  :init
  ;; this must be done before :config
  (define-key global-map (kbd "C-c p") 'projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-globally-ignored-files
        (append '("*.txt" "*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#")
                projectile-globally-ignored-files))
  )

(use-package helm-projectile
  :straight t
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
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

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
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (show-paren-mode 1)
  (electric-pair-mode 1))

(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

(use-package helm-ag
  :straight t)

(use-package helm-rg
  :straight t)

(use-package company-quickhelp
  :straight t
  :config
  (company-quickhelp-mode))

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

(use-package helm-pydoc
  :straight t
  :config
  (with-eval-after-load "python"
    (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)))
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
  :straight t
  :config
  (global-linum-mode 1)

  (defun my-find-file-check-make-large-file-read-only-hook ()
    "If a file is over a given size, turn off nlinum and font-lock-mode."
    (if (> (buffer-size) (* 1024 1024))
        (progn (linum-mode -1)
               (font-lock-mode -1))))

  (add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook))

(use-package smart-mode-line-powerline-theme
  :straight t
  :commands (powerline-default-theme))

(use-package arc-dark-theme
  :straight (:host github :repo "cfraz89/arc-dark-theme"))

;; This doesn't seem to work right in use-package
(add-hook 'after-init-hook (lambda ()
                             (load-theme 'arc-dark t)
                             (sml/setup)
                             (powerline-default-theme)))

(use-package tide
  :straight t
  :config
  (defun setup-tide-mode ()
    "Set up Tide mode."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save-mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

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

  (use-package company-statistics
    :straight t
    :init
    (company-statistics-mode))

  (use-package company-web
    :straight t
    :commands (company-web-bootstrap+ company-web-html))

  (use-package company-try-hard
    :straight t
    :bind
    (("C-<tab>" . company-try-hard)
     :map company-active-map
     ("C-<tab>" . company-try-hard)))
)

(use-package ignoramus
  :straight t
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


(use-package switch-buffer-functions
  :straight t
  :config
  (defun update-pyenv-on-buffer-switch (prev curr)
    "Function that will set appropriate pyenv and restart RPC server if needed
when switching buffers "
    (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
        (progn
          (let* ((old_pyenv (pyenv--active-python-version))
                 (local_pyenv (pyenv--locate-file ".python-version"))
                 (new_pyenv (if local_pyenv
                                (pyenv--read-version-from-file local_pyenv)
                              (pyenv--global-python-version))))
            (if (not (string-equal old_pyenv new_pyenv))
                (progn
                  (pyenv-use new_pyenv)
                  (elpy-rpc-restart))
              )))))
  ;; Update pyenv and restart Elpy RPC if needed when switching buffers
  (add-hook 'switch-buffer-functions 'update-pyenv-on-buffer-switch)
  )

(use-package smart-jump
  :straight t
  :config
  (smart-jump-setup-default-registers))

;;; init.el ends here
