;;; init.el --- startup script
;;; Commentary:
;;; startup script
;;; Code:
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)

(use-package dired
  :ensure nil
  :config
  (when (string-equal system-type "darwin")
    (setq dired-use-ls-dired nil)))

;; common
(setq use-short-answers t)
;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)
(setq-default tab-width 4)
;;(setq-default indent-tabs-mode nil)

(when (>= emacs-major-version 26)
  ;; real auto save
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 30))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; dired
(setq dired-dwim-target t)

;; mark and region
(use-package expand-region
  :ensure t
  :config
  (setq expand-region-contract-fast-key "C--")    ;; Set the key for contracting fast
  (setq expand-region-reset-fast-key "C-M-=")     ;; Set the key for resetting the selection
  :bind ("C-=" . er/expand-region))

(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe () 
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun backward-global-mark () 
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

;; Terminal specific key bindings
(unless (display-graphic-p)
  (global-set-key (kbd "ESC <left>") 'backward-global-mark)
  (global-set-key (kbd "ESC <right>") 'forward-global-mark))

;; GUI specific key bindings
(when (display-graphic-p)
  (global-set-key (kbd "M-<left>") 'backward-global-mark)
  (global-set-key (kbd "M-<right>") 'forward-global-mark))

(use-package visible-mark
  :ensure t
  :config
  ;; Enable visible-mark-mode globally
  (global-visible-mark-mode 1)
  ;; Set the number of marks to highlight
  (setq visible-mark-max 2)
  ;; Set the faces for the marks
  (setq visible-mark-faces '(visible-mark-face1 visible-mark-face2)))


(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-*" . mc/mark-all-like-this)))


;; shell config
(use-package shell-pop
  :ensure t
  :bind ("C-t" . shell-pop)
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (cond
   ((eq system-type 'darwin)
    (shell-pop-term-shell "/bin/zsh"))
   (t
    (shell-pop-term-shell "/bin/bash")))
  (shell-pop-universal-key "C-t")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))

(prefer-coding-system 'utf-8)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative) ; Use 'relative for absolute line numbers

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Ensure project is loaded
(require 'project)

;; Define a project-roots method for projectile projects
(cl-defmethod project-roots ((project (head projectile)))
  (list (cdr project)))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ("C-c h" . consult-imenu)
         ("C-c k" . consult-ripgrep)
         ("C-c l" . consult-locate)
         ("C-c m" . consult-mark)
         ("C-c b" . consult-bookmark)
         ("C-x r b" . consult-register)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-s l" . consult-line)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s m" . consult-man)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq consult-narrow-key "<")
  ;; Use consult-project-root for project root detection
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(use-package corfu-terminal
  :unless window-system
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode)
  )

;; Optionally enable icons in Corfu
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  ;;(kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :bind (("C-c p f" . consult-projectile-find-file)
         ("C-c p g" . consult-projectile-ripgrep)
         ("C-c p b" . consult-projectile-switch-to-buffer)))

;; Enable consult-imenu with lsp-mode
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "M-i") 'consult-imenu))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package avy
  :ensure t
  :init
  (setq avy-case-fold-search nil)       ;; case sensitive makes selection easier
  (bind-key "C-;"    'avy-goto-char-2)  ;; I use this most frequently
  (bind-key "C-'"    'avy-goto-line)    ;; Consistent with ivy-avy
  (bind-key "M-g c"  'avy-goto-char)
  (bind-key "M-g e"  'avy-goto-word-0)  ;; lots of candidates
  (bind-key "M-g g"  'avy-goto-line)    ;; digits behave like goto-line
  (bind-key "M-g w"  'avy-goto-word-1)  ;; first character of the word
  (bind-key "M-g ("  'avy-goto-open-paren)
  (bind-key "M-g )"  'avy-goto-close-paren)
  (bind-key "M-g P"  'avy-pop-mark))

;; (use-package ivy-xref
;;   :ensure t
;;   :init
;;   ;; xref initialization is different in Emacs 27 - there are two different
;;   ;; variables which can be set rather than just one
;;   (when (>= emacs-major-version 27)
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs))
;;   ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;;   ;; commands other than xref-find-definitions (e.g. project-find-regexp)
;;   ;; as well
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure flex
  :custom
   (lsp-completion-provider :none)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error" "--compile-commands-dir=./build"))
  ;; Ensure imenu is refreshed when lsp updates
  ;;(add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  ;;(add-hook 'lsp-after-diagnostics-hook #'imenu-list-update)
  ;;(add-hook 'lsp-on-idle-hook #'imenu-list-update)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
		 (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
		 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-ignore-duplicate t)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :config
  ;; Set up DAP for LLDB
  (require 'dap-lldb)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  ;; Configure the debugger executable path
  (cond
   ((eq system-type 'darwin)
    (setq dap-lldb-debug-program '("/opt/homebrew/bin/lldb-vscode")))
   (t
    (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))))
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))

  ;; Additional DAP configurations
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

(use-package wgrep-ag
  :ensure t)

;;; which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

;;; window management
(use-package winum
  :ensure t
  :config
  (winum-mode))

;;; treemacs and projectile
(use-package projectile
  :ensure t
  :demand t
  :init
  (setq projectile-completion-system 'default)
  :bind (:map projectile-mode-map
	      ("C-c p" . 'projectile-command-map))
  :config
  (projectile-mode))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; Set custom file
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(load custom-file)

(provide 'init)
;;; init.el ends here
