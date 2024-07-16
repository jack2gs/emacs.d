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

(use-package general
  :ensure t
  )

(use-package dired
  :ensure nil
  :config
  (when (string-equal system-type "darwin")
    (setq dired-use-ls-dired nil)))


(if (display-graphic-p) (tool-bar-mode -1))

;; common
(setq use-short-answers t)
;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  (setq ns-pop-up-frames nil))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq typescript-ts-mode-indent-offset 4)
            (setq treesit-font-lock-level 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (which-function-mode 1)
            (show-paren-mode 1)
            (electric-pair-mode 1)
            (eglot-ensure)))

(when (>= emacs-major-version 26)
  ;; real auto save
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 30))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; dired
(setq dired-dwim-target t)

(set-face-attribute 'default nil :height 160) ; 160 is equivalent to 12pt font size

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; mark and region
(use-package expand-region
  :ensure t)

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

(use-package visible-mark
  :ensure t
  :config
  ;; Enable visible-mark-mode globally
  (global-visible-mark-mode 1)
  ;; Set the number of marks to highlight
  (setq visible-mark-max 100)
  ;; Set the faces for the marks
  (setq visible-mark-faces '(visible-mark-face1 visible-mark-face2)))

(use-package multiple-cursors
  :ensure t
  ;; :bind (("C-S-c C-S-c" . mc/edit-lines)
  ;;        ("C->" . mc/mark-next-like-this)
  ;;        ("C-<" . mc/mark-previous-like-this)
  ;;        ("C-*" . mc/mark-all-like-this))
  )

;; shell config
(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (cond
   ((eq system-type 'darwin)
    (shell-pop-term-shell "/bin/zsh"))
   (t
    (shell-pop-term-shell "/bin/bash")))
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))

(prefer-coding-system 'utf-8)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; (setq consult-narrow-key "<")
  ;; Use consult-project-root for project root detection
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package embark
  :ensure t
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

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; for further config look at:
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title "Welcome back!"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-navigator t
        dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5))
        dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline)
        dashboard-icon-type 'all-the-icons))  ; use `all-the-icons' package

(use-package avy
  :ensure t
  :init
  (setq avy-case-fold-search nil))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30
        treemacs-position 'left) ; Opens on the left side
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-hide-gitignored-files-mode nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(defun my-c-c++-header-mode ()
  "Set either `c-mode` or `c++-mode` depending on the content of the header."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\s*#\s*if\\(n\\|\\)def\\|class\\|namespace\\|template\\|public:" nil t)
        (c++-mode)
      (c-mode))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . my-c-c++-header-mode))
(dolist (mapping '(("\\.ts\\'" . tsx-ts-mode)
				   ("\\.tsx\\'" . tsx-ts-mode)
				   ("\\.js\\'" . tsx-ts-mode)
				   ("\\.jsx\\'" . tsx-ts-mode)
				   ("CMakeLists\\.txt\\'" . cmake-ts-mode)
				   ("\\.cmake\\'" . cmake-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(dolist (mapping '((c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   (csharp-mode . csharp-ts-mode)
                   (css-mode . css-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

;;; which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; window management
(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package dape
  :ensure t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")
  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load))
  :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  (setq dape-info-hide-mode-line nil)
  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  )

(use-package general
  :config
  ;; global key bindings without prefix
  ;; some key bindings may be related to minor modes
  (general-define-key
   "<f5>" 'dape
   "<f6>" 'treemacs ; treemacs toggle
   "<f12>" 'xref-find-definitions
   "S-<f12>" 'xref-find-references
   "C-M-=" 'er/expand-region
   "M-<left>" 'backward-global-mark
   "M-<right>" 'forward-global-mark
   "C-`" 'shell-pop
   "M-0" 'treemacs-select-window
   "C-." 'embark-act
   "C-;" 'embark-dwim
   "C-h B" 'embark-bindings
   [remap goto-char] 'avy-goto-char-timer
   [remap isearch-forward] 'consult-line
   [remap find-file] 'consult-find
   [remap switch-to-buffer] 'consult-buffer
   [remap project-switch-to-buffer] 'consult-project-buffer
   [remap imenu] 'consult-imenu
   [remap bookmark-jump] 'consult-bookmark
   [remap go-to-line] 'consult-goto-line
   [remap isearch-forward-regexp] 'consult-ripgrep
   )
  (general-define-key
   :keymaps 'prog-mode-map
   "<f9>" 'dape-breakpoint-toggle
   "<f10>" 'dape-next
   "<f11>" 'dape-step-in
   "S-<f11>" 'dape-step-out
   )
  (general-define-key
   :keymaps 'prog-mode-map
   "C-<f12>" 'eglot-find-implementation
   "C-M-<f12>" 'eglot-find-declaration)
  ;; global key bindings with prefix
  ;; some key bindings maybe related to minor modes
  )

;; Set custom file
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

(provide 'init)
;;; init.el ends here
