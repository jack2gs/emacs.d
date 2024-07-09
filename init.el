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


(if (display-graphic-p) (tool-bar-mode -1))

;; common
(setq use-short-answers t)
;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)
;;(setq-default indent-tabs-mode nil)
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  (setq ns-pop-up-frames nil))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq typescript-ts-mode-indent-offset 4)
            (setq treesit-font-lock-level 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
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
  :ensure t
  :config
  (setq expand-region-contract-fast-key "-")    ;; Set the key for contracting fast
  (setq expand-region-reset-fast-key "C-M-="))     ;; Set the key for resetting the selection

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
  :bind ("C-`" . shell-pop)
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (cond
   ((eq system-type 'darwin)
    (shell-pop-term-shell "/bin/zsh"))
   (t
    (shell-pop-term-shell "/bin/bash")))
  (shell-pop-universal-key "C-`")
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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )

;; my key bindings
;; (use-package general
;;   :ensure t
;;   :config
;;   (general-define-key
;;    "C-=" 'er/expand-region
;;    "M-<left>" 'backward-global-mark
;;    "M-<right>" 'forward-global-mark
;;    )
  ;; (general-create-definer my-global-leader-def
  ;; 	:prefix "C-x")
  ;; (general-create-definer my-leader-def
  ;; 	:prefix "C-c")

  ;; (my-leader-def
  ;; 	:keymaps 'lsp-mode-map
  ;; 	[remap xref-find-definitions] 'lsp-find-definitions
  ;; 	[remap xref-find-references] 'lsp-find-references
  ;; 	"xd" 'lsp-find-declaration
  ;; 	"xx" 'lsp-find-definition
  ;; 	"xi" 'lsp-find-implementation
  ;; 	"xr" 'lsp-find-references
  ;; 	"xl" 'lsp-find-locations
  ;; 	"xw" 'lsp-find-workspace
  ;; 	"xs" 'lsp-find-session-folder
  ;; 	"gl" 'lsp-goto-location
  ;; 	"gg" 'lsp-goto-type-definition
  ;; 	"gi" 'lsp-goto-implementation)
  ;; (my-leader-def
  ;; 	:keymaps 'lsp-ui-mode-map
  ;;   "Xd" 'lsp-ui-peek-find-definitions
  ;; 	"Xr" 'lsp-ui-peek-find-references
  ;; 	"Xi" 'lsp-ui-peek-find-implementation
  ;; 	"Xs" 'lsp-ui-peek-find-workspace-symbol
  ;; 	"Ii" 'lsp-ui-imenu)
  ;; (my-leader-def
  ;; 	"tr" 'lsp-treemacs-references
  ;; 	"ti" 'lsp-treemacs-implementations
  ;; 	"ts" 'lsp-treemacs-symbols
  ;; 	"te" 'lsp-treemacs-errors-list
  ;; 	"tc" 'lsp-treemacs-call-hierarchy
  ;; 	"th" 'lsp-treemacs-type-hierarchy)
  ;; (general-define-key
  ;;  :keymaps 'override
  ;;  :prefix-map 'my-leader-map
  ;;  :prefix "C-t"
  ;;  "f" '(:prefix-command my-file-command :which-key "files")
  ;;  )

  ;; (general-create-definer my-file-def
  ;; 	:prefix-map 'my-file-map
  ;; 	:prefix-command 'my-file-command)

  ;; (my-file-def
  ;; 	"f" 'consult-find
  ;; 	"l" 'consult-line)
  ;; (general-define-key
  ;;  ;;:keymaps 'override
  ;;  :prefix "C-c"
  ;;  :prefix-map 'my-main-map)

  ;; (general-create-definer my-main-def
  ;; 	:keymaps 'my-main-map)

  ;; (my-main-def
  ;; 	"f" '(:prefix-command my-file-command :wk "files")
  ;; 	"v" '(:prefix-command my-view-command :wk "views"))

  ;; (general-create-definer my-file-def
  ;; 	:keymaps 'my-main-map
  ;; 	:prefix "f"
  ;; 	:prefix-command 'my-file-command)

  ;; (my-file-def
  ;;  "f" 'consult-file
  ;;  "d" 'dired)


  ;; (general-create-definer my-view-def
  ;; 	:keymaps 'my-main-map
  ;; 	:prefix "v"
  ;; 	:prefix-command 'my-view-command)


  ;;  (my-view-def
  ;; 	:keymaps 'lsp-ui-mode-map
  ;;    "xd" 'lsp-ui-peek-find-definitions
  ;; 	 "xr" 'lsp-ui-peek-find-references
  ;; 	 "xi" 'lsp-ui-peek-find-implementation
  ;;  	 "xs" 'lsp-ui-peek-find-workspace-symbol
  ;;  	 "i" 'lsp-ui-imenu)
  
  ;; ;; (general-create-definer my-leader-def
  ;; ;; 			  :keymaps 'override
  ;; ;; 			  :prefix-map my-leader-map
  ;; ;;   :prefix "C-t")

  ;; ;; ;; Define the nested file-related keymap under `C-t f`
  ;; ;; (my-leader-def
  ;; ;;   "f" '(:prefix-command my-file-command :which-key "files")
  ;; ;; 	"T" 'consult-line)

  ;; ;; ;; Define the file-related keymap and its commands
  ;; ;; (general-define-key
  ;; ;;  :prefix ""
  ;; ;;  :prefix-command 'my-file-command
  ;; ;;  "f" 'consult-find
  ;; ;;  "s" 'consult-line))
  ;; )

;; Set custom file
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(load custom-file)

(provide 'init)
;;; init.el ends here
