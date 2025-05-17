;;; init.el --- startup script
;;; Commentary:
;;; startup script
;;; Code:

(setq-local enable-full-feature t)


(if enable-full-feature
    (setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                             ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
			                 ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
			                 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  
  (setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa-stable" . "https://melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))

(let ((backup-dir "~/.emacs.d/_backups/"))
  ;; Set the custom backup directory
  (setq backup-directory-alist `(("." . ,backup-dir)))
  ;; Create the backup directory if it doesn't exist
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t)))

(use-package fzf
  :ensure t
  :bind
  ("C-c f" . fzf))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package rainbow-mode
  :ensure t)

(use-package pyvenv
  :ensure t
  :hook
  ((python-mode python-ts-mode) . pyvenv-mode))


(use-package jupyter
  :ensure t
  :defer t)

;; common
(use-package emacs
  :custom
  (use-short-answers t)
  (completion-ignore-case t)
  :config
  (set-face-attribute 'default nil :height 160) ; 160 is equivalent to 12pt font size
  (if (display-graphic-p) (tool-bar-mode -1))
  (when (eq system-type 'darwin)
    (setq ns-use-native-fullscreen nil)
    (setq ns-pop-up-frames nil))
  ;; real auto save
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 30)
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
  ;; make typing delete/overwrites selected text
  (delete-selection-mode 1)
  ;; start the initial frame maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; start every frame maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (prefer-coding-system 'utf-8)
  ;; Set custom file
  (setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load custom-file)
  (setq inhibit-warning-function
        (lambda (type message)
          (or (and (eq type 'deprecation)
                   (string-match-p "events-buffer-scrollback-size" message))
              (and (eq type 'deprecation)
                   (string-match-p "events-buffer-config" message))))))

(use-package dired
  :custom
  (dired-dwim-target t)
  :config
  (when (string-equal system-type "darwin")
    (setq dired-use-ls-dired nil))
  (defun melon/create-file-or-directory (name)
    "Create a file or folder. If NAME ends with /, create a folder. Otherwise, create a file. 
If the parent directories don't exist, create them as well."
    (interactive "Enter name: ")
    (let ((path (expand-file-name name (dired-current-directory))))
      (message "file name: %s" path)
      (if (string-suffix-p "/" name)
          (progn
            (message "create directory: %s" path)
            (dired-create-directory path))  ;; Create directory, create parents if necessary            
        (progn
          (message "create file: %s" path)
          ;; Create parent directories if necessary
          (let ((dir (file-name-directory path))
                (file_name (file-name-nondirectory path)))
            (unless (file-exists-p dir)
              (make-directory dir  t))
            (dired-create-empty-file path)
            (revert-buffer))))))  ;; Create an empty file
  (defun melon/dired-create-files-or-directories (names)
    "Create empty files or directories.
If the name ends with '/', it's a directory otherwise it's a file."
    (interactive (list (split-string (read-string "sEnter names: "))))
    (dolist (name names)
      (melon/create-file-or-directory name))
    (revert-buffer))
  :bind (:map dired-mode-map
              ([remap dired-create-directory] . melon/dired-create-files-or-directories)))

;; Set indentation level for Python mode explicitly
;; Prevent Emacs from guessing the python-indent-offset
(setq python-indent-guess-indent-offset nil)
(setq python-indent-offset 4)

(use-package prog-mode
  :custom
  (typescript-ts-mode-indent-offset 4)
  (treesit-font-lock-level 4)
  (indent-tabs-mode nil)
  (tab-width 4)
  :config
  (which-function-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1))


(use-package eldoc-box
  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :ensure t
      :custom
      (eldoc-box-lighter nil)
      (eldoc-box-only-multi-line t)
      (eldoc-box-clear-with-C-g t)
      :custom-face
      (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
      (eldoc-box-body ((t (:inherit tooltip))))
      ;;:hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
      :config
      ;; Prettify `eldoc-box' frame
      (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
            (alist-get 'right-fringe eldoc-box-frame-parameters) 8))

(use-package eglot
  :init
  (defun my-defer-eglot ()
    "Defer eglot until after file is loaded."
    (unless (bound-and-true-p eglot--managed-mode)
      (run-with-idle-timer 1 nil #'eglot-ensure)))
  (setq eglot-workspace-configuration
        '((:css . (:validate t
                             :lint (:validate t)))
          (:scss . (:validate t
                              :lint (:validate t)))
          (:less . (:validate t
                              :lint (:validate t)))))
  (setq eglot-events-buffer-config '(:size 0))
  :config
  (add-to-list 'eglot-server-programs
			   '((scss-mode :language-id "scss") . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '((less-css-mode :language-id "less") . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((web-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c++-ts-mode)
                 . ("clangd"
                    "-j=8"
                    "--log=error"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--all-scopes-completion"
                    "--function-arg-placeholders"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0")))
  :hook ((cmake-mode
          cmake-ts-mode
          csharp-mode
          csharp-ts-mode
          css-mode
          css-ts-mode
          scss-mode
          scss-ts-mode
          less-css-mode
          c-mode
          c-ts-mode
          c++-mode
          c++-ts-mode
          python-base-mode
          rust-ts-mode
          tsx-ts-mode
          web-mode)
         . eglot-ensure))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package clang-format+
  :ensure t
  :init
  (setq clang-format+-always-enable t)
  :hook
  ((c-ts-mode c++-ts-mode) .  #'clang-format+-mode))

;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

;; special setup for C/C++
;; (use-package irony
;;   :ensure t
;;   :custom
;;   (irony-supported-major-modes '(c++-mode c-mode objc-mode c++-ts-mode c-ts-mode))
;;   :config
;;   (add-hook 'c++-ts-mode-hook 'irony-mode)
;;   (add-hook 'c-ts-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package ggtags
;;   :ensure t
;;   :hook ((c-mode c-ts-mode c++-mode c++-ts-mode) . ggtags-mode))

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-backends (delete 'company-semantic company-backends)) ; optional
;;   ;;(define-key c++-ts-mode-map  [(tab)] 'company-complete)
;;   (add-hook 'c++-ts-mode-hook 'company-mode))

;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-irony)))

;; keep a list of recently opened files
(use-package recentf
  :config
  (recentf-mode t))

;; mark and region
(use-package expand-region
  :ensure t)

(use-package visible-mark
  :defer t
  :custom
  ;; Set the number of marks to highlight
  (setq visible-mark-max 100)
  ;; Set the faces for the marks
  (setq visible-mark-faces '(visible-mark-face1 visible-mark-face2)))

(use-package multiple-cursors
  :ensure t)

;; shell config
(use-package shell-pop
  :ensure t
  :defer t
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

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

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
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect-first nil)
  (corfu-preselect 'directory)
  (corfu-on-exact-match 'show)
  ;; Option 1: Specify explicitly to use Orderless for Eglot
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  :init
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    #'eglot-completion-at-point
                    ;;#'my/emmet-expand-capf
                    #'yasnippet-capf
                    #'cape-dabbrev
                    #'cape-file))))
  :hook (
         (after-init . global-corfu-mode)
         (corfu-mode . corfu-popupinfo-mode)
         (eshell-mode . (lambda ()
                          (setq-local corfu-auto nil)
                          (corfu-mode -1))))
  :config
  ;; when in shell or eshell, when press RET, it will send it to the shell directly, which will save another RET.
  ;; (define-key corfu-map (kbd "RET")
  ;;             `(menu-item "" nil :filter ,(lambda (&optional _) (and (or (derived-mode-p 'eshell-mode) (derived-mode-p 'comint-mode)) #'corfu-send))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  ;;(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(when (and (not window-system) enable-full-feature)
  (use-package corfu-terminal
    :ensure t
    :after corfu
    :config
    (corfu-terminal-mode))
  ;; (use-package kind-icon
  ;;   :ensure t
  ;;   :after corfu
  ;;   :custom
  ;;   (kind-icon-default-face 'corfu-default)
  ;;   ;;(kind-icon-use-icons nil)
  ;;   :config
  ;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  )

(use-package cape
  :ensure t
  :after corfu
  :init
  ;;  (setq cape-dabbrev-min-length 2)
  ;;  (setq cape-dabbrev-check-other-buffers 'some)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  :bind ("C-c SPC" . cape-dabbrev))

(use-package yasnippet-capf
  :ensure t
  :after cape)

(use-package dabbrev
  :custom
  ;; just match alphanumeric characters and underscores
  ;; incase 'app.' matches 'application' by mistake
  ;; (dabbrev-abbrev-char-regexp "\\([^-._[:alnum:]]\\|\\'\\)")
  (dabbrev-case-fold-search nil) ;; case sensitive
  (dabbrev-upcase-means-case-search t)
  (dabbrev-check-all-buffers nil)
  (dabbrev-check-other-buffers nil)
  (dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package yasnippet
  :ensure t
  ;; eglot looks like will enable it by default for lsp
  ;; https://github.com/joaotavora/eglot/blob/db91d58374627a195b731a61bead9b4f84a7e4bc/eglot.el#L1797
  :init
  (setq yas-trigger-key 'nil)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


(use-package web-mode
  :ensure t
  :mode ("\\.js[x]?\\'" "\\.ts[x]?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")
                                       ("tsx" . "\\.ts[x]?\\'")))
  ;; Web-mode tag auto-closing and other features
  (setq web-mode-enable-auto-closing t)   ;; Auto-close tags like <div></div>
  (setq web-mode-enable-auto-pairing t)   ;; Auto-pair quotes, etc.
  (setq web-mode-enable-auto-quoting t)   ;; Auto-insert quotes after =
  (setq web-mode-enable-auto-indentation t) ;; Auto-indent when typing
  )


(use-package emmet-mode
  :ensure t
  :hook ((web-mode css-mode scss-mode) . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)  ;; Move between quotes after expansion
  )

(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode web-mode css-mode css-ts-mode scss-mode scss-ts-mode html-mode html-ts-mode) . emmet-mode)
  :config
  (defun my/emmet-expand-capf ()
    (let ((bounds (bounds-of-thing-at-point 'word))
          (tap (thing-at-point 'word)))
      (list (car bounds) (cdr bounds)
            ;; Just return the symbol at point to so completion will be possible
            ;; TODO Determine if there is a less hacky option
            ;; (let ((cands (lambda (string pred action)
            ;;                (if (eq action t)
            ;;                    cands
            ;;                  (complete-with-action action (list (thing-at-point 'line)) string pred))))))

            (lambda (string pred action) (complete-with-action action (list (thing-at-point 'word)) string pred))

            ;; Annotate with what emmet expands to
            ;; TODO find a way for this to show since right now
            ;; corfu doesn't display this on a single completion
            :annotation-function (lambda (str)  " Emmet Abbrev")
            ;; Don't try to complete with emmet if there is no possible
            ;; expansion
            ;; :predicate (not (string= (emmet-transform tap)
            ;;                          tap))
            ;; Expand Emmet Template On Match
            :exit-function (lambda (str status)
                             (when (eql status 'finished)
                               (emmet-expand-line nil)))
            ;; Allow for other completions to follow
            :exlcusive 'no)))
  )

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; hl-line-mode
     `(hl-line-face ((t (:background ,zenburn-bg+1 ))))
     `(hl-line ((t (:background ,zenburn-bg+1 )))))))

(use-package hl-line
  :hook
  ((prog-mode text-mode dired-mode) . hl-line-mode))

;; (use-package all-the-icons
;;   :ensure t
;;   :if (display-graphic-p))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  ;; for further config look at:
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
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
  :defer t
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

(if (treesit-available-p)
    (progn
      (defun my-c-c++-header-mode ()
        "Set either `c-mode` or `c++-mode` depending on the content of the header."
        (interactive)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^\s*#\s*if\\(n\\|\\)def\\|class\\|namespace\\|template\\|public:" nil t)
              (c++-mode)
            (c-mode))))
      (add-to-list 'auto-mode-alist '("\\.h\\'" . my-c-c++-header-mode))
      (dolist (mapping '(;; ("\\.ts\\'" . tsx-ts-mode)
			             ;; ("\\.tsx\\'" . tsx-ts-mode)
			             ;; ("\\.js\\'" . tsx-ts-mode)
			             ;; ("\\.jsx\\'" . tsx-ts-mode)
                         ;; ("\\.rs\\'" . rust-ts-mode)
                         ;; ("\\.scss\\'" . scss-mode)
                         ;; ("\\.less\\'" . less-css-mode)
			             ("CMakeLists\\.txt\\'" . cmake-ts-mode)
			             ("\\.cmake\\'" . cmake-ts-mode)))
        (add-to-list 'auto-mode-alist mapping))
      (dolist (mapping '((c-mode . c-ts-mode)
                         (c++-mode . c++-ts-mode)
                         (csharp-mode . csharp-ts-mode)
                         (css-mode . css-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))))

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

(use-package goto-chg
  :ensure t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;; Enable Flyspell in programming modes for comments and strings
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Optionally, you can set the dictionary (if not already set)
(setq ispell-dictionary "en_US")

(use-package general
  :ensure t
  :config
  ;; global key bindings without prefix
  ;; some key bindings may be related to minor modes
  (general-define-key
   "<f6>" 'treemacs ; treemacs toggle
   "<f12>" 'xref-find-definitions
   "S-<f12>" 'xref-find-references
   "C-M-=" 'er/expand-region
   "C-`" 'shell-pop
   "M-`" 'shell-pop
   "M-0" 'treemacs-select-window
   "C-." 'embark-act
   "C-;" 'embark-dwim
   "C-h B" 'embark-bindings
   "C-x r e" 'mc/edit-lines
   "C->" 'mc/mark-next-like-this
   "C-<" 'mc/mark-previous-like-this
   "C-*" 'mc/mark-all-like-this
   ;; mark ring
   "M-g m" 'consult-mark
   "M-g M" 'consult-global-mark
   "M-<left>" 'goto-last-change
   "M-<right>" 'goto-last-change-reverse
   "C--" 'goto-last-change
   "C-_" 'goto-last-change-reverse
   [remap goto-char] 'avy-goto-char-timer
   [remap isearch-forward] 'consult-line
   ;;[remap find-file] 'consult-find
   [remap switch-to-buffer] 'consult-buffer
   [remap project-switch-to-buffer] 'consult-project-buffer
   [remap imenu] 'consult-imenu
   [remap bookmark-jump] 'consult-bookmark
   [remap go-to-line] 'consult-goto-line
   [remap isearch-forward-regexp] 'consult-ripgrep)

  (general-define-key
   :keymaps 'prog-mode-map
   "<f1>" 'eldoc-box-help-at-point
   "<f5>" 'dape
   "<f9>" 'dape-breakpoint-toggle
   "<f10>" 'dape-next
   "<f11>" 'dape-step-in
   "S-<f11>" 'dape-step-out
   ;; lsp xref keybindings
   "C-<f12>" 'eglot-find-implementation
   "C-M-<f12>" 'eglot-find-declaration))



;; Define our filename finder
(defun my-compilation-eslint-filename-for-msg ()
  "Return the filename for an ESLint error by looking backward to the first non-whitespace line."
  (save-excursion
    (beginning-of-line)
    ;; Keep going up until we find a non-whitespace line or beginning of buffer
    (while (and (not (bobp))  ; not beginning of buffer
                (looking-at-p "^[[:space:]]"))  ; current line starts with whitespace
      (forward-line -1))
    ;; Now return the filename (whole line)
    (buffer-substring (line-beginning-position) (line-end-position))))

(add-to-list 'compilation-error-regexp-alist 'eslint-multiline)
(add-to-list 
  'compilation-error-regexp-alist-alist
  '(eslint-multiline
    ;; Matches the error/warning lines
    "^[[:space:]]+\\([0-9]+\\):\\([0-9]+\\)[[:space:]]+\\(warning\\|error\\)[[:space:]]+\\(.*\\)$"
    my-compilation-eslint-filename-for-msg  ;; Special function to get filename
    1    ;; line
    2    ;; column
    (3 . 4)  ;; type and message
    ))


;; (defun my-compilation-eslint-filename ()
;;   "Return the filename for an ESLint error if the current line is a filename
;; and the next line contains an error/warning."
;;   (save-excursion
;;     (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
;;       (message line)
;;       (forward-line 1)
;;       (when (looking-at-p "^[[:space:]]+\\([0-9]+\\):\\([0-9]+\\)[[:space:]]+\\(warning\\|error\\)")
;;         line))))

;; ;; Add pattern to match ESLint filename lines
;; (add-to-list 'compilation-error-regexp-alist 'eslint-filename-line)
;; (add-to-list 
;;   'compilation-error-regexp-alist-alist
;;   '(eslint-filename-line
;;     "^/\\(?:[^/ \n]+/\\)+[^/ \n]+$"
;;     my-compilation-eslint-filename))

(provide 'init)
;;; init.el ends here
