;; add and enable melpa
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.emacs-china.org/gnu/") 
			 ("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   (quote
    (chinese-fonts-setup cl-lib-highlight org-plus-contrib yasnippet htmlize color-theme-sanityinc-solarized exec-path-from-shell find-file-in-project ## evil markdown-mode markdown-mode+ omnisharp tide))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun setup-tide-mode ()
    (interactive)
      (tide-setup)
        (flycheck-mode +1)
          (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
              (tide-hl-identifier-mode +1)
                ;; company is an optional dependency. you have to
                  ;; install it separately via package-install
                    ;; `m-x package-install [ret] company`
                      (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default major-mode 'text-mode)
(setq-default auto-fill-hook 'do-auto-fill)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; active org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell      . t)
   (js         . t)
   (emacs-lisp . t)
   (perl       . t)
   (scala      . t)
   (clojure    . t)
   (python     . t)
   (ruby       . t)
   (dot        . t)
   (latex      . t)
   (java       . t)
   (plantuml   . t)))

;; path to plantuml
(setq org-plantuml-jar-path
      (expand-file-name "/opt/jar/plantuml/plantuml.jar"))

;; Org Publish to Stat Blog to Jekyll config Added 11 Jun 2017
;; http://orgmode.org/worg/org-tutorials/org-jekyll.html and
;; http://www.grantschissler.com/blog/2015/04/10/org-jekyll-github.html
;; Thanks to Ian Barton and grizant
(require 'ox-publish)
(require 'ox-html)
(setq org-publish-project-alist
   '(
 ("org-blog"
     ;; Path to your org files.
     :base-directory "~/WorkStation/blog/orgFiles-stat-blog/"
     :base-extension "org"

     ;; Path to your Jekyll project.
     :publishing-directory "~/WorkStation/blog/jack2gs.github.io/source/_posts"
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4 
     :html-extension "html"
     :body-only t ;; Only export section between <body> </body>
  )

  ("org-static-blog"
     :base-directory "~/WorkStation/blog/orgFiles-stat-blog"
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
     :publishing-directory "~/WorkStation/blog/jack2gs.github.io/source/_posts"
     :recursive t
     :publishing-function org-publish-attachment)

  ("blog" :components ("org-blog" "org-static-blog"))

))

;; enable line number for editor
(global-linum-mode t)

;; enable to export to markdown for org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; for literate programming
;; you need to install htmlize.el first.
;; syntax highlight your code
;; removes the annoying "Do you want to execute" your code when you type: C-c C-c
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; ensure environment variables inside Emacs look the same as in the user's shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; utf-8 encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; resolve incorrect Colours in Terminal


;; set the theme to solarized-dark
(load-theme 'sanityinc-solarized-dark)

;; close the tool-bar, menu-bar and scroll-bar
(tool-bar-mode 0)  
(menu-bar-mode 0)  
(scroll-bar-mode 0)  

;; to automatically load omnisharp-emacs when editing csharp files
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; set omnisharp-server-executable-path
(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")

;; set latex compiler
;;(require 'ox-latex)
;;(setq org-latex-compiler 'xelatex)

;; disable footer when export org to html
(setq org-html-validation-link nil)

;; enable yasnippet
(yas-global-mode t)

;; -----------------------------------------------------------------------------  
;; setting font for mac system  
;; -----------------------------------------------------------------------------  
;; Setting English Font   
(set-face-attribute  
 'default nil :font "Monaco 12")  
;; Chinese Font 配制中文字体  
(dolist (charset '(kana han symbol cjk-misc bopomofo))  
  (set-fontset-font (frame-parameter nil 'font)  
                    charset  
                    (font-spec :family "Microsoft YaHei" :size 14)))  
;; tune rescale so that Chinese character width = 2 * English character width
;; (setq face-font-rescale-alist '(("Monaco" . 1.0) ("Microsoft YaHei" . 1.23)))
