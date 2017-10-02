;; add and enable melpa
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.emacs-china.org/gnu/") 
			 ("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fill-column 120)
 '(fill-nobreak-predicate (quote (fill-single-word-nobreak-p)))
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   (quote
    (magit py-autopep8 elpy ein better-defaults visual-fill-column pandoc-mode chinese-fonts-setup cl-lib-highlight org-plus-contrib yasnippet htmlize color-theme-sanityinc-solarized exec-path-from-shell find-file-in-project ## evil markdown-mode markdown-mode+ omnisharp tide))))
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

;; disable footer when export org to html
(setq org-html-validation-link nil)

;; enable yasnippet
(yas-global-mode t)

;; -----------------------------------------------------------------------------  
;; setting font for mac system  
;; -----------------------------------------------------------------------------  
;; Setting English Font
(defun s-font()
  (interactive)
  ;; font config for org table showing.
  (set-face-attribute  
   'default nil :font "Monaco 12")  
;; Chinese Font 配制中文字体  
  (dolist (charset '(kana han symbol cjk-misc bopomofo))  
    (set-fontset-font (frame-parameter nil 'font)  
                    charset  
                    (font-spec :family "Microsoft YaHei" :size 14))))  
;; tune rescale so that Chinese character width = 2 * English character width
;;(setq face-font-rescale-alist '(("Monaco" . 1.0) ("Microsoft YaHei" . 1.23)))
(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (s-font))))
(if window-system
    (s-font))

;; set latex
(require 'ox-latex)
(setq org-latex-compiler "xelatex")

(add-to-list 'org-latex-classes
             '("cn-article"
               "\\documentclass[10pt,a4paper]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\usepackage{fontspec,xunicode,xltxtra}
\\setmainfont[BoldFont=Adobe Heiti Std]{Adobe Song Std}  
\\setsansfont[BoldFont=Adobe Heiti Std]{AR PL UKai CN}  
\\setmonofont{Bitstream Vera Sans Mono}  
\\newcommand\\fontnamemono{AR PL UKai CN}%等宽字体
\\newfontinstance\\MONO{\\fontnamemono}
\\newcommand{\\mono}[1]{{\\MONO #1}}
\\setCJKmainfont[Scale=0.9]{Adobe Heiti Std}%中文字体
\\setCJKmonofont[Scale=0.9]{Adobe Heiti Std}
\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}
\\definecolor{foreground}{RGB}{220,220,204}%浅灰
\\definecolor{background}{RGB}{62,62,62}%浅黑
\\definecolor{preprocess}{RGB}{250,187,249}%浅紫
\\definecolor{var}{RGB}{239,224,174}%浅肉色
\\definecolor{string}{RGB}{154,150,230}%浅紫色
\\definecolor{type}{RGB}{225,225,116}%浅黄
\\definecolor{function}{RGB}{140,206,211}%浅天蓝
\\definecolor{keyword}{RGB}{239,224,174}%浅肉色
\\definecolor{comment}{RGB}{180,98,4}%深褐色
\\definecolor{doc}{RGB}{175,215,175}%浅铅绿
\\definecolor{comdil}{RGB}{111,128,111}%深灰
\\definecolor{constant}{RGB}{220,162,170}%粉红
\\definecolor{buildin}{RGB}{127,159,127}%深铅绿
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; use minted to hightlight the source code
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-minted-langs '(csharp "csharp"))
(setq org-latex-minted-options
      '(
	("linenos=true")
;;	("mathescape=true")
;;        ("numbersep=5pt")
;;        ("gobble=2")
        ("frame=lines")
;;        ("framesep=2mm")
	))
(setq org-latex-pdf-process
      '("xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; set up visual-fill-column-mode
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(global-visual-line-mode)

;; disable terminal theme
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; python configration
(defvar myPackages
  '(ein
    elpy
    flycheck
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)
(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq python-shell-completion-native-enable nil)
;; For elpy
(setq elpy-rpc-python-command "/usr/local/bin/python3")
;; For interactive shell
(setq python-shell-interpreter "/usr/local/bin/ipython3")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
;; python configration end
