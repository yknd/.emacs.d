;; start up
(setq initial-frame-alist
  (append (list
           '(width . 160)
           '(height . 48)
           '(top . 120)
           '(left . 120)
          )
          initial-frame-alist))
(setq default-frame-alist initial-frame-alist)
(setq inhibit-startup-message t)
(tool-bar-mode nil)
(menu-bar-mode nil)
(scroll-bar-mode nil)
(setq make-backup-files nil)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(setq-default indent-tabs-mode nil)
(setq Man-switches "-Lja")

;; Buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; line number
(global-linum-mode)
(global-set-key [f9] 'linum-mode)

; Insert current time
(defun my-gen-dtime (form) (insert (format-time-string form)))
(defun my-get-dtime () (interactive) (my-gen-dtime "%Y/%m/%d %H:%M:%S"))
(global-set-key [f6] 'my-get-dtime)

;; swap screen
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; font
(set-default-font "ricty-12")
(set-face-font 'variable-pitch "ricty-12")
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("ricty". "unicode-bmp")
)

;; character code
(set-language-environment "japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

; mozc
(require 'mozc)
(setq default-input-method "japanese-mozc")

;; load path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp")

; color
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(color-theme-zenburn)

;; AutoInstall (http://www.emacswiki.org/emacs/download/auto-install.el)
;; (usage) M-x install-elisp RET -> [URL] -> C-c C-c
(when (require 'auto-install nil t)
 (setq auto-install-directory "~/.emacs.d/elisp/")
 (auto-install-update-emacswiki-package-name t)
 (auto-install-compatibility-setup))

;; multi-term (http://www.emacswiki.org/emacs/download//multi-term.el)
(when (require 'multi-term nil t)
 (setq multi-term-program "/bin/bash")
 (setq term-default-bg-color "#3f3f3f")
 (setq term-default-fg-color "#dcdccc")
 (setq ansi-term-color-vector
    [unspecified
     "#000000"           ; black
     "#ff3c3c"           ; red
     "#84dd27"           ; green
     "#eab93d"           ; yellow
     "#94bff3"           ; blue
     "#f47006"           ; magenta
     "#89b6e2"           ; cyan
     "#ffffff"]          ; white
 )
)
(add-hook 'term-mode-hook
         '(lambda ()
                (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
                (define-key term-raw-map (kbd "C-y") 'term-paste)))
(global-set-key (kbd "C-c t") 'multi-term)

;; anything.el
;; M-x auto-install-batch anything
(require 'anything-startup)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-bookmarks
            anything-c-source-recentf
            anything-c-source-file-name-history
            anything-c-source-locate))
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(global-set-key (kbd "C-;") 'anything)

;; wdired
(require 'wdired)
(define-key dired-mode-map "r"
  'wdired-change-to-wdired-mode)

;; zen-coding
(require 'zencoding-mode)
(add-hook 'xml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)

;; rvm
(require 'rvm)
(rvm-use-default)

;; ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '((".rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))
;; rubydb
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer" t)
;;ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (ruby-electric-mode t)))

;; flymakeRuby
(require 'flymake)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
 (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
   (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook 'ruby-mode-hook
         '(lambda ()
     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))

;; encoding comment
(defun ruby-mode-set-encoding () ())

;; xmpfilter
(require 'rcodetools)

;; javascript (js2-mode & espresso-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          #'(lambda ()
              (require 'js)
              (setq js-indent-level 2
                    js-expr-indent-offset 2
                    indent-tabs-mode nil)
              (set (make-local-variable 'indent-line-function) 'js-indent-line)))

;; JSON
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;; CSS
(defun semicolon-ret ()
  (interactive)
  (insert ";")
  (newline-and-indent)
  )
(defun brace-ret-brace ()
  (interactive)
  (insert "{") (newline-and-indent)
  (newline-and-indent)
  (insert "}") (indent-for-tab-command)
  (newline-and-indent) (newline-and-indent)
  (previous-line) (previous-line) (previous-line)
  (indent-for-tab-command)
  )
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (define-key css-mode-map ";" 'semicolon-ret)
            (define-key css-mode-map "{" 'brace-ret-brace)
            ))

;; Haml
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; Sass
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; CoffeeScript
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
  (setq coffee-command "~/node_modules/.bin/coffee"))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; git
(require 'magit)

;; C
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "linux")
             (setq indent-tabs-mode nil)    
             (setq c-basic-offset 4)
             ))