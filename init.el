;; ELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; load path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp")

;; system-type
(setq os-type-mac (eq system-type 'darwin)
      os-type-linux (eq system-type 'gnu/linux))

;; start up
(when os-type-linux
  (setq initial-frame-alist
        (append (list
                 '(width . 168)
                 '(height . 64)
                 '(top . 72)
                 '(left . 512)
                 )
                initial-frame-alist)))
(when os-type-mac  ;; MacBookPro
  (setq initial-frame-alist
        (append (list
                 '(width . 172)
                 '(height . 45)
                 '(top . 36)
                 '(left . 18)
                 )
                initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode nil)
(setq make-backup-files nil)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(setq-default indent-tabs-mode nil)
(setq Man-switches "-Lja")
(server-start)

(when os-type-mac
  (setq ns-command-modifier (quote meta))  ;; command -> Alt
  ;(setq ns-alternate-modifier (quote super))
  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setenv "PATH"
          (concat '"/usr/local/bin:" (getenv "PATH")))
  (setenv "LANG" "ja_JP.UTF-8"))

;; Buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; buffer-menu
(global-set-key "\C-x\C-b" 'buffer-menu)

; line number
(global-linum-mode t)
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
(when os-type-linux
  (set-default-font "ricty-12")
  (set-face-font 'variable-pitch "ricty-12"))
(when os-type-mac
  (set-default-font "ricty-14.5")
  (set-face-font 'variable-pitch "ricty-14.5"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("ricty". "unicode-bmp")
)

;; character code
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;; limit line length
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
; (add-hook 'prog-mode-hook 'whitespace-mode)
(global-whitespace-mode +1)

; mozc
(when os-type-linux
  (require 'mozc)
  (setq default-input-method "japanese-mozc"))

; color
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(color-theme-zenburn)

;; shell-mode
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; wdired
(require 'wdired)
(define-key dired-mode-map "r"
  'wdired-change-to-wdired-mode)

;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-for-files)

;; Magit
(require 'magit)

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(setq emmet-move-cursor-between-quotes t)

;; rbenv
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '((".rb$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Rakefile" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '((".rake$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Gemfile" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Vagrantfile" . ruby-mode)) auto-mode-alist))
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

;; javascript
(setq js3-mirror-mode t)
(autoload 'js3-mode "js3-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(add-hook 'js3-mode-hook
          #'(lambda ()
             (require 'js)
             (setq js-indent-level 2
                   js-expr-indent-offset 2
                   indent-tabs-mode nil)
             (set (make-local-variable 'indent-line-function) 'js-indent-line))) 

;; CSS
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
            (define-key css-mode-map "{" 'brace-ret-brace)
            ))

;; Slim
(require 'slim-mode)
(add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode))

;; Sass
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; C
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "linux")
             (setq indent-tabs-mode nil)    
             (setq c-basic-offset 4)
             ))

;; Scheme
(setq quack-default-program "gosh")
(require 'quack)

;; CoffeeScript
(autoload 'coffee-mode "coffee-mode" "Major mode for editing CoffeeScript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-local-variable 'coffee-tab-width) 2))
  )
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; for Mac
(cd "~/")
