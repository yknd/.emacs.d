(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; system-type
(setq os-type-mac (eq system-type 'darwin))

;; start up
(setq default-frame-alist initial-frame-alist)
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq make-backup-files nil)
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(setq-default indent-tabs-mode nil)
(setq visible-bell t)
(server-start)
(when os-type-mac
  (setq initial-frame-alist
        (append (list
                 '(width . 172)
                 '(height . 45)
                 '(top . 36)
                 '(left . 18)
                 )
                initial-frame-alist))
  (setq ns-command-modifier (quote meta))  ;; command -> Alt
  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setenv "PATH" (concat '"/usr/local/bin:" (getenv "PATH")))
  (setenv "LANG" "ja_JP.UTF-8")
  (set-default-font "Cica-18")
  (set-face-font 'variable-pitch "Cica-18"))

;; buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(global-set-key "\C-x\C-b" 'buffer-menu)

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
(global-whitespace-mode +1)

; theme
(load-theme 'wombat t)

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
(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (ruby-electric-mode t)))
(require 'flymake)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
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
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))
(defun ruby-mode-set-encoding () ())

;; elixir
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

;; javascript
(setq js2-mirror-mode t)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          #'(lambda ()
             (require 'js)
             (setq js-indent-level 2
                   js-expr-indent-offset 2
                   indent-tabs-mode nil)
             (set (make-local-variable 'indent-line-function) 'js-indent-line)))

;; css
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
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-auto-pairing t)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; C
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "linux")
             (setq indent-tabs-mode nil)    
             (setq c-basic-offset 4)
             ))

;; WSL
(defun wsl-copy-region-to-clipboard (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))
(defun wsl-clipboard-to-string ()
  "Return Windows clipboard as string."
  (let ((coding-system-for-read 'dos))
    (substring; remove added trailing \n
     (shell-command-to-string
      "powershell.exe -Command Get-Clipboard") 0 -1)))
(defun wsl-paste-from-clipboard (arg)
  "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
  (interactive "P")
  (let ((clip (wsl-clipboard-to-string)))
    (insert clip)
        (if arg (kill-new clip))))
(global-set-key (kbd "C-c C-w") 'wsl-copy-region-to-clipboard)
(global-set-key (kbd "C-c C-y") 'wsl-paste-from-clipboard)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(web-mode vterm scss-mode ruby-electric magit js2-mode inf-ruby helm exec-path-from-shell emmet-mode elixir-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
