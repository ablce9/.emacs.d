;;; package -- init.el
;;; Commentary:
;;; This is my Emacs config.
;;; Code:

(require 'package)
(add-to-list 'package-archives
	     '("MELPA Stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(if (not
     (fboundp 'flycheck-mode))
    (package-list-packages)
  (message "ready for work")
  )

;; check if I have favourite packages
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 '(
   flycheck
   helm
   auto-complete
   go-mode
   magit
   web-mode
   elpy
   exec-path-from-shell
   )
 )

(require 'helm)
(require 'helm-config)
(setq helm-candidate-number-limit 100)

(progn
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-h C-m") 'helm-mini)
  (global-set-key (kbd "C-h C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-h C-a") 'helm-apropos)
  (global-set-key (kbd "C-c t")   'beginning-of-buffer)
  (global-set-key (kbd "C-c r")   'end-of-buffer)
  (global-auto-complete-mode 1)
  (global-auto-revert-mode 1))

;; disable auto-loading
(require 'flycheck)
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)
		      '(json-jsonlist)))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; flycheck prefix
(setq-default flycheck-temp-prefix "~/.backups/.flycheck")

(add-hook 'web-mode-hook (lambda ()
			   (setq indent-tabs-mode nil)
			   (setq web-mode-css-indent-offset 4)))

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

(add-hook 'js-jsx-mode (lambda ()
			 (setq js-indent-level 4)
			 (setq indent-tabs-mode nil)))

(setq web-mode-content-types-alist
  '(("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
(setq web-mode-code-indent-offset 2)

;; eslint loading from node_modules/eslint/bin/eslint
;; https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun load-eslint-from-node_modules ()
  "Eslint loading from node_modules/eslint/bin/eslint."
  (interactive)
  (let* ((root (locate-dominating-file
		(or (buffer-file-name) default-directory)
		"node-module"))
	 (eslint (and root
		      (expand-file-name "node_modules/eslint/bin/eslint.js" root))))
       (when (and eslint (file-executeable-p eslint))
	 (setq-local flycheck-javascript-eslint-executeable eslint))))

;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'flycheck-mode-hook
	  #'load-eslint-from-node_modules)

(require 'magit)
(global-set-key (kbd "C-c p") 'magit-status)
(global-set-key (kbd "C-c i")
		(lambda() (interactive) (load-file "~/.emacs.d/init.el")))

;; indent for javscript
(add-hook 'js2-mode-hook (lambda() (setq indent-tabs-mode nil)))
(add-hook 'js2-mode-hook (lambda() (message "js2-mode-hook?")))

(defun search (item)
  "Quick search :ITEM."
  (interactive "sitem: ")
  (shell-command(shell-command-to-string
		 (concat "chromium https://duckduckgo.com/\?q=" "'"item"'" ))))

(defun json-format ()
  "The go-format for json."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
			   "python -mjson.tool"
			   (current-buffer) t))

(define-key global-map (kbd "C-c q") 'replace-regexp)

;; magit
(define-key global-map (kbd "C-c g") 'magit-status)

;; elpy
;; configure with flake8 https://github.com/jorgenschaefer/elpy/wiki/Configuration
;; cp pep.cfg $HOME/.config/flake8
(when (require 'elpy nil t)
  (elpy-enable))

;; emacs/init-examples
;; (setq-default case-fold-search nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defvar linum-format
  (setq linum-format "%3d \u2502"))
(line-number-mode)
(global-linum-mode 1)
(setq column-number-mode t)

;; control auto-save-list
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq backup-directory-alist'(("."."~/.backups")))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'jqg t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "green")
(set-face-foreground 'highlight nil)

(defun open-nice()
  "A macro lets you open windows nice"
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)))

(global-set-key (kbd "C-c o") 'open-nice)

(defun eshell-here()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that directory
to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name (car (last (split-string parent "/" t)))))
  (split-window-vertically (- height))
  (other-window 1)
  (eshell "new")
  (rename-buffer (concat "*eshell: " name "*"))

  (insert (concat "ls"))
  (eshell-send-input)))

(global-set-key (kbd "C-c e") 'eshell-here)
(global-set-key (kbd "C-x g") 'goto-line)

(add-to-list 'default-frame-alist '(font "Monospace-8"))

(defun eshell/x ()
  "Exit eshell."
  (insert "exit")
  (eshell-send-input)
  (delete-window)
  )

;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html#you-ve-made-a-mess-of-it
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces :IGNORED."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Add kernel style
	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name)))
	      ;; Enable kernel mode for the appropriate files
	      (when (and filename
			 (string-match (expand-file-name "~/src/linux-trees")
				       filename))
		(setq indent-tabs-mode t)
		(setq show-trailing-whitespace t)
		(c-set-style "linux-tabs-only")))))

;; support languages
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)" . cperl-mode))
(add-to-list 'flycheck-disabled-checkers 'javascript)
(add-to-list 'auto-mode-alist '("\\.\\(js\\\\|html\\)" .
				(lambda ()
				  (web-mode)
				  (flycheck-mode -1))))
;; json indent ...
(add-to-list 'auto-mode-alist '("\\.\\(json\\)" . json-mode))
(add-to-list 'auto-mode-alist '("\\.\\(scss\\|css\\)" . web-mode))
(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(add-to-list 'auto-mode-alist '("\\.\\(zsh\\|sh\\|bash\\|ch\\)" . shell-script-mode))

(require 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go'" . go-mode))

(load-file "~/.emacs.d/yaml-mode.el")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))

(add-to-list 'load-path "~/.emacs.d/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;; init.el ends here
