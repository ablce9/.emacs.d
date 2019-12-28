;;; package -- init.el
;;; Commentary:
;;; This is my Emacs config.
;;; Code:

;; hooks

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(progn
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  ;; (global-set-key (kbd "C-h C-m") 'helm-mini)
  (global-set-key (kbd "C-h C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-h C-a") 'helm-apropos)
  (global-set-key (kbd "C-c t")   'beginning-of-buffer)
  (global-set-key (kbd "C-c r")   'end-of-buffer)
  (global-set-key (kbd "C-c u")   'ispell-region)
  (global-set-key (kbd "C-c ]") 'windmove-right)
  (global-set-key (kbd "C-c [") 'windmove-left)
  (global-set-key (kbd "C-c <up>") 'windmove-up)
  (global-set-key (kbd "C-c <down>") 'windmove-down)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <left>") 'windmove-left))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; konsole fucks things up.
(normal-erase-is-backspace-mode 0)

;; prevent from adding a utf-8 comment
(setq ruby-insert-encoding-magic-comment nil)

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


(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; control auto-save-list
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist'(("."."~/.backups")))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)

;; load-path
(defun  load-directory (dir)
  (let ((loading (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc loading (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/vendor/")
;; (load-directory "/usr/share/emacs/site-lisp/")
(load-directory "~/.emacs.d/elpa/")

;; theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'jqg t)
(global-hl-line-mode 1)
(set-face-background 'hl-line "green")
(set-face-foreground 'highlight nil)

;; line number
(defvar linum-format
  (setq linum-format "%3d \u2502"))
(line-number-mode)
;; (global-linum-mode 1)
(setq column-number-mode t)

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
(global-set-key (kbd "C-c s") 'shell-script-mode)
(global-set-key (kbd "C-c m") 'set-mark-command)
(global-set-key (kbd "C-h C-g") 'helm-grep-do-git-grep)

(add-to-list 'default-frame-alist '(font "Monospace-8"))
(defun eshell/x ()
  "Exit eshell."
  (insert "exit")
  (eshell-send-input)
  (delete-window)
  )

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

;; cc-mode indent
(setq-default c-basic-offset 4)

;; javascript FUCKS
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

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
  (eldoc-mode +1)
  (company-mode +1)
  (tide-hl-identifier-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook (setq indent-tabs-mode nil))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
;; funky typescript linting in web-mode
(flycheck-add-mode 'typescript-tslint 'web-mode)
(with-eval-after-load 'tide
  (flycheck-add-mode 'typescript-tslint 'react-ts-mode)
  (flycheck-add-mode 'typescript-tide 'react-ts-mode)
  )

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(add-to-list 'auto-mode-alist '("\\.\\(json\\)" . json-mode))
(add-to-list 'flycheck-disabled-checkers 'javascript)

(add-hook 'js-jsx-mode (lambda ()
			 (setq js-indent-level 4)
			 (setq indent-tabs-mode nil)))

(setq web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.\\(html\\|scss\\|css\\|jsx\\|js\\)" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.\\(zsh\\|sh\\|bash\\|ch\\)" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.go'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.slim" . slim-mode))
;;; init.el ends here
