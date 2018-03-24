;;; package -- init.el -- Let's get started with Emacs
;;; on whatever machines

;;; Commentary:

;;; Author: xun shunsuketamiya@posteo.net and bunch of
;;; Emacs hackers from the world

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
   ;; add packages you like
   flycheck
   helm
   auto-complete
   go-mode
   )
 )

(global-set-key (kbd "C-c p") 'magit-status)

(global-set-key (kbd "C-c i")
		(lambda() (interactive) (load-file "~/.emacs.d/init.el")))

(define-key global-map (kbd "C-c q") 'replace-regexp)

(defvar linum-format
  (setq linum-format "%3d \u2502")
  )
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

; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'jqg t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "red")
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
    (other-window 1)
    )
  )

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

(defun eshell/x ()
  "Exit eshell."
  (insert "exit")
  (eshell-send-input)
  (delete-window)
  )

;; support languages
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.\\(jsx\\|js\\)" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.\\(zsh\\|sh\\|bash\\|ch\\)" . shell-script-mode))
;;
;; go get github.com/dominikh/go-mode.el/blob/master/go-mode.el
;; (add-to-list 'auto-mode-alist '("\\.\\(go\\)" . go-mode))

;;; init.el ends here
