;;; package -- init.el

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(global-set-key (kbd "C-c i")
		(lambda() (interactive) (load-file "~/.emacs.d/init.el")))

(define-key global-map (kbd "C-c q") 'replace-regexp)

(set-face-attribute 'default nil :height 50)
(setq linum-format "%3d \u2502")
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
  (insert "exit")
  (eshell-send-input)
  (delete-window)
  )

;;; init.el ends here
