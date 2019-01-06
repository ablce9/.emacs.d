(package-initialize)
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
   rubocop
   ruby-electric
   tide
   )
 )

(require 'auto-complete)
(require 'auto-complete-config)

;; elpy
;; configure with flake8 https://github.com/jorgenschaefer/elpy/wiki/Configuration
;; cp pep.cfg $HOME/.config/flake8
(when (require 'elpy nil t)
  (elpy-enable))

(require 'helm)
(require 'helm-config)
(setq helm-candidate-number-limit 100)

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

;; magit
(require 'magit)
(global-set-key (kbd "C-c p") 'magit-status)
(global-set-key (kbd "C-c i")
                (lambda() (interactive) (load-file "~/.emacs.d/init.el")))
(define-key global-map (kbd "C-c g") 'magit-status)

;; configure with flake8 https://github.com/jorgenschaefer/elpy/wiki/Configuration
(when (require 'elpy nil t)
  (elpy-enable))

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

(require 'go-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/rust-mode")
(require 'rust-mode)

(ac-config-default)
(global-auto-complete-mode t)

(setq ac-modes '(rust-mode go-mode ruby-mode shell-script-mode
                           yaml-mode slim-mode web-mode
                           cperl-mode c-mode))

(provide 'packages)
;;; packages.el ends here
