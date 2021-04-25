;;; jqg-theme.el

;;; jqg-theme

;;; Code:
(deftheme jqg
  "created 2015-01-20")

(custom-theme-set-faces
 `jqg
 `(bold ((t (:bold t))))
 `(bold-italic ((t (:bold t))))
 `(border-glyph ((t (nil))))
 `(fringe ((t (:background "#a4c2cc"))))
;;   `(mode-line ((t (:foreground "#072d40" :background "#99bac7"))))

 ;; Custom theme and more customed by me.
 ;; Region
 `(region ((t (:background "MidnightBlue"))))
 ;; `(region ((t (:background "#356a9c"))))

 ;; Cursor
 `(cursor ((t (:foregound "#000000"))))

  `(secondary-selection ((t (:background "dodger blue"))))
 ;; `(linum ((t (:background "black" :foreground "#99bac7"))))
 ;; For comments
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 ;; '(font-lock-comment-face ((t (:foreground "#ff00e1" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#ffb8fb" :slant italic))))

 `(font-lock-builtin-face ((t (:foreground "#ff00ff"))))
 `(font-lock-comment-face ((t (:foreground "#575b5b"))))
 `(font-lock-function-name-face ((t (:foreground "#ec9346"))))
;;  `(font-lock-keyword-face ((t (:foreground "#a4cee5")))) blue
  `(font-lock-string-face ((t (:foreground "#e8b778"))))
  `(font-lock-type-face ((t (:foreground"#74abbe"))))
  `(font-lock-constant-face ((t (:foreground "#eeedec"))))
  `(font-lock-variable-name-face ((t (:foreground "#ff0ff"))))
  `(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
  `(font-lock-warning-face ((t (:foreground "red" :bold t))))

 '(header-line ((t (:box nil :foreground "grey90" :background "grey20" :inherit (mode-line)))))
)
(provide-theme 'jqg)
