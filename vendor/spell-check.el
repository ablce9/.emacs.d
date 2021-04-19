;;; package -- spell-check.el
;;; Commentary:
;;;  Spell check the fuck out.
(setq ispell-program-name "aspell")
(setq-default ispell-extra-args '("--sug-mode=ultra"
				  "--lang=en_US"
				  "--camel-case"))
(provide 'spell-check)
;;; spell-check.el ends here
