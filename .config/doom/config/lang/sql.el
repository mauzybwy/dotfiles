;;; config/lang/sql.el -*- lexical-binding: t; -*-

(use-package! sqlformat
  :config
  (setq sqlformat-command 'sqlfluff)
  (setq sqlformat-args '("--config" "/Users/mauzy/.sqlfluff.postgres.cfg")))

;;; config/lang/sql.el ends here
