;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(provide 'direnv)

(require 'json)

(defun direnv--json-from-proc (program &rest args)
  "Execute PROGRAM with ARGS, parsing stdout as JSON."
  (with-temp-buffer
    (let ((status (apply 'call-process program nil '(t nil) nil args)))
      (unless (eq status 0)
        (error "%s exited with status %s" program status))
      (if (= 0 (buffer-size))
          nil
        (progn
          (goto-char (point-min))
          (json-read))))))

(defun direnv--format-json-entry (entry)
  "Formats a JSON ENTRY pair having a symbol and string to a pair of string and string.

e.g.
(KEY . \"value\") ==> (\"key\" . \"value\" )"
  (cons (symbol-name (car entry)) (cdr entry)))

(defun direnv--env ()
  "Executes the program 'direnv export json' and returns a list of pairs having the env variable and the value."
  (mapcar
   'direnv--format-json-entry
   (direnv--json-from-proc "direnv" "export" "json")))


(defun direnv--update-env (env-vars)
  "Set ENV-VARS on 'process-environment'.

ENV-VARS is a list of pairs of environment variables and their
values."
  (let ((new-vars (mapcar 'direnv--format-env env-vars)))
    (setq process-environment (nconc new-vars process-environment))))

(defun direnv--format-env (env-var)
  "Format cs built-in 'process-et' variable.

e.g.
  (direnv--format-env (\"foo\" . \"bar\")) ==> \"foo=bar\""
  (mapconcat 'identity (list (car env-var) (cdr env-var)) "="))

(defun direnv-load ()
  (interactive)
  (direnv--update-env (direnv--env)))
