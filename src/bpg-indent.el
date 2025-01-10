;;; bgp-indent.el --- rewrite bullets to increase nesting level                -*- lexical-binding: t; -*-

(cl-defun gen-rewrite-rules (abc n &optional (origin ()))
  (if (> n 0)
      (gen-rewrite-rules abc (1- n)
                         (cons abc (mapcar (lambda (l) (-zip-with 'concat abc l)) origin)))
    origin))

(provide 'bpg-indent)
;;; bgp-indent.el ends here
