;;; bgp-indent.el --- rewrite bullets to increase nesting level                -*- lexical-binding: t; -*-
(require 'dash)
(require 'bpg-collection)

(cl-defun gen-rewrite-rules (abc n &optional (origin ()))
  " generate sequence: '(- + -- ++ ...) from alphabet '(- +)"
  (if (> n 0)
      (gen-rewrite-rules abc (1- n)
                         (cons abc (mapcar (lambda (l) (-zip-with 'concat abc l)) origin)))
    origin))

(defun gen-bullet-indent-alist (bullet-levels shift-fn)
  (hash-table-of-alist
   (-zip-with 'cons bullet-levels (funcall shift-fn bullet-levels))
   (make-hash-table :test 'equal)))

(defun gen-bullet-indent-right-alist (bullet-levels)
  "hash table mapping:  - => +; * => --  ; ..."
  (gen-bullet-indent-alist bullet-levels 'cdr))

(defun gen-bullet-indent-left-alist (bullet-levels)
  "hash table mapping:  - => "" ; + => - ; -- => *  ; ..."
  (gen-bullet-indent-alist
   bullet-levels
   (apply-partially 'cons "")))

(provide 'bpg-indent)
;;; bgp-indent.el ends here
