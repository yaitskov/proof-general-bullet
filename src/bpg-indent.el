;;; bgp-indent.el --- rewrite bullets to increase nesting level                -*- lexical-binding: t; -*-
(require 'bpg-collection)

(cl-defun gen-rewrite-rules (abc n &optional (origin ()))
  " generate sequence: '(- + -- ++ ...) from alphabet '(- +)"
  (if (> n 0)
      (gen-rewrite-rules abc (1- n)
                         (cons abc (mapcar (lambda (l) (seq-mapn 'concat abc l)) origin)))
    origin))

(defun gen-bullet-indent-alist (bullet-levels shift-fn)
  (hash-table-of-alist
   (seq-mapn 'cons bullet-levels (funcall shift-fn bullet-levels))))

(defun gen-bullet-indent-right-alist (bullet-levels)
  "hash table mapping:  - => +; * => --  ; ..."
  (gen-bullet-indent-alist bullet-levels 'cdr))

(defun gen-bullet-indent-left-alist (bullet-levels)
  "hash table mapping:  - => "" ; + => - ; -- => *  ; ..."
  (gen-bullet-indent-alist
   bullet-levels
   (apply-partially 'cons "")))

(defcustom bpg-bullet-abc "-+*" "bullet characters in the order")

(defvar bpg-bullet-levels (apply 'append
                                 (gen-rewrite-rules
                                  (mapcar 'char-to-string
                                          (string-to-list bpg-bullet-abc))
                                  2)))
(defvar bpg-indent-left-map (gen-bullet-indent-left-alist bpg-bullet-levels))
(defvar bpg-indent-right-map (gen-bullet-indent-right-alist bpg-bullet-levels))

(defun bpg-indent-region (bullet-map)
  (with-undo-amalgamate
    (save-excursion
      (letrec ((start-line (line-number-at-pos (region-beginning)))
               (lines-in-region
                (- (1+ (line-number-at-pos (region-end)))
                   start-line)))
        (goto-char (region-beginning))
        ;; (message "start line %d; lines in region: %d;" start-line lines-in-region)
        (line-beginning-position)
        (while (> lines-in-region 0)
          (skip-chars-forward " ")
          (let ((bullet-start (point))
                (bullet-len (skip-chars-forward bpg-bullet-abc)))
            ;; (message "bullet len  %d; point: %d" bullet-len (point))
            (when (> bullet-len 0)
              (letrec ((bullet-end (point))
                       (bullet (buffer-substring bullet-start bullet-end)))
                (when (> (skip-chars-forward " ") 0)
                  ;; (message "space after bullet")
                  (let ((new-bullet (gethash bullet bullet-map)))
                    ;; (message "new bullet: %s" new-bullet)
                    (when new-bullet
                      (replace-string-in-region bullet new-bullet bullet-start bullet-end))))))
            )
          (setq lines-in-region (- lines-in-region 1))
          (forward-line)
          )
        )
      )
    )
  )


(defun bpg-indent-left ()
  "walk through the region by line match line prefix"
  (interactive)
  (bpg-indent-region bpg-indent-left-map))

(defun bpg-indent-right ()
  "walk through the region by line match line prefix"
  (interactive)
  (bpg-indent-region bpg-indent-right-map))

(provide 'bpg-indent)
;;; bgp-indent.el ends here
