;;; bpg-indent.el --- rewrite bullets to increase nesting level                -*- lexical-binding: t; -*-

;; The software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'bpg-collection)

(cl-defun gen-rewrite-rules (abc n &optional (origin ()))
  "Generate a sequence of length N from alphabet ABC.
ABC is a list of bullets.
If it is a b c then the result is a sequence of a b c aa bb cc aaa bbb ccc ..."
  (if (> n 0)
      (gen-rewrite-rules abc (1- n)
                         (cons abc (mapcar (lambda (l) (seq-mapn 'concat abc l)) origin)))
    origin))

(defun gen-bullet-indent-alist (bullet-levels shift-fn)
  "Return a hash table with keys from BULLET-LEVELS and values form BULLET-LEVELS too but values are shifted with SHIFT-FN."
  (hash-table-of-alist
   (seq-mapn 'cons bullet-levels (funcall shift-fn bullet-levels))))

(defun gen-bullet-indent-right-alist (bullet-levels)
  "Generate hash table mapping for bullets a level higher.
- => +; * => --  ; ...
BULLET-LEVELS is a list of ordered bullets."
  (gen-bullet-indent-alist bullet-levels 'cdr))

(defun gen-bullet-indent-left-alist (bullet-levels)
  "Generate hash table mapping for bullets a level lower.
- => \"\" ; + => - ; -- => * ; ...
BULLET-LEVELS is a list of ordered bullets."
  (gen-bullet-indent-alist
   bullet-levels
   (apply-partially 'cons "")))

(defcustom bpg-bullet-abc "-+*" "Bullet characters in the order.")

(defvar bpg-bullet-levels (apply 'append
                                 (gen-rewrite-rules
                                  (mapcar 'char-to-string
                                          (string-to-list bpg-bullet-abc))
                                  2)))
(defvar bpg-indent-left-map (gen-bullet-indent-left-alist bpg-bullet-levels))
(defvar bpg-indent-right-map (gen-bullet-indent-right-alist bpg-bullet-levels))

(defun bpg-indent-region (bullet-map)
  "Rewrite bullets in the region according the the BULLET-MAP."
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
  "Replace bullets in the region with siblings from the left (e.g. + => - and -- => *)."
  (interactive)
  (bpg-indent-region bpg-indent-left-map))

(defun bpg-indent-right ()
  "Replace bullets in the region with siblings from the right (e.g. - => + and * => --)."
  (interactive)
  (bpg-indent-region bpg-indent-right-map))

(provide 'bpg-indent)
;;; bpg-indent.el ends here
