;;; bgp-bullet.el --- rewrite bullets to increase nesting level                -*- lexical-binding: t; -*-

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

(require 'bpg-response-buffer)

(defun extract-bullet (msg)
  (and
   (string-match
    (concat
     "^This subproof is complete, but there are some unfocused goals[.]\n"
     " +Focus next goal with bullet \\([+*-]+\\)[.][ \n\t]*$")
    msg)
   (match-string 1 msg)))

(defun find-next-bullet ()
  (save-excursion
    (skip-chars-forward " \t\n")
    (when (not (eobp))
      (move-beginning-of-line 1)
      (and
       (re-search-forward "^ *\\([^ \n]+\\)" (+ 88 (point)) t)
       (match-string-no-properties 1)))))

(defun find-bullet-indent (b)
  (save-excursion
    (and
     (re-search-backward (rx line-start (group (1+ " ")) (literal b) " ") nil t)
     (match-string-no-properties 1))))

(defclass InsertBulletIfMissing (ResponseBufferHandler)
  ((bullet :initarg :bullet)
   (eval-next-cb :initarg :eval-next-cb))
"")

(cl-defmethod
 handle-response-buffer ((o InsertBulletIfMissing))
 (let ((next-bullet (slot-value o :bullet)))
   (mytrace "next-bullet: [%s]" next-bullet)
   (when next-bullet
     (let ((following-bullet (find-next-bullet)))
       (mytrace "following-bullet: [%s]" following-bullet)
       (if (and following-bullet (equal following-bullet next-bullet))
           (progn
             (funcall (slot-value o :eval-next-cb))
             (when (bolp) (left-char 1))
             (when (not (= (char-from-name "SPACE") (preceding-char)))
               (insert " ")))
         (let ((next-bullet-indent (find-bullet-indent next-bullet)))
           (when next-bullet-indent
             (mytrace "next-bullet-indent: %d [%s]" (length next-bullet-indent) next-bullet-indent)
             (when (not (bolp))
               (insert "\n"))
             (insert next-bullet-indent next-bullet " ")
             (when (not (eolp))
               (insert "\n") (left-char 1))
             (mytrace "eval-next; point %d; point-max %d " (point) (point-max))
             (funcall (slot-value o :eval-next-cb))
             (when (bolp)
               (mytrace "before left-char; point %d; point-max %d " (point) (point-max))
               (left-char 1))))
         )
       )
     )
   )
 )

(defclass SubproofRemains (ResponseBufferClassifier) () "See `InsertBulletIfMissing'")

(cl-defmethod try-to-classify
  ((o SubproofRemains) response-buffer-content eval-next-cb)
  "doc string here"
  (let ((next-bullet (extract-bullet response-buffer-content)))
    (when next-bullet
      (InsertBulletIfMissing :bullet next-bullet
                             :eval-next-cb eval-next-cb))))

(provide 'bpg-bullet)
;;; bgp-bullet.el ends here
