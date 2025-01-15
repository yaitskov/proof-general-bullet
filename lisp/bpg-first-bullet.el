;;; bpg-first-bullet.el --- detects first bullet                  -*- lexical-binding: t; -*-

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

(require 'eieio)
(require 'bpg-response-buffer)
(require 'bpg-bullet)
(require 'bpg-indent)

(defclass InsertFirstBulletIfMissing (ResponseBufferHandler)
  ((bullet :initarg :bullet)
   (eval-next-cb :initarg :eval-next-cb))
  "")

(defun current-line-indent ()
  "Return a string of spaces.
Its length is equal to distance between line start and
first non space character excluding bullet if presented."
  (save-excursion
    (string-pad " "
                (or (and (re-search-backward
                          (rx line-start (+ " ") (* (or "+" "-" "*" "{")) " ") nil t)
                         (length (match-string-no-properties 0)))
                    2))))

(cl-defmethod
  handle-response-buffer ((o InsertFirstBulletIfMissing))
  "Insert a bullet if goals buffer has more than 1 subgoal.
O this."
  (let ((bullet (or (gethash (find-closest-parent-bullet) bpg-indent-right-map)
                    (car bpg-bullet-levels)))
        (following-bullet (find-next-bullet)))
    (mytrace "following-bullet: [%s]" following-bullet)
    (if (and following-bullet (equal following-bullet bullet))
        (progn
          (funcall (slot-value o :eval-next-cb))
          (when (bolp) (left-char 1))
          (when (not (= (char-from-name "SPACE") (preceding-char)))
            (insert " ")))
      (let ((bullet-indent (current-line-indent)))
        (when (not (bolp))
          (insert "\n"))
        (insert bullet-indent bullet " ")
        (when (not (eolp))
          (insert "\n") (left-char 1))
        (mytrace "eval-next; point %d; point-max %d " (point) (point-max))
        (funcall (slot-value o :eval-next-cb))
        (when (bolp)
          (mytrace "before left-char; point %d; point-max %d " (point) (point-max))
          (left-char 1)))
      )
    )
  )

(defun find-closest-parent-bullet ()
  "Return closest parent bullet or nil."
  (let ((current-indent
         (save-excursion
           (or (and (re-search-backward (rx line-start (group (* " ")) (not " ")) nil t)
                    (length (match-string-no-properties 1)))
               0))))
    (save-excursion
      (cl-loop while (> current-indent 0)
               do
               (move-beginning-of-line 1)
               (let ((i (skip-chars-forward " ")))
                 (mytrace "i = %d; current-indent = %d" i current-indent)
                 (when (<= i current-indent)
                   (let ((s (point))
                         (l (skip-chars-forward "-+*")))
                     (when (> l 0)
                       (cl-return (buffer-substring s (+ s l)))))
                   (setq currentl-indent i)
                   )
                 )
               (unless (line-move -1 t) (cl-return nil))
               )

      )
    )
  )

(defclass SubGoalsDetector (ResponseBufferClassifier) ())

(cl-defmethod try-to-classify
  ((_ SubGoalsDetector) _response-buffer-content eval-next-cb)
  "Return new `InsertFirstBulletIfMissing' if *goals* buffer has subgoals.
EVAL-NEXT-CB eval next tactic."
  (with-current-buffer proof-goals-buffer
    (save-excursion
      (goto-char (point-min))
      ;; Example of first line in *goals* buffer relevant to this behavior
      ;; 2 goals (ID 13)

      (mytrace "goals buffer is empty ?: %s" (buffer-substring-no-properties (point-min) (point-max)))
      (when (looking-at "[2-9][0-9]* goals [(]ID [0-9]+[)]")
        (InsertFirstBulletIfMissing :eval-next-cb eval-next-cb)))))

(provide 'bpg-first-bullet)
;;; bpg-first-bullet.el ends here
