;;; bpg-qed.el --- insert Qed at the end of lemma proof                -*- lexical-binding: t; -*-

;; The software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License

;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'bpg-response-buffer)
(require 'eieio)


(defun is-qed-next ()
  "True if the point is before Qed."
  (save-excursion
    (skip-chars-forward " \t\n")
    ;; how to skip comments?
    ;; `proof-script-generic-parse-find-comment-end' should be fit IMHO
    (re-search-forward "Qed[.]\n" (+ 6 (point)) t)))

(defclass InsertQedIfMissing (ResponseBufferHandler)
  ((eval-next-cb :initarg :eval-next-cb))
  "")

(cl-defmethod
  handle-response-buffer ((o InsertQedIfMissing))
  "Insert Qed if it is missing before point.
O this."
  (if (is-qed-next)
      (funcall (slot-value o :eval-next-cb))
    (progn
      (when (not (bolp))
        (skip-chars-forward " \t\n")
        (when (not (bolp)) (insert "\n")))
      (insert "Qed.")
      (when (or (not (eolp))
                (not (char-after))) ;; last buffer line without \n
        (insert "\n"))
      (funcall (slot-value o :eval-next-cb))
      )
    )
  )

(defclass QedDetector (ResponseBufferClassifier) () "See `InsertQedIfMissing'")

(cl-defmethod try-to-classify
  ((_ QedDetector) response-buffer-content eval-next-cb)
  "Return `InsertQedIfMissing' if no goals.
RESPONSE-BUFFER-CONTENT EVAL-NEXT-CB"
    (when (string-match "^No more goals.[\n]*$" response-buffer-content)
      (InsertQedIfMissing :eval-next-cb eval-next-cb)))

(provide 'bpg-qed)
;;; bpg-qed.el ends here
