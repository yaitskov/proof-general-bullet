;;; bgp-qed.el --- rewrite bullets to increase nesting level                -*- lexical-binding: t; -*-
(require 'bpg-response-buffer)

(defun is-qed-next ()
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
  ((o QedDetector) response-buffer-content eval-next-cb)
  "doc string here"
    (when (string-match "^No more goals.[\n]*$" response-buffer-content)
      (InsertQedIfMissing :eval-next-cb eval-next-cb)))

(provide 'bpg-qed)
;;; bgp-qed.el ends here
