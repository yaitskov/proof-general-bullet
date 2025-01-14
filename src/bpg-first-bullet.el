;;; bpg-first-bullet.el --- detects first bullet                  -*- lexical-binding: t; -*-
(require 'bpg-response-buffer)
(require 'bpg-bullet) ;; reuse InsertBulletIfMissing
(require 'bpg-indent)

(defclass InsertFirstBulletIfMissing (ResponseBufferHandler)
  ((bullet :initarg :bullet)
   (eval-next-cb :initarg :eval-next-cb))
  "")

(defun current-line-indent ()
  (save-excursion
    (string-pad " "
                (or (and (re-search-backward
                          (rx line-start (+ " ") (* (or "+" "-" "*" "{")) " ") nil t)
                         (length (match-string-no-properties 0)))
                    2))))

(cl-defmethod
  handle-response-buffer ((o InsertFirstBulletIfMissing))
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
  "returns closest parent bullet or nin"
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
  ((o SubGoalsDetector) response-buffer-content eval-next-cb)
  "doc string here"
  (with-current-buffer proof-goals-buffer
    (save-excursion
      (goto-char (point-min))
      ;; Example of first line in *goals* buffer relevant to this behavior
      ;; 2 goals (ID 13)

      (when (looking-at "[2-9][0-9]* goals [(]ID [0-9]+[)]")
        (InsertFirstBulletIfMissing :eval-next-cb eval-next-cb)))))

(provide 'bpg-first-bullet)
;;; bpg-first-bullet.el ends here
