(require 'proof-general-bullet)
(require 'ert)

(defmacro with-buffer-x (buffer-var buffer-name cb content)
  `(let ((,buffer-var nil))
    (unwind-protect
        (progn
          (setq ,buffer-var (get-buffer-create ,buffer-name))
          (with-current-buffer ,buffer-var (insert ,content))
          (funcall ,cb))
      (kill-buffer ,buffer-var))))

(defun with-response-buffer (cb buf-content)
  (with-buffer-x proof-response-buffer "*response*" cb buf-content))

(defun with-goals-buffer (cb buf-content)
  (with-buffer-x proof-goals-buffer "*goals*" cb buf-content))


(ert-deftest get-response-buffer-message-out-of-coq-mode ()
  (with-response-buffer
   (lambda () (should (equal "hello" (get-response-buffer-message))))
   "hello"))

(ert-deftest extract-bullet-match ()
  (should (equal (extract-bullet "This subproof is complete, but there are some unfocused goals.
 Focus next goal with bullet +.
")
                 "+"))
  (should (equal (extract-bullet "This subproof is complete, but there are some unfocused goals.
 Focus next goal with bullet ---.")
                 "---"))
  )

(ert-deftest extract-bullet-dont-match ()
  (should (null (extract-bullet "No more goals.")))
  (should (null (extract-bullet "This subproof is complete, but there are some unfocused goals.
 Focus next goal with bullet ??.")))
  )

(ert-deftest handle-response-buffer-insert-bullet-for-next-sibling ()
  (ert-test-erts-file "erts/handle-end-of-subproof/insert-bullet-for-next-sibling-eof.erts"
                      (lambda ()
                        (search-forward "(* CURSOR HERE *)")
                        (search-backward "(")
                        (kill-line)
                        (with-response-buffer
                         (lambda () (handle-response-buffer-content (apply-partially 'move-end-of-line 1)))
                         "This subproof is complete, but there are some unfocused goals.
 Focus next goal with bullet -.
"
                         )
                        )))

(ert-deftest handle-response-buffer-dont-insert ()
  (ert-test-erts-file "erts/handle-end-of-subproof/dont-insert-bullet-after-last-subproof-eof.erts"
                      (lambda ()
                        (search-forward "(* CURSOR HERE *)")
                        (search-backward "(")
                        (kill-line)
                        (with-goals-buffer
                         (lambda ()
                           (with-response-buffer
                            (lambda ()
                              (handle-response-buffer-content (apply-partially 'move-end-of-line 1)))
                            ""))
                         "goals buffer content should be irrelevant"
                         ))))

(ert-deftest handle-response-buffer-insert-qed ()
  (ert-test-erts-file "erts/handle-end-of-subproof/insert-qed.erts"
                      (lambda ()
                        (search-forward "(* CURSOR HERE *)")
                        (search-backward "(")
                        (kill-sexp)
                        (with-response-buffer
                         (lambda () (handle-response-buffer-content (lambda () nil)))
                         "No more goals.")
                        )))

(ert-deftest find-closest-parent-bullet-bullet-on-current-line ()
  (with-temp-buffer
    (insert "  - split.")
    (goto-char (point-max))
    (should (equal (find-closest-parent-bullet) "-"))
    )
    (with-temp-buffer
    (insert "  - split.")
    (goto-char (point-min))
    (should (null (find-closest-parent-bullet)))
    )
  )

(ert-deftest insert-first-bullet-erts ()
  (ert-test-erts-file "erts/insert-first-bullet.erts"
                      (lambda ()
                        (search-forward "(* CURSOR HERE *)")
                        (search-backward "(")
                        (kill-sexp)
                        (with-goals-buffer
                         (lambda ()
                           (with-response-buffer
                            (lambda () (handle-response-buffer-content (lambda () nil)))
                            ""))
                         "2 goals (ID 13)\n\ngoal 2 ..."
                         ))))
