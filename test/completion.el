(require 'proof-general-bullet)
(require 'ert)

(defun with-response-buffer (cb response-content)
  (let ((proof-response-buffer nil))
    (unwind-protect
        (progn
          (setq proof-response-buffer (get-buffer-create "*response*"))
          (with-current-buffer proof-response-buffer (insert response-content))
          (funcall cb))
      (kill-buffer proof-response-buffer))))

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

(ert-deftest handle-end-of-subproof-erts ()
  (ert-test-erts-file "handle-end-of-subproof.erts"
                      (lambda ()
                        (search-forward "(* CURSOR HERE *)")
                        (search-backward "(")
                        (kill-line)
                        (with-response-buffer
                         (lambda () (handle-end-of-subproof (apply-partially 'move-end-of-line 1)))
                         "This subproof is complete, but there are some unfocused goals.
 Focus next goal with bullet -.
"
                         )
                        )))