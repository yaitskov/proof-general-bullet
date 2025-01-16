(require 'bpg-indent)
(require 'ert)

(ert-deftest gen-rewrite-rules ()
  (should (equal (bpg-gen-rewrite-rules '("-" "+") 2) '(("-" "+") ("--" "++"))))
  (should (equal (bpg-gen-rewrite-rules '("-") 1) '(("-"))))
  (should (equal (bpg-gen-rewrite-rules '("-") 0) '()))
  )

(ert-deftest gen-bullet-indent-left-alist ()
  (should (equal (gethash "aa"
                          (bpg-gen-bullet-indent-left-alist  '("a" "b" "aa" "bb" "aaa" "bbb")))
                 "b")))

(ert-deftest gen-bullet-indent-right-alist ()
  (should (equal (gethash "bb"
                          (bpg-gen-bullet-indent-right-alist  '("a" "b" "aa" "bb" "aaa" "bbb")))
                 "aaa")))

(defun test-bpg-indent-erts (ert-file indent-fun)
  (ert-test-erts-file ert-file
                      (lambda ()
                        (goto-char (point-min))
                        (push-mark)
                        (goto-char (point-max))
                        (funcall indent-fun)
                        )))

(ert-deftest bpg-indent-left-erts ()
  (test-bpg-indent-erts "erts/indent-left-whole-file.erts" 'bpg-indent-left))

(ert-deftest bpg-indent-right-erts ()
  (test-bpg-indent-erts "erts/indent-right-whole-file.erts" 'bpg-indent-right))

(ert-deftest bpg-indent-left-middle-erts ()
    (ert-test-erts-file "erts/indent-left-middle-line.erts"
                      (lambda ()
                        (forward-line 2)
                        (cl-assert (= 3 (line-number-at-pos (point))))
                        (push-mark)
                        (forward-line 3)
                        (cl-assert (= 6 (line-number-at-pos (point))))
                        (bpg-indent-left)
                        )))

(ert-deftest bpg-indent-left-middle-point-at-beginning-region-erts ()
    (ert-test-erts-file "erts/indent-left-middle-line.erts"
                      (lambda ()
                        (forward-line 5)
                        (right-char 6)
                        (cl-assert (= 6 (line-number-at-pos (point))))
                        (push-mark)
                        (previous-line 3)
                        (cl-assert (= 3 (line-number-at-pos (point))))
                        (bpg-indent-left)
                        )))
