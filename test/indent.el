;; (add-to-list 'load-path "~/pro/my-emacs/proof-general-bullet/src")
(require 'bpg-indent)
(require 'ert)

(ert-deftest gen-rewrite-rules ()
  (should (equal (gen-rewrite-rules '("-" "+") 2) '(("-" "+") ("--" "++"))))
  (should (equal (gen-rewrite-rules '("-") 1) '(("-"))))
  (should (equal (gen-rewrite-rules '("-") 0) '()))
  )
