;; (add-to-list 'load-path "~/pro/my-emacs/proof-general-bullet/src")
(require 'bpg-indent)
(require 'ert)

(ert-deftest gen-rewrite-rules ()
  (should (equal (gen-rewrite-rules '("-" "+") 2) '(("-" "+") ("--" "++"))))
  (should (equal (gen-rewrite-rules '("-") 1) '(("-"))))
  (should (equal (gen-rewrite-rules '("-") 0) '()))
  )

(ert-deftest gen-bullet-indent-left-alist ()
  (should (equal (gethash "aa"
                          (gen-bullet-indent-left-alist  '("a" "b" "aa" "bb" "aaa" "bbb")))
                 "b")))

(ert-deftest gen-bullet-indent-right-alist ()
  (should (equal (gethash "bb"
                          (gen-bullet-indent-right-alist  '("a" "b" "aa" "bb" "aaa" "bbb")))
                 "aaa")))
