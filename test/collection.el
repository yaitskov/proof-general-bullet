;; (add-to-list 'load-path "~/pro/my-emacs/proof-general-bullet/src")
(require 'bpg-collection)
(require 'ert)

(ert-deftest hash-table-of-alist ()
  (should (equal (hash-table-count #s(hash-table data ())) 0))
  (should (= (hash-table-count (hash-table-of-alist '(("a" . 2)) #s(hash-table data ()))) 1))
  )
