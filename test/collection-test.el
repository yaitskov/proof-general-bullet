;; (add-to-list 'load-path "~/pro/my-emacs/proof-general-bullet/src")
(require 'bpg-collection)
(require 'ert)


(ert-deftest hash-table-of-alist ()
  (should (equal (hash-table-count #s(hash-table data ())) 0))
  (should (= (hash-table-count (hash-table-of-alist '(("a" . 2)))) 1))
  (should (= (hash-table-count
              (hash-table-of-alist
               '(("a" . 2))
               #s(hash-table test equal data ("b" 6))))
             2))
  (should (equal
           (alist-of-hash-table
              (hash-table-of-alist
               '(("a" . 3) ("a" . 8))
               #s(hash-table test equal data ("a" 6))))
           '(("a" . 8))))
  (let ((al '(("a" . "b") ("c" . "d"))))
    (should (equal
             (sort (alist-of-hash-table (hash-table-of-alist al)) :key 'car)
             al)))
  )


(ert-deftest alist-of-hash-table ()
  (should (equal
           (sort (alist-of-hash-table #s(hash-table test equal data ("a" "b" "c" "d")))
                 :key 'car)
           '(("a" . "b") ("c" . "d"))))
  )
