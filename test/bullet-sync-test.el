;;; bullet-sync.el --- test bpg-bullet-sync  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'bpg-bullet-sync)
(require 'ert)

(ert-deftest sync-bullets-by-indent-erts ()
    (ert-test-erts-file "erts/sync-bullets-by-indent.erts"
                      (lambda ()
                        (push-mark)
                        (forward-line 2)
                        (bpg-sync-bullets-by-indent)
                        )))
;;; bullet-sync-test.el ends here
