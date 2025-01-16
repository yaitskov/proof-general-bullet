;;; bpg-collection.el --- hashtable conversions                 -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-macs)

(cl-defun bpg-hash-table-of-alist (al &optional (default-ht))
  "Put all entries of alist AL into a new hash table if DEFAULT-HT is nil.

a hash table is returned"
  (let ((ht (or default-ht (make-hash-table :test 'equal))))
    (mapc (lambda (e)
            (puthash (car e) (cdr e) ht))
          al)
    ht))

(defun bpg-alist-of-hash-table (ht)
  "Convert a hash table HT into alist."
  (let ((al '()))
    (maphash (lambda (k v) (setq al (cons (cons k v) al))) ht)
    al))

(provide 'bpg-collection)
;;; bpg-collection.el ends here
