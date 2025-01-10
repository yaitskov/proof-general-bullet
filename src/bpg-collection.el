;;; bgp-collection.el --- collection                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  dan

;; Author: dan <dan@diehard>
;; Keywords: data

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

;;

;;; Code:

(defun hash-table-of-alist (al ht)
  "put all entries of alist into a hash table

hash table is returned"

  (mapc (lambda (e) (puthash (car e) (cdr e) ht)) al)
  ht)

(provide 'bpg-collection)
;;; bgp-collection.el ends here
