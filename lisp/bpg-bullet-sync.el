;;; bgp-indent.el --- rewrite bullets according to line indentation  -*- lexical-binding: t; -*-

;; The software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

(setq bullet-regexp (rx (or (+ "-") (+ "*") (+ "+")) (+ " ")))
(setq sync-bullet-regexes
      (list
       (cons (rx line-start (group (= 2 " ")) (regexp bullet-regexp)) "  - ")
       (cons (rx line-start (group (= 4 " ")) (regexp bullet-regexp)) "\\1+ ")
       (cons (rx line-start (group (= 6 " ")) (regexp bullet-regexp)) "\\1* ")
       (cons (rx line-start (group (** 8 10 " ")) (regexp bullet-regexp)) "\\1-- ")
       (cons (rx line-start (group (** 11 13 " ")) (regexp bullet-regexp)) "\\1++ ")
       (cons (rx line-start (group (** 14 15 " ")) (regexp bullet-regexp)) "\\1** ")
       ))

(defun bpg-sync-bullets-by-indent ()
  "Align bullets with line indentation.

This is an alternative to `bpg-indent-left'.

Use the function on a region with sub proves after a tactic (such as
split) is removed.

Region example:

  + idtac \"x\".
    split.
    * auto.
    * auto.
  + idtac \"y\".

After:

  - idtac \"x\".
    split.
    + auto.
    + auto.
  - idtac \"y\".

"

  (interactive)
  (with-undo-amalgamate
    (letrec (
             (rs (region-beginning))
             (re (region-end))
             (point-line (line-number-at-pos (point)))
             (cols (- (point) (line-beginning-position)))
             (start (and (goto-char rs) (line-beginning-position)))
             (start-line (line-number-at-pos start))
             (lines-in-region (- (1+ (line-number-at-pos re)) start-line)))
      (cl-loop for replace-pair in sync-bullet-regexes do
               (goto-char start)
               (forward-line lines-in-region)
               (message "Replace %s => %s in region %d:%d"
                        (car replace-pair) (cdr replace-pair) start (point))
               (replace-regexp (car replace-pair) (cdr replace-pair) nil start (point)))
      ;; fix indent
      (goto-char start)
      (forward-line lines-in-region)
      (indent-region start (point))
      ;; restore position
      (goto-char start)
      (forward-line (- point-line start-line))
      (forward-char cols)
      )
    )
  )

(provide 'bpg-bullet-sync)
;;; bgp-bullet-sync.el ends here
