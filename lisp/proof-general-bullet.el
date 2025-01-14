;;; proof-general-bullet.el --- PG Coq bullet automation  -*- lexical-binding: t -*-

;; Author: Daniil Iaitskov <dyaitskov@gmail.com>
;; Maintainer: Daniil Iaitskov <dyaitskov@gmail.com>
;; URL: https://github.com/yaitskov/proof-general-bullet
;; Version: v0.0.1
;; Keywords: proof assistant, PG, proof-general, Coq, Rocq, goal selector, autocomplete
;; Package-Requires ((emacs "29.1") (proof-general "20241126.32"))

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; This is a proof general (https://proofgeneral.github.io/) extension
;; which automates handling bullet (aka goal selectors).


;; Use cases:
;; - Insert a bullet after a tactic (such as split, induction etc) generating subgoals.
;; - Insert a bullet after current subgoal is proved but there is a sibling one.
;; - Insert Qed if lemma is proved.
;; - Functions `bpg-indent-right' and `bpg-indent-right' help to increase/decrease
;;   nesting of subproves.

;;; Code:

(require 'proof-general)
(require 'bpg-indent)
(require 'bpg-bullet)
(require 'bpg-qed)
(require 'bpg-first-bullet)

(defun get-response-buffer-message ()
  (with-current-buffer proof-response-buffer
    (buffer-string)))

(defvar response-buffer-classifiers
  (list (SubproofRemains) (QedDetector) (SubGoalsDetector)))

(defun handle-response-buffer-content (eval-next-cb)
  (let ((rbm (get-response-buffer-message)))
    (cl-loop for m in response-buffer-classifiers do
             (let ((h (try-to-classify m rbm eval-next-cb)))
               (when h
                 (handle-response-buffer h)
                 (cl-return) ;; break loop
                 )))))

;; help to avoid looping when content of response buffer triggers correction
(defvar C-c_C-n-hit-counter 0)

(defcustom bpg-delay-seconds 1
  "Delay for reaction on content is *response* buffer callback. Hackish parameter.")

(defun coq-auto-bullet-sync-hook-binding (eval-next)
  (let ((cnc (1- C-c_C-n-hit-counter)))
    (when (<= 0 cnc)
        (progn
          (setq C-c_C-n-hit-counter 0)
          (with-current-buffer proof-script-buffer
            (handle-response-buffer-content eval-next)
            )))))

(defun coq-auto-bullet-hook-binding ()
  (run-at-time
   bpg-delay-seconds nil
   (lambda () (coq-auto-bullet-sync-hook-binding 'proof-assert-next-command-interactive))))

;; proof-assert-next-command-interactive is called in loop after C-c C-Enter
(defun proof-assert-next-command-interactive-shortcut ()
  (interactive)
  (setq C-c_C-n-hit-counter (1+ C-c_C-n-hit-counter))
  (proof-assert-next-command-interactive))

(defun proof-goto-point-interactive (&optional RAW)
  (interactive)
  (setq C-c_C-n-hit-counter (1+ C-c_C-n-hit-counter))
  (proof-goto-point RAW))

(add-hook 'coq-mode-hook
          (lambda ()
            (define-key coq-mode-map [(control c) (control n)] 'proof-assert-next-command-interactive-shortcut)
            (define-key coq-mode-map [(control c) (control return)] 'proof-goto-point-interactive)))

(add-hook 'proof-shell-handle-delayed-output-hook #'coq-auto-bullet-hook-binding 100)

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

(defun sync-bullets-by-indent ()
  "Changes goal selectors.

Use the function on a region with sub proves after tactic (such as
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

(provide 'proof-general-bullet)

;;; proof-general-bullet.el ends here
