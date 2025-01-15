;;; proof-general-bullet.el --- PG Coq bullet automation  -*- lexical-binding: t -*-

;; Author: Daniil Iaitskov <dyaitskov@gmail.com>
;; Maintainer: Daniil Iaitskov <dyaitskov@gmail.com>
;; URL: https://github.com/yaitskov/proof-general-bullet
;; Version: v0.0.1
;; Keywords: proof assistant, PG, proof-general, Coq, Rocq, goal selector, autocomplete
;; Package-Requires ((emacs "30.0") (proof-general "20241126.32"))

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
;; - `bpg-sync-bullets-by-indent' corrects bullets based on line indentation

;;; Code:

(require 'proof-general)
(require 'bpg-indent)
(require 'bpg-bullet)
(require 'bpg-bullet-sync)
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

(defun coq-auto-bullet-sync-hook-binding (eval-next)
  (let ((cnc (1- C-c_C-n-hit-counter)))
    (when (<= 0 cnc)
        (progn
          (setq C-c_C-n-hit-counter 0)
          (with-current-buffer proof-script-buffer
            (handle-response-buffer-content eval-next)
            )))))

(defun coq-auto-bullet-hook-binding ()
  (cl-flet
      ((is-goals-empty ()
         (with-current-buffer proof-goals-buffer (= (point-min) (point-max))))
       (is-response-empty ()
         (with-current-buffer proof-response-buffer (= (point-min) (point-max)))))
    (if (and (is-response-empty) (is-goals-empty))
        (mytrace "response and goal buffers are empty - skip hook")
        (coq-auto-bullet-sync-hook-binding 'proof-assert-next-command-interactive))))

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
            (add-hook 'proof-shell-handle-delayed-output-hook #'coq-auto-bullet-hook-binding 100)
            (define-key coq-mode-map [(control c) (control n)] 'proof-assert-next-command-interactive-shortcut)
            (define-key coq-mode-map [(control c) (control return)] 'proof-goto-point-interactive)))


(provide 'proof-general-bullet)
;;; proof-general-bullet.el ends here
