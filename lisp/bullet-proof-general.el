;;; bullet-proof-general.el --- PG Coq bullet automation                         -*- lexical-binding: t -*-

;; Author: Daniil Iaitskov <dyaitskov@gmail.com>
;; Maintainer: Daniil Iaitskov <dyaitskov@gmail.com>
;; URL: https://github.com/yaitskov/proof-general-bullet
;; Version: 0.0.1
;; Keywords: tools, processes, languages
;; Package-Requires: ((emacs "30.0") (proof-general "20241126.32"))

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
(require 'pg-user)
(require 'bpg-indent)
(require 'bpg-bullet)
(require 'bpg-bullet-sync)
(require 'bpg-qed)
(require 'bpg-first-bullet)
(require 'bpg-response-buffer)

(defun bpg-get-response-buffer-message ()
  "Return content of active Coq buffer."
  (with-current-buffer proof-response-buffer
    (buffer-string)))

(defvar bpg-response-buffer-classifiers
  (list (bpg-SubproofRemains) (bpg-QedDetector) (bpg-SubGoalsDetector)))

(defun bpg-handle-response-buffer-content (eval-next-cb)
  "Do autocompletion if neede to an active Coq buffer.
EVAL-NEXT-CB eval tactic."
  (let ((rbm (bpg-get-response-buffer-message)))
    (cl-loop for m in bpg-response-buffer-classifiers do
             (let ((h (bpg-try-to-classify m rbm eval-next-cb)))
               (when h
                 (bpg-handle-response-buffer h)
                 (cl-return) ;; break loop
                 )))))

;; help to avoid looping when content of response buffer triggers correction
(defvar bpg-C-c_C-n-hit-counter 0)

(defun bpg-coq-auto-bullet-hook-binding ()
  "Hook behavior for processing Coq response and goals buffer states."
  (cl-flet
      ((is-goals-empty ()
         (with-current-buffer proof-goals-buffer (= (point-min) (point-max))))
       (is-response-empty ()
         (with-current-buffer proof-response-buffer (= (point-min) (point-max)))))
    (if (and (is-response-empty) (is-goals-empty))
        (bpg-mytrace "response and goal buffers are empty - skip hook")
        (let ((cnc (1- bpg-C-c_C-n-hit-counter)))
          (when (<= 0 cnc)
            (progn
              (setq bpg-C-c_C-n-hit-counter 0)
              (with-current-buffer proof-script-buffer
                (bpg-handle-response-buffer-content 'proof-assert-next-command-interactive))))))))

(defun bpg-proof-assert-next-command-interactive ()
  "Wrapper around `proof-assert-next-command-interactive' enables response hook."
  (interactive)
  (setq bpg-C-c_C-n-hit-counter (1+ bpg-C-c_C-n-hit-counter))
  (proof-assert-next-command-interactive))

(defun bpg-proof-goto-point (&optional RAW)
  "Wrapper around `proof-goto-point' enables response hook.
RAW is fed to `proof-goto-point'."
  (interactive)
  (setq bpg-C-c_C-n-hit-counter (1+ bpg-C-c_C-n-hit-counter))
  (proof-goto-point RAW))

(define-minor-mode bullet-proof-general-mode
  "Bullet Proof General mode.
This command toggles an auxiliary mode for \\[coq-mode] which
inserts bullets (goal selectors) automatically."
  :lighter " ⊥٭"
  :group 'bullet-proof-general
  :keymap
  (list
   (cons (kbd "C-c C-n") 'bpg-proof-assert-next-command-interactive)
   (cons (kbd "C-c C-<return>") 'bpg-proof-goto-point))

  :after-hook
    (add-hook 'proof-shell-handle-delayed-output-hook #'bpg-coq-auto-bullet-hook-binding 100))

(add-hook 'coq-mode-hook 'bullet-proof-general-mode)

(provide 'bullet-proof-general)
;;; bullet-proof-general.el ends here
