;;; proof-general-bullet.el --- PG Coq auto bullet  -*- lexical-binding: t -*-

;; Author: Daniil Iaitskov <dyaitskov@gmail.com>
;; Version: 0.0.1
;; Keywords: proof assisnat, PG, proof-general, Coq, Rocq
;; Package-Requires ((emacs "25.5"))

;; This file is NOT part of GNU Emacs.

;; proof-general-bullet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; proof-general-bullet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a proof general extension which detects when the current
;; subgoal, annotated with a bullet, is proved (after C-c C-n) and
;; automatically inserts a proper bullet for a next sibling subgoal or
;; for a sibling of the goal.

;;; Code:

(require 'proof-general)

(defun mytrace (fmt &rest args)
  (identity (list fmt args))) ;; (apply 'message fmt args))

(defun get-response-buffer-message ()
  (with-current-buffer proof-response-buffer
    (buffer-string)))

(defun extract-bullet (msg)
  (and
   (string-match
    (concat
     "^This subproof is complete, but there are some unfocused goals[.]\n"
     " +Focus next goal with bullet \\([+*-]+\\)[.][ \n\t]*$")
    msg)
   (match-string 1 msg)))

(defun find-next-bullet ()
  (save-excursion
    (skip-chars-forward " \t\n")
    (when (not (eobp))
      (move-beginning-of-line 1)
      (and
       (re-search-forward "^ *\\([^ \n]+\\)" (+ 88 (point)) t)
       (match-string-no-properties 1)))))

(defun find-bullet-indent (b)
  (save-excursion
    (and
     (re-search-backward (rx line-start (group (1+ " ")) (literal b) " ") nil t)
     (match-string-no-properties 1))))

(defun handle-end-of-subproof (eval-next)
  (let ((next-bullet (extract-bullet (get-response-buffer-message))))
    (mytrace "next-bullet: [%s]" next-bullet)
    (when next-bullet
      (let ((following-bullet (find-next-bullet)))
        (mytrace "following-bullet: [%s]" following-bullet)
        (if (and following-bullet (equal following-bullet next-bullet))
            (progn
              (funcall eval-next)
              (when (bolp) (left-char 1))
              (when (not (= (char-from-name "SPACE") (preceding-char)))
                (insert " ")))
          (let ((next-bullet-indent (find-bullet-indent next-bullet)))
            (when next-bullet-indent
              (mytrace "next-bullet-indent: %d [%s]" (length next-bullet-indent) next-bullet-indent)
              (when (not (bolp))
                (insert "\n"))
              (insert next-bullet-indent next-bullet " ")
              (when (not (eolp))
                (insert "\n") (left-char 1))
              (mytrace "eval-next; point %d; point-max %d " (point) (point-max))
              (funcall eval-next)
              (when (bolp)
                (mytrace "before left-char; point %d; point-max %d " (point) (point-max))
                (left-char 1))))
          )))))

;; help to avoid looping when content of response buffer triggers correction
(setq C-c_C-n-hit-counter 0)

(defun coq-auto-bullet-hook-binding ()
  ;; (mytrace "coq-auto-bullet-hook-binding")
  (run-at-time
   0 nil
   (lambda ()
     (let ((cnc (1- C-c_C-n-hit-counter)))
       ;; (mytrace "CNC %d" C-c_C-n-hit-counter)
       (if (<= 0 cnc)
           (progn
             (setq C-c_C-n-hit-counter 0)
             (with-current-buffer proof-script-buffer
               (handle-end-of-subproof 'proof-assert-next-command-interactive)
               )))))))

(advice-add
 'proof-assert-next-command-interactive
 :before
 (lambda ()
   ;; (mytrace "C-c C-n is pressed: %d" C-c_C-n-hit-counter)
   (setq C-c_C-n-hit-counter (1+ C-c_C-n-hit-counter))))

(add-hook 'proof-shell-handle-delayed-output-hook #'coq-auto-bullet-hook-binding 100)

(provide 'proof-general-bullet)

;;; proof-general-bullet.el ends here
