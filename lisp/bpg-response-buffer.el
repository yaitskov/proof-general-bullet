;;; bpg-response-buffer.el --- Abstract classes                               -*- lexical-binding: t; -*-

;; URL: https://github.com/yaitskov/proof-general-bullet
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.0"))

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

;; Classes/Interfaces

;;; Code:
(require 'eieio)

(defun bpg-mytrace (fmt &rest args)
  "An alias to `message' to quickly enable/disable logging during debugging.
FMT and ARGS are passed to `message' as-is."
  (identity (list fmt args)))
  ;; (apply 'message fmt args))

(defclass bpg-ResponseBufferHandler () ()
  "A base class for behaviors triggered by content of response buffer or goals buffer"
  :abstract t)

(cl-defmethod
  bpg-handle-response-buffer ((_ bpg-ResponseBufferHandler))
  "React to content in *response* or *goals* buffers."
 (error "`bpg-handle-response-buffer' is not implemented"))

(defclass bpg-ResponseBufferClassifier () ()
  "`bpg-ResponseBufferHandler' factory"
  :abstract t)

(cl-defmethod bpg-try-to-classify ((_ bpg-ResponseBufferClassifier)
                                   _response-buffer-content _eval-next-cb)
  "`bpg-ResponseBufferHandler' factory."
  (error "`bpg-try-to-classify' is not implemented"))

(provide 'bpg-response-buffer)
;;; bpg-response-buffer.el ends here
