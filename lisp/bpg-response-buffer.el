;;; bgp-response-buffer.el --- abstract classes                               -*- lexical-binding: t; -*-

(defun mytrace (fmt &rest args)
  ;; (identity (list fmt args)))
  (apply 'message fmt args))

(defclass ResponseBufferHandler () ()
  "A base class for behaviors triggered by content of response buffer"
  :abstract t)

(cl-defmethod
 handle-response-buffer ((o ResponseBufferHandler))
 "expects current buffer contains Coq code related to
 the message from response buffer"
 (error "handle-response-buffer is not implemented"))

(defclass ResponseBufferClassifier () ()
  "create instance of `ResponseBufferHandler'"
  :abstract t)
(cl-defmethod try-to-classify ((o ResponseBufferClassifier)
                            response-buffer-content eval-next-cb)
           "return an applicable handler or nil"
           (error "try-to-classify is not implemented"))

(provide 'bpg-response-buffer)
;;; bgp-response-buffer.el ends here
