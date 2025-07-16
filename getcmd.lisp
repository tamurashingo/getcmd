;;;; Copyright 2025 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)
(defpackage #:getcmd
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf
                #:flatten)
  (:import-from #:serapeum
                #:drop
                #:take)
  (:export #:getcmd))

(in-package #:getcmd)


(defparameter *function* nil)
(defparameter *arguments* nil)
(defparameter *options* nil)


(defun getcmd (args config &optional default-function)
  (let ((*function* nil)
        (*arguments* nil)
        (*options* nil))
    (eval-cmd args config)
    `(:function ,(if *function*
                     *function*
                     default-function)
      :args ,(flatten `,(list *arguments* *options*)))))


(defun eval-cmd (args config)
  (cond ((null args)
         nil)
        ((and (null *function*)
              (command-p args))
         (eval-command args config))
        ((option-p args)
         (eval-option args config))
        (t
         (eval-argument args config))))


(defun command-p (args)
  (ppcre:scan "^[a-zA-Z].+$" (car args)))


(defun option-p (args)
  (eq (aref (car args) 0) #\-))


(defun eval-command (args config)
  (let* ((cmd (car args))
         (cmd-config (loop for cmd-config in (getf config :commands)
                        when (string= cmd (getf cmd-config :command))
                          return cmd-config)))
    (when cmd-config
      (setf *function* (getf cmd-config :function)))
    (eval-cmd (cdr args) cmd-config)))


(defun eval-option (args config)
  (let* ((param (car args))
         (option-name (multiple-value-bind (m o)
                          (ppcre:scan-to-strings "^-([^-].*)$|^--(.+)$" param)
                        (when m
                          `(:short-option ,(aref o 0)
                            :long-option ,(aref o 1)))))
         (option (loop for cmd-opt in (getf config :options)
                       when (or (string= (getf cmd-opt :short-option)
                                         (getf option-name :short-option))
                                (string= (getf cmd-opt :long-option)
                                         (getf option-name :long-option)))
                         return cmd-opt)))
    (let ((keyword (getf option :keyword))
          (consume (getf option :consume nil))
          (converter (getf option :converter #'identity)))

      (appendf *options*
               `(,keyword
                 ,(if consume
                      (funcall converter (cadr args))
                      T)))
      (eval-cmd (if consume (cddr args)
                            (cdr args))
                config))))


(defun eval-argument (args config)
  (appendf *arguments* (list (car args)))
  (eval-cmd (cdr args) config))

