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


(defun getcmd (args commands-and-options)
  (let ((*function* nil)
        (*arguments* nil)
        (*options* nil))
    (eval-cmd args commands-and-options)
    `(:function ,*function*
      :args ,(flatten `,(list *arguments* *options*)))))


(defun eval-cmd (args commands-and-options)
  (cond ((null args)
         nil)
        ((and (null *function*)
              (command-p args))
         (eval-command args commands-and-options))
        ((option-p args)
         (eval-option args commands-and-options))
        (t
         (eval-argument args commands-and-options))))


(defun command-p (args)
  (ppcre:scan "^[a-zA-Z].+$" (car args)))


(defun option-p (args)
  (eq (aref (car args) 0) #\-))


(defun eval-command (args commands-and-options)
  (let* ((cmd (car args))
         (command (loop for command in (getf commands-and-options :commands)
                        when (string= cmd (getf command :command))
                          return command)))
    (when (not command)
      (error "command not found:~A" command))
    (setf *function* (getf command :function))
    (eval-cmd (cdr args) command)))


(defun eval-option (args commands-and-options)
  (let* ((param (car args))
         (option-name (multiple-value-bind (m o)
                          (ppcre:scan-to-strings "^-([^-].*)$|^--(.+)$" param)
                        (when m
                          `(:short-option ,(aref o 0)
                            :long-option ,(aref o 1)))))
         (option (loop for cmd-opt in (getf commands-and-options :options)
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
                commands-and-options))))


(defun eval-argument (args commands-and-options)
  (appendf *arguments* (car args))
  (eval-cmd (cdr args) commands-and-options))

