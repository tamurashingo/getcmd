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
    (eval/cmd-option-arg args config)
    (let ((fn (if *function*
                  (if (stringp *function*)
                      (symbol-function (read-from-string *function*))
                      *function*)
                  default-function)))
      `(:function ,fn
        :args ,(flatten `,(list *arguments* *options*))))))


;; ----------------------------------------

(defun eval/cmd-option-arg (args config)
  (cond ((null args)
         nil)
        ((check/cmd-p args config)
         (apply/cmd args config))
        ((check/option-p args config)
         (apply/option args config))
        (t
         (apply/arg args config))))

;; ----------------------------------------

(defun check/cmd-p (args config)
  (loop for cmd-conf in (getf config :commands)
        when (string= (getf cmd-conf :command)
                      (car args))
          return T))

(defun check/option-p (args config)
  (declare (ignore config))
  (eq (aref (car args) 0) #\-))


;; ----------------------------------------

(defun apply/cmd (args config)
  (let ((cmdopt (loop for cmd in (getf config :commands)
                      for cmd-name = (getf cmd :command)
                      when (string= (car args) cmd-name)
                        return cmd)))
    (assert cmdopt)
    (setf *function* (getf cmdopt :function))
    (eval/cmd-option-arg (cdr args) cmdopt)))


(defun apply/option (args config)
  (let* ((arg-opt (multiple-value-bind (m o)
                      (ppcre:scan-to-strings "^-([^-].*)$|^--(.+)$" (car args))
                    (when m
                      `(:short-option ,(aref o 0)
                        :long-option ,(aref o 1)))))
         (option (loop for cmd-opt in (getf config :options)
                       when (or (string= (getf cmd-opt :short-option)
                                         (getf arg-opt :short-option))
                                (string= (getf cmd-opt :long-option)
                                         (getf arg-opt :long-option)))
                         return cmd-opt)))
    (when (null option)
      (error "option not found: ~A" (car args)))
    (let ((keyword (getf option :keyword))
          (consume (getf option :consume nil))
          (converter (getf option :converter #'identity)))
      (when (null (cadr args))
        (error "option parameter not found. optoion: ~A" (car args)))

      (appendf *options*
               `(,keyword
                 ,(if consume
                      (funcall converter (cadr args))
                      T)))

      (eval/cmd-option-arg (if consume (cddr args)
                                       (cdr args))
                           config))))

(defun apply/arg (args config)
  (appendf *arguments* (list (car args)))
  (eval/cmd-option-arg (cdr args) config))
