(in-package #:cl-user)
(defpackage #:getcmd-test-system
  (:use #:asdf #:cl))
(in-package #:getcmd-test-system)

(defsystem getcmd-test
  :depends-on (#:getcmd
               #:rove)
  :components ((:file "getcmd-test"))
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))

