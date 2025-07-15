;;;; Copyright 2025 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)

(defpackage #:getcmd-system
  (:use #:asdf #:cl))

(in-package #:getcmd-system)

(defsystem getcmd
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:alexandria
               :cl-ppcre
               :serapeum)
  :components ((:file "getcmd")))

;;;;
