(in-package #:cl-user)
(defpackage #:getcmd-test
  (:use #:cl
        #:rove
        #:getcmd))

(in-package #:getcmd-test)


(defun help ()
  "help message")

(defun add (x y)
  (let ((x (parse-integer x))
        (y (parse-integer y)))
    (+ x y)))

(defun addn (x &key (base 0))
  (let ((x (parse-integer x)))
    (+ x base)))



(defparameter *config*
  `(:commands ((:command "help"
                :function ,#'help)
               (:command "add"
                :function ,#'add)
               (:command "add-n"
                :function ,#'addn
                :options ((:short-option "b"
                           :long-option "base"
                           :keyword :base
                           :consume t
                           :converter ,#'parse-integer))))))

(deftest no-args-test
  (testing "without default function"
    (let ((c (getcmd '() *config*)))
      (ok (null (getf c :function)))
      ;; function indicator exists, so that returns nil
      (ok (null (getf c :function #'help)))
      (ok (null (getf c :args)))))

  (testing "with default function"
    (let ((c (getcmd '() *config* #'help)))
      (ok (eq (getf c :function) #'help))
      (ok (null (getf c :args)))
      (ok (string= "help message" (apply (getf c :function)
                                         (getf c :args)))))))

(deftest command-test
  (testing "command exists"
    (let ((c (getcmd '("help") *config*)))
      (ok (eq #'help (getf c :function)))
      (ok (equal '() (getf c :args)))
      (ok (string= "help message" (apply (getf c :function)
                                         (getf c :args))))))

  (testing "command not exists - without default function"
    (let ((c (getcmd '("test") *config*)))
      (ok (null (getf c :function)))
      (ok (equal '() (getf c :args)))))

  (testing "command not exists - with default function"
    (let ((c (getcmd '("test") *config* #'help)))
      (ok (eq #'help (getf c :function)))
      (ok (equal '() (getf c :args))))))


(deftest no-option-test
  (let ((c (getcmd '("add" "3" "4") *config*)))
    (ok (eq #'add (getf c :function)))
    (ok (equal '("3" "4") (getf c :args)))
    (ok (= 7 (apply (getf c :function)
                    (getf c :args))))))

(deftest option-test
  (testing "no-option"
    (let ((c (getcmd '("add-n" "3") *config*)))
      (ok (eq #'addn (getf c :function)))
      (ok (equal '("3") (getf c :args)))
      (ok (= 3 (apply (getf c :function)
                      (getf c :args))))))

  (testing "with-short-option"
    (let ((c (getcmd '("add-n" "3" "-b" "4") *config*)))
      (ok (eq #'addn (getf c :function)))
      (ok (equal '("3" :base 4) (getf c :args)))
      (ok (= 7 (apply (getf c :function)
                      (getf c :args))))))

  (testing "with-long-option"
    (let ((c (getcmd '("add-n" "3" "--base" "4") *config*)))
      (ok (eq #'addn (getf c :function)))
      (ok (equal '("3" :base 4) (getf c :args)))
      (ok (= 7 (apply (getf c :function)
                      (getf c :args)))))))


