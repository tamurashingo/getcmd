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
      (ok (equal '("test") (getf c :args)))))

  (testing "command not exists - with default function"
    (let ((c (getcmd '("test") *config* #'help)))
      (ok (eq #'help (getf c :function)))
      (ok (equal '("test") (getf c :args))))))


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


(defun flag-test-func (arg &key flag)
  (format nil "arg:~A flag:~A" arg flag))

(defparameter *flag-config*
  `(:commands ((:command "test"
                :function ,#'flag-test-func
                :options ((:short-option "f"
                           :long-option "flag"
                           :keyword :flag
                           :consume nil))))))

(deftest flag-option-test
  (testing "flag option followed by argument"
    (let ((c (getcmd '("test" "--flag" "argument") *flag-config*)))
      (ok (eq #'flag-test-func (getf c :function)))
      (ok (equal '("argument" :flag t) (getf c :args)))
      (ok (string= "arg:argument flag:T" (apply (getf c :function)
                                                 (getf c :args))))))

  (testing "flag option followed by multiple arguments"
    (let ((c (getcmd '("test" "--flag" "arg1" "arg2") *flag-config*)))
      (ok (eq #'flag-test-func (getf c :function)))
      (ok (equal '("arg1" "arg2" :flag t) (getf c :args)))))

  (testing "flag option as last argument (bug case)"
    (let ((c (getcmd '("test" "argument" "--flag") *flag-config*)))
      (ok (eq #'flag-test-func (getf c :function)))
      (ok (equal '("argument" :flag t) (getf c :args)))
      (ok (string= "arg:argument flag:T" (apply (getf c :function)
                                                 (getf c :args))))))

  (testing "flag option alone (no command arguments)"
    (let ((c (getcmd '("test" "--flag") *flag-config*)))
      (ok (eq #'flag-test-func (getf c :function)))
      (ok (equal '(:flag t) (getf c :args))))))


(defun db/help (&rest rest)
  (declare (ignore rest))
  "db migrate [up|down]")

(defun db/migrate/up (&rest rest)
  (declare (ignore rest))
  "db/migrate/up")

(defun db/migrate/down (&rest rest)
  (declare (ignore rest))
  "db/migrate/down")

(defparameter *ommit-config*
  `(:commands ((:command "db"
                :function ,#'db/help
                :commands ((:command "migrate"
                            :function ,#'db/migrate/up
                            :commands ((:command "up"
                                        :function ,#'db/migrate/up)
                                       (:command "down"
                                        :function ,#'db/migrate/down))))))))

(deftest subcommand
  (testing "exists subcommand"
    (let ((c (getcmd '("db" "migrate" "up") *ommit-config*)))
      (ok (eq #'db/migrate/up (getf c :function)))
      (ok (string= "db/migrate/up" (apply (getf c :function)
                                          (getf c :args)))))

    (let ((c (getcmd '("db" "migrate" "down") *ommit-config*)))
      (ok (eq #'db/migrate/down (getf c :function)))
      (ok (string= "db/migrate/down" (apply (getf c :function)
                                            (getf c :args))))))

  (testing "ommit subcommand"
    (let ((c (getcmd '("db") *ommit-config*)))
      (ok (eq #'db/help (getf c :function)))
      (ok (string= "db migrate [up|down]" (apply (getf c :function)
                                                 (getf c :args)))))

    (let ((c (getcmd '("db" "migrate") *ommit-config*)))
      (ok (eq #'db/migrate/up (getf c :function)))
      (ok (string= "db/migrate/up" (apply (getf c :function)
                                          (getf c :args)))))))



(defpackage #:getcmd-test-strfunc
  (:use #:cl)
  (:export #:exp-func1))
(in-package #:getcmd-test-strfunc)

(defun exp-func1 (&rest rest)
  (declare (ignore rest))
  "func1!!")

(defun in-func2 (&rest rest)
  (declare (ignore rest))
  "func2!!")

(in-package #:getcmd-test)

(defparameter *str-func*
  '(:commands ((:command "func1"
                :function "getcmd-test-strfunc:exp-func1")
               (:command "func2"
                :function "getcmd-test-strfunc::in-func2"))))


(deftest function-as-string
  (let ((c (getcmd '("func1") *str-func*)))
    (ok (eq #'getcmd-test-strfunc:exp-func1 (getf c :function)))
    (ok (string= "func1!!" (apply (getf c :function)
                                (getf c :args)))))

  (let ((c (getcmd '("func2") *str-func*)))
    (ok (eq #'getcmd-test-strfunc::in-func2 (getf c :function)))
    (ok (string= "func2!!" (apply (getf c :function)
                                  (getf c :args))))))


(defun option-only-test-func (&key database path)
  (format nil "database:~A path:~A" database path))

(defparameter *only-long-option-config*
  `(:commands ((:command "message"
                :function ,#'option-only-test-func
                :options ((:long-option "database"
                           :keyword :database)
                          (:long-option "path"
                           :keyword :path))))))

(defparameter *only-short-option-config*
  `(:commands ((:command "message"
                :function ,#'option-only-test-func
                :options ((:short-option "d"
                           :keyword :database)
                          (:short-option "p"
                           :keyword :path))))))

(deftest option-matching-test
  (testing "only long-option defined - valid option"
    (let ((c (getcmd '("message" "--database") *only-long-option-config*)))
      (ok (eq #'option-only-test-func (getf c :function)))
      (ok (equal '(:database t) (getf c :args)))))

  (testing "only long-option defined - invalid option should error"
    (ok (signals (getcmd '("message" "--1") *only-long-option-config*) 'error)))

  (testing "only long-option defined - undefined option should error"
    (ok (signals (getcmd '("message" "--unknown") *only-long-option-config*) 'error)))

  (testing "only short-option defined - valid option"
    (let ((c (getcmd '("message" "-d") *only-short-option-config*)))
      (ok (eq #'option-only-test-func (getf c :function)))
      (ok (equal '(:database t) (getf c :args)))))

  (testing "only short-option defined - invalid short option should error"
    (ok (signals (getcmd '("message" "-x") *only-short-option-config*) 'error)))

  (testing "only short-option defined - long option format should error"
    (ok (signals (getcmd '("message" "--database") *only-short-option-config*) 'error))))


(defun show-dir (&key dir-list)
  (format nil "dirs:~{~A~^,~}" dir-list))

(defun show-numbers (&key num-list)
  (format nil "numbers:~{~A~^,~}" num-list))

(defparameter *multiple-option-config*
  `(:commands ((:command "show-dir"
                :function ,#'show-dir
                :options ((:short-option "d"
                           :long-option "dir"
                           :keyword :dir-list
                           :consume t
                           :multiple t))))))

(defparameter *multiple-with-converter-config*
  `(:commands ((:command "show-numbers"
                :function ,#'show-numbers
                :options ((:short-option "n"
                           :long-option "num"
                           :keyword :num-list
                           :consume t
                           :multiple t
                           :converter ,#'parse-integer))))))

(defparameter *invalid-multiple-config*
  `(:commands ((:command "test"
                :function ,#'show-dir
                :options ((:short-option "f"
                           :long-option "flag"
                           :keyword :flag
                           :consume nil
                           :multiple t))))))

(deftest multiple-option-test
  (testing "multiple option - single value"
    (let ((c (getcmd '("show-dir" "--dir" "a") *multiple-option-config*)))
      (ok (eq #'show-dir (getf c :function)))
      (ok (equal '(:dir-list ("a")) (getf c :args)))
      (ok (string= "dirs:a" (apply (getf c :function) (getf c :args))))))

  (testing "multiple option - multiple long options"
    (let ((c (getcmd '("show-dir" "--dir" "a" "--dir" "b") *multiple-option-config*)))
      (ok (eq #'show-dir (getf c :function)))
      (ok (equal '(:dir-list ("a" "b")) (getf c :args)))
      (ok (string= "dirs:a,b" (apply (getf c :function) (getf c :args))))))

  (testing "multiple option - mixed short and long options"
    (let ((c (getcmd '("show-dir" "--dir" "a" "--dir" "b" "-d" "c") *multiple-option-config*)))
      (ok (eq #'show-dir (getf c :function)))
      (ok (equal '(:dir-list ("a" "b" "c")) (getf c :args)))
      (ok (string= "dirs:a,b,c" (apply (getf c :function) (getf c :args))))))

  (testing "multiple option with converter"
    (let ((c (getcmd '("show-numbers" "-n" "1" "-n" "2" "--num" "3") *multiple-with-converter-config*)))
      (ok (eq #'show-numbers (getf c :function)))
      (ok (equal '(:num-list (1 2 3)) (getf c :args)))
      (ok (string= "numbers:1,2,3" (apply (getf c :function) (getf c :args))))))

  (testing "multiple option - with arguments interleaved"
    (let ((c (getcmd '("show-dir" "arg1" "--dir" "a" "arg2" "--dir" "b") *multiple-option-config*)))
      (ok (eq #'show-dir (getf c :function)))
      (ok (equal '("arg1" "arg2" :dir-list ("a" "b")) (getf c :args)))))

  (testing "multiple option requires consume to be true"
    (ok (signals (getcmd '("test" "--flag") *invalid-multiple-config*) 'error))))


