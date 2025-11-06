# getcmd

Get command and options from command line arguments.\
This library is intended for use in cli programs with commands and options, such as `rails` or `docker` commands.

This library is particularly useful for creating Roswell scripts that need to parse command line arguments.

# usage

## Roswell script example

This is a simple example of using getcmd in a Roswell script:

```common-lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#

(ql:quickload :getcmd :silent t)

(defun hello (name &key greeting)
  (format t "~A, ~A!~%" (or greeting "Hello") name))

(defun goodbye (name)
  (format t "Goodbye, ~A!~%" name))

(defparameter *config*
  `(:commands ((:command "hello"
                :function ,#'hello
                :options ((:short-option "g"
                           :long-option "greeting"
                           :keyword :greeting
                           :consume t)))
               (:command "goodbye"
                :function ,#'goodbye))))

(defun main (&rest argv)
  (let ((c (getcmd:getcmd argv *config*)))
    (when (getf c :function)
      (apply (getf c :function) (getf c :args)))))
```

Usage:
```bash
$ ./script.ros hello World
Hello, World!

$ ./script.ros hello World --greeting Hi
Hi, World!

$ ./script.ros goodbye World
Goodbye, World!
```

## define functions

```common-lisp

(defun new-project (project-name &key (path "/projects") (database-name :sqlite3))
  (format t "new-project: ~A~%" project-name)
  (format t "  project-path: ~S~%" path)
  (format t "  database-name: ~S~%" database-name))

(defun generate/model (model-name &key no-migration)
  (format t "generate/model: ~A~%" model-name)
  (format t "  no-migration: ~A~%" no-migration))

(defun generate/migration (migration-name)
  (format t "generate/migration: ~A~%" migration-name))

```


## configuration

```common-lisp
(defparameter *configuration*
  `(:commands ((:command "new-project"
                :function ,#'new-project
                :options ((:short-option "p"
                           :long-option "path"
                           :keyword :path
                           :consume t
                           :converter ,#'identity)
                          (:short-option "d"
                           :long-option "database"
                           :keyword :database-name
                           :consume t
                           :converter ,#'(lambda (s)
                                           (intern (string-upcase s) :KEYWORD)))))
               (:command "generate"
                :commands ((:command "model"
                            :function ,#'generate/model
                            :options ((:long-option "no-migration"
                                       :keyword :no-migration
                                       :consume nil)))
                           (:command "migration"
                            :function "generate/migration"))))))
```

## parse and run


```common-lisp
(let ((c (getcmd '("new-project" "--path" "/app" "--database" "mysql") *configuration*)))
  (apply (getf c :function)
         (getf c :args)))
```


## Syntax:

**getcmd** *args* *configuration* *&optional* *default-function* => *result*

### Arguments and Values:

*args* -- a list of string

*configuration* -- a property list

*default-function* -- a function

*result* -- a property list


### Description:

Parse *args* according to the contents of *configuration* and
return the function corresponding to the command and its arguments as property list.

If no function corresponding to the command is found, the function becomes nil.
If *default-function* is specified, the function becomes *default-function*.


# configuration format

```
configuration := configuration-list

configuration-list := :commands command-list
                    | :options option-list

command-list := ( command-def * )

command-def := :command string
             | :commands command-list
             | :function function
             | :options option-list
             | :arguments-as-list [ T | nil ]


option-list := ( option-def * )

option-def := :short-option string
            | :long-option string
            | :keyword keyword
            | :consume [ T | nil ]
            | :converter function
            | :multiple [ T | nil ]

```

## `:command`

- type: string

This is the command name specified in the argument.

## `:function`

- type: function or string

This is a function corresponding to the specified command.

Since this function is executed with `apply`,
arguments and keyword parameters must be defined to accept them.

```common-lisp
;; bad
(defun help ()
  (format t "this is help~%"))

;; good
(defun help (&rest rest)
  (declare (ignore rest))
  (format t "this is help~%"))
```

## `:short-option`

- type: string


This is an option name specified with a single hyphen.

It says `short`, but it can be longer.


## `:long-option`

- type: string

This is an option name specified with a double hyphen.

It says `long`, but it can be short.


## `:keyword`

- type: keyword

This is the keyword used when passing the option to the function as a keyword parameter.


```common-lisp
(defun help (&rest args &key message &allow-other-keys)
  (declare (ignore args))
  (format t "message: ~A~%" message))
```

When there is a function like the one above, it is defined as follows.


```
:command "help"
:function #'help
:options ((:shor-option "m"
           :keyword :message
           :consume T))
```


## `:consume`

- type: T or NIL

If T, consume the parameter; if nil or unspecified, do not consume the parameter.

In the case of T, pass the option as a string to the function.
If NIL or unspecified, pass the option as T or NIL to the function.


## `:converter`

- type: function

Convert parameters.

Parameters are passed as strings, but if you want to convert them to numbers or keywords
before passing them, specify that here.


## `:multiple`

- type: T or NIL

If T, allows the same option to be specified multiple times, and collects all values into a list.
If NIL or unspecified, the option can only be specified once.

When `:multiple` is T, `:consume` must also be T.

Example:

```common-lisp
(defun show-dir (&key dir-list)
  (loop for dir in dir-list
    do (format t "~S~%" dir)))

(defparameter *config*
  `(:commands ((:command "show-dir"
                :function ,#'show-dir
                :options ((:short-option "d"
                           :long-option "dir"
                           :keyword :dir-list
                           :consume t
                           :multiple t))))))

;; Usage:
;; cmd show-dir --dir a --dir b -d c
;; => (show-dir :dir-list '("a" "b" "c"))
```

If `:converter` is specified with `:multiple`, the converter is applied to each individual value:

```common-lisp
(:options ((:short-option "n"
            :long-option "num"
            :keyword :num-list
            :consume t
            :multiple t
            :converter ,#'parse-integer)))

;; Usage:
;; cmd --num 1 --num 2 -n 3
;; => :num-list (1 2 3)
```

## `:arguments-as-list`

- type: T or NIL

If T, all positional arguments are collected into a single list before being passed to the function.
This is useful when the function has a `&rest` parameter or expects a list of arguments as its first parameter.

When `:arguments-as-list` is nil or not specified (default), positional arguments
are passed individually as they have been.

### Background

In CLI applications, it's common to write functions that accept a variable number of files or items:

```common-lisp
(defun show-files (files &key verbose)
  ;; files is expected to be a list
  (loop for file in files
        do (format t "~A~%" file)))
```

Without `:arguments-as-list`, positional arguments would be passed individually:
```common-lisp
;; cmd show-files file1.txt file2.txt
;; => (show-files "file1.txt" "file2.txt")  ; Error: too many arguments
```

With `:arguments-as-list t`, all positional arguments are collected into a list:
```common-lisp
;; cmd show-files file1.txt file2.txt
;; => (show-files ("file1.txt" "file2.txt"))  ; Correct!
```

This makes it easy to handle commands that process multiple items without having to manually collect them.

### Example with `:arguments-as-list t`:

```common-lisp
(defun show-files (files &key verbose)
  (loop for file in files
        do (if verbose
               (format t "File: ~A~%" file)
               (format t "~A~%" file))))

(defparameter *config*
  `(:commands ((:command "show-files"
                :function ,#'show-files
                :arguments-as-list t
                :options ((:short-option "v"
                           :long-option "verbose"
                           :keyword :verbose
                           :consume nil))))))

;; Usage:
;; cmd show-files --verbose file1.txt file2.txt
;; => (show-files ("file1.txt" "file2.txt") :verbose t)

;; With no files:
;; cmd show-files --verbose
;; => (show-files nil :verbose t)
```

### Example without `:arguments-as-list` (default):

```common-lisp
(defun add (x y)
  (+ (parse-integer x) (parse-integer y)))

(defparameter *config*
  `(:commands ((:command "add"
                :function ,#'add))))

;; Usage:
;; cmd add 3 4
;; => (add "3" "4")
```

# Copyright

Copyright (c) 2025 tamura shingo

# License

Licensed under the MIT License.


