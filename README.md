# getcmd

Get command and options from command line arguments.\
This library is intended for use in cli programs with commands and options, such as `rails` or `docker` commands.

# usage

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


option-list := ( option-def * )

option-def := :short-option string
            | :long-option string
            | :keyword keyword
            | :consume [ T | nil ]
            | :converter function

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

# Copyright

Copyright (c) 2025 tamura shingo

# License

Licensed under the MIT License.


