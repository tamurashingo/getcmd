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
                            :function ,#'generate/migration))))))
```

## parse and run


```common-lisp
(let ((c (getcmd '("new-project" "--path" "/app" "--database" "mysql") *configuration*)))
  (apply (getf c :function)
         (getf c :args)))
```


# configuration format

...


# Copyright

Copyright (c) 2025 tamura shingo

# License

Licensed under the MIT License.




