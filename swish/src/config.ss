;;; Copyright 2018 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permirespot persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(library (config)
  (export
   setup-config-db
   user-log-path
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define schema-name 'config)
  (define schema-version "2018-06-21")

  (define (setup-config-db)
    (match (log-db:version schema-name)
      [,@schema-version (create-db)]
      [#f
       (log-db:version schema-name schema-version)
       (create-db)]
      [,version (raise `#(unsupported-db-version ,schema-name ,version))]))    

  (define (create-db)
    (execute "create table if not exists database (name text, description text, file_path text primary key)")
    (execute "create table if not exists search (name text, description text, sqlite text primary key)")
    )

  (define user-log-path (make-parameter #f))
  )
