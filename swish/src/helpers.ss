;;; Copyright 2018 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
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

#!chezscheme
(library (helpers)
  (export
   string-param
   integer-param
   previous-sql-valid?
   stringify
   quote-sqlite-identifier
   trim-whitespace
   column-info
   get-db-tables
   remove-tags
   make-table-drop-down
   make-col-drop-downs
   get-bracketed
   get-quoted)
  (import
   (chezscheme)
   (swish imports))

  ;;Common helpers
  (define (string-param name params)
    (let ([value (http:find-param name params)])
      (and value (trim-whitespace value))))

  (define (integer-param name min-value params)
    (let* ([string-value (http:find-param name params)]
           [number-value (and string-value (string->number string-value))])
      (and string-value
           (if (and (integer? number-value) (>= number-value min-value))
               number-value
               (raise `#(bad-integer-param ,name ,min-value ,number-value))))))

  (define (previous-sql-valid? sql)
    (and (string? sql) (not (string=? sql ""))))


  ;; String manipulation
  (define (stringify x)
    (cond
     [(string? x) x]
     [(symbol? x) (symbol->string x)]
     [else (format "~a" x)]))

  (define (trim-whitespace s)
    (define (find-start s i len)
      (cond
       [(eqv? i len) ""]
       [(char-whitespace? (string-ref s i)) (find-start s (fx+ i 1) len)]
       [else (find-end s (fx- len 1) i len)]))
    (define (find-end s i start len)
      (cond
       [(eqv? i start)
        (if (eqv? len 1)
            s
            (string (string-ref s i)))]
       [(char-whitespace? (string-ref s i)) (find-end s (fx- i 1) start len)]
       [else
        (if (and (eqv? start 0) (eqv? (fx- len 1) i))
            s
            (substring s start (fx+ i 1)))]))
    (find-start s 0 (string-length s)))

  ;;SQLite helpers
  (define (quote-sqlite-identifier s)
      (let ([op (open-output-string)])
        (write-char #\" op)
        (string-for-each
         (lambda (c)
           (write-char c op)
           (when (char=? c #\")
             (write-char #\" op)))
         s)
        (write-char #\" op)
        (get-output-string op)))

  (define (column-info table-info)
    (match table-info
      [#(,_ ,name ,type ,_ ,_ ,_)
       name]))

  

  (define (remove-tags val)
  (match val
    [#(,table-name)
     (string->symbol table-name)]))

  (define (make-table-drop-down db table-name selected)
    (let ((tables (map remove-tags
                    (execute-sql db
                      "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view"))))
      `(select (@ (name ,table-name) (id ,table-name))
         (option (@ (style "color: grey")) "(please select a table)") 
         ,@(map (lambda (c) `(option ,(if (string=? selected (stringify c)) `(@ (selected "selected"))) ,(stringify c))) tables))))

  (define (make-col-drop-downs db-tables cont-name drop-name val)
    (define (db-table->selection table)
      (match table
        [(,name . ,columns)
         `(div (@ (class ,cont-name))
            (div (@ (class ,(stringify name)))
              (select (@ (name ,drop-name) (class ,drop-name))
                (option "")
                ,@(map column->option columns))))]))
    (define (column->option column-type)
      (match column-type
        [(,column . ,type)
         `(option ,(if (string=? val (stringify column)) `(@ (selected "selected"))) ,(stringify column))]))
    `(div
      ,@(map db-table->selection db-tables)))

  (define (get-db-tables db)
    (define (table-info master-row)
      (define (column-info-type table-info)
        (match table-info
          [#(,_ ,name ,type ,_ ,_ ,_)
           (cons (string->symbol name) type)]))
      (match master-row
        [#(,table-name)
         (cons
          (string->symbol table-name)
          (map column-info-type
            (execute-sql db (format "pragma table_info(~s)" table-name))))]))
    (map table-info
           (execute-sql db
             "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view")))

   (define (get-bracketed priorKeyword sql)
    (match (pregexp-match  (format "~a\\[(.*?)]" priorKeyword) sql)
      [(,full . (,val)) val]
      [,_ ""]))

  (define (get-quoted priorKeyword sql)
    (match (pregexp-match (format "~a\\('(.*?)'" priorKeyword) sql)
      [(,full . (,val)) val]
      [,_ ""]))

  )


