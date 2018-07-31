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


;; HTML responses
(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
      (hosted-page (get-page-name)
        (list (css-include "css/query-db.css")
          (js-include "js/jquery-1.4.4.min.js") ;Enforces data entry constraints
          (js-include "js/query-db.js"))
        c1 c2 ...)]))

;; Running a query
(define (do-query db sql limit offset type f)
  (define (nav-form where from-offset enabled?)
    `(form (@ (name "query") (method "get"))
       (textarea (@ (name "sql") (class "hidden")) ,sql)
       (input (@ (name "limit") (class "hidden") (value ,(stringify limit))))
       (input (@ (name "offset") (class "hidden") (value ,(stringify from-offset))))
       (input (@ (name "type") (class "hidden") (value ,(stringify type))))
       (button ,(if enabled? 
                    '(@ (type "submit"))
                    '(@ (type "submit") (disabled)))
         ,(stringify where))))
  (define (get-results next-row f)
    (let lp ([results '()])
      (match (next-row)
        [#f (reverse results)]
        [,row (lp (cons (f row) results))])))
  (define (row->tr row)
    `(tr ,@(map value->td (vector->list row))))
  (define (value->td v)
    `(td ,(cond
           [(bytevector? v) `(i "Binary data")]
           [(not v) "<null>"]
           [else (stringify v)])))
  (match-let*
   ([,stmt (sqlite:prepare db (format "~a limit ? offset ?" sql))]
    [,_ (sqlite:bind stmt (list limit  offset))]
    [,results (get-results (lambda () (sqlite:step stmt)) row->tr)]
    [,count (length results)])
   (if (= count 0)
       (respond (section "Query finished" `(p ,(home-link sql))))
       (respond
        
        `(table
          (tr (@ (style "text-align: center;"))
            (td (@ (class "navigation"))
              ,(nav-form "Previous Page" (max 0 (- offset limit)) (> offset 0)))
            (td (@ (class "navigation"))
              (form (@ (id "rowForm") (method "get"))
                (textarea (@ (name "sql") (class "hidden")) ,sql)
                (input (@ (name "limit") (class "hidden") (value ,(stringify limit))))
                (input (@ (name "type") (class "hidden") (value ,(stringify type))))
                (button (@ (id "offsetButton") (type "submit")) "Go to row")
                (p (input (@ (id "offsetInput") (name "offset") (class "offset"))))))
            (td (@ (class "navigation"))
              ,(nav-form "Next Page" (+ offset limit) (= count limit)))
            (td (@ (class "link"))
              ,(home-link sql))))
        (section (format "Rows ~d to ~d" (+ offset 1) (+ offset count))
          (match (cons (sqlite:columns stmt) (sqlite:execute stmt '()))
            [(,cols . ,rows) (data->html-table 1 cols rows f)]))))))

(define (make-td c r)
  (let* ([text (format "~a" r)]
        [len (string-length text)])
    (cond
     [(< len 64)
      `(td (@ (class "narrow")) ,text)]
     [(< len 256)
      `(td (@ (class "normal")) ,text)]
     [(< len 512)
      `(td (@ (class "wide")) ,text)]
     [else
      (let ([id (symbol->string (gensym))])
          `(td (@ (class "extra-wide")) (div (@ (class ,(format "elide ~a" c)) (word-break "break-all"))
            (input (@ (class "elide") (id ,id) (type "checkbox") (checked "yes")))
            (label (@ (for ,id) (class "elide")) ,text))))])))
     
(define (data->html-table border columns rows f)
  (define (widths ls-cols)
    (let* ([num-cols (length ls-cols)]
           [min (round (/ (/ 100 num-cols) 2))]
           [max (round (* (/ 100 num-cols) 2))])
      `(@ (max-width ,max) (min-width ,min) )))
     ; `(p ,min "%"))) 
           
  (let ([columns (vector->list columns)])
    `(div (@ (class "dataCont"))
    (table (@ (class "dataTable"))
     (tbody
       (tr ,@(map (lambda (c) `(th  ,c)) columns))
       ,@(map
          (lambda (row)
            `(tr ,@(map make-td columns (apply f (vector->list row)))))
          rows))))))

;;Common helpers
(define (string-param name)
    (let ([value (find-param name)])
      (and value (trim-whitespace value))))

(define (integer-param name min-value)
    (let* ([string-value (find-param name)]
           [number-value (and string-value (string->number string-value))])
      (and string-value
           (if (and (integer? number-value) (>= number-value min-value))
               number-value
               (raise `#(bad-integer-param ,name ,min-value ,number-value))))))

(define (schema->html db-tables)
  (define (db-table->tr table)
    (match table
      [(,name . ,columns)
       (subsection (stringify name)
         `(table ,@(map column->tr columns)))]))
  (define (column->tr column-type)
    (match column-type
      [(,column . ,type)
       `(tr (td ,(stringify column))
          (td ,type))]))
  `(div (@ (class "schema"))
     ,@(map db-table->tr db-tables)))

(define (previous-sql-valid? sql)
   (and sql (not (string=? sql ""))))

;; String manipulation
(define (stringify x) (format "~a" x))

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

(define (containsStr? list x)
    (cond 
        ((null? list) #f)
        ((string-ci=? (car list) x) #t)
        (else (containsStr? (cdr list) x))))

(define (slist->string slst div)
  (cond ((null? slst) "")
        ((null? (cdr slst)) (car slst))
    (else (string-append (car slst)
            div
            (slist->string (cdr slst) div)))))

(define string-replace 
   (lambda (s match replacement)
      (let ((ll (string->list s)))
         (if (= (string-length match) 1)
             (let ((z (map (lambda (x)
                              (if (string-ci=? (stringify x) match)
                                  (string->list replacement)
                                  x))
                           ll)))
                (list->string (flatten z)))
             z))))

(define (flatten list)
   (cond ((null? list) '())
         ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
         (else
          (cons (car list) (flatten (cdr list))))))



