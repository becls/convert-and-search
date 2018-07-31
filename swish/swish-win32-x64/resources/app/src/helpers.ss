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
   string-param-sql
   previous-sql-valid?
   stringify
   trim-whitespace
   containsStr?
   slist->string
   string-replace
   flatten
   valid-file)
  (import
   (chezscheme)
   (swish imports))

  ;;Common helpers
  (define (string-param name params)
    (let ([value (http:find-param name params)])
      (and value (trim-whitespace value))))

  (define (string-param-sql name params)
    (let ([val (string-param name params)])
      (if val
          (string-replace val "'" "''")
          val)))

  (define (integer-param name min-value params)
    (let* ([string-value (http:find-param name params)]
           [number-value (and string-value (string->number string-value))])
      (and string-value
           (if (and (integer? number-value) (>= number-value min-value))
               number-value
               (raise `#(bad-integer-param ,name ,min-value ,number-value))))))

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

        (let ((z (map (lambda (x)
                        (if (string-ci=? (stringify x) match)
                            (string->list replacement)
                            x))
                   ll)))
          (list->string (flatten z))))))


  (define (flatten list)
    (cond ((null? list) '())
      ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
      (else
       (cons (car list) (flatten (cdr list))))))


  (define (valid-file file-path)
    (match (catch (read-file file-path))
      [#(EXIT ,details) #f]
      [,value #t]))
  )


