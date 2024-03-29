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

(http:include "displayQuery.ss")
(import (helpers))

;; HTTP/HTML responses
(define (get-page-name)
  "Log database query")

(define (respond:error reason sql)
  (respond
   (match reason
     [#(db-query-failed empty-query ,sql)
      (section "No query given" `(p ,sql) (home-link sql))]
     [#(db-query-failed not-a-query ,sql)
      (section "Not a SELECT or EXPLAIN statement" `(p ,sql) (home-link sql))]
     [,_
      (section "Query failed" `(p "Suggestion: Check the current database is the correct database") `(p ,(exit-reason->english reason)) (home-link sql))])))

;; Home page
(define (do-home db last-sql)
  (let ([db-tables (get-db-tables db)])
    (respond
     `(div
       (p "Please enter a SELECT or EXPLAIN statement in SQLite syntax")
       (p "Please see http://www.sqlite.org/lang_select.html for examples")
       (p (i "Note: LIMIT and OFFSET clauses are not allowed."))
       (form (@ (method "get") (class "schema"))
         (input (@ (name "limit") (class "hidden") (value 100)))
         (input (@ (name "offset") (class "hidden") (value 0)))
         (p (textarea (@ (id "sql") (name "sql") (class "sql"))
              ,(or last-sql "")))
         (p (button (@ (type "submit")) "Run Query"))))
     (section "Schema"
       (schema->html db-tables)))))

(define (check-run-query db sql limit offset)
  (define (check-request)
    (cond
     [(string=? sql "") (raise `#(db-query-failed empty-query ,sql))]
     [(or (starts-with-ci? sql "select ")
          (starts-with-ci? sql "with ")
          (starts-with-ci? sql "explain ")
          (starts-with-ci? sql "select* "))
      (if (and limit offset)
          'ok
          (raise `#(db-query-failed missing-limit-offset ,limit ,offset ,sql)))]
     [else (raise `#(db-query-failed not-a-query ,sql))]))
  (check-request)
  (do-query db sql limit offset "" (lambda x x)))

(define (home-link last-sql)
  `(table
    (tr (td (@ (style "border: 0px solid;"))
          (a (@ (href ,(format "saveSearch?sql=~a"
                         (http:percent-encode last-sql)))) "Save Search"))
      (td (@ (style "border: 0px solid; background: #FaFaFa;"))
         (a (@ (href ,(format "adv-search?lastSql=~a"
                        (http:percent-encode last-sql)))) "Edit Search")))))

;; Dispatching requests
(define (dispatch)
  (let ([sql (string-param "sql" params)]
        [last-sql (string-param "lastSql" params)]
        [limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)])
    (unless (user-log-path)
      (respond `(p "Please select a database first")))
    (with-db [db (user-log-path) SQLITE_OPEN_READONLY]
      (if sql
          (match (catch (check-run-query db sql limit offset))
            [#(EXIT ,reason) (respond:error reason sql)]
            [,value value])
          (do-home db last-sql)))))

(dispatch)
