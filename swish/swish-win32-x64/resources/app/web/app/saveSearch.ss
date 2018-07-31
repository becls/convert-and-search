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

(http:include "components.ss")
(import (helpers))

;; HTML responses
(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
     (hosted-page "Save search" 
       (list (css-include "css/saveSearch.css"))
       c1 c2 ...)]))

(define (respond:error reason)
  (respond
   (match reason
     [,_ (section "insert failed" `(p ,(exit-reason->english reason)))])))

(define (intial-setup)
  (let ([sql (get-param "sql")])
    (respond `(form
               (table
                (tr (th (p "Field")) (th (p "Value")))
                (tr (td (p "Name")) (td (p (textarea (@ (id "name") (name "name") (class "textBox"))))))
                (tr (td (p "Description")) (td (p (textarea (@ (id "desc") (name "desc") (class "desc")))))))
               (p (button (@ (type "submit")) "Save"))
               (input (@ (id "sql") (name "sql") (class "hidden") (value ,sql)))))))

(define (save-query name desc sql)
  (match (db:transaction 'log-db (lambda () (execute  (format "insert into search (name, description, sqlite)
values ('~a', '~a', '~a')" name desc sql))))
    [#(ok ,_) (redirect "saved?type=search&sql=&limit=100&offset=0&flag=Save successful")]
    [,error (respond:error error)]))



(define (dispatch)
  (let ([name (string-param-sql "name" params)]
        [desc (string-param-sql "desc" params)]
        [sql (string-param-sql "sql" params)])
    (if name
        (save-query name desc sql)
        (intial-setup))))

(dispatch)

