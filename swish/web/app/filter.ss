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
     (hosted-page "Create a view" 
       (list (css-include "css/confirm-delete.css"))
       c1 c2 ...)]))

(define (respond:error reason)
  (respond
   (match reason
     [,_ (section "Failed to create a view" `(p "It is possible the application does not have permission to edit the database file, possibly because another program is editing the database.")`(p ,(exit-reason->english reason)))])))

(define (instruct sql)
  (respond `(div (@ (style "padding-left:4px;")) (p  "Would you like to create a view out of the results of this search?")
              (p "This is similar to creating a table in the active database from the search results.")
              (p "The selected search is:")
              (p ,sql))
    
    `(form (table (tr
                   (td  (@ (class "nav"))(p (@ (style "font-weight: bold;")) "Name for the view:"))
                   (td (@ (class "nav"))(input (@ (id "viewName") (name "viewName"))))))
       (div (@ (style "padding-left:4px;"))
         (table (tr (td (@ (class "nav")) ,(link "saved?type=search&sql=&limit=100&offset=0" "Cancel"))
                  (td (@ (class "nav"))
                    (input (@ (id "sql") (name "sql") (class "hidden") (value ,sql)))
                    (p (button (@ (type "submit")) "Create view")))))))))

(define (create-view sql viewName)
   (define (quote-identifier s)
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
  (with-db [db (user-log-path) SQLITE_OPEN_READWRITE]
    (execute-sql db (format "create view ~a as ~a" (quote-identifier viewName) sql)))
  (respond `(p "A new view was created") `(div  (@ (style "padding-left:4px;")),(link "search"  "Go to search page"))))


(define (dispatch)
  (let ([create-clicked (string-param "viewName" params)]
        [sql (string-param "sql" params)])
    (if create-clicked
        (match (catch (create-view sql create-clicked))
          [#(EXIT ,reason) (respond:error reason)]
          [,value value])
        (instruct sql))))

(dispatch)
