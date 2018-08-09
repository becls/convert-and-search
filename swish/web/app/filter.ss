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
     ["Invalid Search"
      (section "Invalid search"
        `(p "The selected search causes an error in the current database. Therefore, you can't create view of the selected search in the current database. Please select a different search or change the current database.")
        `(table
          (tr (td (@ (style "border: 0px solid; padding-left:0px"))
                ,(link "saved?type=database&sql=&limit=100&offset=0" "Change database"))
            (td (@ (style "border: 0px solid;"))
              ,(link "saved?type=search&sql=&limit=100&offset=0" "Back to saved searches")))))]

     [,_ (section "Failed to create a view" `(p "Suggestion: Make sure you have permission to edit this database and that no other programs are editing the database.")`(p ,(exit-reason->english reason)) (link "saved?type=search&sql=&limit=100&offset=0" "Back to saved searches"))])))

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

(define (create-view sql viewName db)
  (execute-sql db (format "create view ~a as ~a" (quote-quote-identifier viewName) sql))
  (respond `(p "A new view was created") `(div  (@ (style "padding-left:4px;")),(link "search"  "Go to search page"))))


(define (dispatch)
  (let ([create-clicked (string-param "viewName" params)]
        [sql (string-param "sql" params)])
    (with-db [db (user-log-path) SQLITE_OPEN_READWRITE]
      (match (catch (execute-sql db sql))
        [#(EXIT ,reason) (respond:error "Invalid Search")]
        [,val (if create-clicked
                  (match (catch (create-view sql create-clicked db))
                    [#(EXIT ,reason) (respond:error reason)]
                    [,value value])
                  (instruct sql))]))))

(dispatch)
