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

(http:include "displayQuery.ss")
(import (helpers))

(define (get-page-name)
  "Searching two tables")

(define (respond:error reason)
  (respond
   (match reason
     [no-table (section "Search failed" `(p "You must specify a value for both tables.") `(div (@ (style "padding-left: 7px; padding-top: 3px")) ,(link "twoTableSearch" "Go Back")))]
     [no-join (section "Search failed" `(p "You must specify a value for both join columns.") `(div (@ (style "padding-left: 7px")) ,(link "twoTableSearch" "Go Back")))]
     [,_ (section "Query failed" `(p ,(exit-reason->english reason)) `(div (@ (style "padding-left: 7px")) ,(link "twoTableSearch" "Go Back")))])))

(define (construct-sql table1 table2 join1 join2 newName db)
  ;;In order to increase query readablity for the user, only alias table if necessary
  (let ([table2alias (if (string=? table1 table2)
                         (string-append table2 "2")
                         table2)])
    (define (check-request-blank-vals)
      (cond
       [(or (string=? "(please select a table)" table1)
            (string=? "(please select a table)" table2))
        (raise 'no-table)]
       [(or (string=? "" join1)
            (string=? "" join2))
        (raise 'no-join)]))

    (define (build-join-condition)
      (let ([t1-info (formatCond table1 join1)]
            [t2-info (formatCond table2alias join2)])
        (string-append " WHERE "  t1-info " = " t2-info)))

    (define (build-table-info)
      (string-append " FROM [" table1 "] JOIN [" table2 "] AS " table2alias ))

    (define (build-new-col)
      (string-append (formatCond table1 join1) " AS " (quote-sqlite-identifier newName) ", "))
    
    (check-request-blank-vals)
    (let* ([t1-cols (get-columns table1 table1 db join1)]
           [t2-cols (get-columns table2 table2alias db join2)]
           [all-cols (append t1-cols t2-cols)]
           [all-cols (remq ""  all-cols)]
           [formatted-cols (join all-cols ", ")]
           [new-col (build-new-col)]
           [table-info (build-table-info)]
           [join-cond (build-join-condition)])
      (string-append "SELECT " new-col formatted-cols table-info join-cond))))


(define (formatCond table column)
  (string-append "["  table "].[" column "]"))


(define (get-columns table tableAlias db remove-col)
  (define (format-table-info master-row)
    (match master-row
      [,table-name
       (map (lambda(x) (if (string=? x remove-col)
                           ""
                           (string-append "["  tableAlias "].[" x "]")))
         (map column-info
           (execute-sql db (format "pragma table_info(~a)" (quote-sqlite-identifier table-name)))))]
      [,_ (raise "Invalid-table")]))

  (format-table-info table))

(define (edit-setup sql db)
  (define (get-newName)
    (match (pregexp-match "(?i:as) \"(.*?)\"" sql)
      [(,full . (,val)) val]
      [,_ ""]))
  (initial-setup db (get-bracketed "from " sql) (get-bracketed "join " sql) (get-bracketed "where \\[(?:.*?)]\\." sql) (get-bracketed "= \\[(?:.*?)]\\." sql) (get-newName))) 

;;Initial setup
(define (initial-setup db t1 t2 c1 c2 newName) 
  (let ([db-tables (get-db-tables db)])
    (respond
     (section "Please enter the following fields"
       `(form (@ (method "get") (class "schema"))
          (table
           (tr (@ (style "text-align:center;"))
             (th (p "Field")) (th (p "Value")) (th (p "Notes")))
           (tr (td (p "Table 1")) (td ,(make-table-drop-down db "t1" t1)) (td (p "Required")))
           (tr (td (p "Table 2")) (td ,(make-table-drop-down db "t2" t2)) (td (p "Required")))
           
           (tr (td (p "Join column 1")) (td ,(make-col-drop-downs db-tables "contJ1" "j1" c1)) (td (p "Select table 1 first.") (p "The system combines rows with the same value in this column and join column 2.")))
           (tr (td (p "Join column 2")) (td ,(make-col-drop-downs db-tables "contJ2" "j2" c2)) (td (p "Select table 2 first.")))
           
           (tr (td (p "New name for joined columns")) (td (p (textarea (@ (id "newName") (name "newName") (class "textBox")),newName)))
             (td (p "Since the two join columns contain the same value, only one of them is displayed.")
               (p "This field is the name of that newly created column."))))
          (input (@ (name "limit") (class "hidden") (value 100)))
          (input (@ (name "offset") (class "hidden") (value 0)))
          (input (@ (name "type") (class "hidden") (value "")))
          (input (@ (id "join1Val") (name "join1Val") (class "hidden") (value "")))
          (input (@ (id "join2Val") (name "join2Val") (class "hidden") (value "")))
          (p (button (@ (type "submit")) "Run Search"))
          (p (textarea (@ (id "sql") (name "sql") (class "hidden"))))
          (script "
window.addEventListener('load', joinInitialupdate, false)
var select = document.getElementById('t1');
select.addEventListener('change', updateJoin1, false);")
          
          (script "
var select = document.getElementById('t2');
select.addEventListener('change', updateJoin2, false);")
          (script "$('.j1').bind('change', updateOtherFieldJ1).trigger('change')")
          (script "$('.j2').bind('change', updateOtherFieldJ2).trigger('change')")))
     
     (section "Schema"
       (schema->html db-tables)))))

(define (home-link last-sql)
  `(table
    (tr (td (@ (style "border: 0px solid;"))
          (a (@ (href ,(format "saveSearch?sql=~a"
                         (http:percent-encode last-sql)))) "Save Search"))
      (td (@ (style "border: 0px solid; background: #FaFaFa;"))
         (a (@ (href ,(format "twoTableSearch?edit-sql=~a"
                        (http:percent-encode last-sql)))) "Edit Search"))
      (td (@ (style "border: 0px solid; background: #FaFaFa;"))
        (a (@ (href ,(format "export?inst=&sql=~a" (http:percent-encode last-sql)))) "Export Search")))))

;;Runs each time page loaded, calls initial-setup or do-query
(define (dispatch)
  (let ([table1 (string-param "t1" params)]
        [table2 (string-param "t2" params)]
        [join1 (string-param "join1Val" params)]
        [join2 (string-param "join2Val" params)]
        [newName (string-param "newName" params)]
        [limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)]
        [sql (string-param "sql" params)]
        [edit-sql (string-param "edit-sql" params)])
    (unless (user-log-path)
      (respond `(p "Please select a database first.")))

    (catch (with-db [db (user-log-path) SQLITE_OPEN_READONLY]
                    (cond
                     [(previous-sql-valid? sql) (do-query db sql limit offset "" (lambda x x))]
                     [(and table2 table1)
                      (match (catch
                              (construct-sql table1 table2 join1 join2 (if (string=? newName "") "\"\"" newName) db))
                        [#(EXIT ,reason) (respond:error reason)]
                        [,value (match (catch (do-query db value limit offset "" (lambda x x)))
                                  [#(EXIT ,reason) (respond:error reason)]
                                  [,val val])])]
                     [edit-sql (edit-setup edit-sql db)]
                     [else (initial-setup db "" "" "" "" "")])))))


(dispatch)


