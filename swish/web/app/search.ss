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
  (let ([table (string-param "table" params)])
    (if table
        (string-append "Searching table: " (stringify table))
        "Search")))

(define (respond:error reason)
  (respond
   (match reason
     [no-table (section "Search failed" `(p "You must select a table.") (link "Search" "Go Back"))]
     
     [search-term-or-column-empty (section "Search failed" `(p "If you specify a column, you must specify a search term. Similarly, if you specify a search term you must specify a column.")(link "Search" "Go Back"))]

     [min-or-max-empty (section "Search failed" `(p "You must enter both a min and a max if you want to limit by time.")(link "Search" "Go Back"))]

     [exc-empty (section "Search failed" `(p "If you specify an exclude column, you must specify an exclude term. Similarly, if you specify an exclude term you must specify an exclude column.")(link "Search" "Go Back"))]
     
     [no-timestamp (section "Search failed: no timestamp" `(p "Please select a table with that has a column named timestamp in order to search by timestamp.")(link "Search" "Go Back"))]
     
     [,_
      (section "Query failed" `(p "Suggestion: Check the current database is the correct database") `(p ,(exit-reason->english reason)) (link "Search" "Go Back"))])))


;: SQL helpers
(define (construct-sql  search-table search-column search-term range-min range-max desc db order-col exc-col exc-term)
  (define (check-none-or-both v1 v2)
    (or (and (not (string=? v1 ""))
               (string=? v2 ""))
          (and (string=? v1 "")
               (not (string=? v2 "")))))

    
  (define (check-request-blank-vals)
    (cond
     [(string=? "(please select a table)" search-table)
      (raise `no-table)]
     [(check-none-or-both search-column search-term)
      (raise `search-term-or-column-empty)]
     [(check-none-or-both range-min range-max)
      (raise `min-or-max-empty)]
     [(check-none-or-both exc-col exc-term)
      (raise `exc-empty)]))
  (define (removeTimestamp columns)
    (if (string-ci=? (car columns) "timestamp")
        (cdr columns)
        (cons (car columns) (removeTimestamp (cdr columns)))))
  (define (build-order)
    (let ([order-col (if (string=? order-col "")
                         "1"
                        (string-append "[" order-col "]"))]
          [desc-or-asc (if desc
                           "DESC"
                           "ASC")])
      (string-append " ORDER by " order-col " "  desc-or-asc)))
  (define (build-search-str)
    (if (string=? search-column "")
        ""
        (string-append "[" search-column  "] like (?)")))

  (define (format-cols cols)
    (let ([str (apply string-append
                 (map (lambda (x) (if (string=? "timestamp" x)
                                      "datetime(timestamp/1000, 'unixepoch','localtime') as timestamp, "
                                      (string-append "["x"], ")))
                   cols))])
      (substring str 0 (- (string-length str) 2))))
  
  (define (build-time-range cols)
    (if (string=? range-min "")
        ""
        (if (member "timestamp" cols)
            (string-append "datetime(timestamp/1000,'unixepoch','localtime')"
              "between (?) and (?)")
            (string-append "dateTime between (?) and (?)"))))

  (define (build-except cols)
    (if (string=? exc-col "")
        ""
        (string-append " EXCEPT SELECT " cols " FROM [" search-table "] WHERE [" exc-col "] LIKE (?)"))) 

  (define (has-value str)
    (not (string=? "" str)))

  (check-request-blank-vals)
  
  (let* ([all-cols (get-columns search-table db)]
         [timed? (or (member "timestamp" all-cols) (member "dateTime" all-cols))]
         [time-range (build-time-range all-cols)]
         [where? (if (or (has-value search-column) (has-value range-min)) " WHERE " "")]
         [and? (if (and (has-value search-column) (has-value range-min)) " AND " "")]
         [formated-cols (format-cols all-cols)]
         [except-clause (build-except formated-cols)])
    (if (and (has-value time-range) (not timed?))
        (raise `no-timestamp))

    
    (string-append "SELECT " formated-cols " FROM [" search-table "]" where? (build-search-str) and? time-range except-clause (build-order))))


(define (get-columns table db)
  (define (table-info master-row)
    (match master-row
      [,table-name
       (map column-info
         (execute-sql db (format "pragma table_info(~a)" (quote-sqlite-identifier table-name))))]
      [,_ (raise `Invalid-table)]))
  (table-info table))



(define (edit-setup sql db)
  (define (get-desc)
    (match (pregexp-match "DESC" sql)
      [(,val) #t]
      [,_ #f]))

  (define (get-exc-col)
    (match (pregexp-match "(?i:Except) (?:.*?) (?i:where) \\[(.*?)]" sql)
      [(,full . (,val)) val]
      [,_ ""]))

  (define (get-exc-term)
    (match (pregexp-match "(?i:Except) (?:.*?) (?i:like) \\('(.*?)'" sql)
      [(,full . (,val)) val]
      [,_ ""]))

  
  (initial-setup db "The system filled in what fields it could from the existing query. If your active database is different than the one used to create the search some fields may be blank." (get-bracketed "from " sql) (get-bracketed "where " sql) (get-quoted "like " sql) (get-quoted "between " sql) (get-quoted "and " sql) (get-exc-col) (get-exc-term) (get-bracketed "by " sql) (get-desc)))


;;Initial setup
(define (initial-setup db inst table column search-term min max excCol excTerm order desc)
  
  (let ([db-tables (get-db-tables db)])
    (respond
     (section inst
       `(form (@ (method "get") (class "schema"))
          (table
           (tr
             (th (p "Field")) (th (p "Value")) (th (p "Explination")) (th (p "Example")))
           (tr (td (p "Table")) (td (form ,(make-table-drop-down db "table" table))) (td (p (@ (style "color:red")) "Required") (p "The table to search.")) (td (p "")))
           (tr (td (p "Column")) (td ,(make-col-drop-downs db-tables "container" "cols" column))
             (td (@ (rowspan "2")) (p "Optional, use if you wish to search for a specifc term in a specifc column.") (p "Does an exact match search. You can do a keyword search using % to represent don't care characters.")) (td (@ (rowspan "2")) (p "Selecting \"Desc\" and entering  \"%light curtain%\" will show you all results where the desc column contains the phrase  \"light curtain\".") (p "You can also use % only at the begining or end, for example \"#%\" returns all results that start with #.")))
           (tr (@ (style "background-color: #FaFaFa;")) (td (p "Search term")) (td (p (textarea (@ (id "keyWord") (name "keyWord") (class "textBox")),search-term))))

           (tr (@ (style "background-color: #DDE;")) (td (p "Exclude column")) (td ,(make-col-drop-downs db-tables "excCont" "excCol" excCol))
             (td  (@ (rowspan "2"))(p "Optional, similar to the above two rows, except excludes results from the search that match this condition.") (p "Still shows this column, just not the rows that match the condition.") (p "Same guidelines as above for the keyword.")) (td (@ (rowspan "2")) (p "Selecting \"Method\" and entering  \"Clinical%\" will remove all rows where the method name starts with clinical.")))
           (tr (td (p "Exclude term")) (td (p (textarea (@ (id "execTerm") (name "execTerm") (class "textBox")) ,excTerm))))
           
           (tr (td (p "Minimum date-time"))
             (td (p (textarea (@ (id "min") (name "min") (class "textBox")),min)))
             (td (@ (rowspan "2")) (p "Optional, limits the time range of results shown.") (p "Inclusive")
               (p "Table must include a column labeled eaither \"timestamp\" or \"dateTime\"."))
             (td (@ (rowspan "2"))(p "Formats:  yyyy-mm-dd HH:MM:SS or yyyy-mm-dd.") (p "For example, 2018-06-17 15:38:59")))
           (tr (@ (style "background-color: #FaFaFa;"))
             (td (p "Maximum date-time")) (td (p (textarea (@ (id "max") (name "max") (class "textBox")),max))))
           (tr (@ (style "background-color: #DDE;")) (td (p "Order by")) (td ,(make-col-drop-downs db-tables "order-contain" "orders" order)) (td (p "Optional, defaults to the order entered into the database.")) (td (p ""))) 
           (tr (@ (style "background-color: #FaFaFa;")) (td (p "Desc")) (td (label (@ (class "checkbox-inline"))
                                     (input (@ (name "desc")
                                               (type "checkbox") ,(if desc `(checked)))))) (td (p "If left order by blank, shows most recent first.")) (td (p ""))))
          (input (@ (name "limit") (class "hidden") (value 100)))
          (input (@ (name "offset") (class "hidden") (value 0)))
          (input (@ (name "type") (class "hidden") (value "")))
          (input (@ (id "column") (name "column") (class "hidden") (value "")))
          (input (@ (id "exec") (name "exec") (class "hidden") (value "")))
          (input (@ (id "order") (name "order") (class "hidden") (value "")))
          (p (button (@ (type "submit")) "Run Search"))
          (p (textarea (@ (id "sql") (name "sql") (class "hidden"))))
          
          (script "
window.addEventListener('load', initialupdate, false);
var select = document.getElementById('table');
select.addEventListener('click', updateDrops, false);")
          (script "$('.excCol').bind('change', updateExecVal).trigger('change')")
          (script "$('.cols').bind('change', updateColVal).trigger('change')")
          (script "$('.orders').bind('change', updateOrderVal).trigger('change')")))
     
     (section "Schema"
       (schema->html db-tables)))))

(define (home-link last-sql)
  `(table
    (tr (td (@ (style "border: 0px solid;"))
          (a (@ (href ,(format "saveSearch?sql=~a"
                         (http:percent-encode last-sql)))) "Save Search"))
      (td (@ (style "border: 0px solid; background: #FaFaFa;"))
         (a (@ (href ,(format "search?edit-sql=~a"
                        (http:percent-encode last-sql)))) "Edit Search"))
      (td (@ (style "border: 0px solid; background: #FaFaFa;"))
        (a (@ (href ,(format "export?inst=&sql=~a" (http:percent-encode last-sql)))) "Export Search")))))

(define (do-query-cleanup db sql limit offset type f . bindings)
  (apply do-query db sql limit offset type f (remq "" bindings)))


;;Runs each time page loaded, calls initial-setup or do-query
(define (dispatch)
  (let ([keyword (string-param "keyWord" params)]
        [table (string-param "table" params)]
        [min (string-param "min" params)]
        [max (string-param "max" params)]
        [desc (find-param "desc")]
        [limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)]
        [excludeCol (string-param "exec" params)]
        [excludeTerm (string-param "execTerm" params)]
        [sql (string-param "sql" params)]
        [order-col (string-param "order" params)]
        [edit-sql (string-param "edit-sql" params)])
    (unless (user-log-path)
      (respond `(p "Please select a database first")))

    (match (catch (with-db [db (user-log-path) SQLITE_OPEN_READONLY]
                    (cond
                     [(previous-sql-valid? sql) (do-query db sql limit offset "" (lambda x x))]
                     [table
                      (let ([column (string-param "column" params)])
                        (match (catch
                                (construct-sql table column keyword min max desc db order-col excludeCol excludeTerm))
                          [#(EXIT ,reason) (respond:error reason)]
                          [,value  (do-query-cleanup db value limit offset "" (lambda x x) keyword min max excludeTerm)]))]
                     [edit-sql (edit-setup edit-sql db)]
                     [else (initial-setup db "Please enter the following fields:" "" "" "" "" "" "" ""  "" #t)])))
      [#(EXIT ,reason) (respond:error reason)]
      [,value value])))


(dispatch)


