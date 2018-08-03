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
     [#(no-table) (section "Search failed" `(p "You must select a table") (link "Search" "Go Back"))]
     
     [#(search-term-or-column-empty) (section "Search failed" `(p "If you specify a column, you must specify a search term. Similarly, if you specify a search term you must specify a column")(link "Search" "Go Back"))]

     [#(min-or-max-empty) (section "Search failed" `(p "You must enter both a min and a max if you want to limit by time")(link "Search" "Go Back"))]

     [#(exc-empty) (section "Search failed" `(p "If you specify an exclude column, you must specify an exclude term. Similarly, if you specify an exclude term you must specify an exclude column")(link "Search" "Go Back"))]
     
     [#(no-timestamp) (section "Search failed: no timestamp" `(p "Please select a table with that has a column named timestamp in order to search by timestamp")(link "Search" "Go Back"))]
     
     [,_
      (section "Query failed" `(p ,(exit-reason->english reason)) (link "Search" "Go Back"))])))


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
      (raise `#(no-table))]
     [(check-none-or-both search-column search-term)
      (raise `#(search-term-or-column-empty))]
     [(check-none-or-both range-min range-max)
      (raise `#(min-or-max-empty))]
     [(check-none-or-both exc-col exc-term)
      (raise `#(exc-empty))]))
  (define (removeTimestamp columns)
    (if (string-ci=? (car columns) "timestamp")
        (cdr columns)
        (cons (car columns) (removeTimestamp (cdr columns)))))
  (define (build-order all-cols)
    (let ([order-col (if (string=? order-col "")
                         "ROWID"
                         order-col)]
          [desc-or-asc (if desc
                           "DESC"
                           "ASC")])
      (string-append "ORDER by [" order-col "] "  desc-or-asc)))
  (define (build-search-str)
    (if (string=? search-column "")
        #f
        (string-append "[" search-column  "] like ('" search-term "')")))

  (define (build-time-range cols)
    (if (string=? range-min "")
        #f
        (if (containsStr? cols "timestamp")
            (string-append "strftime('%m/%d/%Y %H:%M:%S', timestamp/1000,'unixepoch','localtime')"
              "between ('" range-min "') and ('" range-max "')")
            (string-append "dateTime between ('" range-min "') and ('" range-max"')"))))

  (check-request-blank-vals)
  (let* ([all-cols (get-columns search-table db)]
         [ls (cons (build-order all-cols) '())]
         [time-range (build-time-range all-cols)]
         [ls (if time-range
                 (cons time-range ls)
                 ls)]

         [search-str (build-search-str)]
         [ls (cond [(and search-str time-range)
                    (let ((temp (cons "and" ls)))
                      (cons search-str temp))]
               [search-str (cons search-str ls)]
               [else ls])]
         
         [ls (if (or time-range search-str)
                 (cons "where" ls)
                 ls)]
         
         [ls (cons (string-append "[" search-table "]") ls)]
         [ls (cons "from" ls)]
         [timestamp?  (if (containsStr? all-cols "timestamp")
                          #t
                          #f)]
         [timed? (if (or timestamp? (containsStr? all-cols "dateTime"))
                     #t
                     #f)]
         [columns (if timestamp?
                      (removeTimestamp all-cols)
                      all-cols)]
         [formated-columns (format-cols columns)]
         [ls (cons formated-columns ls)]
         [ls (if timestamp?
                 (cons "select strftime('%m/%d/%Y %H:%M:%S', timestamp/1000,'unixepoch','localtime') as timestamp," ls)
                 (cons "select" ls))])
    
    (if (and time-range (not timed?))
        (raise `#(no-timestamp)))
    (slist->string ls " ")))

(define (remove-tags val)
  (match val
    [#(,table-name)
     (string->symbol table-name)]))

(define (get-columns table db)
  (define (table-info master-row)
    (match master-row
      [,table-name
       (map stringify (map column-info
                        (execute-sql db (format "pragma table_info(~s)" table-name))))]
      [,_ (raise `#(Invalid-table))]))
  (define (column-info table-info)
    (match table-info
      [#(,_ ,name ,type ,_ ,_ ,_)
       (string->symbol name)]))
  (table-info table))

(define (format-cols cols)
  (let* ([string (slist->string cols "], [")]
         [with-start-and-end (string-append "[" string "]")])
    with-start-and-end))

(define (edit-setup sql db)
  (define (get-bracketed priorKeyword)
    (match (pregexp-match  (format "~a \\[(.*?)]" priorKeyword) sql)
      [(,full . (,val)) val]
      [,_ #f]))

  (define (get-quoted priorKeyword)
    (match (pregexp-match (format "~a \\('(.*?)'" priorKeyword) sql)
      [(,full . (,val)) val]
      [,_ ""]))

  (define (get-desc)
    (match (pregexp-match "DESC" sql)
      [(,val) #t]
      [,_ #f]))
  ; (respond `(p ,sql)))          
  (intial-setup db "The system filled in what fields it could from the existing query. If your active database is different than the one used to create the search some fields may be blank" (get-bracketed "from") (get-bracketed "where") (get-quoted "like") (get-quoted "between") (get-quoted "and") (get-bracketed "by") (get-desc)))


;;Intial setup
(define (intial-setup db inst table column search-term min max order desc)
  (define (table-info master-row)
    (match master-row
      [#(,table-name)
       (cons
        (string->symbol table-name)
        (map column-info
          (execute-sql db (format "pragma table_info(~s)" table-name))))]))
  (define (column-info table-info)
    (match table-info
      [#(,_ ,name ,type ,_ ,_ ,_)
       (cons (string->symbol name) type)]))
  
  (define (make-table-drop-down)
    (let ((tables (map remove-tags (execute-sql db
                                     "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view"))))
      `(select (@ (name "table") (id "table"))
         (option (@ (style "color: grey")) "(please select a table)") ;;Consider: changing to blank option
         ,@(map (lambda (c) `(option ,(if (and table (string=? table (stringify c))) `(@ (selected "selected"))) ,(stringify c))) tables))))

  (define (make-col-drop-downs db-tables cont-name drop-name)
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
         `(option ,(stringify column))]))
    `(div
      ,@(map db-table->selection db-tables)))
  
  
  (let ([db-tables
         (map table-info
           (execute-sql db
             "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view"))])
    (respond
     (section inst
       `(form (@ (method "get") (class "schema"))
          (table
           ;Consider using rowspan or background color to make row pairings clearer
           (tr
             (th (p "Field")) (th (p "Value")) (th (p "Explination")) (th (p "Example")))
           (tr (td (p "Table")) (td (form ,(make-table-drop-down))) (td (p (@ (style "color:red")) "Required") (p "The table to search")) (td (p "")))
           (tr (td (p "Column")) (td ,(make-col-drop-downs db-tables "container" "cols"))
             (td (@ (rowspan "2")) (p "Optional, use if you wish to search for a specifc term in a specifc column") (p "Does an exact match search. You can do a keyword search using % to represent don't care characters")) (td (@ (rowspan "2")) (p "Selecting \"Desc\" and entering  \"%light curtain%\" will show you all results where the desc column contains the phrase  \"light curtain\".") (p "You can also use % only at the begining or end, for example \"#%\" returns all results that start with #")))
           (tr (@ (style "background-color: #FaFaFa;")) (td (p "Search term")) (td (p (textarea (@ (id "keyWord") (name "keyWord") (class "textBox")),search-term))))

           (tr (@ (style "background-color: #DDE;")) (td (p "Exclude column")) (td ,(make-col-drop-downs db-tables "excCont" "excCol"))
             (td  (@ (rowspan "2"))(p "Optional, similar to the above two rows, except excludes results from the search that match this condition") (p "Still shows this column, just not the rows that match the condition") (p "Same guidelines as above for the keyword")) (td (@ (rowspan "2")) (p "Selecting \"Method\" and entering  \"Clinical%\" will remove all rows where the method name starts with clinical")))
           (tr (td (p "Exclude term")) (td (p (textarea (@ (id "execTerm") (name "execTerm") (class "textBox"))))))
           
           (tr (td (p "Minimum date-time"))
             (td (p (textarea (@ (id "min") (name "min") (class "textBox")),min)))
             (td (@ (rowspan "2")) (p "Optional, limits the time range of results shown") (p "Inclusive")
               (p "Table must include a column labeled eaither \"timestamp\" or \"dateTime\""))
             (td (@ (rowspan "2"))(p "Formats:  mm/dd/yyyy HH:MM:SS or mm/dd/yyyy") (p "For example, 07/13/2018 15:38:59")))
           (tr (@ (style "background-color: #FaFaFa;"))
             (td (p "Maximum date-time")) (td (p (textarea (@ (id "max") (name "max") (class "textBox")),max))))
           (tr (@ (style "background-color: #DDE;")) (td (p "Order by")) (td ,(make-col-drop-downs db-tables "order-contain" "orders")) (td (p "Optional, defaults to the order entered into the database")) (td (p ""))) 
           (tr (@ (style "background-color: #FaFaFa;")) (td (p "Desc")) (td (label (@ (class "checkbox-inline"))
                                     (input (@ (name "desc")
                                               (type "checkbox") ,(if desc `(checked)))))) (td (p "If left order by blank, shows most recent first")) (td (p ""))))
          (input (@ (name "limit") (class "hidden") (value 100)))
          (input (@ (name "offset") (class "hidden") (value 0)))
          (input (@ (name "type") (class "hidden") (value "")))
          (input (@ (id "column") (name "column") (class "hidden") (value "")))
          (input (@ (id "exec") (name "exec") (class "hidden") (value "")))
          (input (@ (id "order") (name "order") (class "hidden") (value "")))
          (p (button (@ (type "submit")) "Run Search"))
          (p (textarea (@ (id "sql") (name "sql") (class "hidden"))))
          (script "$('div.container').children().hide();
var select = document.getElementById('table');
select.addEventListener('change', updateColumnSearch, false);")
          
          (script "$('div.excCont').children().hide();
var select = document.getElementById('table');
select.addEventListener('change', updateColumnExc, false);")
          
          (script "$('div.order-contain').children().hide();
var select = document.getElementById('table');
select.addEventListener('change', updateColumnOrder, false);")
          (script "$('.excCol').bind('change', updateOtherExecCol).trigger('change')")
          (script "$('.cols').bind('change', updateOtherFeildSearch).trigger('change')")
          (script "$('.orders').bind('change', updateOtherFeildOrder).trigger('change')"))
       )
     
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


;;Runs each time page loaded, calls intial-setup or do-query
(define (dispatch)
  (let ([keyword (string-param-sql "keyWord" params)]
        [table (string-param "table" params)]
        [min (string-param-sql "min" params)]
        [max (string-param-sql "max" params)]
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
                        (match (catch (construct-sql table column keyword min max desc db order-col excludeCol excludeTerm))
                          [#(EXIT ,reason) (respond:error reason)]
                          [,value (do-query db value limit offset "" (lambda x x))]))]
                     [edit-sql (edit-setup edit-sql db)]
                     [else (intial-setup db "Please enter the following fields" #f "" "" "" "" "" #t)]))))))


(dispatch)


