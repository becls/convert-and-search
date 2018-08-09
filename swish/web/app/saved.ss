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

(define (get-page-name)
  (match (get-param "type")
    ["database" "Saved databases"]
    ["search" "Saved searches"]
    [,_ (raise "invalid-type")]))

(define (respond:error reason)
  (respond
   (match reason
     ["invalid-type" (section "Fail" `(p "Invalid type"))]
     [,_ (section "Critical error" `(p ,(exit-reason->english reason)))])))

(define (home-link last-sql)
  (match (get-param "type")
    ["database" `(table
                   (tr (td (@ (style "border: 0px solid; padding-left:0px")) ,(link "addDatabase" "Add database"))
                    (td (@ (style "border: 0px solid;")),(link "confirm-delete?type=dataAll&val=" "Delete all"))))]
    
    ["search" `(table (tr (td (@ (style "border: 0px solid;")) ,(link "confirm-delete?type=searchAll&val=" "Delete all"))))]))

(define (dispatch)
  (let ([limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)]
        [search-sql "Select name, description, sqlite, null as [Edit], null as [Export],null as [Create view], null as [Delete] from search order by name collate nocase"]
        [data-sql "Select name, description, file_path, null as [delete] from database order by name collate nocase"]
        [type (get-param "type")]
        [sql (string-param "sql" params)]
        [search-func (lambda (name desc sqlite edit export view delete)
                       (let ([display-name (if (string=? name "") "()" name)])
                         (list
                          (link (format "query-db?limit=100&offset=0&sql=~a" (http:percent-encode sqlite)) (format "~a" display-name))
                          desc sqlite
                          (link (format "search?edit-sql=~a" (http:percent-encode sqlite))  "Edit")
                          (link (format "export?inst=&sql=~a" (http:percent-encode sqlite))  "Export to Excel")
                          (link (format "filter?sql=~a" (http:percent-encode sqlite))  "Create view")
                          (link (format "confirm-delete?type=search&val=~a" (http:percent-encode sqlite))  "Delete"))))]
        [data-func (lambda (name desc filePath delete)
                     (let ([display-name (if (string=? name "") "()" name)])
                       (list
                        (link (format "updatePath?val=~a" (http:percent-encode filePath)) (format "~a" display-name))
                        desc filePath
                        (link (format "confirm-delete?type=database&val=~a" (http:percent-encode filePath)) "Delete"))))])
    
    
    (let ([sql (if (previous-sql-valid? sql)
                   sql
                   (match type
                     ["database" data-sql]
                     ["search" search-sql]))]
          [func (match type
                  ["database" data-func]
                  ["search" search-func])])
      
      (with-db [db (log-file) SQLITE_OPEN_READONLY]
        (do-query db sql limit offset type func)))))

(dispatch)
