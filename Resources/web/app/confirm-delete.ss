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
     (hosted-page "Confirm delete" 
       (list (css-include "css/confirm-delete.css"))
       c1 c2 ...)]))

(define (instruct message value link type)
  (respond message `(p ,value) 
    `(table (tr (td (@ (class "nav")) ,link)
              (td (@ (class "nav")) (form
                                     (input (@ (id "click") (name "click") (class "hidden")))
                                     (input (@ (id "val") (name "val") (class "hidden") (value ,value)))
                                     (input (@ (id "type") (name "type") (class "hidden") (value ,type)))
                                     (p (button (@ (type "submit")) "Delete"))))))))


(define (delete-and-show-confirmation value type)
  (define (return-to-saved)
    (match type
      ["database" (redirect "/app/saved?type=database&sql=&limit=100&offset=0&flag=Delete+Successful")]
      ["dataAll" (redirect "/app/saved?type=database&sql=&limit=100&offset=0&flag=Delete+Successful")]
      ["search" (redirect "/app/saved?type=search&sql=&limit=100&offset=0&flag=Delete+Successful")]
      ["searchAll" (redirect "/app/saved?type=search&sql=&limit=100&offset=0&flag=Delete+Successful")]))

  (define (do-delete)
    (cond [(string=? "dataAll" type)
           (transaction 'log-db (execute "delete from database"))]
      [(string=? "searchAll" type)
       (transaction 'log-db (execute "delete from search"))]
      [else  (transaction 'log-db (execute
                                   (format "delete from ~a where ~a = ~a" type
                                     (match type ["database" "file_path"] ["search" "sqlite"])
                                     (quote-sqlite-identifier value))))]))
  (match (catch (do-delete)) 
    [#(EXIT ,reason) (respond `(p ,reason))]
    [,val (return-to-saved)]))


(define (dispatch)
  (let* ([delete-clicked (string-param "click" params)]
         [value (string-param "val" params)]
         [value (or value "")]
         [type (get-param "type")]
         [link
          (match type
            ["database" (link "saved?type=database&sql=&limit=100&offset=0" "Cancel")]
            ["dataAll" (link "saved?type=database&sql=&limit=100&offset=0" "Cancel")]
            ["search"  (link "saved?type=search&sql=&limit=100&offset=0" "Cancel")]
            ["searchAll"  (link "saved?type=search&sql=&limit=100&offset=0" "Cancel")])]
         [message
          (match type
            ["database" `(p "Are you sure you wish to delete this database?" (br) "The database will not be removed from memory, just from this application.")]
            ["dataAll" `(p "Are you sure you would like to remove ALL databases?")]
            ["search" `(p "Are you sure you want to remove this search?")]
            ["searchAll" `(p "Are you sure you would like to remove ALL saved searches?")])])

    (if delete-clicked
        (delete-and-show-confirmation value type)
        (instruct message value link type))))


(dispatch)
