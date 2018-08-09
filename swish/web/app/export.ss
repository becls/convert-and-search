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
     (hosted-page "Export instructions"
       '()
       c1 c2 ...)]))

(define (respond:error reason sql)
  (respond-minimal
   (match reason
     [,_
      (section "Query failed. This is most likely because the active database is different than expected for this search" `(p ,(exit-reason->english reason)))])))

(define-syntax respond-minimal
  (syntax-rules ()
    [(_ c1 c2 ...)
     (hosted-page-minimal ""
      '()
      c1 c2 ...)]))
     
     
(define (hosted-page-minimal page-title heads . content)
  (http:respond op 200 '(("Content-Type" . "text/html"))
    (html->bytevector
     `(html5
       (head
        (meta (@ (charset "UTF-8")))
        (title ,page-title)
        ,@heads)
       (body
        ,(column "content right"
           (apply panel page-title content))
        )))))

(define (instructions sql)
  (respond (section "Follow the instructions below to import the results of your search into Excel."
             `(p "1) Open the data tab in excel.")
             `(p "2) Under the \"get external data\" section there should be an option called \"From Web,\" select this option.")
             `(p "2a) If the button is greyed out, open a new sheet or workbook.")
             `(p "3) Copy the following into the address box:")
             `(p (@ (style "word-wrap:break-word; color: #26d693")) ,(format "http://127.0.0.1:54321/app/export?sql=~a" (http:percent-encode sql)))
             `(p "3a) If the system complains about a long URL, you can shorten it using TinyURL.com or another URL shortener.")
             `(p "4) Hit either enter or the button labeled go to load the specified page.")
             `(p "5) Click the small yellow arrow in the top left corner to select all content on the page.")
             `(p "6) Click import in the bottom right corner.")
             `(p "7) Select where to import the data.")
             `(p "8) The import may take a moment, but your data should load."))))

(define (results sql db)
  (define (get-results next-row f)
    (let lp ([results '()])
      (match (next-row)
        [#f (reverse results)]
        [,row (lp (cons (f row) results))])))
  (define (row->tr row)
    `(tr ,@(map value->td (vector->list row))))
  (define (value->td v)
    `(td ,(cond
           [(bytevector? v) `(i "Binary data")]
           [(not v) "<null>"]
           [else (stringify v)])))
  (define (data->html-table border columns rows)
    (let ([columns (vector->list columns)])
      `(div (@ (class "dataCont"))
         (table (@ (class "dataTable"))
           (tbody
            (tr ,@(map (lambda (c) `(th  ,c)) columns))
            ,@(map
               (lambda (row)
                 `(tr ,@(map make-td columns (vector->list row))))
               rows))))))
  (define (make-td c r)
    (let* ([text (format "~a" r)]
           [len (string-length text)]
           [text (if (starts-with-ci? text "(a (")
                     r
                     text)])
      `(td ,text)))

  
  (match-let*
   ([,stmt (sqlite:prepare db sql)]
    [,results (get-results (lambda () (sqlite:step stmt)) row->tr)]
    [,count (length results)]
    [,flag (string-param "flag" params)]
    [,flag (if flag flag "")]
    [,button (string-param "button" params)])
   (if (= count 0)
       (respond  `(p "No results found. Try changing the current database"))
       (respond-minimal (match (cons (sqlite:columns stmt) (sqlite:execute stmt '()))
                          [(,cols . ,rows) (data->html-table 1 cols rows)])))))


(define (dispatch)
  (let ([sql (string-param "sql" params)]
        [inst (string-param "inst" params)])
    (unless (user-log-path)
      (respond `(p "Please select a database first")))
    (if inst
        (instructions sql)
        (with-db [db (user-log-path) SQLITE_OPEN_READONLY]
        (match (catch (results sql db))
          [#(EXIT ,reason) (respond:error reason sql)]
          [,value value])))))

  (dispatch)
