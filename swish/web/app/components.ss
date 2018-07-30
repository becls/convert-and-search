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

(define (redirect url)
  (http:respond op 302 `(("Location" . ,url)) '#vu8()))

(define (non-hosted-page op params page-title heads . content)
  (http:respond op 200 '(("Content-Type" . "text/html"))
    (html->bytevector
     `(html5
       (head
        (meta (@ (charset "UTF-8")))
        (title ,page-title)
        ,(css-include "css/components.css")
        ,(js-include "js/components.js")
        ,@heads)
       (body
        ,@content
        (div (@ (class "undocked menu"))
          ,(link "#Debugging" "Debugging")
          (div (@ (class "menu item"))
            ,(panel "Params" `(p ,(format "~a\n" params))))
          ,(link "#Navigation" "Navigation")
          (div (@ (class "menu item"))
            ,(navigation))))))))

(define (hosted-page page-title heads . content)
  (http:respond op 200 '(("Content-Type" . "text/html"))
    (html->bytevector
     `(html5
       (head
        (meta (@ (charset "UTF-8")))
        (title ,page-title)
        ,(css-include "css/components.css")
        ,(js-include "js/components.js")
        ,@heads)
       (body
        ,(docked-navigation)
        ,(column "content right"
           (apply panel page-title content))
        )))))

;;HTML Helpers
(define (link url anchor)
  `(a (@ (href ,url)) ,anchor))

(define (js-include location)
  `(script (@ (src ,location))))

(define (css-include location)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,location))))

(define (table . content)
  `(table (@ (style "padding: 0; border-spacing: 0;")) ,@content))

;;Page Helpers

(define (active-database)
  `(div (p " ")
    (p "Current Database:")
     (p ,(get-database-name))))

(define (get-database-name)
  (with-db [db (log-path) SQLITE_OPEN_READONLY]
    (let* ([stmt (sqlite:prepare db (format "select name from database where file_path = '~a'" (user-log-path)))]
           [results (sqlite:step stmt)])
      (if results
          `(p ,(format "~a" (car (vector->list results))))
          `(p "None selected")))))

(define (docked-navigation)
  (column-with-id "main-nav" "docked menu left" (navigation)))

(define (navigation)
  (panel (osi_get_hostname)
    (section software-product-name
      (link "saved?type=database&sql=&limit=100&offset=0" "Manage databases")
      (link "saved?type=search&sql=&limit=100&offset=0" "Saved searches")
      (link "search" "Search")
      (link "query-db" "Advanced search")
      (link "converter" "Convert log files")
      (link "/swish/errors?type=child&sql=&limit=100&offset=0" "Debug")
    (active-database))))

(define (stilts height)
  `(div (@ (style ,(format "height:~apx;" height)) (class "stilts"))))

;;Form Helpers
(define (column type . content)
  `(div (@ (class ,(format "~a column" type))) ,@content))

(define (column-with-id id type . content)
  `(div (@ (id ,id) (class ,(format "~a column" type))) ,@content))

(define (panel header . content)
  `(div (@ (class "panel"))
     ;;things to think on: scrolling inside of the panel
     (h2 ,header)
     ,@content))

(define (section header . content)
  `(div (@ (class "section"))
     (h3 ,header)
     ,@content))

(define (subsection header . content)
  `(div
    (h4 ,header)
    ,@content))

(define (row label . content)
  `(div (@ (class "row"))
     (label ,label)
     ,@content))



