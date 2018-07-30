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
     (hosted-page "Database changed" 
       '()
       c1 c2 ...)])) 

(define (dispatch)
  (let* ([val (string-param "val" params)]
         [valid (valid-file val)])
    (if valid
        (begin
          (user-log-path val)
          (respond `(p "Active database was changed to:") `(p ,(get-database-name))
            (link "search"  "Go to search page")))
        (respond `(p "This database no longer exists in your computer's memory at the specified location. Remove now?") (link (format "confirm-delete?type=database&val=~a" (http:percent-encode val)) "Delete database")))))


(dispatch)
