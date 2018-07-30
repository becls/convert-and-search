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

(define-syntax respond2
  (syntax-rules ()
    [(_ c1 c2 ...)
     (hosted-page "Testing"
       (list
        ;; (css-include "css/drop.css") ;Formating
        ;; (js-include "js/colResizable-1.6.min.js") 
        ;; (js-include "js/resize.js")
        (js-include "js/jquery-1.4.4.min.js")
        (js-include "js/drop.js"))
       c1 c2 ...)]))

(define (drop-test)
  (let ([test (string-param "Test" params)])
    (if test
        (let ([other (string-param "abc" params)])
          (respond2 `(p ,other)))
        (respond2
          `(input (@ (id "abba")))
          
          `(script "document.getElementById('abba').value = 'Success (not at spelling success)'")
          `(form (@ (method "get"))
             (select (@ (name "Test") (id "drop"))
               (option "a")
               (option "b")
               (option "c"))
             (div (div (@ (class "container"))
               (div (@ (class "a"))
                 (select (@ (class "test2"))
                  (option "1")
                  (option "2"))))
             (div (@ (class "container"))
               (div (@ (class "b"))
                 (select (@ (class "test2"))
                  (option "3")
                  (option "4"))))
             (div (@ (class "container"))
               (div (@ (class "c"))
                 (select (@ (class "test2") (id "mine"))
                  (option "5")
                  (option "6"))))
               (p (button (@ (type "submit")) "Go")))
             (input (@ (id "abcd") (name "abcd") (value "my val")))
            
             (script "$('div.container').children().hide();
var select = document.getElementById('drop');
;select.addEventListener('change', updateFeild, false);"
               )
             (script "
                        $('.test2').bind('change', updateOtherFeild).trigger('change')"))
         
          
          `(p (@ (id "maker")) "Text")
          ))))


(define (table-test)
 (respond2 
    `(div (@ (class "work"))  (table (@ (id "specialTable"))  (tr (th (p "H1"))  (th (p "header2")))
       (tr (td (@ (width "50%")) (p "Short text"))
         (td (p "also short"))
         (td (p "Much longer text AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAbcd")))))))

(define (basic-test)
  (respond2
   `(input (@ (id "testNot")))
   `(script "document.getElementById('testNot').value = 'WORK'")
   `(input (@ (name "path") (class "path") (type "file") (id "path")))
   `(script "function func(){var x = document.getElementById('path').files[0].path;
document.getElementById('testNot').value = x} $('.path').bind('change', func).trigger('change')")))

(define (run-program)
  (system "cd ../../Python/ZipProcessor/ZipProcessor/; python ZipProcessor.py")
  (respond2 `(p "Here")))
  ;(respond2 `(p "Simple test passed")))

(define (intial)
  (respond2 `(form (input (@ (id "click") (name "click") (class "hidden")))
     (p (button (@ (type "submit")) "Run python")))))

(define (python-test)
  (let ([clicked (string-param "click" params)])
    (if clicked
        (run-program)
        (intial))))


;(table-test)
;(drop-test)
;(basic-test)

;(python-test)

(respond2 `(p ,(user-log-path)))
