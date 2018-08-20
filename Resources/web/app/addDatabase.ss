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

(import (helpers))
(http:include "components.ss")

;; HTML responses
(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
     (hosted-page "Add a new database"
       (list (css-include "css/saveSearch.css")
         (js-include "js/jquery-1.4.4.min.js"))
       c1 c2 ...)]))

(define (respond:error reason)
  (respond
   (match reason
     ["not-database" (section "insert failed" `(p "Invalid file type") `(p "valid file types are: .db, .db3, .sqlite"))]
     ["browser-add" (section "Must use desktop app to add databases")]
     [,_ (section "Insert failed" `(p "It is possible you tried to add the same database twice") `(p ,(exit-reason->english reason)))])))

(define (initial-setup)
  (respond (section "Add a new database" `(form
             (div (@ (style "padding-left: 8px; padding-top: 3px; padding-bottom: 5px;"))(table
              (tr (th (p "Field")) (th (p "Value")))
              (tr (td (p "Name")) (td  (table (tr (@ (style "background-color: #DDE;")) (td (@ (style "border-width: 0px; padding: 0px;")) (textarea (@ (id "name") (name "name") (class "textBox")))) (td (@ (style "border-width: 0px; padding: 0px"))(p "Optional, defaults to filename"))))))
              (tr (td (p "Description")) (td (p (textarea (@ (id "desc") (name "desc") (class "desc"))))))
              (tr (td (p "File")) (td (table (tr (@ (style "background-color: #DDE;"))(td (p (input (@ (name "path") (class "path") (type "button") (value "Choose a database") (id "path"))) (td (p (@ (id "filename")) "No file choosen"))))))))))
             (input (@ (id "file-path") (name "file-path") (class "hidden")))
             (script "var app = require('electron').remote; 
            var dialog = app.dialog;
            var fs = require('fs');
const {basename} = require('path');
document.getElementById('path').addEventListener('click', function(){
dialog.showOpenDialog({title: 'Choose database'}, function (fileNames){
document.getElementById('file-path').value = fileNames[0];
document.getElementById('filename').innerHTML = basename(fileNames[0]);})
});")
             (p (button (@ (type "submit")) "Save"))))))

(define (initial-nofile file)
  (respond (section "Add the newly created database" `(form
             (table
              (tr (th (p "Field")) (th (p "Value")))
              (tr (td (p "Name")) (td (table (tr (@ (style "background-color: #DDE;")) (td (@ (style "border-width: 0px; padding: 0px;")) (textarea (@ (id "name") (name "name") (class "textBox"))))
                                              (td (@ (style "border-width: 0px; padding: 0px"))(p "Optional, defaults to filename"))))))
              (tr (td (p "Description")) (td (p (textarea (@ (id "desc") (name "desc") (class "desc")))))))
             (input (@ (id "file-path") (name "file-path") (class "hidden") (value ,file)))
             (p (button (@ (type "submit")) "Save"))))))

(define (update-path name desc file)
  (define (get-file-name)
    (if (string=? "" name)
        (match (pregexp-match "\\\\([-\\w \\.,]*?\\..*)$" file)
          [(,full . (,val)) val]
          [,_ ""])
        name))
  
  (unless (not (string=? "undefined" file))
    (raise "browser-add"))
  (unless (or (ends-with-ci? file ".db3")
              (ends-with-ci? file ".db")
              (ends-with-ci? file ".sqlite"))
    (raise "not-database"))
  (match (catch (transaction 'log-db (execute "insert into database (name, description, file_path)
values (?, ?, ?)" (get-file-name) desc file)))
    
    [#(EXIT ,reason)  (respond:error reason)]
    [,val (begin (user-log-path file) (redirect "saved?type=database&sql=&limit=100&offset=0&flag=Save+successful,+active+database+changed."))]))


(define (dispatch)
  (let ([name (string-param "name" params)]
        [desc (string-param "desc" params)]
        [file (string-param "file-path" params)])
    (cond [name
           (match (catch (update-path name desc file))
             [#(EXIT ,reason) (respond:error reason)]
             [,value value])]
      [file (initial-nofile file)]
      [else (initial-setup)])))

(dispatch)
