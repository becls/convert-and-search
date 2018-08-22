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

(define (get-page-name)
  "Convert log files")

(define (respond:error reason)
  (define (back-link)
    `(div (@ (style "padding-left: 5px")) ,(link "converter" "Go back")))
  
  (respond
   (match reason
     [browser-add (section "Must use desktop app to convert files" (back-link))]
     [empty-dest (section "Must select a destination to save the newly created database" (back-link))]
     [empty-src (section "Must select a folder to convert" (back-link))]
     [empty-name (section "Must enter a file name" (back-link))]
     [file-exists (section "That file already exists in that folder. Please select a different name or destination folder" (back-link))]
     [too_many_groups (section "The specifed pattern can contain a maxium of one group" (back-link))]
     [,_ (section "insert failed" `(p ,(exit-reason->english reason)) (back-link))])))


(define (get-paths)
  (respond
   `(form
     (table (tr
             (td (@ (style "border: 0px solid; padding-left: 0px"))
               (table (tr (th (p "Field")) (th (p "Value")) (th (p "Information")))
                 (tr (td (p "Folder to convert")) (td (table (tr (@ (style "background-color: #DDE;"))
                                                               (td (p (input (@ (name "folder") (class "path") (type "button") (value "Choose a folder") (id "folder"))))) (td (p (@ (id "path-name")) "No file selected")))))(td))
                 (tr (td (p "Destination folder"))(td (table (tr (td (p (input (@ (name "dest") (class "path") (type "button") (value "Choose a folder")  (id "dest"))))) (td (p (@ (id "dest-name")) "No file selected"))))) (td))
                 (tr (td (p "New file name")) (td (p (textarea (@ (name "name")) "")))(td))
                 (tr (td (p "Only convert files where "(br) "the filename ends in a date.") (td (@ (style "text-align: center; zoom: 1.25;")) (input (@ (name "datesOnly") (type "checkbox") (checked)))))(td (p "Checking this box ignores files like DeckEditor" (br) "which often have a different format and therefore convert strangely.")))
                 (tr (td (p "Start of line regular expression"))
                   (td (p (textarea (@ (style "width: 90%; padding:0px;")(name "pattern")) "")))
                   (td (p (@ (style "color:red;")) "Important:")
                     (p"Leave blank for \"mm/dd/yyyy HH:MM:SS,\" which is common in biomek logs.")
                     (p "Information on regular expressions can by found at" (br) "http://ds26gte.github.io/pregexp/index.html#node_toc_start")))))
             (td (@ (style "border: 0px solid;")) (table (tr (th (@ (style "border: 0px solid; background: #FaFaFa;")) (h1 (@ (style "text-decoration: underline;" )) "Help"))) (tr (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?file=" "Expected file formatting"))) (tr (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?setup=" "Generated database setup")))))))
     (input (@ (id "folder-path") (name "folder-path") (class "hidden")))
     (input (@ (id "dest-path") (name "dest-path") (class "hidden")))
     (script "var app = require('electron').remote; 
            var dialog = app.dialog;
            var fs = require('fs');
const {basename} = require('path');
document.getElementById('folder').addEventListener('click', function(){
dialog.showOpenDialog({title:\"Select a folder to convert\",
    properties: [\"openDirectory\"]}, function (fileNames){
 document.getElementById('folder-path').value = fileNames;
document.getElementById('path-name').innerHTML = basename(fileNames[0]);})});

document.getElementById('dest').addEventListener('click', function(){
dialog.showOpenDialog({title:\"Select a destination folder\",
    properties: [\"openDirectory\"]}, function (fileNames){
 document.getElementById('dest-path').value = fileNames;
document.getElementById('dest-name').innerHTML = basename(fileNames[0]);})});")

   (p (button (@ (type "submit")) "Convert")))
   `(p (@ (style "padding-left: 5px; padding-top:7px;")) "Depending on folder size the conversion may take a few minutes. If you leave this page the conversion will continue in the background. If you stay on this page you will be notified when the conversion is complete.")))

(define (file-explain)
  (respond (section "Expected file formatting:"
             `(div (@ (style "padding-left: 3px; padding-top: 2px"))
                (p "The converter only converts files in the current
directory. Subdirectories are ignored. Therefore, navigate to the folder that contains the log files themselves.")
                (p "If you convert files that end in a date, only files that end in some combination of digits, periods, and dashes are converted.")
                (P "For example, this mode would convert Details05-22-2018 12.42.04.log but not DeckEditor.log")
                (p "Otherwise all .log files are converted.")
                (p "In either mode, the table name is the part of the filename that is not the ending date.")
                (p "The files themselves can contain some header information, then each data entry should start with either the entered pattern or \"mm/dd/yyyy HH:MM:SS,\" followed by a value.")))
             (section "Entering a start of line pattern:"
               `(div (@ (style "padding-left: 3px; padding-top: 2px"))
                  (p "This allows the user to define what counts as the start of an entry.")
                  (p "The pattern is only matched if it occurs at the start of a line.")
                  (p "If a group is included in the regular expression, the value of that group becomes the value in dateTime.")
                  (p "Otherwise, the entire match becomes dateTime.")
                  (p "Including more than one capturing group will cause an error.")
                  (p "It is possible to enter the regular expression \"(\\d\\d/\\d\\d/\\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d),\" and get the same result as leaving the start of line regular expression blank.")
                  (p "However, this will cause the conversion to take longer.")))
             `(table (tr (td (@ (style "border: 0px solid;")) ,(link "converter" "Back"))
                       (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?setup=" "Database setup"))))))

(define (setup-explain)
  (respond (section "New database setup:" `(div (@ (style "width: 90%; padding-left: 6px;")) (p "There are two types of tables created, data tables and header
    tables. There is only one header table created, called HeaderInfo. Each file adds one
    entry to the HeaderInfo table that describes the information found at
    the top of the given file. Data tables contain the information
    found in the main body of the log file. There are as many data
    tables created as there are unique convertible file names.")
              (p "Each data table created has four columns, Run number, method, dateTime, and
    desc. Run number is automatically generated and references the
    corresponding entry in the HeaderInfo table, allowing the user to determine the header information if necessary.")
              (p "Method is obtained from the header information.")
              (p "dateTime is the value found at the start of each data entry in the log file.")
              (p "desc is everything that follows the dateTime value in the data entry.")
              (p "For instance, the line \"06/19/2018 19:25:58,Run ended.\" would have \"06/19/2018 19:25:58\" as its dateTime value and \"Run ended.\" as its desc."))
    `(table (tr (td (@ (style "border: 0px solid;")) ,(link "converter" "Back"))
              (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?file=" "Expected file formatting")))))))

(define (do-conversion src dest name datesOnly pattern)
  (unless (not (string=? "undefined" src))
    (raise `browser-add))
  (unless (not (string=? "" src))
    (raise `empty-src))
  (unless (not (string=? "" dest))
    (raise `empty-dest))
  (unless (not (string=? "" name))
    (raise `empty-name))
  
  (let* ([new-file (path-combine dest name)]
         [new-file (string-append new-file ".db3")]
         [file-name (if datesOnly
                       (pregexp "(.+?)[-0-9 .]+(?i:\\.log)")
                       (pregexp "(.+?)[-0-9 .]*(?i:\\.log)"))]
         [re-pattern (if (string=? "" pattern)
                         #f
                         (pregexp (string-append "^" pattern)))])
    (unless (not (regular-file? new-file))
      (raise `file-exists))
    (make-db-and-convert src new-file file-name re-pattern)
    (conversion-complete dest name)))


(define (conversion-complete dest name)
  (let* ([new-file (path-combine dest name)]
         [new-file (string-append new-file ".db3")])
    (respond `(p "Conversion sucessful") `(div (@ (style "padding-left: 5px")) ,(link (format "addDatabase?file-path=~a" (http:percent-encode new-file)) "Add to saved databases")))))


;;Conversion related functions
(define (processfile table-name file-path db prepared-insert header-insert pattern)
  (let* ([ip (open-file-to-read file-path)]
         [op (open-output-string)])

    (define (header run-number method)
      (let ([line (get-line ip)])
        (cond
         [(eof-object? line) (complete-header)]
         [(parse-method line) =>
          (lambda (method)
            (display-string line op)
            (newline op)
            (header run-number method))]
         [(parse-start-line-pattern line) =>
          (lambda (x)
            (let ([run-number (complete-header)])
              (match-let* ([(,date . ,desc) x])
                (display-string desc op)
                (seen-date date run-number method))))]
         [else
          (display-string line op)
          (newline op)
          (header run-number method)])))

    (define (complete-header)
      (sqlite:execute header-insert (list table-name (get-output-string op)))
      (match (execute-sql db "select last_insert_rowid()")
        [(#(,num)) num]))

    (define (parse-method line)
      ;;Method = 
      (let ([n (string-length line)])
        (and (>= n 9)
             (string=? (substring line 0 9) "Method = ")
             (substring line 9 (- n 1)))))
    
    (define (reformat date)
      (let ([year (substring date 6 10)]
            [month (substring date 0 2)]
            [day (substring date 3 5)])
        (string-append year "-" month "-" day (substring date 10 (string-length date)))))

    (define (parse-start-line-pattern line)
      (if pattern
          (parse-pattern line)
          (parse-date-line line)))

    (define (parse-pattern line)
      (match (pregexp-match-positions pattern line)
        [((,start . ,end) (,mstart . ,mend)) (cons (substring line mstart mend) (substring line (+ 1 end) (- (string-length line) 1)))]
        [((,start . ,end)) (cons (substring line start end) (substring line end (- (string-length line) 1)))]
        [#f #f]
        [,_ (raise `too_many_groups)]))

    (define (parse-date-line line)
      ;; mm/dd/yyyy HH:MM:SS,
      (let ([n (string-length line)])
        (and (>= n 20)
             (eqv? (string-ref line 2) #\/)
             (eqv? (string-ref line 5) #\/)
             (eqv? (string-ref line 10) #\space)
             (eqv? (string-ref line 13) #\:)
             (eqv? (string-ref line 16) #\:)
             (eqv? (string-ref line 19) #\,)
             (char-numeric? (string-ref line 0))
             (char-numeric? (string-ref line 1))
             (char-numeric? (string-ref line 3))
             (char-numeric? (string-ref line 4))
             (char-numeric? (string-ref line 6))
             (char-numeric? (string-ref line 7))
             (char-numeric? (string-ref line 8))
             (char-numeric? (string-ref line 9))
             (char-numeric? (string-ref line 11))
             (char-numeric? (string-ref line 12))
             (char-numeric? (string-ref line 14))
             (char-numeric? (string-ref line 15))
             (char-numeric? (string-ref line 17))
             (char-numeric? (string-ref line 18))
             (cons (reformat (substring line 0 19)) (substring line 20 (- n 1))))))

    (define (seen-date date run-number method)
      (let ([line (get-line ip)])
        (cond
         [(eof-object? line) (complete-line date run-number method)]
         [(parse-start-line-pattern line) =>
          (lambda (x)
            (complete-line date run-number method)
            (match-let* ([(,date . ,desc) x])
              (display-string desc op)
              (seen-date date run-number method)))]
         [else
          (newline op)
          (display-string line op)
          (seen-date date run-number method)])))

    (define (complete-line date run-number method)
      (sqlite:execute prepared-insert (list run-number method date (get-output-string op))))
    
    (on-exit (close-input-port ip)
      (header -1 ""))))

(define (fullConvert src-path db file-name pattern)
  (define (process-each-file remaining-files existing-tables header-insert)
    (match remaining-files
      [((,name . ,num) . ,rest)
       (if (= num 1)
           (let* ([short-name (get-name name)]
                  [path (path-combine src-path name)]
                  [pair (assoc short-name existing-tables)]
                  [prepared (match pair
                              [#f #f]
                              [(,key . ,value) value])])
             (cond [(not short-name) (process-each-file rest existing-tables header-insert)] ;Wrong file format, skip
               [prepared ;Table and prepared statement already created
                (begin (processfile short-name path db prepared header-insert pattern)
                       (process-each-file rest existing-tables header-insert))]

               [else ;Need to create table and prepare statment
                (let*  ([table (create-table short-name)]
                        [prepared-insert (sqlite:prepare db (format "insert into ~a ([Run number], Method, dateTime, desc) values (?, ?, ?, ?)" (quote-sqlite-identifier short-name)))]
                        [new-table (cons short-name prepared-insert)])
                  (processfile short-name path db prepared-insert header-insert pattern)
                  (process-each-file rest (cons new-table existing-tables)  header-insert))]))
           (process-each-file rest existing-tables header-insert))]
      
      [() "Finished?"]))

  (define (get-name full-name)
    (let ([pattern-match (pregexp-match file-name full-name)])
      (match pattern-match
        [(,full ,name) name]
        (#f #f))))

  (define (create-table table-name)
    (let ([sql (format "create table if not exists ~a ([Run number] integer, Method text, dateTime text, desc text, foreign key([Run number]) references HeaderInfo([Unique Run Number]))" (quote-sqlite-identifier table-name))]) 
      (execute-sql db sql)))

  (let ([file-list (list-directory src-path)]
        [header-insert (sqlite:prepare db "insert into HeaderInfo ([Corresponding table], [header contents]) values (?, ?)")])
    (process-each-file file-list '() header-insert)))

(define (set-up-conversion folder db file-name pattern)
  (execute-sql db "create table if not exists HeaderInfo ([Unique Run Number] integer primary key,[Corresponding table] text, [header contents] text)")
  (fullConvert folder db file-name pattern))

(define (make-db-and-convert folder db-path file-name pattern)
  (with-db [db db-path (logor SQLITE_OPEN_READWRITE
  SQLITE_OPEN_CREATE)]
    (execute-sql db "begin transaction")
    (set-up-conversion folder db file-name pattern)
    (execute-sql db "end transaction")))

(define (dispatch)
  (let* ([src (string-param "folder-path" params)]
         [dest (string-param "dest-path" params)]
         [name (string-param "name" params)]
         [file (string-param "file" params)]
         [setup (string-param "setup" params)]
         [datesOnly (string-param "datesOnly" params)]
         [pattern (string-param "pattern" params)])
    (cond
     [src
      (match (catch (do-conversion src dest name datesOnly pattern))
        [#(EXIT ,reason) (respond:error reason)]
        [,value value])]
     [file (file-explain)]
     [setup (setup-explain)]
     [else (get-paths)])))

(dispatch)
