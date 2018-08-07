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
  (respond
   (match reason
     [#(browser-add) (section "Must use desktop app to convert files")]
     [#(empty-dest) (section "Must select a destination to save the newly created database")]
     [#(empty-src) (section "Must select a folder to convert")]
     [#(empty-name) (section "Must enter a file name")]
     [#(file-exists) (section "That file already exists in that folder. Please select a different name or destination folder")]
     [,_ (section "insert failed" `(p ,(exit-reason->english reason)))])))


(define (get-paths)
  (respond
   `(form
     (table (tr (td (@ (style "border: 0px solid;")) (table (tr (th (p "Field")) (th (p "Value")))
                       (tr (td (p "Folder to convert")) (td (p (input (@ (name "folder") (class "path") (type "file") (webkitdirectory) (mozdirectory) (id "folder"))))))
                       (tr (td (p "Destination folder")) (td (p (input (@ (name "dest") (class "path") (type "file") (webkitdirectory) (mozdirectory) (id "dest"))))))
                       (tr (td (p "New file name")) (td (p (textarea (@ (name "name")) ""))))))
              (td (@ (style "border: 0px solid;")) (table (tr (th (@ (style "border: 0px solid; background: #FaFaFa;")) (h1 (@ (style "text-decoration: underline;" )) "Help"))) (tr (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?file=" "Expected file formating"))) (tr (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?setup=" "Generated database setup")))))))
     (input (@ (id "folder-path") (name "folder-path") (class "hidden")))
     (input (@ (id "dest-path") (name "dest-path") (class "hidden")))
     (script "function func(){var x = document.getElementById('folder').files[0].path;
document.getElementById('folder-path').value = x} $('.path').bind('change', func).trigger('change')")
     (script "function func(){var x = document.getElementById('dest').files[0].path;
document.getElementById('dest-path').value = x} $('.path').bind('change', func).trigger('change')")
   (p (button (@ (type "submit")) "Convert")))
   `(p "Depending on folder size the conversion may take a few minutes. If you leave this page the conversion will continue in the background. If you stay on this page you will be notified when the conversion is complete.")))

(define (file-explain)
  (respond `(p "The converter only converts files in the current
directory. Subdirectories are ignored. Therefore, navigate to the folder that contains the log files themselves.")
    `(p "Only files whose file name follow the pattern: \"<name>dd-mm-yyyy HH.MM.SS.log\" are converted. Other files in the folder are ignored.")
    `(p "<name> becomes the name of the table in the database.")
    `(p "The files themselves can contain some header information, then each data entry should start with \"mm/dd/yyyy HH:MM:SS,\" followed by a value.")
    `(table (tr (td (@ (style "border: 0px solid;")) ,(link "converter" "Back"))
              (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?setup=" "Database setup"))))))

(define (setup-explain)
  (respond `(div (@ (style "width: 90%")) (p "There are two types of tables created, data tables and header
    tables. There is only one header table created, called Runs. Each file adds one
    entry to the runs table that describes the information found at
    the top of the runs file. Data tables contain the information
    found in the main body of the log file. There are as many data
    tables created as there are unique convertable file names.")
              (p "Each data table created has four columns, Run number, method, dateTime, and
    desc. Run number is automatically generated and references the
    corresponding entry in the Runs table, allowing the user to determine the header information if necessary.")
              (p "Method is obtained from the header information.")
              (p "dateTime is the value found at the start of each data entry in the log file.")
              (p "desc is everything that follows the dateTime value in the data entry.")
              (p "For instance, the line \"06/19/2018 19:25:58,Run ended.\" would have \"06/19/2018 19:25:58\" as its dateTime value and \"Run ended.\" as its desc."))
    `(table (tr (td (@ (style "border: 0px solid;")) ,(link "converter" "Back"))
              (td (@ (style "border: 0px solid; background: #FaFaFa;")),(link "converter?file=" "Expected file formating"))))))

(define (do-conversion src dest name)
  (unless (not (string=? "undefined" src))
    (raise `#(browser-add)))
  (unless (not (string=? "" src))
    (raise `#(empty-src)))
  (unless (not (string=? "" dest))
    (raise `#(empty-dest)))
  (unless (not (string=? "" name))
    (raise `#(empty-name)))
  
  (let* ([new-file (path-combine dest name)]
         [new-file (string-append new-file ".db3")])
    (unless (not (regular-file? new-file))
      (raise `#(file-exists)))
    (make-db-and-convert src new-file)
    (conversion-complete dest name)))


(define (conversion-complete dest name)
  (let* ([new-file (path-combine dest name)]
         [new-file (string-append new-file ".db3")])
    (respond `(p "Conversion sucessful") (link (format "addDatabase?file-path=~a" (http:percent-encode new-file)) "Add to saved databases"))))


                                        ;Conversion related functions
(define file-name (pregexp "([A-z ]*[0-9]?[A-z ]+)[0-9]+.*\\.log"))
(define (processfile table-name file-path db prepared-insert header-insert)
  (let* ([tx (make-transcoder (latin-1-codec))]
         [ip (open-file-input-port file-path (file-options) (buffer-mode block) tx)]
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
         [(parse-date-line line) =>
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
      (sqlite:execute header-insert (list (get-output-string op)))
      (match (execute-sql db "select last_insert_rowid()")
        [(#(,num)) num]))

    (define (parse-method line)
                                        ;Method = 
      (let ([n (string-length line)])
        (and (>= n 9)
             (string=? (substring line 0 9) "Method = ")
             (substring line 9 (- n 1)))))
    
    (define (reformat date)
      (let ([year (substring date 6 10)]
            [month (substring date 0 2)]
            [day (substring date 3 5)])
        (string-append year "-" month "-" day (substring date 10 (string-length date)))))

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
         [(parse-date-line line) =>
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

(define (fullConvert src-path db)
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
                (begin (processfile short-name path db prepared header-insert)
                       (process-each-file rest existing-tables header-insert))]

               [else ;Need to create table and prepare statment
                (let*  ([table (create-table short-name)]
                        [prepared-insert (sqlite:prepare db (format "insert into ~a ([Run number], Method, dateTime, desc) values (?, ?, ?, ?)" short-name))]
                        [new-table (cons short-name prepared-insert)])
                  (processfile short-name path db prepared-insert header-insert)
                  (process-each-file rest (cons new-table existing-tables)  header-insert))]))
           (process-each-file rest existing-tables header-insert))]
      
      [() "Finished?"]))

  (define (get-name full-name)
    (let ([pattern-match (pregexp-match file-name full-name)])
      (match pattern-match
        [(,full ,name) name]
        (#f #f))))

  (define (create-table table-name)
    (let ([sql (format "create table if not exists ~a ([Run number] integer, Method text, dateTime text, desc text, foreign key([Run number]) references Runs([Unique Run Number]))" table-name)]) 
      (execute-sql db sql)))

  (let ([file-list (list-directory src-path)]
        [header-insert (sqlite:prepare db "insert into Runs ([header contents]) values (?)")])
    (process-each-file file-list '() header-insert)))

(define (set-up-conversion folder db)
  (execute-sql db "create table if not exists  Runs ([Unique Run Number] integer primary key, [header contents] text)")
  (fullConvert folder db))

(define (make-db-and-convert folder db-path)
  (with-db [db db-path (logor SQLITE_OPEN_READWRITE
  SQLITE_OPEN_CREATE)]
    (execute-sql db "begin transaction")
    (set-up-conversion folder db)
    (execute-sql db "end transaction")))

(define (dispatch)
  (let* ([src (string-param "folder-path" params)]
         [dest (string-param "dest-path" params)]
         [name (string-param "name" params)]
         [file (string-param "file" params)]
         [setup (string-param "setup" params)])
    (cond
     [src
      (match (catch (do-conversion src dest name))
        [#(EXIT ,reason) (respond:error reason)]
        [,value value])]
     [file (file-explain)]
     [setup (setup-explain)]
     [else (get-paths)])))

(dispatch)
