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

(library (logConverter)
  (export
   make-db-and-convert
   get-convert-status
   reset-status)
  (import
   (helpers)
   (chezscheme)
   (swish imports))

  (define file-name (pregexp "([A-z]*8?[A-z]*)[0-9]+.*\\.log"))

  ;Used for communication with display
  (define status "")

  (define (get-convert-status)
    status)

  (define (reset-status)
    (set! status ""))


  (define (processfile table-name file-path db prepared-insert header-insert)
    (let* ([tx (make-transcoder (latin-1-codec))]
           [ip (open-file-input-port file-path (file-options) (buffer-mode block) tx)]
           [op (open-output-string)])

      (define (header run-number method)
        (let ([line (get-line ip)])
          (cond
           [(eof-object? line) (complete-header)]
           [(parse-method line) =>
            (lambda (x)
              (match-let* ([,method x])
                (display-string line op)
                (newline op)
                (header run-number method)))]
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
                 (cons (substring line 0 19) (substring line 20 (- n 1))))))

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
           (let* ([short-name (get-name name)]
                  [path (path-combine src-path name)]
                  [prepared (get-second-val short-name existing-tables)])
             (cond [(not short-name) (process-each-file rest existing-tables header-insert)] ;Wrong file format, skip
               [prepared ;Table and prepared statement already created
                (begin (processfile short-name path db prepared header-insert)
                       (process-each-file rest existing-tables header-insert))]

               [else ;Need to create table and prepare statment
                (let*  ([table (create-table short-name)]
                        [prepared-insert (sqlite:prepare db (format "insert into ~a ([Run number], Method, dateTime, desc) values (?, ?, ?, ?)" short-name))]
                        [new-table (cons short-name prepared-insert)])
                  (processfile short-name path db prepared-insert header-insert)
                  (process-each-file rest (cons new-table existing-tables)  header-insert))]))]
          
          [() "Finished?"]))

      (define (get-name full-name)
        (let ([pattern-match (pregexp-match file-name full-name)])
          (match pattern-match
            [(,full ,name) name]
            (#f #f))))

      (define (get-second-val name list)
        (if name
            (match list
              [((,key . ,value) . ,rest)
               (if (string=? name key)
                   value
                   (get-second-val name rest))]
              [() #f]
              [,_ list])
            #f))

      (define (create-table table-name)
        (let ([sql (format "create table if not exists ~a ([Run number] integer, Method text, dateTime text, desc text, foreign key([Run number]) references Runs([Unique Run Number]))" table-name)]) 
          (execute-sql db sql)))

      (let ([file-list (list-directory src-path)]
            [header-insert (sqlite:prepare db "insert into Runs ([header contents]) values (?)")])
        (process-each-file file-list '() header-insert)))

    (define (set-up-conversion folder db)
      (execute-sql db "create table if not exists  Runs ([Unique Run Number] integer primary key, [header contents] text)")
      (fullConvert folder db)
      (set! status "Conversion complete"))

    (define (make-db-and-convert folder db-path)
      (set! status "Running conversion")
      (db:start&link 'new-db db-path 'create)
      (db:stop 'new-db)
      (with-db [db db-path SQLITE_OPEN_READWRITE]
        (execute-sql db "begin transaction")
        (set-up-conversion folder db)
        (execute-sql db "end transaction"))))


