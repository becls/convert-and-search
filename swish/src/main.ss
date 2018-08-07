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

(import (config) (helpers) (scheme) (swish imports))
(http-port-number 54321)
(app-sup-spec
 (make-swish-sup-spec
  (list swish-event-logger
    (<event-logger> make [setup setup-config-db] [log (lambda (e) #f)]))))
(match (command-line)
  [("") (swish-start
    (lambda (cmdline)
      (printf "starting as stand-alone application\n")
      (base-dir (path-parent (osi_get_executable_path))) (eval '(import (config) (helpers) (scheme) (swish imports))) 
      (app:start)
      (hook-console-input)
      (new-cafe))
      )]

  [(,me . ,rest)
   (printf "running in debug mode\n")
   (base-dir "..")
   (app:start)
   ((swish-start) (cons "--" rest))
   ])
