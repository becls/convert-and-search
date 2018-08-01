(import (config) (helpers) (scheme) (swish imports))
(http-port-number 54321)
(app-sup-spec
 (make-swish-sup-spec
  (list swish-event-logger
    (<event-logger> make [setup setup-config-db] [log (lambda (e) #f)]))))
;;(app:start)
(match (command-line)
  [("") (swish-start
    (lambda (cmdline)
      (printf "starting as stand-alone application\n")
      (base-dir (path-parent (osi_get_executable_path))) (pretty-print "HERE") (eval '(import (config) (helpers) (scheme) (swish imports))) 
      (app:start)
      (hook-console-input)
      (new-cafe)
    #; (receive))
      )]

                                        ;(base-dir "..") (app:start)]
                                        ;[,_ (swish-start (lambda (command-line) (base-dir (path-parent (osi_get_executable_path))) (pretty-print "HERE") (eval '(import (config) (scheme) (swish imports))) (app:start)))])
  [(,me . ,rest)
   (printf "running in debug mode\n")
   (base-dir "..")
   (app:start)
   ((swish-start) (cons "--" rest))
   ])
