(import (config))
(http-port-number 54321)
(app-sup-spec
 (make-swish-sup-spec
  (list swish-event-logger
    (<event-logger> make [setup setup-config-db] [log (lambda (e) #f)]))))
(app:start)
