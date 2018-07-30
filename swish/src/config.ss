(library (config)
  (export
   setup-config-db
   user-log-path
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define schema-name 'config)
  (define schema-version "2018-06-21")

  (define (setup-config-db)
    (match (log-db:version schema-name)
      [,@schema-version (create-db)]
      [#f
       (log-db:version schema-name schema-version)
       (create-db)]
      [,version (raise `#(unsupported-db-version ,schema-name ,version))]))    

  (define (create-db)
    (execute "create table if not exists database (name text, description text, file_path text primary key)")
    (execute "create table if not exists search (name text, description text, sqlite text primary key)")
    )

  (define user-log-path (make-parameter #f))
  )
