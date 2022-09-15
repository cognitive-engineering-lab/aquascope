#lang racket/base

(require racket/contract
         racket/string
         racket/port
         json

         "io-spec.rkt"
         "serde.rkt")

;; TODO some of these functions should not block and immediately
;; return to the caller with a future. This can be changed later.

(provide
 (contract-out [aquascope-query (Command? . -> . Result?)]))

(define cargo (find-executable-path "cargo"))

#;(: exec! (->* (String)
                (#:cwd (U False Path) #:noisy Boolean)
                #:rest-star (String)
                Void))
(define (exec! cmd
               #:cwd [cwd #false]
               #:noisy? [noisy? #true]
               #:cleanup [cleanup
                          (lambda (sp in out err)
                            (subprocess-wait sp)
                            (close-input-port out)
                            (close-output-port in)
                            (close-input-port err))] . args)
  (define curr-dir (if cwd cwd (current-directory)))
  (when noisy?
    (printf "Running command: ~a ~a\nFrom directory: ~a\n" cmd (string-join args) curr-dir))
  (define-values (sp out in err)
    (parameterize ([current-directory curr-dir])
      (apply subprocess (list* #false #false #false cmd args))))
  (cleanup sp in out err))

(define (aquascope-query cmd)
  (define general-cleanup
    (lambda (sp in out err)
      (subprocess-wait sp)
      (close-output-port in)
      (eprintf "Query: ~a\n" (port->string err))
      (close-input-port err)

      (define res (read-enum-variant Result out))
      (close-input-port out)
      res))
  ;; TODO make general aquascope backend commands
  (cond [(Command.Source? cmd)
         (apply exec!
                (list* cargo "aquascope" "source"
                       (Command.Source-file cmd)
                       (Command.Source-flags cmd))
                #:noisy? #true
                #:cleanup general-cleanup)]))

(module+ test
  (require rackunit)
  (define test-src "/Users/gavinleroy/dev/prj/aquascope/files/hello/src/main.rs")
  (define my-log (make-logger))

  (check-true (file-exists? test-src)
              "The test file has moved, please readjust the system")

  (parameterize ([current-directory "/Users/gavinleroy/dev/prj/aquascope/files/hello"]
                 [current-logger my-log]
                 [debug? #true])
    (aquascope-query (Command.Source test-src '()))))
