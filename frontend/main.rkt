#lang racket/base

(require racket/rerequire)

(define gui-module "aqualens.rkt")

(struct local-env (dbg? #;...))

(define (loop-it loc-env)
  (dynamic-rerequire gui-module)
  (let/ec quit
    (let loop ([swap-state #false])
      (let* ([aqualens-main (dynamic-require gui-module 'aqualens-main)]
             [debugging? (dynamic-require gui-module 'debug?)]
             [editor-custodian (make-custodian)]
             [next-swap-state
              (parameterize ([current-custodian editor-custodian])
                (call/ec
                 (parameterize ([debugging? (local-env-dbg? loc-env)])
                   (aqualens-main swap-state quit))))])
        (custodian-shutdown-all editor-custodian)
        (dynamic-rerequire gui-module)
        (loop next-swap-state)))))

(module+ main
  ;; Here all of the environment variables should be read
  ;; and passed to aqualens.
  ;; TODO define an input source.
  ;; TODO define a 'profiling? option.
  ;; TODO define a configuruation directory for extensions.
  (define debugging? (getenv "AQUALENS_DEBUG"))
  (loop-it (local-env debugging?))
  (exit))
