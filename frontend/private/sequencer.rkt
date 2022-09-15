#lang racket/base

(require racket/match)

(provide
 make-sequencer
 sequencer-add!)

(struct sequencer (ht name-proc [names #:mutable] update!-proc))

(define (make-sequencer ht name-proc update!-proc)
  (sequencer ht name-proc null update!-proc))

(define (sequencer-add! s v)
  (match-define (sequencer ht name-proc names update!-proc) s)
  (define name (name-proc v))
  (unless (symbol? name)
    (raise-argument-error 'sequencer-next! "(symbol? (name-proc v))" v))
  (when (memq name names)
    (error 'sequencer-next! "duplicate name ~s" name))
  (define new-names
    (sort (cons name names) symbol<?))
  (set-sequencer-names! s new-names)
  (define new-ids
    (for/hasheq ([name (in-list new-names)]
                 [id (in-naturals)])
      (values name id)))
  (define vs (hash-values ht))
  (hash-clear! ht)
  (for ([v (in-list (cons v vs))])
    (define id (hash-ref new-ids (name-proc v)))
    (update!-proc v id)
    (hash-set! ht id v)))
