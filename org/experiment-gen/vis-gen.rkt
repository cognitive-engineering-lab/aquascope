#lang racket/base

(require (for-syntax "lifetime-vis.rkt")
         "lifetime-vis.rkt"
         "receiver-type-vis.rkt"
         "utils.rkt")

;; ---------------------------------------
;; Main Things

(define (gen-visual s . rest)
  (cond [(eq? s 'receiver) (apply get-representation rest)]
        [(eq? s 'lifetime) (apply (lambda (c) (render-code c)) rest)]
        [else (error "unrecognized visualization type ~v" s)]))
