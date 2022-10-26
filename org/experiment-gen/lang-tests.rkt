#lang at-exp racket/base

(require "utils.rkt"
         "lifetime-vis.rkt")

(define (id . rest) rest)

@id{Hello World}
