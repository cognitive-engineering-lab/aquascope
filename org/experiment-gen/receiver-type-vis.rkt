#lang racket/base

(require racket/draw
         racket/class
         racket/match
         racket/function
         racket/string
         racket/promise
         pict
         "utils.rkt")

(provide get-representation)

;; ------------------------------------------------
;; Receiver / Method Expectations

;; -----------------------------
;; Data layout & pretty printers

(define-values (write-visual-symbol write-problem-representation)
  (let ([make-write (lambda (mode)
                      (case mode
                        [(#t) write]
                        [(#f) display]
                        [else (lambda (p port) (print p port mode))])) ])
    (values (lambda (vs port mode)
              (define do-write (make-write mode))
              (write-string "Actual: " port)
              (do-write (visual-symbol-actual vs) port)
              (write-string ", Expected: " port)
              (do-write (visual-symbol-expected vs) port)
              (write-string "  " port)
              (do-write (visual-symbol-visual vs) port))
            (lambda (pr port mode)
              (define do-write (make-write mode))
              (define gs (problem-representation-goods pr))
              (define es (problem-representation-bads pr))
              (define space-print (lambda (v)
                                    (write-string "\n" port)
                                    (do-write v port)))
              (write-string "--- Accepted ---" port)
              (for-each  space-print gs)
              (write-string "\n\n--- Rejected ---" port)
              (for-each space-print es)))))

(struct allowed-receivers (expected allowed) #:transparent)

(struct visual-symbol (expected actual visual)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc write-visual-symbol)])

(struct problem-representation (goods bads)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc write-problem-representation)])

;; ---------------------
;; Rust type information

(define rust-T-options '(&T T &mut-T mut-T))
(define rust-T-allowed-receiver-relations
  (list (allowed-receivers '&T     '(&T T &mut-T mut-T))
        (allowed-receivers 'T      '(T mut-T))
        (allowed-receivers '&mut-T '(&mut-T mut-T))
        (allowed-receivers 'mut-T  '(mut-T T))))

(define rust-T-allowed?
    (lambda (recv exp)
      (define p
        (findf (lambda (s) (eq? (allowed-receivers-expected s) exp))
               rust-T-allowed-receiver-relations))
      (member recv (allowed-receivers-allowed p))))

(define (decompose-type s)
    (define st (symbol->string s))
    (values (string-contains? st "mut")
            (not (char=? #\& (string-ref st 0)))))

;; -------
;; Helpers

(define-values (shade lighten)
  (let* ([d 10]
         [alter-tone (lambda (o c)
                       (if (string? c)
                           c
                           (let ([r (send c red)]
                                 [g (send c green)]
                                 [b (send c blue)])
                             (make-object color% (o r d) (o g d) (o b d) 1.0))))])
  (values (curry alter-tone -)
          (curry alter-tone +))))

(define (receiver/expected-options-generator expected-shape-gen
                                             actual-shape-gen
                                             type-options
                                             allowed-recv/exp?
                                             get-color
                                             #:black-list-receivers [ignore-recvs '()]
                                             #:black-list-expected [ignore-exp '()])
  (let* ([cons-if (lambda (cnd e ls) (if cnd (cons e ls) ls))])
    (for*/fold ([allowed '()] [errors  '()]
                              #:result (problem-representation allowed errors))
               ([expected (in-list type-options)]
                #:unless (member expected ignore-exp)
                [actual (in-list type-options)]
                #:unless (member actual ignore-recvs))
      (define ok? (allowed-recv/exp? actual expected))
      (define exp/color (get-color expected))
      (define act/color (get-color actual))
      (define (make-entry [combine cc-superimpose] [alter identity])
        (visual-symbol
         expected actual
         (launder (alter (combine (expected-shape-gen expected exp/color)
                         (actual-shape-gen actual act/color))))))
      (define elem (make-entry))
      (values (cons-if ok? elem allowed)
              (cons-if (not ok?) elem errors)))))

;; -----------------------
;; Current display options
;; TODO can we make these even more concise?

(define size 30)


(define square/square/color
  (delay
    (let ()
      (define scaled-size (* 1.3 size))
      (define colors-blue/gold
        (list
         "White Smoke"
         (make-object color% 113 166 210 1.0)
         (make-object color% 247 203 21 1.0)
         (make-object color% 69 88 66 1.0)))
      (receiver/expected-options-generator
       (lambda (_ c) (filled-rectangle scaled-size scaled-size #:draw-border? #false #:color c))
       (lambda (_ c) (filled-rectangle size size #:draw-border? #false #:color c))
       rust-T-options
       rust-T-allowed?
       (let ([zip (map cons rust-T-options colors-blue/gold)])
         (lambda (s) (cdr (assq s zip))))
       #:black-list-expected '(mut-T)))))

(define square/circle/size
  (delay
    (let ()
      (define color-pop "magenta")
      (define color-boring "gainsboro")
      (define (oval s c)
        (filled-ellipse (* s 2) s #:draw-border? #true #:border-color c #:color c))
      (define (rect s c)
        (filled-rectangle (* s 2) s #:draw-border? #true #:border-color c #:color c))
      (define (sym-draw sym c)
        (define-values (mut? owned?) (decompose-type sym))
        (define s (if owned? (* 1.20 size) size))
        (define drawer (if mut? rect oval))
        (drawer s c))
      (receiver/expected-options-generator
       (lambda (sym _) (sym-draw sym color-pop))
       (lambda (sym _) (sym-draw sym color-boring))
       rust-T-options
       rust-T-allowed?
       (lambda _ #false) ;; XXX color not used
       #:black-list-expected '(mut-T)))))

(define square/triangle/color
  (delay
    (let ()
      (define (sym-draw sym c [l? #false])
        (define-values (mut? owned?) (decompose-type sym))
        (define drawer (if owned? filled-triangle filled-rectangle))
        (define color (if mut? "darkslategray" "gainsboro"))
        (drawer size size
                #:color color
                #:draw-border? #true
                #:border-color color
                #:border-width 3))

      (receiver/expected-options-generator
       (lambda (sym _) (scale (sym-draw sym #f) 1.4))
       (lambda (sym _) (cellophane (sym-draw sym #f) 1.0))
       rust-T-options
       rust-T-allowed?
       (lambda _ #false) ;; XXX color not used
       #:black-list-receivers '(mut-T)
       #:black-list-expected '(mut-T)))))

(define square/hatch/color
  (delay
    (let ()
      (define (sym-draw sym c bs)
        (define-values (mut? owned?) (decompose-type sym))
        (define color (if mut? "firebrick" "slategray"))
        (define shape altered-rectangle)
        (define under (shape size size
                             #:color color
                             #:draw-border? #true
                             #:border-color "white"
                             #:border-width 2
                             #:brush-style 'solid))
        (if owned?
            under
            (launder (cc-superimpose under
                                     (shape
                                      size size
                                      #:color "white"
                                      #:draw-border? #false
                                      #:brush-style bs)))))
      (receiver/expected-options-generator
       (lambda (sym _) (scale (sym-draw sym #f 'fdiagonal-hatch) 1.5))
       (lambda (sym _) (sym-draw sym #f 'fdiagonal-hatch))
       rust-T-options
       rust-T-allowed?
       (lambda _ #false) ;; XXX color not used
       #:black-list-receivers '()
       #:black-list-expected '(mut-T)))))

#;(define rust-T-mut '(mut immut))

(define owned/ref
  (delay
    (let ()
      (define rust-T '(T &T))
      (define (sym-draw sym c [bs 'solid])
        (define color (if (eq? sym 'T) "slategray" "whitesmoke"))
        (define shape (if #false #;(eq? sym 'T) filled-triangle altered-rectangle))
        (shape size size
               #:color color
               #:draw-border? #true
               #:border-color "white"
               #:border-width 2
               #:brush-style bs))
      (receiver/expected-options-generator
       (lambda (sym c) (scale (sym-draw sym c) 1.5))
       (lambda (sym c) (sym-draw sym c))
       rust-T
       (lambda (recv exp)
         (or (and (eq? recv '&T) (eq? exp '&T))
             (and (eq? recv 'T)   (eq? exp '&T))
             (and (eq? recv 'T) (eq? exp 'T))))
       (lambda _ "slategray") ;; XXX color not used
       #:black-list-receivers '()
       #:black-list-expected '(mut-T)))))

(define mut/immut
  (delay
    (let ()
      (define rust-T '(mut immut))
      (define (sym-draw sym c [bs 'solid])
        (define color (if (eq? sym 'mut) #;"firebrick" "slategray" "whitesmoke"))
        (disk size
               #:color color
               #:draw-border? #true
               #:border-color "white"
               #:border-width 2))
      (receiver/expected-options-generator
       (lambda (sym c) (scale (sym-draw sym c) 1.5))
       (lambda (sym c) (sym-draw sym c))

       rust-T
       (lambda (recv exp)
         (or (eq? exp 'immut)
             (eq? recv 'mut)))

       (lambda _ "slategray") ;; XXX color not used
       #:black-list-receivers '()
       #:black-list-expected '(mut-T)))))

(define (visual-symbol-append vs1 vs2)
  (match-define (visual-symbol e1 a1 v1) vs1)
  (match-define (visual-symbol e2 a2 v2) vs2)
  (visual-symbol (symbol-append e1 e2)
                 (symbol-append a1 a2)
                 (launder (hc-append v1 v2))))

(define owned/ref/mut/immut
  (delay
    (let ()
      (define owned-ref (force owned/ref))
      (define mut-immut (force mut/immut))
      (define goods
        (for*/list ([ownr (in-list (problem-representation-goods owned-ref))]
                    [muti (in-list (problem-representation-goods mut-immut))])
          (visual-symbol-append ownr muti)))
      (define bads
        (let ([bb (for*/list ([ownr (in-list (problem-representation-bads owned-ref))]
                              [muti (in-list (problem-representation-bads mut-immut))])
                    (visual-symbol-append ownr muti))]
              [gb (for*/list ([ownr (in-list (problem-representation-goods owned-ref))]
                              [muti (in-list (problem-representation-bads mut-immut))])
                    (visual-symbol-append ownr muti))]
              [bg (for*/list ([ownr (in-list (problem-representation-bads owned-ref))]
                              [muti (in-list (problem-representation-goods mut-immut))])
                    (visual-symbol-append ownr muti))])
          (append bb gb bg)))
      (problem-representation goods bads))))

;; ----------
;; Main entry

(define (get-representation s)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (error (format (string-append "~v:"
                                                   "unknown option ~v")
                                    'show-table-for s)))])
    (force (eval s))))
