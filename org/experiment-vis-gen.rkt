#lang racket

(require racket/string
         racket/draw
         racket/function
         pict)

;; ------------------------------------------------
;; Lifetime Things

(define default-font-color (make-object color% 0 0 0))
;; tab 10 colors
(define color-wheel
  (vector (make-object color% 78  121 167)
          (make-object color% 242 142 43 )
          (make-object color% 225 87  89 )
          (make-object color% 118 183 178)
          (make-object color% 89  161 79 )
          (make-object color% 237 201 72 )
          (make-object color% 176 122 161)
          (make-object color% 255 157 167)
          (make-object color% 156 117 95 )
          (make-object color% 186 176 172)))

(define interp-end #\©)

(define code-string
#<<```
fn foo() {
`ES:0`   let x;
    {
    `AS:0`   let y = 0;
    `AE:0`   x = &`H0:'a` y;
    }
`EE:0`   println!("{}", x);
}
```
  )

(define font-size 18)
(define line-height 40)
(define line-width  500)
(define font-face "Fantasque Sans Mono")

(struct visual-line (style from to))
(struct group (text styles))

(define (parse-source code)
  (define enriched? (lambda (s) (string-contains? s "`")))

  (define (handle-enriched-word word)
    (define make-basic-group
      (lambda (w) (group w `((font . ,font-face)
                        (color . ,default-font-color)))))
    (define has-prefix? (not (string-prefix? word "`")))
    (define has-suffix? (not (string-suffix? word "`")))
    (define split (string-split word "`"))
    (define (make-enriched-group w)
      w)
    (match split
      [(list pref tok suff)
       (list (make-basic-group pref)
             (make-enriched-group tok)
             (make-basic-group suff))]
      [(list pref tok)
       #:when has-prefix?
       (list (make-basic-group pref)
             (make-enriched-group tok))]
      [(list tok suff)
       #:when has-suffix?
       (list (make-enriched-group tok)
             (make-basic-group suff))]
      [(list tok)
       (list (make-enriched-group tok))]))

  (define (handle-any-word word)
    (if (enriched? word)
        (handle-enriched-word word)
        (list word)))

  (for/fold ()
            ([(line ri) (in-indexed (string-split code "\n"))])
    (for/fold (#;[ci 0])
              ([(word ci) (in-indexed (string-split line))])

      (define i (handle-any-word word))

      (values)
      )
    ))

(module+ test)

#;(define (txt msg)
  (define line (blank line-width line-height))
  (define (draw-font m . styles)
    (define style
      (let loop ([sty styles])
        (if (null? sty)
            font-face
            (cons (car sty) (loop (cdr sty))))))
    (text m style font-size))

  (define fontified
    (map (lambda (tok)
           (if (string-contains? tok interp-end)
               (let* ([inners (string-split tok interp-end)]
                      [special-str (car inners)]
                      [special (draw-font special-str (get-color special-str)) ])
                 (apply hc-append (cons special (map draw-font (cdr inners)))))
               (draw-font tok)))
         (string-split msg interp-start)))
  (launder (lc-superimpose line (apply hc-append fontified))))

#;(define (gen-example-lifetime . _)
  (define lines (map txt (string-split code-string "\n")))
  (define base-text (apply vl-append lines))
  (define first-lifetime-bar
    (filled-rectangle
     10
     (* (- (length lines) 1.5)
        line-height)
     #:draw-border? #false
     #:color light-blue))
  (define second-lifetime-bar
    (filled-rectangle
     10
     (* 2.5 line-height)
     #:draw-border? #false
     #:color light-orange))
  (define with-first-lifetime
    (lc-superimpose base-text
                    first-lifetime-bar))
  (define with-second-lifetime
    (pin-over with-first-lifetime
              40
              70
              second-lifetime-bar))
  with-second-lifetime)

;; ------------------------------------------------
;; Receiver / Method Expectations


(define (filled-triangle h base
                         #:draw-border? [db? #true]
                         #:border-width [bw 1]
                         #:border-color [bc "black"]
                         #:color [c "white"])
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-brush (new brush%
                                [style 'solid]
                                [color c]))
        (send dc set-pen (new pen%
                              [width (if db? bw 0)]
                              [color (if db? bc c)]))
        (define path (new dc-path%))
        (send path move-to 0 h)
        (send path line-to (/ base 2) 0)
        (send path line-to base h)
        (send path close)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      base h))


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
         (alter (combine (expected-shape-gen expected exp/color)
                         (actual-shape-gen actual act/color)))))
      (define elem (make-entry))
      (values (cons-if ok? elem allowed)
              (cons-if (not ok?) elem errors)))))


;; ---------------------------------------
;; Main Things

(define rust-T-options '(&T T &mut-T mut-T))
(define rust-T-allowed-receiver-relations
  (list (allowed-receivers '&T     '(&T T &mut-T mut-T))
        (allowed-receivers 'T      '(T mut-T))
        (allowed-receivers '&mut-T '(&mut-T mut-T))
        (allowed-receivers 'mut-T  '(mut-T T))))
#;(module+ test
  (define rust-T-allowed?
    (lambda (recv exp)
      (define p
        (findf (lambda (s) (eq? (allowed-receivers-expected s) exp))
               rust-T-allowed-receiver-relations))
      (member recv (allowed-receivers-allowed p))))

  (define (gen-visual s)
    (define size 30)
    (cond

      ;; - all shapes are square
      ;; - each type gets it's own color
      ;; - darker border is an error
      ;; - blue/gold cyan/magenta combinations are disallowed
      [(eq? s 'square/color)
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
        #:black-list-expected '(mut-T))]

      ;; - expected color (background) is 'color-pop
      ;; - actual color (foreground) is 'color-boring
      ;; - owned values are drawn at scale x1.2
      ;; - mutable values are drawn as rectangles refs as disks
      [(eq? s 'square/circle/size)
       (define color-pop "magenta")
       (define color-boring "gainsboro")
       (define decompose-type
         (lambda (s)
           (define st (symbol->string s))
           (values (string-contains? st "mut")
                   (not (char=? #\& (string-ref st 0))))))
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
        #:black-list-expected '(mut-T))]

      ;; ---------------------------
      ;; lifetime related operations
      [(eq? s 'lifetime) (apply gen-example-lifetime)]

      [else
       (error 'gen-visual "unrecognized data source ~v" s)])))

#;(module+ main
  (send (pict->bitmap (gen-example-lifetime) 'smoothed)
        save-file
        "with-lifetimes.png"
        'png))
