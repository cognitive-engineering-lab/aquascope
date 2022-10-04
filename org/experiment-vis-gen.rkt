#lang racket

(require racket/string
         racket/draw
         racket/function
         pict)

#;(define color-wheel '())

(define interp-start "@{")
(define interp-split "@:")
(define interp-end "@}")

(define code-string
#<<```
fn make_separator(user_str: &@{'a@} str) -> &@{'a@} str {
    if user_str == "" {
        let @{default@} = String::default()
        &@{'b@} default
    } else {
        user_str
    }
}
```
  )

(define light-blue (make-object color% 30 129 176 0.2))
(define light-blue-font (make-object color% 30 129 176 1.0))

(define light-orange (make-object color% 226 135 67 0.2))
(define light-orange-font (make-object color% 226 135 67 1.0))
(define black-color (make-object color% 0 0 0 1.0))

(define get-color
  (let ([lifetime-colors (make-immutable-hash
                          `(("'a" . ,light-blue-font)
                            ("'b" . ,light-orange-font)))])
    (lambda (s) (hash-ref lifetime-colors s black-color))))

(define font-size 18)
(define line-height 40)
(define line-width  500)
(define font-face "Fantasque Sans Mono")

(define (txt msg)
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

(define (gen-example-lifetime . _)
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
;; Color Type Squares

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

(define options '(&T T &mut-T mut-T))
(define colors-cyan/violet
  (list
   "White Smoke"
   "Deep Sky Blue"
   "Medium Orchid"
   "Dark Slate Gray"
   ))
(define colors-blue/gold
  (list
   "White Smoke"
   (make-object color% 113 166 210 1.0)
   (make-object color% 247 203 21 1.0)
   (make-object color% 69 88 66 1.0)))

(define allowed-relations
  '((&T T &mut-T mut-T) ; &T
   (T mut-T) ; T
   (&mut-T mut-T) ; &mut T
   (mut-T T) ; mut T
   ))

(define (list-subtract l1 l2)
  (set->list (set-subtract (list->set l1)
                           (list->set l2))))

(define (gen-squares [scheme 'blue/gold] [invert #false])
  (let* ([colors (eval (string->symbol (string-append "colors-" (symbol->string scheme))))]
         [colors (if invert (reverse colors) colors)]
         [zipped-colors (map list options colors)]
         [get-colors (lambda (syms) (filter-map (lambda (s)
                                             (let ([m-c (assq s zipped-colors)])
                                               (and m-c (cadr m-c)))) syms))]
         [make-entry (lambda (e ec r rc [alter-with identity])
                       (list r e (filled-rectangle
                                  30 30
                                  #:draw-border? #true
                                  #:border-width 8
                                  #:border-color (alter-with ec)
                                  #:color (alter-with rc))))])
    (for/fold ([allowed '()]
               [errors  '()])
              ([p (in-list zipped-colors)]
               #:do [(define expected (car p))
                     (define rec-color (cadr p))
                     (define make-square (curry make-entry expected rec-color))]

               #:unless (eq? expected 'mut-T))
      (define ok-passed (assq expected allowed-relations))
      (define bad-passed (list-subtract options ok-passed))
      (define ok-square (map (lambda (f s) (make-square f s)) ok-passed (get-colors ok-passed)))
      (define bad-square (map (lambda (f s) (make-square f s shade)) bad-passed (get-colors bad-passed)))
      (values (append allowed ok-square #;(map (lambda (t)
                                     (list (car t)
                                           (cadr t)
                                           (cellophane (caddr t) 0.3))) ok-square))
              (append errors bad-square)))))


;; -------------
;; TODO

(module+ test
  (define (get-data s . oths)
    (cond [(eq? s 'squares) (apply gen-squares oths)]
          [(eq? s 'lifetime) (apply gen-example-lifetime oths)]
          [else (raise-argument-error "unrecognized data source" s)])))

#;(module+ main
  (send (pict->bitmap (gen-example-lifetime) 'smoothed)
        save-file
        "with-lifetimes.png"
        'png))
