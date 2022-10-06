#lang racket

(require (for-syntax racket/syntax)
         racket/string
         racket/draw
         racket/function
         pict)

;; ------------------------------------------------
;; Lifetime Things

;; tab 10 colors
(define hash->color
  (let* ([color-wheel
          (vector (make-object color% 78  121 167)
                  (make-object color% 242 142 43 )
                  (make-object color% 225 87  89 )
                  (make-object color% 118 183 178)
                  (make-object color% 89  161 79 )
                  (make-object color% 237 201 72 )
                  (make-object color% 176 122 161)
                  (make-object color% 255 157 167)
                  (make-object color% 156 117 95 )
                  (make-object color% 186 176 172))]
         [maxl (vector-length color-wheel)])
    (lambda (n)
      (when (and (0 . <= . n)
                 (n . >= . maxl))
        (raise-argument-error 'hash->color
                              (format "expected value in range ~v ~v" 0 maxl)
                              n))
      (vector-ref color-wheel n))))

(define simple-lifetime-0
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

(define theme 'light)
(define font-size 18)
(define default-font-color
  (cond [(eq? theme 'light) (make-object color% 0 0 0)]
        [(eq? theme 'dark) (make-object color% 250 250 250)]))
(define default-font
  (make-object font% font-size "Fantasque Sans Mono" 'modern))

(struct visual-line (styles from to) #:transparent)
(struct group (text meta styles) #:transparent)

(define (group-color g) (hash-ref (group-styles g) 'color default-font-color))
(define (group-font g) (hash-ref (group-styles g) 'font default-font))
(define (visual-line-color vl) (hash-ref (visual-line-styles vl) 'color))
(define (visual-line-brush vl) (hash-ref (visual-line-styles vl) 'brush))
(define (visual-line-pen vl) (hash-ref (visual-line-styles vl) 'pen))

(define-syntax (render-code stx)
  (syntax-case stx ()
    [(_ src)
     (with-syntax ([f (format-id #f "~a.png" #'src)])
       #'(*render-code src (symbol->string 'f)))]
    [(_ src f) #'(*render-code src f)]))

(define (*render-code source fn #:save? [s? #true])
  (define p (render-parsed-source (parse-source source)))
  (when s?
    (send (pict->bitmap p 'smoothed) save-file fn 'png))
  p)

(define (render-parsed-source ls)
  (struct point (row col))
  (define default-pen (new pen% [width 1] [color default-font-color]))
  (define (expected-brush c) (new brush% [style 'fdiagonal-hatch] [color c]))
  (define (actual-brush c) (new brush% [style 'solid] [color c]))
  (define (collapse-into-lines asps aeps esps eeps)
    (define make-visual-line
      (lambda (brsh pen v s e)
        (visual-line (make-immutable-hasheq `((brush . ,(brsh (hash->color v)))
                                              (pen . ,pen)))
                     s e)))
    (define actual-lines
      (for/list ([(v start) (in-hash asps)])
        (define end (hash-ref aeps v))
        (make-visual-line actual-brush default-pen v start end)))
    (define expected-lines
      (for/list ([(v start) (in-hash esps)])
        (define end (hash-ref eeps v))
        (make-visual-line expected-brush default-pen v start end)))
    (append expected-lines actual-lines))
  (define lines
    (for*/fold ([asps (make-immutable-hash)]
                [aeps (make-immutable-hash)]
                [esps (make-immutable-hash)]
                [eeps (make-immutable-hash)]
                #:result (collapse-into-lines asps aeps esps eeps))
               ([l (in-list ls)]
                [g (in-list l)]
                [(k v) (in-hash (group-meta g))])
      (define with-point
        (lambda (hsh)
          (define m (group-meta g))
          (hash-set hsh v (point (hash-ref m 'row) (hash-ref m 'col)))))
      (cond [(eq? 'actual-start k)
             (values (with-point asps) aeps esps eeps)]
            [(eq? 'actual-end k)
             (values asps (with-point aeps) esps eeps)]
            [(eq? 'expected-start k)
             (values asps aeps (with-point esps) eeps)]
            [(eq? 'expected-end k)
             (values asps aeps esps (with-point eeps))]
            [else (values asps aeps esps eeps)])))
  (define base-render
    (apply vl-append
           (map (lambda (l)
                  (apply hc-append
                         (map (lambda (g)
                                (define t (group-text g))
                                (define f (group-font g))
                                (define c (group-color g))
                                (text t (cons c f))) l))) ls)))
  (define (draw-visual-line vl base)
    (match-define (point row-from col-from) (visual-line-from vl))
    (match-define (point row-to col-to) (visual-line-to vl))
    (define brush (visual-line-brush vl))
    (define pen (visual-line-pen vl))
    ;; HACK using the 'font-size as an adjuster is very crude.
    ;; It kind of works but you can tell it's off. Additionally
    ;; manually drawing the box with 'dc is hacky especially
    ;; since I use the same width and height as the base pict, then
    ;; the pen is moved further to the right.
    (define lifetime
      (let* ([adjust-c (/ font-size 2)]
             [adjust-r font-size])
        (dc (λ (dc dx dy)
              (define old-brush (send dc get-brush))
              (define old-pen (send dc get-pen))
              (send dc set-brush brush)
              (send dc set-pen pen)
              (define path (new dc-path%))
              (send path move-to
                    (* adjust-c col-from)
                    (* adjust-r row-from))
              (send path line-to
                    (* adjust-c col-to)
                    (+ (* adjust-r row-to) adjust-r))
              (send path line-to
                    (+ adjust-c (* adjust-c col-to))
                    (+ (* adjust-r row-to) adjust-r))
              (send path line-to
                    (+ adjust-c (* adjust-c col-from))
                    (* adjust-r row-from))
              (send path close)
              (send dc draw-path path dx dy)
              (send dc set-brush old-brush)
              (send dc set-pen old-pen))
            (pict-width base) (pict-height base))))
    (lc-superimpose base (cellophane lifetime 0.5)))

  (foldl draw-visual-line base-render lines))

(define (parse-source code)
  (define enriched? (lambda (s) (string-contains? s "`")))
  (define basic-styles
    (make-immutable-hasheq `((font . ,default-font)
                             (color . ,default-font-color))))
  (define make-basic-group (lambda (w) (group w (make-immutable-hasheq) basic-styles)))
  (define (handle-enriched-word word)
    (define has-prefix? (not (string-prefix? word "`")))
    (define has-suffix? (not (string-suffix? word "`")))
    (define split (string-split word "`"))
    (define (make-enriched-group w)
      (match-define (list id v) (string-split w ":"))
      (define-values (t mta ss)
        (cond [(char=? #\H (string-ref id 0))
               (values v (make-immutable-hasheq)
                       (hash-set basic-styles
                                 'color (hash->color (string->number (substring id 1)))))]
              [else
               (define make-it (lambda (id) (make-immutable-hasheq `((,id . ,(string->number v))))))
               (define mta
                 (match id
                   ["AS" (make-it 'actual-start)]
                   ["AE" (make-it 'actual-end)]
                   ["ES" (make-it 'expected-start)]
                   ["EE" (make-it 'expected-end)]
                   [else (error "in token" word "malformed tag" id)]))
               (values " " mta basic-styles)]))
      (group t mta ss))
    (match split
      [(list pref tok suff)
       #:when (and has-prefix? has-suffix?)
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
        (list (make-basic-group word))))
  (for/list ([(line ri) (in-indexed (string-split code "\n"))])
    (for/fold ([col-pnt 0]
                [current-line '()]
                #:result (reverse current-line))
                ([word (in-list (add-between (string-split line " " #:trim? #false) " " ))])
                (define gs (handle-any-word word))
                (define gls
                  (map (curry + col-pnt)
                       (cons 0 (map (compose string-length group-text)
                                    gs))))
                (define w/pnts
                  (map (lambda (g ci)
                         (struct-copy group g
                                      [meta (hash-set* (group-meta g)
                                                      'row ri
                                                      'col ci)]))
                       gs (take gls (length gs))))

                (values (last gls)
                        (append (reverse w/pnts) current-line)))))

(module+ test)

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

