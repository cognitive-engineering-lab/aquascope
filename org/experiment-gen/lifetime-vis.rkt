#lang at-exp racket

(require (for-syntax racket/syntax)
         racket/string
         racket/draw
         racket/function
         pict
         "utils.rkt")

(provide source-block
         render-code
         hash->color
         default-font-color)

;; LINKS
;;
;; Using set relationships
;; https://ieeexplore.ieee.org/document/8564143
;; https://csbiology.github.io/CSBlog/posts/4_set_relationships.html

;; Visualizing large sets of data
;; https://www.yworks.com/pages/visualizing-database-relationships

;; Regarding Polonius
;; Vytautas often complains about a lack of information,
;; I don't see why we can't create our own set of analyses
;; using DataFrog that computes the things we want (naively).
;; Things like outlives constraints, etc ... can be done
;; separately.

;; ------------------------------------------------
;; Lifetime Things

(define hash->color
  (let* ([color-wheel
          (vector (make-object color% 191 191 191) ;; light gray fo extraneous text
                  ;; tab 10 colors
                  (make-object color% 78  121 167)
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

;; ----------------------
;; Source style modifiers

(define theme 'light #;'dark)
(define font-size 18)
(define default-font (make-object font% font-size "Fantasque Sans Mono" 'modern))
(define font-pnt-size (send default-font get-size #true))
(define default-font-color
  (cond [(eq? theme 'light) (make-object color% 0 0 0)]
        [(eq? theme 'dark) (make-object color% 250 250 250)]))


;; --------------------
;; Data represendations

(struct group (text meta styles) #:transparent)
(define (group-color g) (hash-ref (group-styles g) 'color default-font-color))
(define (group-font g) (hash-ref (group-styles g) 'font default-font))

(struct visual-line (styles from to) #:transparent)

;; ---------
;; Rendering


(struct point (row col)  #:transparent)
(define brush-styles '(transparent
                       solid
                       hilite
                       panel
                       bdiagonal-hatch
                       crossdiag-hatch
                       fdiagonal-hatch cross-hatch
                       horizontal-hatch
                       vertical-hatch))

(define (modify-pict mod pict)
  (cond [(string=? mod "cellophane") (cellophane pict 0.5)]
        [else (printf "[warn] ignoring pict modifier ~v\n" mod)
              pict]))

(define-syntax-rule (cons-if cnd value ls)
        (let ([t (thunk value)])
          (if cnd (cons (t) ls) ls)))

(define (render-parsed-source ls)
  (define (life-pen [c default-font-color]) (new pen% [width 1] [color c]))
  (define (life-brush c [sty 'transparent]) (new brush% [style sty] [color c]))

  ;; XXX Pre-rendering the lines might not work (or perhaps it'll enable)
  ;; adding modifying styles to a line.
  (define rendered-lines
    (map (lambda (l)
            (apply
             hc-append ;; glue word groups together
             (map (lambda (g)
                    (define t (group-text g))
                    (define f (group-font g))
                    (define c (group-color g))
                    (text t (cons c f))) l))) ls))

  ;; HACK get the actual line-height based on the picture
  ;; height. This is caused by added spacing in the fonts.
  (define line-height (pict-height (car rendered-lines)))
  ;; Yeah, this is also a HACK
  (define glyph-width (/ line-height 2))

  ;; --------------------------------
  ;; Visual creators

  (define (find-brush-style ls)
    (car (foldl (lambda (v acc)
                  (define cnd (member v brush-styles))
                  (if cnd
                      (cons (car cnd) acc)
                      acc)) '(solid) ls)))

  (define (pin-at-row-col base p r c)
    (pin-over base
              (* glyph-width c)
              (* line-height r)
              p))

  (define (lifetime-drawer meta)
    (define line-span (hash-ref meta 'length))
    (define c (hash->color (hash-ref meta 'color)))
    (define varargs (hash-ref meta 'varargs '()))
    (define col (hash-ref meta 'col))
    (define row-from (hash-ref meta 'row))
    (define brush-style (find-brush-style varargs))
    (define p (if (eq? brush-style 'transparent) (life-pen c) (life-pen)))
    (lambda (base)
      (pin-at-row-col
       base
       (altered-rectangle
        glyph-width
        (* line-span line-height)
        #:brush-style brush-style
        #:color c)
       row-from
       col)))

  (define (set-member-drawer meta)
    (define c (hash->color (hash-ref meta 'color)))
    (define varargs (hash-ref meta 'varargs '()))
    (define col (hash-ref meta 'col))
    (define row (hash-ref meta 'row))
    (define brush-style (find-brush-style varargs))
    (define p (if (eq? brush-style 'transparent) (life-pen c) (life-pen)))
    (lambda (base)
      (printf "pinning circle at ~v ~v ~v\n" row col brush-style)
      (define circ
        (filled-circle
         glyph-width
         #:brush-style brush-style
         #:color c))
      (pin-at-row-col
       base
       (cc-superimpose
        (blank glyph-width line-height)
        circ)
       row col)))

  (define (ref-set-member-from-meta meta)
    (define c (hash->color (hash-ref meta 'color default-font-color)))
    (define varargs (hash-ref meta 'varargs '()))
    (void))
  (define drawers
    (for*/fold ([ds '()])
               ([l (in-list ls)]
                [g (in-list l)])
      (define meta (group-meta g))
      (define starts-lifetime? (hash-ref meta 'lifetime-start? #false))
      (define loan-member? (hash-ref meta 'loan-elem? #false))
      (define dp (cons-if starts-lifetime? (lifetime-drawer meta) ds))
      (cons-if loan-member? (set-member-drawer meta) dp)))
  (define base-render
    (apply vl-append rendered-lines))
  (foldl (lambda (draw-it base)
           (draw-it base)) base-render drawers))

;; -------
;; Parsing

;; DEPRECATED
(define enriched-wrapper "`")
(define value-sep "~")
(define arg-sep "@")

(define basic-styles
    (make-immutable-hasheq `((font . ,default-font)
                             (color . ,default-font-color))))

(define (make-basic-group w)
  (group w (make-immutable-hasheq) basic-styles))

;; Internal enrichers

(define (h c v)
  (group v (make-immutable-hasheq)
         (hash-set basic-styles 'color
                   (hash->color c))))

(define (c clr #:diameter [w 1] . styles)
  (define mta
    (make-immutable-hasheq
     `((loan-elem? . #true)
       (color . ,clr)
       (varargs . ,styles))))
  (group " " mta basic-styles))

(define (l c h . styles)
  (define mta
    (make-immutable-hasheq
     `((lifetime-start? . #true)
       (color . ,c)
       (length . ,h)
       (varargs . ,styles))))
  (group " " mta basic-styles))

(define (tt word)
  (make-basic-group word))

;; ---

;; DEPRECATED
(define (handle-enriched-word word)
  (define e? (lambda (s) (or (string-contains? s value-sep)
                        (string-contains? s arg-sep))))
  (define split (split-string word "`"))
  (define (make-enriched-group w)
    (define-values (id v)
      (match (split-string w value-sep)
        [(list id) (values id "")]
        [(list id v) (values id v)]))
    (match (split-string id arg-sep)

      [(list "H" c)
       (h (string->number c) v)]
      [(list "L" c h styles ...)
       (apply l (list* (string->number c)
                 (string->number h)
                 styles))]
      [(list "S" clr styles ...)
       (apply c (list* (string->number clr) styles))]

      [else (error 'make-enriched-group "invalid enriched definition" w)]))
  (map (lambda (t)
         ((if (e? t)
              make-enriched-group
              make-basic-group) t)) split))

;; DEPRECATED use at-exp notation source-block instead
(define (parse-source code)
  (define enriched? (lambda (s) (string-contains? s enriched-wrapper)))
  (define (handle-any-word word)
    (if (enriched? word)
        (handle-enriched-word word)
        (list (make-basic-group word))))
  (for/list ([(line ri) (in-indexed (split-string code "\n" ))])
    (for/fold ([ci 0] [ls '()]
               #:result (reverse ls))
              ([g (in-list (handle-any-word line))])
      (define ns (struct-copy group g
                              [meta (hash-set* (group-meta g)
                                               'row ri
                                               'col ci)]))
      (values (+ ci (string-length (group-text g)))
              (cons ns ls)))))

;; -----------------------
;; Expected / Actual panel


;; Going back to overlapping borrows

(define (add-row-col g r c)
  (struct-copy
   group g
   [meta (hash-set*
          (group-meta g)
          'row r
          'col c)]))

(define (group-len g)
  (string-length (group-text g)))

(define (is-newline? g)
  (string=? "\n" (group-text g)))

(define (cons-ll g lss [new-list? #false])
  (if new-list?
      (cons '() lss)
      (let ([last-list (car lss)])
        (cons (cons g last-list) (cdr lss)))))

(define (rev-ll lss)
  (reverse (map reverse lss)))

(define (source-block . args)
  (define grouped-groups
    (for/fold ([ci 0] [ri 0] [ls '(())]
                     #:result (rev-ll ls))
             ([e (in-list args)])

     (define b
       (cond [(group? e) e]
             [(string? e) (tt e)]))
     (define g (add-row-col b ri ci))
     (define next-width (group-len g))
     (define nr (if (is-newline? g) (add1 ri) ri))
     (define nc (if (is-newline? g) 0 (+ next-width ci)))
     (values nc nr (cons-ll g ls (is-newline? g)))))
  (render-parsed-source grouped-groups))

;; Experimenting with at-exp


(define show-loans-4
@source-block{
fn main() {
    @h[3]{@"{"} let y: i32 = 0
      let r: &@c[1] i32
      {
        @h[2]{@"{"} let x: i32 = 5
          r = if false {
            &@l[2 1] x
          } else {
            &@l[3 1] y
          }
      } @h[2]{@"}"}
      println!("r: {}",  r: @c[1] = { @l[2 1 'crossdiag-hatch], @l[3 1] });
  } @h[3]{@"}"}
}

  )

#;(define show-loans-5
  @source-block{

  fn remove_zeros<@h[0]{'a}>(v: &@h[0]{'a} mut Vec<i32>) {
    for t in v.iter() {
      v.remove(0);
    }
  }

}
  )


;; -----------------------
;; Main lifetime renderers

(define-syntax (render-code stx)
  (syntax-case stx ()
    [(_ src)
     (with-syntax ([f (format-id #f "~a.png" #'src)])
       #'(*render-code src (symbol->string 'f)))]
    [(_ src f) #'(*render-code src f)]))

(define (*render-code source fn #:save? [s? #true])
  (define parsed (parse-source source))
  (define p (render-parsed-source parsed))
  (when s?
    (send (pict->bitmap p 'smoothed) save-file fn 'png))
  p)
