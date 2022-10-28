#lang at-exp racket

(require (for-syntax racket/syntax
                     syntax/parse)
         syntax/parse
         racket/string
         racket/draw
         racket/function
         pict
         images/icons/stickman
         "utils.rkt")

(provide source-block
         define/source
         arr e h c l t
         hash->color
         font-size
         default-font-color)

;; LINKS
;;
;; Using set relationships
;; https://ieeexplore.ieee.org/document/8564143
;; https://csbiology.github.io/CSBlog/posts/4_set_relationships.html

;; Visualizing large sets of data
;; https://www.yworks.com/pages/visualizing-database-relationships

;; ------------------------------------------------
;; Lifetime Things

(define (color/c n)
  (and (< 0 n)
       (< n 11)))

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

;; --------------
;; Custom drawers

(define (life-pen [c default-font-color]) (new pen% [width 1] [color c]))
(define (life-brush c [sty 'transparent]) (new brush% [style sty] [color c]))

(define ((pin-at-row-col glyph-width line-height) base p r c)
    (pin-over base
              (* glyph-width c)
              (* line-height r)
              p))

(define (find-brush-style ls)
    (car (foldl (lambda (v acc)
                  (define cnd (member v brush-styles))
                  (if cnd
                      (cons (car cnd) acc)
                      acc)) '(solid) ls)))

(define (rectangle-drawer pin-at-row-col glyph-width line-height meta)
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

(define (disk-drawer pin-at-row-col glyph-width line-height meta)
  (define c (hash->color (hash-ref meta 'color)))
  (define diameter (hash-ref meta 'width glyph-width))
  (define varargs (hash-ref meta 'varargs '()))
  (define col (hash-ref meta 'col))
  (define row (hash-ref meta 'row))
  (define brush-style (find-brush-style varargs))
  (define p (if (eq? brush-style 'transparent) (life-pen c) (life-pen)))
  (lambda (base)
    (define circ
      (filled-circle
       diameter
       #:brush-style brush-style
       #:color c))
    (pin-at-row-col
     base
     (cc-superimpose
      (blank glyph-width line-height)
      circ)
     row col)))

(define (triangle-drawer pin-at-row-col glyph-width line-height meta)
  (define c (hash->color (hash-ref meta 'color)))
  (define w (hash-ref meta 'width glyph-width))
  (define varargs (hash-ref meta 'varargs '()))
  (define col (hash-ref meta 'col))
  (define row (hash-ref meta 'row))
  (define brush-style (find-brush-style varargs))
  (define p (if (eq? brush-style 'transparent) (life-pen c) (life-pen)))
  (lambda (base)
    (define tri
      (filled-triangle
       w w
       #:brush-style brush-style
       #:color c))
    (pin-at-row-col
     base
     (cc-superimpose
      (blank glyph-width line-height)
      tri)
     row col)))

(define (exit-drawer pin-at-row-col glyph-width line-height meta)
  (define c (hash->color (hash-ref meta 'color)))
  (define col (hash-ref meta 'col))
  (define row (hash-ref meta 'row))
  (lambda (base)
    (define man
      (bitmap
       (running-stickman-icon
        0
        #:height line-height
        #:arm-color c
        #:head-color c
        #:body-color c)))
    (pin-at-row-col
     base
     (cc-superimpose
      (blank glyph-width line-height)
      man)
     row col)))

(define (arrow-drawer pin-at-row-col glyph-width line-height meta)
  (define c (hash->color (hash-ref meta 'color)))
  (define col (hash-ref meta 'col))
  (define row (hash-ref meta 'row))
  (define dx (hash-ref meta 'dx))
  (define dy (hash-ref meta 'dy))
  (define to-find (if (dy . < . 0) cb-find ct-find))
  (define from-find (if (dy . < . 0) ct-find cb-find))
  (lambda (base)
    (define start (blank glyph-width line-height))
    (define end (blank glyph-width line-height))
    (define with-start
      (pin-at-row-col
       base
       start
       row col))
    (define with-both
      (pin-at-row-col
       with-start
       end
       (+ row dy) (+ col dx)))
    (pin-arrow-line 10
                    with-both
                    start from-find
                    end to-find
                    #:color c)))

;; FIXME this got really hacky after the rewrite
(define (render-parsed-source ls)

  (define empty-group
    (group " "
           (make-immutable-hasheq)
           basic-styles))

  ;; XXX Pre-rendering the lines might not work (or perhaps it'll enable)
  ;; adding modifying styles to a line.
  (define rendered-lines
    (map (lambda (l)
            (apply
             hc-append ;; glue word groups together

             (foldr (lambda (g acc)
                      (define t (group-text g))
                      (define f (group-font g))
                      (define c (group-color g))
                      (cons (text t (cons c f)) acc))
                    ;; Keep the list with a space at the end, this
                    ;; way blank lines will get rendered. There are
                    ;; better ways to handle this but :shrug:
                    (list (text " "))
                    l))) ls))

  ;; HACK this gets the actual line-height based on the picture
  ;; height. This is caused by added spacing in the fonts.
  (define line-height (pict-height (car rendered-lines)))
  ;; Yeah, this is also a HACK
  (define glyph-width (/ line-height 2))

  (define pin-at-r/c (pin-at-row-col glyph-width line-height))
  (define (ref-set-member-from-meta meta)
    (define c (hash->color (hash-ref meta 'color default-font-color)))
    (define varargs (hash-ref meta 'varargs '()))
    (void))

  (define drawers
    (for*/fold ([ds '()])
               ([l (in-list ls)]
                [g (in-list l)])
      (define meta (group-meta g))
      (define draw-visual-elem (hash-ref meta 'visual-elem? #false))
      (if draw-visual-elem
          (cons (draw-visual-elem pin-at-r/c glyph-width line-height meta) ds)
          ds)))
  (define base-render
    (apply vl-append rendered-lines))
  ;; TODO add back in pict modifiers so we can apply things like cellophane
  (foldl (lambda (draw-it base)
           (draw-it base)) base-render drawers))

(define basic-styles
    (make-immutable-hasheq `((font . ,default-font)
                             (color . ,default-font-color))))

(define (make-basic-group w)
  (group w (make-immutable-hasheq) basic-styles))

;; ------------------
;; Internal enrichers

(define (e [clr 0])
  (define mta
    (make-immutable-hasheq
     `((visual-elem? . ,exit-drawer)
       (color . ,clr))))
  (group " " mta basic-styles))

(define/contract (h c v)
  (color/c string? . -> . group?)
  (group v (make-immutable-hasheq)
         (hash-set basic-styles 'color
                   (hash->color c))))

(define/contract (c clr #:diameter [w #false] . styles)
  (->* (color/c) (#:diameter integer?) #:rest (listof any/c)
       group?)
  (define mta
    (make-immutable-hasheq
     `((visual-elem? . ,disk-drawer)
       (color . ,clr)
       (varargs . ,styles))))
  (group " " (if w
                 (hash-set mta 'width w)
                 mta) basic-styles))

(define/contract (t clr #:base [w #false] . styles)
  (->* (color/c) (#:base integer?) #:rest (listof any/c)
       group?)
  (define mta
    (make-immutable-hasheq
     `((visual-elem? . ,triangle-drawer)
       (color . ,clr)
       (varargs . ,styles))))
  (group " " (if w
                 (hash-set mta 'width w)
                 mta) basic-styles))

(define/contract (l c h . styles)
  (->* (color/c integer?) #:rest (listof any/c)
       group?)
  (define mta
    (make-immutable-hasheq
     `((visual-elem? . ,rectangle-drawer)
       (color . ,c)
       (length . ,h)
       (varargs . ,styles))))
  (group " " mta basic-styles))

(define/contract (arr dx dy [clr 0] . styles)
  (->* (integer? integer?) (color/c) #:rest (listof any/c)
       group?)
  (define mta
    (make-immutable-hasheq
     `((visual-elem? . ,arrow-drawer)
       (color . ,clr)
       (dx . ,dx)
       (dy . ,dy)
       (varargs . ,styles))))
  (group "" mta basic-styles))

(define (tt word)
  (make-basic-group word))

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


;; -----------------------
;; Main source renderers

(define (source-block #:save-as [sa #false] . args)
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
  (define p (render-parsed-source grouped-groups))
  (when sa
    (send (pict->bitmap p 'smoothed)
          save-file sa 'png))
  p)

(define-syntax (define/source stx)
  (syntax-parse stx
    [(_ name:id srcs ...)
     (with-syntax ([fn (format "~a.png" (syntax-e #'name))])
       #'(define name
           (source-block #:save-as fn srcs ...)))]))
