#lang racket

(require (for-syntax racket/syntax)
         racket/string
         racket/draw
         racket/function
         pict
         "utils.rkt")

(provide render-code)

;; ------------------------------------------------
;; Lifetime Things

(define hash->color
  (let* ([color-wheel ;; tab 10 colors
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
(define brush-styles '("transparent"
                       "solid"
                       "hilite"
                       "panel"
                       "bdiagonal-hatch"
                       "crossdiag-hatch"
                       "fdiagonal-hatch" "cross-hatch"
                       "horizontal-hatch"
                       "vertical-hatch"))

(define (draw-visual-line vl base)
  (match-define (point row-from col-from) (visual-line-from vl))
  (match-define (point row-to col-to) (visual-line-to vl))
  (define meta (visual-line-styles vl))
  (define brush (hash-ref meta 'brush))
  (define pen (hash-ref meta 'pen))
  (define mods (hash-ref meta 'varargs '()))
  (define lifetime
    (custom-rectangle
     row-from row-to
     col-from col-to
     base
     #:adjust-by font-size
     #:brush brush
     #:pen pen))
  (define styled
    (foldl (lambda (m p)
             (cond [(string=? m "cellophane")
                    (cellophane p 0.5)]
                   [else
                    (printf "[warn] ignoring pict modifier ~v\n" m)
                    p])) lifetime mods))
  (lc-superimpose styled base))

(define (render-parsed-source ls)
  (define (life-pen [c default-font-color]) (new pen% [width 1] [color c]))
  (define (life-brush c [sty 'transparent]) (new brush% [style sty] [color c]))
  (define (visual-line-from-meta meta)
    (define line-span (hash-ref meta 'length))
    (define c (hash->color (hash-ref meta 'color default-font-color)))
    (define varargs (hash-ref meta 'varargs '()))
    (define col (hash-ref meta 'col))
    (define row-from (hash-ref meta 'row))
    (define brush-style ;; FIXME pretty crude
      (string->symbol
       (car (foldl (lambda (v acc)
                     (define cnd (member v brush-styles))
                     (if cnd
                         (cons (car cnd) acc)
                         acc)) '("solid") varargs))))
    (define p
      (if (eq? brush-style 'transparent)
          (life-pen c)
          (life-pen)))
    (visual-line
     (make-immutable-hasheq
      `((brush . ,(life-brush c brush-style))
        (pen . ,p)
        (varargs . ,varargs)))
     (point row-from col)
     (point (+ row-from line-span) col)))
  (define lines
    (for*/fold ([lines '()])
               ([l (in-list ls)]
                [g (in-list l)])
      (define meta (group-meta g))
      (define starts-lifetime? (hash-ref meta 'lifetime-start? #false))
      (if (not starts-lifetime?)
          lines
          (cons (visual-line-from-meta meta) lines))))

  (define base-render
    (apply
     vl-append ;; glue lines together
     (map (lambda (l)
            (apply
             hc-append ;; glue word groups together
             (map (lambda (g)
                    (define t (group-text g))
                    (define f (group-font g))
                    (define c (group-color g))
                    (text t (cons c f))) l))) ls)))
  ;; draw all lifetime lines to the base
  (foldl draw-visual-line base-render lines))

;; -------
;; Parsing

(define enriched-wrapper "`")
(define value-sep "~")
(define arg-sep "@")

(define basic-styles
    (make-immutable-hasheq `((font . ,default-font)
                             (color . ,default-font-color))))

(define (make-basic-group w)
  (group w (make-immutable-hasheq) basic-styles))

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
         (group v (make-immutable-hasheq)
                (hash-set basic-styles 'color (hash->color (string->number c))))]
        [(list "L" c h styles ...)
         (define mta
           (make-immutable-hasheq
            `((lifetime-start? . #true)
              (color . ,(string->number c))
              (length . ,(string->number h))
              (varargs . ,styles))))
         (group v mta basic-styles)]
        [else (error 'make-enriched-group "invalid enriched definition" w)]))
    (map (lambda (t)
           ((if (e? t)
                make-enriched-group
                make-basic-group) t)) split))

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


;; ----------------------
;; Sample pieces of code

;; --- Annotation Guide ---
;;   NOTE lifetimes do not replace space in the provided text string.
;; - (L color height ...) lifetime
;; - (H color ...) highlight text

(define simple-lifetime-0
#<<```
fn foo() {
`L@0@5@crossdiag-hatch`    let x;
    {
    `L@0@1`    let y = 0;
        x = &`H@0~'a` y;
    }
    println!("{}", x);
}
```
  )

;; using cross-hatch as expectation
;; no mut / immut distinction made
(define simple-lifetime-1
#<<```
fn foo() {
`L@0@5`    let mut `H@0~s` = String::from("hello");
 `L@1@2`   let r1 = &`H@1~'a` s;
  `L@2@1`  let r2 = &`H@2~'b` s;
    assert!(compare_strings(r1, r2));
   `L@3@1` let r3 = &`H@3~'c` mut s;
    clear_string(r3);
}
```
  )

;; using cellophane as expectation
;; using cross-hatch as ref
(define simple-lifetime-2
#<<```
fn foo() {
`L@0@0@cellophane`  `L@0@0`    let mut `H@0~s` = String::from("hello");
`L@0@2@crossdiag-hatch@cellophane`  `L@0@2@crossdiag-hatch` `L@1@2@crossdiag-hatch`   let r1 = &`H@1~'a` s;
    `L@2@1@crossdiag-hatch`  let r2 = &`H@2~'b` s;
      assert!(compare_strings(r1, r2));
`L@0@2@crossdiag-hatch@cellophane`  `L@0@2@transparent`   `L@3@2` let r3 = &`H@3~'c` mut s;
      println!("{}", s);
      clear_string(r3);
}
```
  )


;; lifetimes with error : read after mutable ref
(define simple-lifetime-3
#<<```
fn foo() {
   let mut `H@0~s` = String::from("hello");
    let r1 = &`H@1~'a` s;
    let r2 = &`H@2~'b` s;
    assert!(compare_strings(r1, r2));
    let r3 = &`H@3~'c` mut s;
    println!("{}", s);
    clear_string(r3);
}
```
  )

(define simple-lifetime-4
#<<```
fn foo() {

    let mut x: &'0 i32 = &'1 10; L0
    '1: '0
    '1 { L0 }

    if false {

        let y = 0;

        x = &'2 y; L1
        '2: '0
        '2 { L1 }

    }
    println!("{}", x);
}
```
  )

(define simple-lifetime-5
#<<```
fn main() {

  let mut x: i32 = 22;

  let mut v: Vec<&'0 i32> = vec![];

  let r: &'1 mut Vec<&'2 i32> = &'3 mut v;

  let p: &'5 i32 = &'4 x;

  r.push(p);

  x += 1;

  take::<Vec<&'6 i32>>(v);

}
```
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
