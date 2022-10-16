#lang racket

(require (for-syntax racket/syntax)
         racket/string
         racket/draw
         racket/function
         pict
         "utils.rkt")

(provide render-code)

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
(define brush-styles '("transparent"
                       "solid"
                       "hilite"
                       "panel"
                       "bdiagonal-hatch"
                       "crossdiag-hatch"
                       "fdiagonal-hatch" "cross-hatch"
                       "horizontal-hatch"
                       "vertical-hatch"))

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
  (define line-height
    (pict-height (car rendered-lines)))


  ;; --------------------------------
  ;; Visual creators

  (define (find-brush-style ls)
    (string->symbol
     (car (foldl (lambda (v acc)
                   (define cnd (member v brush-styles))
                   (if cnd
                       (cons (car cnd) acc)
                       acc)) '("solid") ls))))

  (define (lifetime-drawer meta)
    (define line-span (hash-ref meta 'length))
    (define c (hash->color (hash-ref meta 'color)))
    (define varargs (hash-ref meta 'varargs '()))
    (define col (hash-ref meta 'col))
    (define row-from (hash-ref meta 'row))
    (define brush-style (find-brush-style varargs))
    (define p (if (eq? brush-style 'transparent) (life-pen c) (life-pen)))
    (lambda (base)
      (lc-superimpose
       (foldl modify-pict
              (custom-rectangle
               row-from (+ row-from line-span)
               col col
               base
               #:adjust-by line-height
               #:brush (life-brush c brush-style)
               #:pen p)
              varargs) base)))

  (define (set-member-drawer meta)
    (define c (hash->color (hash-ref meta 'color)))
    (define varargs (hash-ref meta 'varargs '()))
    (define col (hash-ref meta 'col))
    (define row (hash-ref meta 'row))
    (define brush-style (find-brush-style varargs))
    (define p (if (eq? brush-style 'transparent) (life-pen c) (life-pen)))
    (lambda (base)
      (lc-superimpose
       (foldl modify-pict
              (custom-disk
               row col
               (/ line-height 2)
               base
               #:adjust-by line-height
               #:brush (life-brush c brush-style)
               #:pen p) varargs) base)))

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
      [(list "S" c styles ...)
       (define mta
         (make-immutable-hasheq
          `((loan-elem? . #true)
            (color . ,(string->number c))
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
`L@2@5`     let `H@2~x`;
    {
    `L@1@3@cellophane``L@1@1`    let y = 0;
        x = &`H@1~'a` y;
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
;; TODO
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

;; OUTLIVES relations
;; Showing the lifetimes of values, and references show the
;; set of loans that they could point to.
(define simple-lifetime-4
#<<```
fn foo() {
`L@1@7`    let `H@1~v` = 10;
    let mut x: &`H@0~'0` i32 = &`H@0~'1` v; // `H@0~'1` -> {`S@1~ `}
    if false {
    `L@2@4@cellophane``L@2@1`    let `H@2~y` = 0;
        x = &`H@0~'2` y; // `H@0~'2` -> {`S@2~ `}
    }
    // `H@0~'0` -> {`S@1~ `, `S@2~ `}
    println!("{}", x);
}
```
  )

;; An automatically generated version of this would include
;; lots of set relationships at each point. This is also
;; simplified from what the MIR / Polonius would produce so
;; some serious pruning needs to happen.
(define simple-lifetime-4-issue-0
#<<```
fn foo() {
`L@1@11`    let `H@1~v` = 10;
    let mut x: &`H@0~'0` i32 = &`H@0~'1` v; // `H@0~'1` -> {`S@1~ `}
    // `H@0~'1` -> {`S@1~ ` }
    if false {
        // `H@0~'1` -> {`S@1~ ` }
    `L@2@6@cellophane``L@2@1`    let `H@2~y` = 0; // `H@0~'1` -> {`S@1~ `}
        x = &`H@0~'2` y; // `H@0~'2` -> {`S@2~ `}
                   // `H@0~'1` -> {`S@1~ `}
        // `H@0~'0` -> {`S@1~ `, `S@2~ ` }
    }
    // `H@0~'0` -> {`S@1~ `, `S@2~ ` }
    println!("{}", x);
}
```
  )

;; This DOESN'T scale well to showing the information
;; about any rust body.
(define simple-lifetime-4-issue-1
#<<```
fn foo() {
`L@1@14`    let `H@1~v` = 10;
 `L@3@13`   let `H@3~other_value` = 0;
  `L@4@12`  let `H@4~and_another` = 1;
   `L@5@11` let `H@5~yet_another` = 2;
    let mut x: &`H@0~'0` i32 = &`H@0~'1` v; // `H@0~'1` -> {`S@1~ `}
    // `H@0~'1` -> {`S@1~ `}
    if false {
        // `H@0~'1` -> {`S@1~ `}
    `L@2@6@cellophane``L@2@3`    let `H@2~y` = 0; // `H@0~'1` -> {`S@1~ `}
        x = &`H@0~'2` y; // `H@0~'2` -> {`S@2~ `}
                   // `H@0~'1` -> {`S@1~ `}
        // `H@0~'0` -> {`S@1~ `, `S@2~ `}
    }
    // `H@0~'0` -> {`S@1~ `, `S@2~ `}
    println!("{}", x);
}
```
  )

;; TODO what invalidates a loan?
;; where does this information come from?
;; Can we get this from Rustc? (make a reproducable example pls)
(define simple-lifetime-5
#<<```
fn main() {

    let mut x: i32 = 22;

    let mut v: Vec<&'0 i32> = vec![];

    let r: &'1 mut Vec<&'2 i32> = &'3 mut v; // Loan0
    // '3: '1
    // '0: '2
    // '2: '0
    // requires('3, Loan0)

    let p: &'5 i32 = &'4 x; // Loan1
    // '3: '1
    // '0: '2
    // '2: '0
    // '4: '5
    // requires('3, Loan0)
    // requires('4, Loan1)

    r.push(p);
    // '3: '1
    // '0: '2
    // '2: '0
    // '4: '5
    // '5: '2
    // requires('3, Loan0)
    // requires('4, Loan1)

    x += 1; // Loan1 INVAIDATED
    // '3: '1
    // '0: '2
    // '2: '0
    // '4: '5
    // '5: '2
    // requires('3, Loan0)
    // requires('4, Loan1)

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
