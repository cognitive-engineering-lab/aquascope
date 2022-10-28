#lang at-exp racket

(require pict
         "lifetime-vis.rkt"
         "utils.rkt")

;; ----------------------
;; Sample pieces of code

;; TODO all previous attempts that were using the embedded string
;; style with `render-code` are not longer valid. If they want to
;; be used again translate them to using the at-exp notation.

;; --- Annotation Guide --- DEPRECATED
;;   NOTE lifetimes do not replace space in the provided text string.
;; - (L color height ...) lifetime
;; - (H color ...) highlight text

#;(define simple-lifetime-0
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
#;(define simple-lifetime-1
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
#;(define simple-lifetime-2
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
#;(define simple-lifetime-3
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
#;(define simple-lifetime-4
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
#;(define simple-lifetime-4-issue-0
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
#;(define simple-lifetime-4-issue-1
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
#;(define simple-lifetime-5
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

;; Outlives constraints. Danglinging pointers. Etc
;; Progression of show-loans-0-1

#;(define show-loans-0-0
  #<<```
// Goal: detect overlapping borrows
//
// iter :: <'a, T>(&'a self) -> Iterator<Item = 'a T>
//
fn remove_zeros(v: &'`S@1~ ` mut Vec<i32>) {

  for (t: (&'`S@3~ ` i32), i) in (&'`S@2~ ` v).iter().enumerate().rev() {

    if *t == 0 {

      (&'`S@4~ ` mut v).remove(i);

    }

  }

}
```
  )

#;(define show-loans-0-1
  #<<```
// Goal: detect overlapping borrows
//
// iter :: <'a, T>(&'a self) -> Iterator<Item = 'a T>
//
fn remove_zeros(v: &'`S@1~ ` mut Vec<i32>) {

  for (t: (&'`S@3~ ` i32), i) in (&'`S@3~ ` v).iter().enumerate().rev() {

    if *t == 0 {

      (&'`S@4~ ` mut v).remove(i);

    }

  }

}
```
  )

#;(define panel-show-loans-0-1
  (let* ([make-set (lambda (n #:bs [bs 'solid])
                     (define c (hash->color n))
                     (cellophane
                      (filled-circle 80
                                     #:draw-border? #t
                                     #:border-width 2
                                     #:border-color c
                                     #:color c
                                     #:brush-style bs)
                      0.6))]
         [make-e-set (lambda (n) (make-set n #:bs 'crossdiag-hatch))])
    (define blue-set (make-set 1))
    (define red-set (make-set 3))
    (define green-set (make-set 4))
    (define ex-blue-set (make-e-set 1))
    (define ex-red-set (make-e-set 3))
    (define ex-green-set (make-e-set 4))
    (define (overlap-sets s1 s2)
      (hc-append -10 s1 s2))
    (frame (inset
            (vc-append 10
                       (overlap-sets red-set green-set)

                       (hc-append 10 ex-red-set ex-green-set)

                       )
            10))
    ))

#;(define (show-loans-0-progression)
  (define p
    (vl-append 20
               (render-code show-loans-0-0)
               (hc-append 50 (render-code show-loans-0-1) panel-show-loans-0-1)))
  (send (pict->bitmap p 'smoothed) save-file "show-loans-0-progression.png" 'png)
  p)

#;(define show-loans-1-0
  #<<```
fn add_to_stream<`S@1~ `, `S@2~ `, `S@3~ `>(

  iter: impl Iterator<Item = i32> + `S@1~ `,

  x: &`S@2~ ` i32

) -> impl Iterator<Item = i32> + `S@3~ ` {

  iter.map(move |n| n + *x)

}
```
  )

#;(define show-loans-1-venn
  (let* ([make-set (lambda (n #:bs [bs 'solid])
                     (define c (hash->color n))
                     (cellophane
                      (filled-circle 80
                                     #:draw-border? #t
                                     #:border-width 2
                                     #:border-color c
                                     #:color c
                                     #:brush-style bs)
                      0.6))]
         [make-e-set (lambda (n) (make-set n #:bs 'crossdiag-hatch))])
    (define 1-set (make-set 1))
    (define 2-set (make-set 2))
    (define 3-set (make-set 3))

    (define 1-e-set (make-e-set 1))
    (define 2-e-set (make-e-set 2))
    (define 3-e-set (make-e-set 3))

    (define actual
      (hc-append 20 1-set 2-set 3-set))

    (define expected
      (let* ([big-3 (scale 3-e-set 1.5)]
             [small-1 (scale 1-e-set 0.5)]
             [small-2 (scale 2-e-set 0.5)]
             [p (pin-over big-3
                          (/ (pict-width small-1) 4)
                          (- (/ (pict-width big-3) 2)
                             (/ (pict-height small-2) 2))
                          small-1)]
             [p (pin-over p
                          (- (pict-width p) (* (pict-width small-2) 1.25))
                          (- (/ (pict-height big-3) 2)
                             (/ (pict-height small-2) 2))
                          small-2)])
        p))
    (frame
     (inset
      (vl-append 20
                 actual expected) 10))))

#;(define show-loans-1-tree
  (let* ([make-set (lambda (n #:bs [bs 'solid])
                     (define c (hash->color n))
                     (cellophane
                      (filled-circle 80
                                     #:draw-border? #t
                                     #:border-width 2
                                     #:border-color c
                                     #:color c
                                     #:brush-style bs)
                      0.6))]
         [make-e-set (lambda (n) (make-set n #:bs 'crossdiag-hatch))])
    (define 1-set (make-set 1))
    (define 2-set (make-set 2))
    (define 3-set (make-set 3))

    (define base
      (vc-append 40
                 3-set
                 (hc-append 40 1-set 2-set)))

    (let* ([p (pin-arrow-line 15 base
                              3-set cb-find
                              1-set ct-find
                              #:color default-font-color
                              #:style 'dot
                              #:solid? #f)]
           [p (pin-arrow-line 15 p
                              3-set cb-find
                              2-set ct-find
                              #:color default-font-color
                              #:style 'dot
                              #:solid? #f)])
      (frame (inset p 10)))))

#;(define (show-loans-1-combined)
  (define p
    (let* ([c (render-code show-loans-1-0)])
    (vl-append 40 c
               (ht-append 20
                          show-loans-1-venn
                          show-loans-1-tree))))
  (send (pict->bitmap p 'smoothed) save-file "show-loans-1-options.png" 'png)
  p)

#;(define show-loans-2
  #<<```
fn main() {
    let r: &`S@1~ ` i32;
    {
      `H@2~{` let x: i32 = 5;
        r = &`L@2@0~ ` x;
    } `H@2~}`

`L@2@0~ `
    println!("r: {}", r);
}
```
  )

#;(define show-loans-2-venn
  (let* ()
    (define 1-set (cellophane (colorize (disk 80) (hash->color 1)) 0.5))
    (define 2-loc (filled-rectangle 15 20
                                    #:draw-border? #t
                                    #:border-width 2
                                    #:border-color (hash->color 2)
                                    #:color (hash->color 2)))
    (define p
      (cc-superimpose 1-set 2-loc))
    (frame (inset p 10))))

#;(define (show-loans-2-combined)
  (define p
    (let* ([c (render-code show-loans-2)])
    (ht-append 40 c show-loans-2-venn)))
  (send (pict->bitmap p 'smoothed) save-file "show-loans-2-options.png" 'png)
  p)

#;(define show-loans-3
  #<<```
fn main() {
  `H@3~{` let y: i32 = 0;
    let r: &`S@1~ ` i32;
    {
      `H@2~{` let x: i32 = 5;
        r = if false {
          &`L@2@0~ ` x
        } else {
          &`L@3@0~ ` y
        }
    } `H@2~}`

    println!("r: {}", r);
} `H@3~}`
```
  )

#;(define show-loans-3-venn
  (let* ()
    (define 1-set (cellophane (colorize (disk 80) (hash->color 1)) 0.5))
    (define 2-loc (altered-rectangle 15 20
                                     #:brush-style 'fdiagonal-hatch
                                     #:draw-border? #t
                                     #:border-width 2
                                     #:border-color (hash->color 2)
                                     #:color (hash->color 2)))
    (define 3-loc (filled-rectangle 15 20
                                    #:draw-border? #t
                                    #:border-width 2
                                    #:border-color (hash->color 3)
                                    #:color (hash->color 3)))
    (define p
      (cc-superimpose 1-set (hc-append 15 2-loc 3-loc)))
    (frame (inset p 10))))

#;(define (show-loans-3-combined)
  (define p
    (let* ([c (render-code show-loans-3)])
    (hb-append 40 c show-loans-3-venn)))
  (send (pict->bitmap p 'smoothed) save-file "show-loans-3-options.png" 'png)
  p)


;; How can we view these differently?

@define/source[show-loans-4]{
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

;; using a POV value

@define/source[show-loans-4-value-0]{
  fn main() {
      let y: i32 = 0
      let r: &i32
      {
         let x: i32 @e[2] = 5
          r = if false {
            &x
          } else {
            &y
          }
      }

      println!("r: {}", r);
  }
}

@define/source[show-loans-4-value-1]{
  fn main() {
      let y: i32 = 0
      let r: &i32
      {
         @h[2]{@"{"} let x: i32 @e[2] = 5
          r = if false {
            &@arr[11 6 2]@l[2 1]x
          } else {
            &y
          }
      } @h[2]{@"}"}

      println!("r: {}", r);
  }
}

;; using a POV usage

@define/source[show-loans-4-usage-0]{
  fn main() {
      let y: i32 = 0
      let r: &i32
      {
          let x: i32 = 5
          r = if false {
            &x
          } else {
            &y
          }
      }

      println!("r: {}", r @c[1 'crossdiag-hatch]);
  }
}

@define/source[show-loans-4-usage-1]{
  fn main() {
    @h[3]{@"{"} let y: i32 = 0
      let r: &i32
      {
        @h[2]{@"{"} let x: i32 = 5
          r = if false {
            &@l[2 1 'crossdiag-hatch]x
          } else {
            &@l[3 1] y
          }
      } @h[2]{@"}"}

      println!("r: {}", r @arr[-13 -4 3]@arr[-13 -6 2]@c[1 'crossdiag-hatch]);
  } @h[3]{@"}"}
}

@define/source[show-loans-4-usage-2]{
  fn main() {
    @h[3]{@"{"} let y: i32 = 0
      let r: &i32
      {
        @h[2]{@"{"} let x: i32 = 5
          r = if false {
            &@l[2 1 'crossdiag-hatch]x
          } else {
            &@l[3 1] y
          }
      } @h[2]{@"}"}

      println!("r: {}", r @arr[-22 1 3]@arr[-18 -2 2]@c[1 'crossdiag-hatch]);
  } @h[3]{@"}"}
}

@define/source[show-loans-5]{
@h[1]{@"{"}
  @h[2]{@"{"}
  fn add_to_stream(
    iter: impl Iterator<Item = i32> + @t[1],
    x: &@t[2] i32
    ) -> impl Iterator<Item = i32>  + @t[1 'crossdiag-hatch] {
      iter.map(move |n| n + *x)
    }
  @h[2]{@"}"}
@h[1]{@"}"}
}
