#lang racket

;; -----------------------------------------------
;; High-level overview of static analysis pipeline
;; Thrown together super fast
;; last edited May 15, 2023

(require (for-syntax racket/splicing)
         racket/splicing
         pict
         racket/draw
         file/convertible)

(define *hspace* (make-parameter 10))
(define *vspace* (make-parameter 10))
(define monospace
  (make-object font% 18 "Inconsolata" 'default 'normal))

(define (mono w #:angle [a 0]) (text w monospace 16 a))
(define (var w #:angle [a 0]) (text w "Helvetica" 16 a))

(define (gear #:n [n 10]
              #:rotate [rotate 0]
              #:size [size 100]
              #:ratio [ratio 0.8]
              #:hole-ratio [hole-ratio 0.2]
              #:color [color "darkgray"])
  (define step (/ pi n))
  (define p (new dc-path%))
  (define adj (/ (* (- 1 ratio) 2 pi) (* 2 2 n)))
  (for ([i (in-range n)])
    (send p arc 0 0 size size (+ (* i 2 step) rotate adj) (- (+ (* i 2 step) step rotate) adj))
    (send p arc
          (* (- 1 ratio) 1/2 size) (* (- 1 ratio) 1/2 size)
          (* ratio size) (* ratio size)
          (+ (* i 2 step) step rotate) (+ (* (add1 i) 2 step) rotate)))
  (send p close)
  (send p ellipse
        (* (- 1 hole-ratio) 1/2 size) (* (- 1 hole-ratio) 1/2 size)
        (* hole-ratio size) (* hole-ratio size))
  (define no-pen (make-pen #:style 'transparent))
  (define brush (make-brush #:color color))
  (dc (lambda (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen no-pen)
        (send dc set-brush brush)
        (send dc draw-path p x y)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))
      size size))

(define add-gears
  (let* ([g1 (gear #:n 20)]
         [g2 (scale (gear #:n 10) 1/3)]
         [gears (rotate (hc-append -2 g1 g2) (* pi -1/4))])
    (lambda (base)
      (define gs (scale gears 1/3))
      (pin-over base
                (- (pict-width base) (/ (pict-width gs) 2))
                (- (/ (pict-height gs) 2))
                gs))))

(define (mk-process name #:size [size 100] #:color [color "whitesmoke"] #:with-gear? [g? #t])
  (define r (filled-rectangle size (* size 1.25)
                              #:color color
                              #:draw-border? #t
                              #:border-color "dimgray"))
  (define text+box (lt-superimpose r (inset (mono name #:angle (* pi 3/2)) 2)))
  (launder (refocus (if g? (add-gears text+box) text+box) r)))

(define (alternating . args)
  (define (loop ls)
    (match ls
      [(list) (loop args)]
      [(list x y ...) (stream-cons x (loop y))]))
  (loop args))

(define (back-with p
                   #:color [c "whitesmoke"]
                   #:shape [shape
                            (lambda (w h)
                              (filled-rectangle w h #:color c
                                                #:draw-border? #t
                                                #:border-color "dimgray"))])

  (define s (shape (pict-width p) (pict-height p)))
  (cc-superimpose s p))

(define (mk-fn name #:color [c "whitesmoke"])
  (define t (inset (mono (format "fn ~a(arg ...) { ... }" name))
                   (*hspace*)))
  (back-with t))

(define (mk-lambda name)
  (frame (inset (mono (format "~a|arg ...| { ... }" name)) 10)))

;; -----
;; Icons

(define rustc-plugin (mk-process "rustc_plugin" #:color "honeydew"))
(define permissions (mk-process "permissions" #:color "bisque"))

(define analyses
  (list (mk-process "boundaries" #:color "lightcyan" #:with-gear? #f)
        (mk-process "stepper" #:color "lightcyan" #:with-gear? #f)))

(define pipeline
  (add-gears
   (frame
    (inset
     (apply ht-append
            (list* (*hspace*) (append analyses (list (mono "...")))))
     (*vspace*)))))

(define frontend (desktop-machine 1))

(define ghosty (blank 80 25))

;; ----
;; Data

(define fin
  (let ([width 50]
        [height 60]
        [lines 8])
    (cb-superimpose
     (file-icon width height "whitesmoke")
     (inset (apply vc-append 2
                   (for/list ([_ (in-range lines)]
                              [c (alternating "gainsboro" "lightgray")])
                     (filled-rectangle (* width 0.9)
                                       (/ (* height 0.5)
                                          lines) #:color c #:draw-border? #f)))
            2))))

(define analysis-ctxt
  (scale (cc-superimpose
   (cloud 150 75 "lavenderblush")
   (mono "PermissionsCtxt")) 2/3))

(define fn-foo (scale (mk-fn "foo") 2/3))
(define bodies
  (scale
   (frame
    (inset (vl-append (*vspace*) fn-foo (mono "..."))
           (*vspace*)))
   0.75))

(define json
  (scale (back-with (inset (mono "{ 'muchJSON': { ... } }") (*vspace*))) 2/3))

;; -------
;; Drawing

(define rows
  (list (list ghosty ghosty ghosty permissions)
        (list ghosty rustc-plugin ghosty)
        (list ghosty)
        (list frontend ghosty pipeline)
        ))

(define base
  (parameterize ([*hspace* 100]
                 [*vspace* 50])
    (apply vl-append (*vspace*)
           (for/list ([row (in-list rows)])
             (apply ht-append (*hspace*) row)))))

(define archers
  (let-syntax ([pal (lambda (stx)
                               #`(lambda (base)
                                   #,(list* 'pin-arrow-line 10 #'base (cdr (syntax->list stx)))))]
               [pi (lambda (stx)
                       #`(* pi #,(cadr (syntax->list stx))))])
    (list (pal frontend ct-find rustc-plugin lc-find
                 #:label fin
                 #:start-pull 1/2
                 #:start-angle (pi 2/3)
                 #:end-angle (pi 1/6))
            (pal rustc-plugin rc-find permissions lc-find
                 #:label fn-foo
                 #:start-pull 1/2
                 #:start-angle (pi 11/6)
                 #:end-angle (pi 1/6))
            (pal permissions cb-find pipeline ct-find
                 #:label (vc-append (*vspace*) fn-foo analysis-ctxt)
                 #:x-adjust-label 80
                 #:end-angle (pi 3/2))
            (pal pipeline lc-find rustc-plugin cb-find
                 #:label json
                 #:x-adjust-label 60)
            (pal rustc-plugin cb-find frontend rc-find
                 #:start-angle (pi 3/2)
                 #:end-angle (pi 5/4)
                 #:label
                 (scale
                  (apply vc-append
                         (list* 2 (stream->list
                                   (stream-take (alternating json) 5)))) 1/2)
                 #:x-adjust-label 30))))

(define lifecycle-of-a-file
  (for/fold ([p base])
            ([shoot-arr (in-list archers)])
    (shoot-arr p)))

(define (save)
  (define (save-pict the-pict name [kind 'png])
    (define bm (pict->bitmap the-pict))
    (send bm save-file name kind))
  (define p (scale/improve-new-text
             (panorama (inset lifecycle-of-a-file 20)) 4))
  (save-pict p "../files/lifecycle.png" 'png)
  (with-output-to-file "../files/lifecycle.pdf"
    #:exists 'replace
    (lambda ()
      (write-bytes (convert p 'pdf-bytes))))
  (with-output-to-file "../files/lifecycle.svg"
    #:exists 'replace
    (lambda ()
      (write-bytes (convert p 'svg-bytes)))))
