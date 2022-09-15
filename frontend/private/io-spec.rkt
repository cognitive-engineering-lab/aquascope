#lang racket/base

;; -----------------------------------------------
;; -----------------------------------------------
;; NOTE this file must stay up-to-date with the
;; definitions provided in the
;; aquascope_front/src/plugin.rs file, as well
;; as the other files providing commands.
;;
;; TODO in order to have coherency between the two
;; systems, I think definitions should be made in
;; the frontend (or the backend) that would
;; automatically generate the definitions for the
;; sister system. This would ensure that the two
;; cannot drift apart in supported functionality.
;;
;; The "serde.rkt" file almost provides support
;; for this, I just need an exporting function.
;; -----------------------------------------------
;; -----------------------------------------------

(require racket/contract
         "serde.rkt")

(provide (record-out FontStyle)
         (record-out Color)
         (record-out Style)
         (record-out Source)
         #;(enum-out Output)
         (enum-out Command)
         (enum-out Result))

;; --------------------------------------------------
;; Backend request data format

(define-enum Command
  [Preload]
  [RustcVersion]
  [Source {file : String}
          {flags : (Listof String)}])

;; --------------------------------------------------
;; Backend response data format

(define-record FontStyle
  ;; Currently the frontend will send the
  ;; font style as a byte with different types,
  ;; TODO make this an enum instead which
  ;; defines `Bold`, `Underline`, `Italic`.
  [bits : U8 #:contract U8/c])

(define-record Color
  [r : U8 #:contract U8/c]
  [g : U8 #:contract U8/c]
  [b : U8 #:contract U8/c]
  [a : U8 #:contract U8/c])

(define-record Style
  [foreground : Color]
  [background : Color]
  [font_style : FontStyle])

(define-record Source
  [filename : String #:contract string?]
  [enriched-toks : (Listof (Listof (Tuple Style String)))
                 #:contract (listof (listof (cons/c Style? string?)))])

#;(define-enum Output
  [source-out {source :  Source}])

;; TODO can this be made generic over Output types?
(define-enum Result
  [Ok {value : Source}]
  [Err {message : String}])
