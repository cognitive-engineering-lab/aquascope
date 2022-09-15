#lang racket/base

;; This file was modified from Bogdan Popa's Noise Serde Lib.
;; Support here is given for Rust types and generating a .rs
;; interface is currently not yet supported TODO.
;;
;; NOTE to interface with Rust, serde_json should be used
;; from the rust side. That is, this module only supports
;; reading / writing to a JSON format.

(require (for-syntax racket/base
                     syntax/parse
                     racket/math
                     racket/provide-transform
                     racket/syntax)
         json
         racket/bool
         racket/contract
         racket/function
         racket/generic
         racket/hash
         racket/math
         racket/port
         racket/vector
         "sequencer.rkt")

(define debug? (make-parameter #false))
(provide debug?)

;; --------------------------------------------------
;; :

(provide :)

(define-syntax (: stx)
  (raise-syntax-error ': "may only be used within a define-enum, define-record form" stx))

(define (make-single-jsexpr k v)
  (make-immutable-hasheq `((,k . ,v))))

;; --------------------------------------------------
;; record

(provide write-record
         read-record
         define-record
         record-out)

(define (write-record info v [out (current-output-port)])
  (write-json (record->json info v) out))

(define (read-record info [in (current-input-port)])
  (json->record info (read-json in)))

(define (json->record info jsexp)
  (apply (record-info-constructor info)
         (for/list ([f (in-list (record-info-fields info))])
           (define nested (hash-ref jsexp (record-field-id f)))
           (read-field (record-field-type f) nested))))

(define (record->json info v)
  (for/hash ([f (in-list (record-info-fields info))])
    (values (record-field-id f)
            (write-field
             (record-field-type f)
             ((record-field-accessor f) v)))))

(provide record-infos
         (struct-out record-info)
         (struct-out record-field))

(struct record-info ([id #:mutable] name constructor fields)
  #:property prop:procedure (struct-field-index constructor))
(struct record-field (id type accessor))

(define record-infos (make-hasheqv))
(define record-info-sequencer
  (make-sequencer
   record-infos
   record-info-name
   set-record-info-id!))

(define-syntax (define-record stx)
  (define (id-stx->keyword stx)
    (datum->syntax stx (string->keyword (symbol->string (syntax-e stx)))))

  (define-syntax-class record-field
    #:literals (:)
    (pattern [id:id : ft:expr {~optional {~seq #:contract ctc-expr:expr}}]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'id
             #:with opt? #f
             #:with ctc #'{~? ctc-expr any/c})
    (pattern [(id:id def:expr) : ft:expr {~optional {~seq #:contract ctc-expr:expr}}]
             #:with kwd (id-stx->keyword #'id)
             #:with arg #'[id def]
             #:with opt? #t
             #:with ctc #'{~? ctc-expr any/c}))

  (syntax-parse stx
    [(_ name:id fld:record-field ...)
     #:with name? (format-id #'name "~a?" #'name)
     #:with constructor-id (format-id #'name "make-~a" #'name)
     #:with (fld-accessor-id ...)
     (for/list ([arg (in-list (syntax-e #'(fld.id ...)))])
       (format-id #'name "~a-~a" #'name arg))
     #:with (fld-setter-id ...)
     (for/list ([arg (in-list (syntax-e #'(fld.id ...)))])
       (format-id #'name "set-~a-~a" #'name arg))
     #:with (constructor-arg ...)
     (apply
      append
      (for/list ([kwd (in-list (syntax-e #'(fld.kwd ...)))]
                 [arg (in-list (syntax-e #'(fld.arg ...)))])
        (list kwd arg)))
     #:with (required-ctor-arg-ctc ...)
     (apply
      append
      (for/list ([opt? (in-list (syntax->datum #'(fld.opt? ...)))]
                 [kwd  (in-list (syntax-e #'(fld.kwd ...)))]
                 [ctc  (in-list (syntax-e #'(fld.ctc ...)))]
                 #:unless opt?)
        (list kwd ctc)))
     #:with (optional-ctor-arg-ctc ...)
     (apply
      append
      (for/list ([opt? (in-list (syntax->datum #'(fld.opt? ...)))]
                 [kwd  (in-list (syntax-e #'(fld.kwd ...)))]
                 [ctc  (in-list (syntax-e #'(fld.ctc ...)))]
                 #:when opt?)
        (list kwd ctc)))
     #'(begin
         (define-syntax (name stx-or-mode)
           (case stx-or-mode
             [(provide-record)
              #'(combine-out name name? constructor-id fld-accessor-id ... fld-setter-id ...)]
             [else
              (unless (syntax? stx-or-mode)
                (raise-syntax-error 'name "unexpected argument to transformer" stx-or-mode))
              (syntax-case stx-or-mode ()
                [(_ arg (... ...)) #'(-name arg (... ...))]
                [id (identifier? #'id) #'info])]))
         (define-values (-name name? fld-accessor-id ... fld-setter-id ...)
           (let ()
             (struct name (fld.id ...)
               #:transparent)
             (define/contract (fld-setter-id r v)
               (-> name? fld.ctc name?)
               (struct-copy name r [fld.id v])) ...
             (values name name? fld-accessor-id ... fld-setter-id ...)))
         (define info
           (let ([fields (list (record-field 'fld.id (->field-type 'Record fld.ft) fld-accessor-id) ...)])
             (record-info #f 'name -name fields)))
         (sequencer-add! record-info-sequencer info)
         (define/contract (constructor-id constructor-arg ...)
           (->* (required-ctor-arg-ctc ...)
                (optional-ctor-arg-ctc ...)
                name?)
           (-name fld.id ...)))]))

(define-syntax record-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ id)
        (define export-stx
          ((syntax-local-value #'id) 'provide-record))
        (expand-export export-stx modes)]))))

(module+ test
  (require racket/port
           rackunit)

  (test-case "record serde"
    (define-record Human
      [name : String #:contract string?]
      [age : U8 #:contract U8/c])
    (define h (make-Human #:name "John" #:age 30))
    (define bs (with-output-to-string (lambda () (write-record Human h))))
    (check-equal? h (read-record Human (open-input-string bs)))))


;; --------------------------------------------------
;; enum

(provide read-enum-variant
         write-enum-variant
         define-enum
         enum-out)

(define-generics enum-variant-writer
  {enum-variant->json enum-variant-writer})

(define (read-enum-variant info [in current-input-port])
  (json->enum-variant info (read-json in)))

(define (write-enum-variant enum-variant-writer [out (current-output-port)])
  (write-json (enum-variant->json enum-variant-writer) out))

;; NOTE compared to the old approach, of including an index,
;; the Rust serde will not include this information and we need
;; to try deserializing each and see which works.
(define (json->enum-variant info jexpr)
  (define variants (enum-info-variants info))
  (define-values (index-of nested)
    (if (not (hash? jexpr))
        ;; If there is no associated fields we can simply
        ;; search for the symbol.
        (values (vector-memq (string->symbol jexpr)
                             (vector-map enum-variant-name variants)) '())
        (for/fold ([found-match #false] [args '()])
                  ([(variant idx) (in-indexed (in-vector variants))]
                   #:break found-match
                   #:do [(define name (enum-variant-name variant))
                         (define fields (enum-variant-fields variant))
                         (define nested (hash-ref jexpr name #false))]
                   #:when nested)
          (values idx
                  (for/list ([f (in-list fields)]
                             [item (in-list (if (list? nested) nested (list nested)))])
                    (read-field (enum-variant-field-type f) item))))))
  (unless index-of
    (raise-argument-error "Failed to deserialize enum" (enum-info-name info) jexpr))
  (apply (enum-variant-constructor (vector-ref variants index-of)) nested))

(define (do-enum-variant->json info idx v)
  (define variant (vector-ref (enum-info-variants info) idx))
  (define variant-name (enum-variant-name variant))
  (define field-arr
    (for/list ([f (in-list (enum-variant-fields variant))])
      (write-field (enum-variant-field-type f) ((enum-variant-field-accessor f) v))))
  (if (null? field-arr)
      (symbol->string variant-name)
      (make-single-jsexpr variant-name
                          (if (null? (cdr field-arr))
                              (car field-arr)
                              field-arr))))

(provide write-enum-variant
         enum-infos
         (struct-out enum-info)
         (struct-out enum-variant)
         (struct-out enum-variant-field))

(struct enum-variant-field (name type accessor))
(struct enum-variant (id name constructor fields))
(struct enum-info ([id #:mutable] name variants))

(define enum-infos (make-hasheqv))
(define enum-info-sequencer
  (make-sequencer
   enum-infos
   enum-info-name
   set-enum-info-id!))

(define-syntax (define-enum stx)
  (define-syntax-class enum-variant
    #:literals (:)
    (pattern [name:id {fld-name:id : fld-type:expr} ...]))

  (syntax-parse stx
    #:literals (:)
    [(_ name:id variant:enum-variant ...+)
     #:with name? (format-id #'name "~a?" #'name)
     #:with (variant.idx ...)
     (for/list ([stx (in-list (syntax-e #'(variant ...)))]
                [idx (in-naturals)])
       (datum->syntax stx idx))
     #:with (variant.qualname ...)
     (for/list ([stx (in-list (syntax-e #'(variant.name ...)))])
       (format-id #'name "~a.~a" #'name stx))
     #:with (variant.qualname? ...)
     (for/list ([stx (in-list (syntax-e #'(variant.qualname ...)))])
       (format-id stx "~a?" stx))
     #:with ((variant.fld-accessor-id ...) ...)
     (for/list ([qual-stx (in-list (syntax-e #'(variant.qualname ...)))]
                [names-stx (in-list (syntax-e #'((variant.fld-name ...) ...)))])
       (for/list ([stx (in-list (syntax-e names-stx))])
         (format-id qual-stx "~a-~a" qual-stx stx)))
     #'(begin
         (define-syntax (name stx-or-mode)
           (case stx-or-mode
             [(provide-enum)
              #'(combine-out name name?
                             variant.qualname ...
                             variant.qualname? ...
                             variant.fld-accessor-id ... ...)]
             [else
              (unless (syntax? stx-or-mode)
                (raise-syntax-error 'name "unexpected argument to transformer" stx-or-mode))
              (syntax-case stx-or-mode ()
                [id (identifier? #'id) #'info])]))
         (struct root ()
           #:transparent
           #:reflection-name 'name)
         (define name? root?)
         (struct variant.qualname root (variant.fld-name ...)
           #:transparent
           #:methods gen:enum-variant-writer
           {(define (enum-variant->json self)
              (do-enum-variant->json info variant.idx self))}) ...
         (define variants
           (vector
            (enum-variant
             variant.idx
             'variant.name
             variant.qualname
             (list
              (enum-variant-field
               'variant.fld-name
               (->field-type 'Enum variant.fld-type)
               variant.fld-accessor-id) ...)) ...))
         (define info
           (enum-info #f 'name variants))
         (sequencer-add! enum-info-sequencer info))]))

(define-syntax enum-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ id)
        (define export-stx
          ((syntax-local-value #'id) 'provide-enum))
        (expand-export export-stx modes)]))))

(module+ test
  (test-case "enum serde"
    (define-enum Column
      [nothing]
      [default]
      [text {t : String}])

    (define tests
      (list (Column.nothing)
            (Column.default)
            (Column.text "hello")))
    (for ([t (in-list tests)])
      (define bs (with-output-to-string (lambda () (write-enum-variant t))))
      (check-equal? t (read-enum-variant Column (open-input-string bs))))))

;; --------------------------------------------------
;; field

(provide
 Listof
 Record
 Enum
 Tuple
 (struct-out field-type)
 read-field
 write-field)

(struct field-type
  (read-proc write-proc export-proc))

(define (read-field t obj)
  ((field-type-read-proc t) t obj))

(define (write-field t v)
  ((field-type-write-proc t) t v))

(define-syntax (define-field-type stx)
  (syntax-parse stx
    [(_ id:id
        {~alt
         {~optional {~seq #:read read-expr:expr}}
         {~optional {~seq #:write write-expr:expr}}
         {~optional {~seq #:export export-expr:expr}}} ...)
     #'(begin
         (define id (field-type read-expr write-expr (~? export-expr (lambda () (symbol->string 'id)))))
         (provide id))]))

(define-field-type Bool
  #:read (lambda (_ obj) obj)
  #:write (lambda (_ obj) obj))

(define-syntax (define-float-field-type stx)
  (syntax-parse stx
    [(_ id:id size:number)
     #'(define-field-type id
         #:read (lambda (in) (floating-point-bytes->real (read-bytes size in) #t))
         #:write (lambda (n out) (write-bytes (real->floating-point-bytes n size #t) out)))]))

;; The given number should be in unit of bits
(define-syntax (define-integer-field-type stx)
  (syntax-parse stx
    [(_ id:id size:number)
     #'(define-integer-field-type id size #f)]
    [(_ id:id size:number #:signed)
     #'(define-integer-field-type id size #t)]
    [(_ id:id size:number signed?:boolean)
     #:with id/c (format-id #'id "~a/c" #'id)
     (let* ([sig? (syntax-e #'signed?)]
            [n (syntax-e #'size)]
            [p (expt 2 n)]
            [half (exact-truncate (/ p 2))]
            [mn (if sig? (- half) 0)]
            [mx (sub1 (if sig? half p))])
       #`(begin
           (define id/c (integer-in #,mn #,mx))
           (provide id/c)
           (define-field-type id
             #:read (lambda (_ obj) obj)
             #:write (lambda (_ obj) obj))))]))

;; Rust unsigned integer types
(define-integer-field-type U8     8)
(define-integer-field-type U16   16)
(define-integer-field-type U32   32)
(define-integer-field-type U64   64)
(define-integer-field-type U128 128)

;; Rust signed integer types
(define-integer-field-type I8     8 #:signed)
(define-integer-field-type I16   16 #:signed)
(define-integer-field-type I32   32 #:signed)
(define-integer-field-type I64   64 #:signed)
(define-integer-field-type I128 128 #:signed)

(module+ test
  (let ()
    (define-record U8Box
      [value : U8 #:contract U8/c])
    (define-record I8Box
      [value : I8 #:contract I8/c])
    (check-exn exn:fail? (lambda () (make-U8Box #:value -1)))
    (check-exn exn:fail? (lambda () (make-U8Box #:value 256)))
    (check-exn exn:fail? (lambda () (make-U8Box #:value 1000)))
    (check-not-exn (lambda () (make-U8Box #:value 0)))
    (check-not-exn (lambda () (make-U8Box #:value 255)))
    (check-not-exn (lambda () (make-U8Box #:value 128)))
    (check-exn exn:fail? (lambda () (make-I8Box #:value -200)))
    (check-exn exn:fail? (lambda () (make-I8Box #:value -129)))
    (check-exn exn:fail? (lambda () (make-I8Box #:value 128)))
    (check-exn exn:fail? (lambda () (make-I8Box #:value 200)))
    (check-not-exn (lambda () (make-I8Box #:value -32)))
    (check-not-exn (lambda () (make-I8Box #:value 31)))
    (check-not-exn (lambda () (make-I8Box #:value -128)))
    (check-not-exn (lambda () (make-I8Box #:value 127)))
    (check-not-exn (lambda () (make-I8Box #:value 0)))))

(define-field-type String
  #:read (lambda (_ obj) obj)
  #:write (lambda (_ obj) obj))

(provide ->field-type)

(define (->field-type who t)
  (cond [(record-info? t) (Record t)]
        [(enum-info? t) (Enum t)]
        [else (begin0 t
                (unless (field-type? t)
                  (raise-argument-error who "(or/c field-type? record-info?)" t)))]))

(define (Listof t)
  (let ([t (->field-type 'Listof t)])
    (define read-proc (field-type-read-proc t))
    (define write-proc (field-type-write-proc t))
    (define export-type ((field-type-export-proc t)))
    (field-type
     (lambda (_ vs) (map (lambda (v) (read-proc t v)) vs))
     (lambda (_ vs) (map (lambda (v) (write-proc t v)) vs))
     (lambda () (thunk (error "Export encoding not supported"))))))

(define (Tuple t1 t2)
  (let ([t1 (->field-type 'Tuple t1)]
        [t2 (->field-type 'Tuple t2)])
    (define read-proc-car (field-type-read-proc t1))
    (define write-proc-car (field-type-write-proc t1))
    (define export-type-car ((field-type-export-proc t1)))
    (define read-proc-cdr (field-type-read-proc t2))
    (define write-proc-cdr (field-type-write-proc t2))
    (define export-type-cdr ((field-type-export-proc t2)))
    (field-type
     (lambda (_ vs)
       (cons (read-proc-car t1 (car vs))
             (read-proc-cdr t2 (cadr vs))))
     (lambda (_ vs)
       (list (write-proc-car t1 (car vs))
             (write-proc-cdr t2 (cdr vs))))
     (lambda () (thunk (error "Export encoding not supported"))))))

(define (Record t)
  (unless (record-info? t)
    (raise-argument-error 'Record "record-info?" t))
  (field-type
   (lambda (_ in) (json->record t in))
   (lambda (_ v) (record->json t v))
   (lambda () (symbol->string (record-info-name t)))))

(define (Enum t)
  (unless (enum-info? t)
    (raise-argument-error 'Enum "enum-info?" t))
  (field-type
   (lambda (_ in) (json->enum-variant t in))
   (lambda (_ v) (enum-variant->json v))
   (lambda () (symbol->string (enum-info-name t)))))

(module+ test
  (test-case "complex field serde"
    (define-record Example
      [b : Bool #:contract boolean?]
      [i : U8 #:contract U8/c]
      [s : String #:contract string?]
      [l : (Listof I32) #:contract list?])
    (define v (Example #t -1 "hello" '(0 1 2 #x-FF #x7F #xFF)))
    (define bs (with-output-to-string (lambda () (write-record Example v))))
    (check-equal? v (read-record Example (open-input-string bs))))

  (test-case "tuple field serde"
    (define-record Foo
      [name : String #:contract string?]
      [really : Bool #:contract boolean?]
      [bar : (Listof (Tuple U8 String))
           #:contract (listof (cons/c U8/c string?))])
    (define v (Foo "some name" #f (list (cons 0 "zero"))))
    (define bs (with-output-to-string (lambda () (write-record Foo v))))
    (check-equal? v (read-record Foo (open-input-string bs))))

  (test-case "homogeneous list serde"
    (define-record Story
      [id : U8 #:contract U8/c]
      [title : String #:contract string?]
      [comments : (Listof U8) #:contract (listof U8/c)])
    (define-record Stories
      [stories : (Listof Story) #:contract (listof Story?)])
    (define v
      (Stories
       (list (Story 0 "a" null)
             (Story 1 "b" '(1 2 3)))))
    (define bs (with-output-to-string (lambda () (write-record Stories v))))
    (check-equal? v (read-record Stories (open-input-string bs))))

  (test-case "nested record serde"
    (define-record A
      [s : String])
    (define-record B
      [a : A])
    (define v
      (B (A "test")))
    (define bs (with-output-to-string (lambda () (write-record B v))))
    (check-equal? v (read-record B (open-input-string bs))))

  (test-case "record with enum serde"
    (define-enum Result
      [err {message : String}]
      [ok {value : U8}])
    (define-record C
      [res : Result])
    (define v
      (C (Result.err "an error")))
    (define bs (with-output-to-string (lambda () (write-record C v))))
    (check-equal? v (read-record C (open-input-string bs)))))
