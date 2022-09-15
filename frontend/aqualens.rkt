#lang racket/gui

(require framework
         racket/contract

         "private/query.rkt"
         ;; TODO the GUI shouldn't need the spec
         ;; imported
         "private/io-spec.rkt"
         )

;; FIXME this file is currently _very_ experimental.

(provide debug?
         aqualens-main)

(define debug? (make-parameter #false))

;; TODO FIXME
(define (get-test-data)
  (define test-src "/Users/gavinleroy/dev/prj/aquascope/files/hello/src/main.rs")
  (define my-log (make-logger))
  (parameterize ([current-directory "/Users/gavinleroy/dev/prj/aquascope/files/hello"]
                 [current-logger my-log]
                 [debug? #true])
    (aquascope-query (Command.Source test-src '()))))

(define (insert-newline editor)
  (let ((s (make-object string-snip% "\n")))
    (send s set-flags (cons 'hard-newline (send s get-flags)))
    (send editor insert s)))

(define (insert-and-apply-styles editor txt styles)
  (let ((start (send editor last-position)))
    (send editor insert (make-object string-snip% txt))
    (define end (send editor last-position))
    (for ([style (in-list styles)])
      (send editor change-style style start end #f))))

(define (insert-source editor result)
  (cond [(Result.Ok? result)
         (define source (Result.Ok-value result))
         (eprintf "Inserting source ~a\n" (Source-filename source))
         (for* ([line (in-list (Source-enriched-toks source))]
                #:do [(insert-newline editor)]
                [tok (in-list line)])
           (define style
             (let* ([delta (new style-delta%)]
                    [style (car tok)]
                    [fg (Style-foreground style)]
                    [bg (Style-background style)])
               (define fore-color (make-object color% (Color-r fg) (Color-g fg) (Color-b fg) (/ (Color-a fg) 255)))
               (define back-color (make-object color% (Color-r bg) (Color-g bg) (Color-b bg) (/ (Color-a bg) 255)))
               (send delta set-delta-face "Fantasque Sans Mono")
               (send delta set-size-add 6)
               (send delta set-delta-foreground fore-color)
               #;(send delta set-delta-background back-color)
               delta))
           (insert-and-apply-styles editor (cdr tok) (list style)))]
        [(Result.Err? result)
         (raise-argument-error "I can't handle Result.Error responses :)")]))

(define (aqualens-run main-thread _)
  (define main-frame (new frame%
                          [label "Aquascope"]
                          [width 200]
                          [height 200]))
  (define canvas (new editor-canvas% [parent main-frame]))
  (define text (new text%))
  (define menubar (new menu-bar% [parent main-frame]))
  (define m-file (new menu% [label "File"] [parent menubar]))
  (define mi-quit
    (new menu-item%
         [label "Quit"]
         [parent m-file]
         ; TODO: Prompt to save modified files
         [callback (lambda (i e) (thread-send main-thread 'end))]
         [shortcut #\q]
         [shortcut-prefix '(ctl)]))
  ;; TODO the file workflow should be something like this:
  ;; - Aquascope gets opened within a single Rust workspace.
  ;; - Changing the workspace should close all open files in the current session
  ;; - Files to open can only be chosen from this workspace.
  #;(define mi-open
      (new menu-item%
           [label "Open"]
           [parent m-file]
           [callback
            (lambda (i e)
              (define path (get-file #f main-frame))
              (when path
                (send text load-file path 'text))
              )]
           [shortcut #\o]
           [shortcut-prefix '(ctl)]))
  (define mi-reinstantiate-editor
    (new menu-item%
         [label "Reinstantiate editor"]
         [parent m-file]
         [callback
          (lambda (i e)
            (thread-send
             main-thread
             `(reinstantiate-editor #false)))]
         [shortcut #\r]
         [shortcut-prefix '(ctl)]))
  ; (append-editor-operation-menu-items m-edit #f)
  (send text set-max-undo-history 100)
  (send text set-padding 10 10 10 10)
  (send canvas set-editor text)
  (send main-frame show #t)

  (insert-source text (get-test-data)))


(define ((aqualens-main swap-state escape/exit) reload-program)
  (let ([main-window-es (make-eventspace)]
        [mail-arrived (thread-receive-evt)])

    (parameterize ([current-eventspace main-window-es])
      (aqualens-run (current-thread) swap-state))

    (match (sync (wrap-evt main-window-es (lambda (v) 'closed))
                 (wrap-evt mail-arrived (lambda (v) 'mail)))
      ['mail
       (match (thread-receive)
         [`(reinstantiate-editor ,next-swap-state)
          (reload-program next-swap-state)]
         ['end (escape/exit)]
         [unknown-msg (displayln unknown-msg)])]
      ['closed (escape/exit)]
      [unknown-msg (displayln unknown-msg)])))
