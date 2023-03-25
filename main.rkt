#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  )

(require net/url net/head net/url-connect racket/port file/gunzip racket/runtime-path (for-syntax racket/base))

(define-runtime-path lib "./main.rkt")

(define-syntax (#%rget-module-begin stx)
  (syntax-case stx (:pr :u :h :r :f :e :p :po)
    ((_
      :pr (pre ...) ;;preprocess(optional); can be appended

      ;;these are used to config the downloader
      ;;all of these can be s-expressions
      :u l ;;url; strings are automatically converted; it can be overwritten
      :h (b ...) ;;header(optional); can be appended
      :r n ;;redirection; it can be overwritten; default to 0
      :f o ;;output; it can be overwritten
      :e e ;;`#:exists` kwarg of `call-with-output-file`; it can be overwritten; default to 'truncate/replace
      :p p ;;https protocol; it can be overwritten; default to 'secure
      
      :po (post ...) ;;postprocess(optional); can be appended
      )
     #'(#%module-begin
        pre ...
        (dynamic-wind
          void
          (lambda ()
            (define-values (port headers)
              (parameterize ((current-https-protocol p))
                (get-pure-port/headers (if (string? l) (string->url l) l)
                                       (list b ...)
                                       #:method #"GET"
                                       #:redirections n
                                       #:status? #f)))
            (dynamic-wind
              void
              (lambda ()
                (define type
                  (let/cc ret (car (regexp-match #rx"^(?i:gzip)|^(?i:deflate)" (cond ((extract-field "Content-Encoding" headers))
                                                                                     (else (ret #f)))))))
                (define handler (cond ((not type) copy-port)
                                      ((string-ci=? type "gzip") gunzip-through-ports)
                                      ((string-ci=? type "deflate") inflate)))
                (call-with-output-file #:exists e o (lambda (output) (handler port output))))
              (lambda () (close-input-port port))))
          (lambda () post ...))))))

(define read-syntax
  (lambda (src port)
    (define (get-block sym) (let work ((r null))
                              (define v (read port))
                              (cond ((eq? v sym) (reverse r))
                                    (else (work (cons v r))))))
    (datum->syntax #f (let loop ((u #f)
                                 (r 0)
                                 (e ''truncate/replace)
                                 (f #f)
                                 (p ''secure)
                                 (h null)
                                 (pr null)
                                 (po null))
                        (define v (read port))
                        (cond ((eof-object? v) (list 'module (gensym 'rget) (list 'file (path->string (path->complete-path lib)))
                                                     ':pr (if (null? pr) '((void)) pr)
                                                     ':u u
                                                     ':h h
                                                     ':r r
                                                     ':f f
                                                     ':e e
                                                     ':p p
                                                     ':po (if (null? po) '((void)) po)))
                              ((eq? v ':u)
                               (loop (read port) r e f p h pr po))
                              ((eq? v ':r)
                               (loop u (read port) e f p h pr po))
                              ((eq? v ':e)
                               (loop u r (read port) f p h pr po))
                              ((eq? v ':f)
                               (loop u r e (read port) p h pr po))
                              ((eq? v ':p)
                               (loop u r e f (read port) h pr po))
                              ((eq? v ':h)
                               (loop u r e f p
                                     (append h (get-block ':h))
                                     pr po))
                              ((eq? v ':pr)
                               (loop u r e f p h (append pr (get-block ':pr)) po))
                              ((eq? v ':po)
                               (loop u r e f p h pr (append po (get-block ':po))))
                              (else (raise-syntax-error 'read-syntax (format "fail to parse the syntax due to a ~s" v))))))))

(provide read-syntax (rename-out (#%rget-module-begin #%module-begin)) (except-out (all-from-out racket/base) #%module-begin))

(module+ test
  (test-case
      "read-syntax"
    (check-match (syntax->datum (read-syntax #f (open-input-string ":u \"https://127.0.0.1:8080\" :r 4 :e 'truncate :f \"test\" :p 'auto :h \"Content-Type: gzip\" :h :pr (displayln \"hello\") :pr :po (displayln \"finish\") :po :pr (displayln \"begin\") :pr")))
                 (list 'module _ (list 'file (regexp "^.*main\\.rkt$"))
                       ':pr (list (list 'displayln "hello") (list 'displayln "begin"))
                       ':u "https://127.0.0.1:8080"
                       ':h (list "Content-Type: gzip")
                       ':r 4
                       ':f "test"
                       ':e ''truncate
                       ':p ''auto
                       ':po (list (list 'displayln "finish"))))))
