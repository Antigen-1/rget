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

(struct configure (pr u h r f e p po))

(define (configure->input c)
  (list
   ':pr (let ((pr (configure-pr c))) (if (null? pr) '((void)) pr))
   ':u (configure-u c)
   ':h (configure-h c)
   ':r (configure-r c)
   ':f (configure-f c)
   ':e (configure-e c)
   ':p (configure-p c)
   ':po (let ((po (configure-po c))) (if (null? po) '((void)) po))))

(define read-syntax
  (lambda (src port)
    (define (get-block sym) (let work ((r null))
                              (define v (read port))
                              (cond ((eq? v sym) (reverse r))
                                    (else (work (cons v r))))))
    (datum->syntax #f (let loop ((c (configure null #f null 0 #f ''truncate/replace ''secure null)))
                        (define v (read port))
                        (cond ((eof-object? v) (append (list 'module (gensym 'rget) (list 'file (path->string (path->complete-path lib))))
                                                       (configure->input c)))
                              ((eq? v ':u)
                               (loop (struct-copy configure c (u (read port)))))
                              ((eq? v ':r)
                               (loop (struct-copy configure c (r (read port)))))
                              ((eq? v ':e)
                               (loop (struct-copy configure c (e (read port)))))
                              ((eq? v ':f)
                               (loop (struct-copy configure c (f (read port)))))
                              ((eq? v ':p)
                               (loop (struct-copy configure c (p (read port)))))
                              ((eq? v ':h)
                               (loop (struct-copy configure c (h (append (configure-h c) (get-block ':h))))))
                              ((eq? v ':pr)
                               (loop (struct-copy configure c (pr (append (configure-pr c) (get-block ':pr))))))
                              ((eq? v ':po)
                               (loop (struct-copy configure c (po (append (configure-po c) (get-block ':po))))))
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
