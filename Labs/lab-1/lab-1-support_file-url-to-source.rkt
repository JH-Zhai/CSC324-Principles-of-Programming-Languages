#lang racket/base

; Looser cleaner version of  read-xexpr/web  from  2htdp/batch-io
; (which historically we explicitly imported in the course).
; Gets a forest, not just first element, removes a lot of likely pointless whitespace,
;  and turns symbols into strings (at least until we try symbol/enums/etc in csc104).
(provide url-to-source)

(require (prefix-in net: net/url)
         (prefix-in xml: xml/xml)
         (prefix-in html: html)
         (only-in racket/base raise)
         (only-in racket/list filter-map)
         (only-in racket/string string-trim))

(html:use-html-spec #false)
(xml:permissive-xexprs #true)

#;(xml:eliminate-whitespace '() (lambda (x) #true))

(define (url-to-source url-string)
  (define URL (net:string->url url-string)) ; ✪
  (with-handlers ([exn:fail:network? raise]) ; ✪
    (define first-line (net:call/input-url URL net:get-impure-port read-line)) ; ✪
    (define data (net:call/input-url URL net:get-impure-port html:read-html-as-xml))
    (project (map xml:xml->xexpr (filter xml:element? data)))))

(define (project x)
  (cond [(list? x) (filter-map project x)]
        [(symbol? x) (symbol->string x)]
        [(string? x) (cond [(equal? x "") x]
                           [else (define x′ (string-trim x))
                                 (and (not (equal? x′ "")) x′)])]
        [else x]))