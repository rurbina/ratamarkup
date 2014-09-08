#lang racket/base

(provide ratamarkup
         ratamarkup-inline
         (rename-out [inline-processors     ratamarkup-inline-processors]
                     [link-callback         ratamarkup-link-callback]
                     [image-callback        ratamarkup-image-callback]
                     [set!-link-callback    ratamarkup-set!-link-callback]
                     [set!-image-callback   ratamarkup-set!-image-callback]
                     [section-processors    ratamarkup-section-processors]
                     [add-section-processor ratamarkup-add-section-processor]))

(require racket/string
         racket/list
         racket/set
         xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set!-link-callback  f) (set! link-callback  f))
(define (set!-image-callback f) (set! image-callback f))

(define link-callback (lambda (link #:options [options null])
                        (let ([m (flatten
                                  (regexp-match* "^\\[\\[(.*)?(?<!\\\\)\\|(.*?)(?<!\\\\)\\|(.*?)]]$"
                                                 link
                                                 #:match-select cdr
                                                 ))])
                          (format "<a href=\"~a\"~a>~a</a>"
                                  (first m)
                                  (if (= (string-length (second m)) 0) "" 
                                      (string-append " " (regexp-replace* #px"&quot;" (second m) "\"")))
                                  (third m)))))

(define image-callback (lambda (link #:options [options null])
                        (let ([m (flatten (regexp-match* "^\\{\\{(.*)?(?<!\\\\)\\|(.*?)(?<!\\\\)\\|(.*?)}}$"
                                                link
                                                #:match-select cdr
                                                ))])
                          (format "<img src=\"~a\" title=\"~a\" alt=\"~a\"~a>"
                                  (first m)
                                  (third m)
                                  (third m)
                                  (if (= (string-length (second m)) 0) "" 
                                      (string-append " " (regexp-replace* #px"&quot;" (second m) "\"")))))))

(define inline-processors
  '([#px"&" "\\&amp;"]
    [#px"<" "\\&lt;"]
    [#px">" "\\&gt;"]
    [#px"\"" "\\&quot;"]
    [#px"\\\\\\\\" "\\&#92;"]
    [#px"(?<!\\\\)(`+)\\s*(.*?)\\s*(?<!\\\\)\\1" "<code>\\2</code>"]
    [#px"(?<!')(?<!\\\\)'{4,5}([^']+)(?<!\\\\)'{4,5}(?!')" "<b><i>\\1</i></b>"]
    [#px"(?<=\\b)(?<!\\\\)__(.*?)(?<!\\\\)__(?=$|\\b)" "<u>\\1</u>"]
    [#px"(?<=\\b)(?<!\\\\)_-(.*?)(?<!\\\\)-_(?=$|\\b)" "<s>\\1</s>"]
    [#px"(?<=\\b)(?<!\\\\)&quot;&quot;(.*?)(?<!\\\\)&quot;&quot;(?=$|\\b)" "<s>\\1</s>"]
    [#px"(?<!\\\\)\\[\\[([^]|]+|\\\\\\])\\]\\]" "[[\\1||\\1]]"]
    [#px"(?<!\\\\)\\[\\[([^]|]+|\\\\\\]|\\\\\\|)\\|([^]|]+|\\\\\\])\\]\\]" "[[\\1||\\2]]"]
    [#px"(?<!\\\\)\\{\\{([^}|]+|\\\\\\})\\}\\}" "{{\\1||\\1}}"]
    [#px"(?<!\\\\)\\{\\{([^}|]+|\\\\\\}|\\\\\\|)\\|([^}|]+|\\\\\\})\\}\\}" "{{\\1||\\2}}"]
    [#px"(?<!')'{3}([^']+)'{3}(?!')" "<b>\\1</b>"]
    [#px"(?<!')'{2}([^']+)'{2}(?!')" "<i>\\1</i>"]
    [#px"\\\\(['{}|_^`]|\\[|\\]|&quot;)" "\\1"]
    [#px"^\\s+|\\s+$" ""]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define paragraph-types
  (hash
   #px"^[|]" 'table
   #px"^=|^[*=].*[*=]$" 'heading
   #px"^!" 'comment
   #px"^>" 'bquote
   #px"^(\\s*[+:-]|\\s+[*]).*?[:]{2}" 'dlist
   #px"^\\s*:" 'dlist
   #px"^(\\s*[+-]|\\s+[*]).*" 'ulist
   #px"^\\s*\\d+[.()]|^\\s*#" 'olist
   #px"^\\s{2,}\\S+" 'continue-list
   #px"^$" 'void))

; this is pretty much the core of ratamarkup
; this is where we actually turn text into html
(define (process-std text #:options [options (hash)] #:tokens [tokens (list)])
  (let ([lines (list)] [paragraphs '()])
    (set! lines (regexp-split #px"\n" text))
    (map
     (lambda (line)
       (let ([type 'para])
         (for ([pattern (hash-keys paragraph-types)]
               #:unless (and (not (empty? paragraphs))
                             (eq? (hash-ref paragraph-types pattern) 'continue-list)
                             (not (set-member? '(dlist ulist olist) (car (last paragraphs)))))
               #:break (not (eq? type 'para)))
           (when (regexp-match? pattern line)
             (set! type (hash-ref paragraph-types pattern))))
         (when (eq? type 'continue-list)
           (if (empty? paragraphs)
               (set! type 'para)
               (set! type (car (last paragraphs)))))
         (when (and (eq? type 'ulist) (not (empty? paragraphs)) (eq? (car (last paragraphs)) 'dlist))
           (set! type 'dlist))
         (set! paragraphs (if (empty? paragraphs)
                              (list (cons type line))
                              (append paragraphs (list (cons type line)))))))
     lines)
    (string-join
     (map (lambda (p)
            ((hash-ref section-processors (first p)) (string-join (second p) "\n") #:options options))
          (compress paragraphs))
     "")))

(define (process-table text #:options [options (hash)] #:tokens [tokens (list)])
  (let ([output "<table>\n"] [header ""] [body ""] [rows null])
    (set! rows (string-split "\n" text))
    (map (lambda (row)
           (let ([cells null] [line null])
             (set! cells (regexp-split #px"(?<!\\\\)\\|" row))
             (set! line 
                   (format "\t<tr>\n~a\t</tr>\n"
                           (map (lambda (cell) format "\t\t<td>~a</td>\n" cell) cells)))
             (set! output (string-append output line))))
         rows)
    output))

(define (process-para text #:options [options (hash)] #:tokens [tokens (list)])
  (format "<p>\n\t~a\n</p>\n\n" (string-replace (ratamarkup-inline text #:options options) "\n" "\n\t" )))

(define (process-dlist text #:options [options (hash)] #:tokens [tokens (list)])
  (let ([items (for/list ([item (process-list-itemize text)])
                 (if (list? item)
                     (format "\t<dd>~v</dd>\n" item)
                     (if (regexp-match? #px"[:]{2}" item)
                         (format "\t<dt>~a</dt><dd>~a</dd>\n"
                                 (ratamarkup-inline
                                  (first (regexp-match #px"^.*?(?=\\s*?[:]{2})" item))
                                  #:options options)
                                 (ratamarkup-inline
                                  (first (regexp-match #px"(?<=[:]{2}).*" item))
                                  #:options options))
                         (format "\t<dd>~a</dd>\n" (xexpr->string item)))))])
    (string-join
     items
     ""
     #:before-first "<dl>\n"
     #:after-last "</dl>\n\n")))

(define (process-olist text #:options [options (hash)] #:tokens [tokens (list)])
  (let ([items (for/list ([item (process-list-itemize text)])
                 (if (list? item)
                     (format "\t<li>~v</li>\n" item)
                     (format "\t<li>~a</li>\n" (ratamarkup-inline item #:options options))))])
    (string-join
     items
     ""
     #:before-first "<ol>\n"
     #:after-last "</ol>\n\n")))

(define (process-ulist text #:options [options (hash)] #:tokens [tokens (list)])
  (let ([items (for/list ([item (process-list-itemize text)])
                 (if (list? item)
                     (format "\t<li>~v</li>\n" item)
                     (format "\t<li>~a</li>\n" (ratamarkup-inline item #:options options))))])
    (string-join
     items
     ""
     #:before-first "<ul>\n"
     #:after-last "</ul>\n\n")))

(define (process-list-itemize text)
  (let ([leadup (flatten (regexp-match* #px"(?s:^\\s*([+:*-]|\\d+[.)]|#))" text #:match-select car))]
        [leader ""]
        [leader-regexp ""])
    (set! leader (first leadup))
    (if (regexp-match? #px"^(\\s*)\\d+([.()])" leader)
      (set! leader-regexp (regexp-replace #px"^(\\s*)\\d+([.()])" leader "^\\1\\\\d+\\2"))
      (set! leader-regexp (string-append "^" leader)))
    (set! leader-regexp (regexp-replace* #px"((?<!\\\\d)[*+:().])" leader-regexp "\\\\\\1"))
    (set! leader-regexp (pregexp leader-regexp))
    (for/list ([line (string-split text "\n")])
      (if (regexp-match leader-regexp line)
          (regexp-replace #px"^\\s*" (substring line (string-length leader)) "")
          (list line)))))

(define section-processors
  (make-hash 
   `([table   . ,process-table]
     [std     . ,process-std]
     [para    . ,process-para]
     [bquote  .,(lambda (text #:options [options (hash)] #:tokens [tokens (list)])
                  (format "<blockquote>~a</blockquote>\n\n"
                          (regexp-replace #px"(?m:^)"
                                          (process-std (regexp-replace* #px"(?m:^> ?)" text "")
                                                       #:options options
                                                       #:tokens tokens)
                                          "\t")))]
     [dlist   . ,process-dlist]
     [olist   . ,process-olist]
     [ulist   . ,process-ulist]
     [comment . ,(lambda (text #:options [options (hash)] #:tokens [tokens (list)])
                   (format "<!-- ~a -->\n" (regexp-replace #px"(?m:^!)" text "")))]
     [void    . ,(lambda (text #:options [options (hash)] #:tokens [tokens (list)]) "")]
     [heading . ,(lambda (text #:options [options (hash)] #:tokens [tokens (list)])
                   (string-join
                    (for/list ([line (string-split text "\n")])
                      (let ([lvl (string-length (car (regexp-match #px"(?s:^[*=]+)" line)))])
                        (set! lvl (min 6 lvl))
                        (string-join
                         (list (format "<h~a>" lvl)
                               (string-replace
                                (xexpr->string
                                 (regexp-replace #px"(?s:^[*=]+\\s*(.*?)\\s*[*=]*$)" line "\\1"))
                                "\n" "")
                               (format "</h~a>" lvl))
                         "")))
                    "\n"
                    #:after-last "\n\n"))]
     [default . ,(lambda (text #:options [options null] #:tokens [tokens null])
                   (format "<!-- no section -->\n~a" text))])))

(define (add-section-processor key value)
  (hash-set! section-processors key value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compress items)
  (filter (lambda (x) (not (empty? (second x))))
                 (let ([out (list)] [acc '()] [type 'void])
                   (for ([item items])
                     (cond [(eq? type (car item)) (set! acc (append acc (list (cdr item))))]
                           [else (set! out (append out (list (list type acc))))
                                 (set! acc (list (cdr item)))
                                 (set! type (car item))]))
                   (unless (empty? acc)
                     (set! out (append out (list (list type acc)))))
                   out)))

(define (ratamarkup-inline text #:options [options #hash()])
  (string-join 
   (map (lambda (line)
          (let ([stage1 (regexp-replaces line inline-processors)]
                [links null]
                [images null])
            (set! links (regexp-match* #px"\\[\\[.*?(?<!\\\\)\\]\\]" stage1))
            (set! images (regexp-match* #px"\\{\\{.*?(?<!\\\\)\\}\\}" stage1))
            (for ([link links])
              (set! stage1 (string-replace stage1 link  (link-callback  link  #:options options))))
            (for ([image images])
              (set! stage1 (string-replace stage1 image (image-callback image #:options options))))
            stage1))
        (string-split text "\n"))
   "\n"))
  

; parse tokens
; valid tokens are key="value value", key=value and key
(define (parse-tokens [token-string string?])
  (let ([tokens null] [parsed-tokens '()])
    (set! tokens (flatten
                  (regexp-match*
                   #px"(\\S+=\".*?(?<!\\\\)\"|\\S+=\\S+|\\S+)"
                   token-string
                   #:match-select cdr)))
    (set! parsed-tokens
          (for/list ([token tokens])
            (cond
             [(regexp-match? "=\"" token)
              (set!-values (token)
                           (flatten (regexp-match* #px"(.*)?=\"(.*)\"$" token #:match-select cdr)))
              (set! token (cons (string->symbol (first token)) (second token)))
              token]
             [(regexp-match? "=" token) 
              (set!-values (token)
                           (flatten (regexp-match* #px"(.*)?=(.*)$" token #:match-select cdr)))
              (set! token (cons (string->symbol (first token)) (second token)))
              token]
             [else 
              (set! token (cons (string->symbol token) #t))
              token])))
    parsed-tokens))

(struct rm-section (type tokens body))

; split a section and capture the setup tokens
; if first line begins with § then fetch tokens in that first line (if any)
; otherwise the entire section is the body of a §std section
(define (sectionize text default)
  (let ([sbodies (regexp-split #px"(?m:^(?=§)|§$)" text)]
        [sections (list)])
    (for ([sbody sbodies])
      (if (regexp-match? #px"^§" sbody)
          (let ([head ""] [body ""] [tokens '()] [type 'std] [matches null])
            (set! matches
                  (flatten
                   (regexp-match* #px"(?s:^§([^§\n]+)?(.*)$)" sbody #:match-select cdr)))
            (set! head (if (eq? (car matches) #f) "" (first matches)))
            (set! body (second matches))
            (set! tokens (parse-tokens head))
            (unless (empty? tokens)
              (set! type (caar tokens))
              (set! tokens (cdr tokens)))
            (set! sections (append sections (list (rm-section type tokens body)))))
          (set! sections (append sections (list (rm-section default '() sbody))))))
    sections))

; sectionize and process sections
(define (ratamarkup text
                    #:default [default-type 'std]
                    #:options [options (make-hash '((version 1.0)))])
  (let ([sections (sectionize text default-type)] [output ""]
        [default-fn (hash-ref section-processors default-type)])
    (string-append*
     (for/list ([section sections])
       ((if (hash-has-key? section-processors (rm-section-type section))
            (hash-ref section-processors (rm-section-type section))
            default-fn)
        (rm-section-body section)
        #:options options
        #:tokens (rm-section-tokens section))))))

