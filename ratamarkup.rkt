#lang racket/base

(provide ratamarkup
         (rename-out [inline-processors ratamarkup-inline-processors]
                     [link-callback     ratamarkup-link-callback]))
(require racket/string
         racket/list
         xml)

(define link-callback (lambda (link)
                        (let ([m (flatten (regexp-match* "^\\[\\[(.*)?(?<!\\\\)\\|(.*?)(?<!\\\\)\\|(.*?)]]$"
                                                link
                                                #:match-select cdr
                                                ))])
                          (format "<a href=\"~a\"~a>~a</a>"
                                  (first m)
                                  (if (= (string-length (second m)) 0) "" 
                                      (string-append " " (regexp-replace* #px"&quot;" (second m) "\"")))
                                  (third m)))))


(define inline-processors
  '([#px"&" "\\&amp;"]
    [#px"<" "\\&lt;"]
    [#px">" "\\&gt;"]
    [#px"\"" "\\&quot;"]
    [#px"(?<!')(?<!\\\\)'{4,5}([^']+)(?<!\\\\)'{4,5}(?!')" "<b><i>\\1</i></b>"]
    [#px"(?<=\\b)(?<!\\\\)__(.*?)(?<!\\\\)__(?=$|\\b)" "<u>\\1</u>"]
    [#px"(?<=\\b)(?<!\\\\)_-(.*?)(?<!\\\\)-_(?=$|\\b)" "<s>\\1</s>"]
    [#px"(?<=\\b)(?<!\\\\)&quot;&quot;(.*?)(?<!\\\\)&quot;&quot;(?=$|\\b)" "<s>\\1</s>"]
    [#px"(?<!\\\\)\\[\\[([^]|]+|\\\\\\])\\]\\]" "[[\\1||\\1]]"]
    [#px"(?<!\\\\)\\[\\[([^]|]+|\\\\\\])\\]\\]" "[[\\1||\\1]]"]
    [#px"(?<!\\\\)\\[\\[([^]|]+|\\\\\\]|\\\\\\|)\\|([^]|]+|\\\\\\])\\]\\]" "[[\\1||\\2]]"]
    [#px"(?<!')'{3}([^']+)'{3}(?!')" "<b>\\1</b>"]
    [#px"(?<!')'{2}([^']+)'{2}(?!')" "<i>\\1</i>"]
    [#px"\\\\(['{}|_^]|\\[|\\]|&quot;)" "\\1"]))

(define (ratamarkup-inline text)
  (string-join 
   (map (lambda (line)
          (let ([stage1 (regexp-replaces line inline-processors)]
                [links null])
            (set! links (regexp-match* #px"\\[\\[.*?(?<!\\\\)\\]\\]" stage1))
            (for ([link links]) (set! stage1 (string-replace stage1 link (link-callback link))))
            stage1))
        (string-split text "\n"))
   "\n"))
  
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

(define (process-table text
                       #:options [options (hash)]
                       #:tokens [tokens (list)])
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

(define paragraph-types
  (hash
   #px"^[|]" 'table
   #px"^="   'heading
   #px"^$"   'void))

; this is pretty much the core of ratamarkup
; this is where we actually turn text into html
(define (process-std text
                     #:options [options (hash)]
                     #:tokens [tokens (list)])
  (let ([lines (list)] [paragraphs '()])
    (set! lines (regexp-split #px"\n" text))
    (map
     (lambda (line)
       (let ([type 'para])
         (for ([pattern (hash-keys paragraph-types)])
           #:break (not (eq? type 'para))
           (when (regexp-match? pattern line)
             (set! type (hash-ref paragraph-types pattern))))
         (set! paragraphs (if (empty? paragraphs)
                              (list (cons type line))
                              (append paragraphs (list (cons type line)))))))
     lines)
    (string-join
     (map (lambda (p)
            ((hash-ref section-processors (first p)) (string-join (second p) "\n")))
          (compress paragraphs))
     "")))

(define section-processors
  (hash 
   'table   process-table
   'std     process-std
   'para    (lambda (text) (format "<p>\n\t~a\n</p>\n" (string-replace (ratamarkup-inline text) "\n" "\n\t" )))
   'void    (lambda (text) "")
   'heading (lambda (text)
              (string-join
               (map (lambda (line)
                      (let ([lvl (string-length (car (regexp-match #px"(?s:^=+)" line)))])
                        (set! lvl (min 6 lvl))
                        (string-join
                         (list (format "<h~a>" lvl)
                               (string-replace
                                (xexpr->string (regexp-replace #px"(?s:^=+\\s*(.*?)\\s*=*$)" line "\\1"))
                                 "\n" "")
                               (format "</h~a>" lvl))
                         "")))
                    (string-split text "\n"))
              "\n"
              #:after-last "\n\n"))
   'default (lambda (text) (format "<!-- no section -->\n~a" text))))

; parse tokens
; valid tokens are key="value value", key=value and key
(define (parse-tokens [token-string string?])
  (let ([tokens null])
    (set! tokens (flatten
                  (regexp-match*
                   #px"(\\S+=\".*?(?<!\\\\)\"|\\S+=\\S+|\\S+)"
                   token-string
                   #:match-select cdr)))
    (map (lambda (token)
           (cond
            [(regexp-match? "=\"" token) (set!-values 
                                          (token)
                                          (flatten
                                           (regexp-match*
                                            #px"(.*)?=\"(.*)\"$"
                                            token
                                            #:match-select cdr)))]
            [(regexp-match? "=" token) (set!-values
                                        (token)
                                        (flatten
                                         (regexp-match*
                                          #px"(.*)?=(.*)$"
                                          token
                                          #:match-select cdr)))]))
         tokens)
    tokens))

(struct rm-section (type tokens body))

; split a section and capture the setup tokens
; if first line begins with § then fetch tokens in that first line (if any)
; otherwise the entire section is the body of a §std section
(define (sectionize text default)
  (let ([sbodies (regexp-split #px"(?m:^(?=§))" text)]
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
              (set! type (string->symbol (car tokens)))
              (set! tokens (cdr tokens)))
            (set! sections (append sections (list (rm-section type tokens body)))))
          (set! sections (append sections (list (rm-section default '() sbody)))))
      )
    sections))

; sectionize and process sections
(define (ratamarkup text
                    #:default [default 'std])
  (let ([sections (sectionize text default)] [output ""]
        [default-proc (lambda () (hash-ref section-processors default))])
    (string-join
     (map (lambda (section)
            ((hash-ref section-processors (rm-section-type section) default-proc)
             (rm-section-body section)
             #:tokens (rm-section-tokens section)))
          sections)
     "")))

