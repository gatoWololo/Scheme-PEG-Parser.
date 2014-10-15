#|
Scheme program that given a string will parse the string according to the parsing expression
grammar:
 S -> &(A c) a+ B !(a|b|c)
 A -> a A? b
 B -> b B? c
Or the language L = {a^nb^nc^n| n >= 1}
|#
;==========================================================================================
#|
Filter only the rule we want based on it's non-terminal symbol. This is wrapped in a list
so we get the car first, now we have (N (rule)) so we get the cdr (rule ()) so we car it.
Hence "cadar"
|#
(define get-rule
  (λ (N)
    (cadar (filter (lambda (x) (eq? N (car x))) grammar))))
;==========================================================================================
;Pretty self explanatory, returns wheter the passed expression is a predicate rule or not :)
(define is-predicate
  (λ (x)
    (or (eq? '! (car x)) (eq? '& (car x)))))
;==========================================================================================
;Envaluation function, applies sematic function rule e to our string.
(define E
  (λ (e string)
     (let* ((content (cdr e))
           (symbol (car e))
           (expression (car content)))
       (case symbol
         ((T) (handleTerminal string expression))
         ((N) (handleNonTerminal string content))
         ((*) (handleSequence string content))
         ((/) (handleChoice string content))
         ((**) (handleZeroOrMore  string expression))
         ((+) (handleOneOrMore string expression))
         ((?) (handleOption string expression))
         ((&) (handleAnd string expression))
         ((!) (handleNot string expression))
         (else "Error, Unkown Symbol")))))
;==========================================================================================
;Terminal symbol: compare symbol with head of string.
(define handleTerminal
  (λ (string content)
     ;Special epsilon case, consume no input and can't fail.
     (if (eq? content 'epsilon)
         (list #t string)
         ;If all input has been consumed and we still have a terminal comparison we always
         ;fail.
         (if (string=? string "")
             (list #f string)
             (let ((head (substring string 0 1)) ;First char in string.
                   (tail (substring string 1))) ;Rest of string.
               (if (string=? (symbol->string content) head)
                   (list #t tail)
                   (list #f string)))))))
;==========================================================================================
;Non-terminal symbol: Get rules for non terminal symbol A and evaluate with those rules.
(define handleNonTerminal
  (λ (string content)
     (E (get-rule (car content)) string)))
;==========================================================================================
;Sequence: e1 e2.
(define handleSequence
  (λ (string content)
     ;Start by parsing (E[e1] (string)
     (let* ((e1 (car content))
            (e2 (cadr content))
            (parseResult (E e1 string))
            (r (car parseResult))
            (stringLeft (cadr parseResult)))
       ;Parse e1 first, if that suceeds parse e2.
       (if (eq? r #f)
           (list #f string)
           ;If e1 was a predicate the input should have passed untouched.
           (if (is-predicate e1)
               (E e2 string)
               (E e2 stringLeft))))))
;==========================================================================================
;Choice e1 | e2.
(define handleChoice
  (λ (string content)
     ;Start by attempting to parse (E[e1] (string).
     (let* ((e1 (car content))
            (e2 (cadr content))
            (parseResult (E e1 string))
            (r (car parseResult))
            (stringLeft (cadr parseResult)))
       ;Parse e1 first, if that suceeds parse e2.
       (if (eq? r #t)
           (list #t stringLeft)
           (E e2 string)))))
;==========================================================================================
;0-or-more: e*. E[e*] string = E[ee* | epsilon] string.
(define handleZeroOrMore
  (λ (string content)
     (E (list '/ (list '* content (list '** content)) '(T epsilon)) string)))
;==========================================================================================
;1-or-more: e+. E[e*] string = E[ee*] string
(define handleOneOrMore
  (λ (string content)
     (E (list '* content (list '** content)) string)))
;==========================================================================================
;Optional: e?. E[e?] string = E[e | epsilon] string.
(define handleOption
  (λ (string content)
     (E (list '/ content '(T epsilon)) string)))
;==========================================================================================
;Parse string but do not consume input.
(define handleAnd
  (λ (string content)
     (let ((r (car (E content string))))
       (list r string))))
;==========================================================================================
;Parse string and negate but do not consume input.
(define handleNot
  (λ (string content)
     (let ((r (car (E content string))))
       (list (not r) string))))
;==========================================================================================
;Predicate function return true in string is grammar, else false.
(define P
  (λ (input)
     (if (car (E (get-rule 'S) input))
         #t
         #f)))
;==========================================================================================
;;Actual grammar definition as explained in header.
(define grammar
  (list
   (list 'S '(* (& (* (N A) (T c)))   ;; S -> &(A c) a+ B !(a|b|c)
                (* (+ (T a)) (* (N B) (! (/ (T a) (/ (T b) (T c))))))))
   (list 'A '(* (T a) (* (? (N A)) (T b))))   ;; A -> a A? b
   (list 'B '(* (T b) (* (? (N B)) (T c)))))) ;; B -> b B? c
;==========================================================================================
;Main procedure queries for string and parsers grammar. Mains in scheme are ugly...
(define main
  (λ ()
     (begin
       (display "Please enter expression to parse:")
       (newline)
       (let ((input (read-line)))
         (if (string=? "quit" input)
             ()
             (begin
               (let ((results (P input)))
                 (if results
                     (begin
                     (display "Expression was in the grammar!")
                     (newline)
                     (main))
                     (begin
                       (display "Expression was not in the grammar >:(")
                       (newline)
                       (main))))))))))
;==========================================================================================
(display "Type \"quit\" to quit.")
(newline)
(main)
