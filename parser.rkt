#lang racket

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require gregor)

;parse two successive digits
(define two-digits/p
  (do [one <- digit/p]
    [two <- digit/p]
    (pure (string->number (string one two)))))

;parse a year, wraps interger/p
(define year/p
  integer/p)

;parse a month two digit format wraps parse-two-digits/p
(define month/p
  two-digits/p)

;parse date separator
(define date-sep/p
  (or/p (char/p #\-) (char/p #\/)))

;parse a day two digit format, wraps parse-two-digitis/p
(define day/p
  two-digits/p)

;parse a full date format YYY-MM-DD
(define date/p
  (do [y <- year/p]
    date-sep/p;(char/p #\-)
    [m <- month/p]
    date-sep/p;(char/p #\-)
    [d <- day/p]
    (pure (date y m d))))

;parse any sequence of words separated by one or more spaces
;a word can consist only of letters or numbers
(define words/p
  (do [charlist <- (many+/p (or/p letter/p digit/p (char/p #\space)))]
    (pure (list->string charlist))))

;parse a word
(define single-word/p
  (many+/p letter/p))


;parse comment
(define comment/p
  (do (char/p #\;)
    ;(many*/p (char/p #\space))
    [comment <- words/p]
    (pure (string-trim comment))))

;parse account
(define account/p
  (do [acc <- (many+/p (or/p letter/p digit/p (char/p #\:)))]
    (pure (list->string acc))))

;parse negative integer to list of characters
(define negative-integer/p
  (do [s <- (char/p #\-)]
    [ds <- (many+/p digit/p)]
    (pure (flatten (list s ds)))))

;parse positive or negative integer to list of charracters
(define integer-any/p
  (or/p negative-integer/p (many+/p digit/p)))

;parse float value
(define parse-value/p
  (do 
    [amt <- integer-any/p]
    (char/p #\.)
    [dec <- integer-any/p]
    (pure (string->number(list->string  (flatten (list amt #\. dec)))))))

;parse cash value
(define cash-value/p
  (do (char/p #\$)
    [v <- parse-value/p]
    (pure v)))

;one or more spaces
(define spaces/p
  (many+/p (char/p #\space)))

;parse investment transaction
(define investment-value/p
  (do  [comm <- single-word/p]
    spaces/p
    [num-shares <- parse-value/p]
    spaces/p 
    (char/p #\@)
    spaces/p
    [price <- cash-value/p]
    (pure (list (list->string comm) num-shares price))))
    


;parse first line of an entry
(define first-line/p
  (do [d <- date/p]
    (char/p #\space)
    [w <- words/p]
    [c <- (many*/p comment/p)]
    (char/p #\newline)
    (pure (list d (string-trim w) c))))

;parse posting line
(define posting/p
  (do (char/p #\space)
    (char/p #\space)
    (many*/p (char/p #\space))
    [acc <- account/p]
    (many+/p (char/p #\space))
    [val <- (or/p (try/p investment-value/p) cash-value/p)]
    (many*/p (char/p #\space))
    [c <- (many*/p comment/p)]
    (many*/p (char/p #\space))
    (char/p #\newline)
    (pure (list acc val c))))


;define parse-entry
(define entry/p
  (do [fl <- first-line/p]
    [t1 <- posting/p]   
    [ts <- (many+/p posting/p)]
    (pure (list fl (cons t1 ts)))))

;define parse-file
(define file/p
  (do [js <- (many*/p (or/p (many*/p (char/p #\newline)) entry/p))]
    (pure js)))

(define t1 "2016-11-18 My first post ;important transaction \n  assets:checking $22.0\n  Assets:Checking:203322  $200.00323 \n  Assets:Savings:1111 $-200.28\n")
(define t2 "2016-11-18 My second post ;another important transaction \n  assets:checking $22.0\n  Assets:Checking:203322  $200.00323 \n  Assets:Savings:1111 $-200.28\n")


(define file2 "2016/10/17  Reinvest Dividend JCPUX
    Assets:Investments:Principal401k:JCPUX  JCPUX 168.593525  @ $8.34
    Income:DRIP:Principal401k  $2.33

2016/10/17  Buy JCPUX
    Assets:Investments:Principal401k:JCPUX  JCPUX 89.928057553  @ $8.34
    Assets:Investments:Principal401k:Cash  $-200.00


2016/10/17  RRA Contribution JCPUX
    Assets:Investments:Principal401k:JCPUX  JCPUX 89.4988  @ $8.38
    Income:RRA:ProfitSharing  $-20000.00

2016/10/01  Adjustment
    Assets:Investments:Principal401k:JCPUX  JCPUX 151.701426907  @ $8.34
    Equity:OpeningBalances  $-222.00\n")
  
(define file (string-append t1 "\n" t2))
(parse-result! (parse-string file/p file2))

(provide date/p
         words/p
         comment/p
         account/p
         first-line/p
         cash-value/p
         investment-value/p
         posting/p
         entry/p)