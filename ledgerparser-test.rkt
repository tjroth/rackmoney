#lang racket/base
(require gregor)
(require megaparsack megaparsack/text)
(require rackunit "ledgerparser.rkt")


;Testing library for ledgerparser

;Dates Parser
(check-equal? (date->iso8601 (parse-result!(parse-string date/p "2016-01-24"))) "2016-01-24" "Date parse")
(check-equal? (date->iso8601 (parse-result!(parse-string date/p "2016/01/01"))) "2016-01-01" "Date parse")
(check-equal? (parse-result!(parse-string date/p "2016-01-01")) (date 2016 01 01))

;Words Parser
(check-equal? (parse-result!(parse-string words/p "a \n")) "a " "Words parse")
(check-equal? (parse-result!(parse-string words/p "a bunch of words\n")) "a bunch of words" "Words parse")
(check-equal? (parse-result!(parse-string words/p "a bunch of words;\n")) "a bunch of words" "Words parse")
(check-equal? (parse-result!(parse-string words/p "a bunch of words ; a comment\n")) "a bunch of words " "Words parse")
(check-equal? (parse-result!(parse-string words/p "numbers 111 and words\n")) "numbers 111 and words" "Words parse")
(check-equal? (parse-result!(parse-string words/p "     \n")) "     " "Words parse")

;Comment Parser
(check-equal? (parse-result!(parse-string comment/p "; a comment\n")) "a comment" "Comment parse")
(check-equal? (parse-result!(parse-string comment/p "; \n")) "" "Comment parse")
(check-equal? (parse-result!(parse-string comment/p ";number comment 222 3\n")) "number comment 222 3" "Comment parse")

;First line of transactino
(define first-line/1 "2016-11-25    Deposit Check\n")
(define first-line/2 "2016-11-25  Deposit Check ; mortgage payment\n")
(check-equal? (parse-result!(parse-string first-line/p first-line/1)) (list (date 2016 11 25) "Deposit Check" null))
(check-equal? (parse-result!(parse-string first-line/p first-line/2)) (list (date 2016 11 25) "Deposit Check" (list "mortgage payment")))

;Account Title Parser
(check-equal? (parse-result!(parse-string account/p "Banking")) "Banking" "Account parse")
(check-equal? (parse-result!(parse-string account/p "Banking:BOFA")) "Banking:BOFA" "Account parse")
(check-equal? (parse-result!(parse-string account/p "Banking:BOFA:")) "Banking:BOFA:" "Account parse")
(check-equal? (parse-result!(parse-string account/p "Banking:BOFA:12345")) "Banking:BOFA:12345" "Account parse")
(check-equal? (parse-result!(parse-string account/p ":Banking:BOFA:")) ":Banking:BOFA:" "Account parse")
(check-equal? (parse-result!(parse-string account/p "Banking:BOFA other stuff")) "Banking:BOFA" "Account parse")


;Cash value
(check-equal? (parse-result!(parse-string cash-value/p "$12.0")) 12.0)
(check-equal? (parse-result!(parse-string cash-value/p "$12.00")) 12.00)
(check-equal? (parse-result!(parse-string cash-value/p "$12.000")) 12.000)
(check-equal? (parse-result!(parse-string cash-value/p "$-12.0")) -12.0)
(check-equal? (parse-result!(parse-string cash-value/p "$-12.00")) -12.00)
(check-equal? (parse-result!(parse-string cash-value/p "$-12.000")) -12.000)

;Investment
(check-equal? (parse-result!(parse-string investment-value/p "FB 100.00 @ $75.00")) (list "FB" 100.00 75.00))
(check-equal? (parse-result!(parse-string investment-value/p "FB  100.00  @  $75.00")) (list "FB" 100.00 75.00))
(check-equal? (parse-result!(parse-string investment-value/p "FB -100.00 @ $75.00")) (list "FB" -100.00 75.00))
(check-equal? (parse-result!(parse-string investment-value/p "FB 100.0 @ $75.00")) (list "FB" 100.0 75.00))


;posting
(define posting/1 "  Assets:Banking   $12.00 \n")
(define posting/2 "  Assets:Etrade  FB 100.0 @ $75.0\n")
(define posting/3 "  Assets:Banking   $12.00 ;deposit to bank\n")
(define posting/4 "  Assets:Etrade  FB 100.0 @ $75.0 ; an investment\n")
(check-equal? (parse-result!(parse-string posting/p posting/1)) (list "Assets:Banking" 12.00 null))
(check-equal? (parse-result!(parse-string posting/p posting/2)) (list "Assets:Etrade" (list "FB" 100.0 75.0) null))
(check-equal? (parse-result!(parse-string posting/p posting/3)) (list "Assets:Banking" 12.00 '("deposit to bank")))
(check-equal? (parse-result!(parse-string posting/p posting/4)) (list "Assets:Etrade" (list "FB" 100.0 75.0) '("an investment")))

;transaction
(define t1 "2016-11-18 My first post ;important transaction \n  Assets:Checking $22.0\n  Assets:Checking:203322  $200.23 \n  Assets:Savings:1111 $-200.23\n")
(define t2 "2016-11-18 My second post ;another important transaction \n  assets:checking $22.0\n  Assets:Checking:203322  $200.00323 \n  Assets:Savings:1111 $-200.28\n")
(check-equal? (parse-result!(parse-string entry/p t1)) (list (list (date 2016 11 18) "My first post" (list "important transaction"))
                                                             (list (list "Assets:Checking" 22.0 null)
                                                                   (list "Assets:Checking:203322" 200.23 null)
                                                                   (list "Assets:Savings:1111" -200.23 null))))
                                                             
              
              
                                                                  