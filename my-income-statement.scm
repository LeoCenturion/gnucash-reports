;; -*-scheme-*-

;; This is a minimum report definition in GnuCash.
;; It illustrates the the minimum definitions needed to create
;; a new GnuCash report.
;; It will create an empty page with heading 'Prototype'.
;; To be used as template.

;; ------------------------------------------------------------------
;; Top-level definitions
;; ------------------------------------------------------------------

(define-module (gnucash reports example prototype))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils)) ; for gnc:version and (G_ ...)
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))
(use-modules (rnrs lists))
(use-modules (gnucash engine gnc-numeric))
(use-modules (ice-9 optargs))
(use-modules (srfi srfi-1))
(debug-enable 'backtrace)

;; ------------------------------------------------------------------
;; Define the Options for this report
;; ------------------------------------------------------------------
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-to-date (N_ "End Date"))
(define (options-generator)
  (let* ((options (gnc:new-options)))

    (gnc:options-add-currency!
     options gnc:pagename-general
     optname-report-currency "b")

    (gnc:options-add-price-source!
     options gnc:pagename-general
     optname-price-source "c" 'pricedb-nearest)

    options))


;; ------------------------------------------------------------------
;; Render the HTML document
;; ------------------------------------------------------------------

(define (document-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  ;; (let* ((report-currency (get-option gnc:pagename-general
  ;;                                     optname-report-currency))
  ;;        (price-source (get-option gnc:pagename-general
  ;;                                  optname-price-source))
  ;;        (to-date-t64 (gnc:time64-end-day-time
  ;;                      (gnc-dmy2time64 28 03 2024)))
  ;;        (exchange-fn (gnc:case-exchange-fn
  ;;                      price-source report-currency to-date-t64))
  ;;        (cc (get-comm-account-balance (get-account-by-name "Assets:Investments:invertir online:stocks:SPY") '(01 03 2024) '(01 04 2024)))
  ;;        (commodity (apply gnc:make-gnc-monetary (cc 'getpair (xaccAccountGetCommodity (get-account-by-name "Assets:Investments:invertir online:stocks:SPY")) #f))))

  ;;   ;; (gnc-numeric-convert
  ;;   ;;  (gnc:gnc-monetary-amount (exchange-fn commodity report-currency))
  ;;   ;;  (gnc-commodity-get-fraction commodity)
  ;;   ;;  GNC-RND-ROUND)

    
  ;;   (gnc:debug "sum-collector")
  ;;   (gnc:debug (gnc:sum-collector-commodity
  ;;               cc  report-currency exchange-fn))

  ;;   )


  (let*
      ((doc (gnc:make-html-document))
       (table (gnc:make-html-table))
       (report (gnc:make-html-text
                (gnc:html-markup-h3
                 (format #f (G_ "~a")
                         gnc:optname-reportname))))
       (report-currency (get-option gnc:pagename-general
                                    optname-report-currency))
       (price-source (get-option gnc:pagename-general
                                 optname-price-source))
       (to-date-t64 (gnc:time64-end-day-time
                     (gnc-dmy2time64 28 03 2024)))
       (exchange-fn (gnc:case-exchange-fn
                     price-source report-currency to-date-t64))
       (commodity->currency (lambda (cc) (gnc:sum-collector-commodity cc  report-currency exchange-fn)))

       (all-accounts (append (charity-account-list) (retirement-fund-account-list) (emergency-fund-account-list) (housing-account-list) (utilities-account-list) (groceries-account-list) (transportation-account-list) (clothing-account-list) (medical-health-account-list) (personal-expenses-account-list) (recreation-account-list) (debt-account-list)))

       (sum-accounts (lambda (accs) (reduce gnc:monetary+ 0 (map (lambda (acc-name) (account->monetary acc-name '(01 03 2024) '(01 04 2024))) accs)))))


    (gnc:html-table-append-row! table (list "Item" "Sub item" "Subtotal" "total" "actual" "difference"))

    (gnc:html-table-append-row! table (list "Charity" "-" "-" (sum-accounts (charity-account-list)) "?" "?"))
    (add-to-table! (charity-account-list) table '(01 03 2024) '(01 04 2024))

    (let ((total-savings (sum-accounts (append (retirement-fund-account-list) (emergency-fund-account-list)))))
      (gnc:html-table-append-row! table (list "Savings" "-" "-" total-savings "?" "?")))

    (gnc:html-table-append-row! table (list "Retirement fund" "-" "-" "-" "-" "-"))
    (add-to-table! (retirement-fund-account-list) table '(01 03 2024) '(01 04 2024) #:conversion-fn commodity->currency)
    (gnc:html-table-append-row! table (list "Emergency Fund" "-" "-" "?" "?" "?"))
    (add-to-table! (emergency-fund-account-list) table '(01 03 2024) '(01 04 2024) #:conversion-fn commodity->currency)

    (gnc:html-table-append-row! table (list "Housing" "-" "-" (sum-accounts (housing-account-list)) "?" "?"))
    (add-to-table! (housing-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Utilities" "-" "-" (sum-accounts (utilities-account-list)) "?" "?"))
    (add-to-table! (utilities-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Groceries" "-" "-" (sum-accounts (groceries-account-list)) "?" "?"))
    (add-to-table! (groceries-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Transportation" "-" "-" (sum-accounts (transportation-account-list)) "?" "?"))
    (add-to-table! (transportation-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Clothing" "-" "-" (sum-accounts (clothing-account-list)) "?" "?"))
    (add-to-table! (clothing-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Medical/Health" "-" "-" (sum-accounts (medical-health-account-list)) "?" "?"))
    (add-to-table! (medical-health-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Personal" "-" "-" (sum-accounts (personal-expenses-account-list))  "?" "?"))
    (add-to-table! (personal-expenses-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Recreation" "-" "-" (sum-accounts (recreation-account-list))  "?" "?"))
    (add-to-table! (recreation-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Debt" "-" "-" (sum-accounts (debt-account-list)) "?" "?"))
    (add-to-table! (debt-account-list) table '(01 03 2024) '(01 04 2024))

    (gnc:html-table-append-row! table (list "Grand Total" "" "" "total" "actual" "difference"))
    (let ((total (gnc:monetary->string (sum-accounts all-accounts))))
      (gnc:html-table-append-row! table (list "" "" ""  total "?")))

    (gnc:html-document-add-object! doc table)
    doc))

(define*  (account-balance->monetary account-name from to #:key (domestic "Trading:CURRENCY:ARS"))
  (let* ((account-obj (get-account-by-name account-name)))
    (if (gnc:account-is-stock? account-obj)
        (gnc:exchange-by-pricedb-nearest (get-comm-account-balance account-obj from to) (xaccAccountGetCommodity (get-account-by-name domestic)) (apply gnc-dmy2time64 to))
        (get-comm-account-balance account-obj from to))))

(define* (make-row account-full-name display-name from to #:key conversion-fn)
  (let*
      ((account-obj (get-account-by-name account-full-name))
       (account-balance (gnc:monetary->string (account-balance->monetary account-full-name from to))))
    (list "" display-name account-balance "-" "-")))

(define* (add-to-table! account-list table from to #:key conversion-fn)
  (let*
      ((add-row-to-table (lambda (account-name) (gnc:html-table-append-row! table (make-row account-name account-name from to #:conversion-fn conversion-fn)))))

    (for-each add-row-to-table account-list)))

(define (groceries-account-list) (list "Expenses:Food:Groceries" "Expenses:Food:Restaurant-Delivery" ))
(define (transportation-account-list) (list "Expenses:Transportation"))
(define (clothing-account-list) (list "Expenses:Clothes"))
(define (charity-account-list) (list "Expenses:Charity"))
(define (emergency-fund-account-list) (list "Assets:Investments:Brokerage Account:Mutual Fund:HF Pesos" "Assets:Investments:Brokerage Account:Mutual Fund:HF Pesos PLUS"))
(define (retirement-fund-account-list) (list "Assets:Investments:invertir online:stocks:SPY" "Assets:Investments:ESPP"))
(define (utilities-account-list) (list "Expenses:Utilities:Electric" "Expenses:Utilities:Expensas" "Expenses:Utilities:Garbage collection" "Expenses:Utilities:Gas" "Expenses:Utilities:Internet" "Expenses:Utilities:Phone" "Expenses:Utilities:Water"))
(define (housing-account-list) (list "Expenses:Home:Insurance" "Expenses:Home:Mobiliario" "Expenses:Home:Repair & Maintenance"))
(define (medical-health-account-list) (list "Expenses:Medical Expenses:Medicine"))
(define (personal-expenses-account-list) (list "Expenses:personal:Education" "Expenses:personal:Gifts" "Expenses:personal:Hair, Self care, & beauty" "Expenses:personal:Misc" "Expenses:personal:Subscriptions" "Expenses:personal:Throaway" "Expenses:personal:Workout"))
(define (recreation-account-list) (list "Expenses:Entretainment:Recreation" "Expenses:Entretainment:Travel"))
(define (debt-account-list) (list "Liabilities:Credit Card:BBVA Credit Card" "Liabilities:Credit Card:HSBC Mastercard" "Liabilities:Credit Card:HSBC Visa" "Liabilities:Loans"))
(define (table)
  (gnc:make-html-table ))
;; ------------------------------------------------------------------
;; Define the actual report
;; ------------------------------------------------------------------



;; (gnc:define-report
;;  'version 1
;;  'name (N_ "Prototype")
;;  'report-guid "d895ff06d05d41c0a1beb117124a46d4"
;;  'menu-tip (N_ "Unstable. Used for Testing.")
;;  'menu-path (list gnc:menuname-example)
;;  'options-generator options-generator
;;  'renderer document-renderer)

;; custom functions


(define (get-all-accounts)
  (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))

(get-all-accounts)

;; (get-account-by-name "Assets:Investments:invertir online:stocks:SPY")
;; (cadr ((get-comm-account-balance (get-account-by-name "Assets:Investments:invertir online:stocks:SPY") '(01 03 2024) '(01 04 2024)) 'getpair (xaccAccountGetCommodity (get-account-by-name "Assets:Investments:invertir online:stocks:SPY")) #f))


(define (get-account-by-name full-name)
  (let ((func (lambda (acc) (equal? full-name (gnc-account-get-full-name acc)))))
    (find func (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))))

(let* ((account (get-account-by-name "Assets:Investments:invertir online:stocks:SPY"))
       (commodity (get-account-by-name "Trading:CURRENCY:ARS")))
  (gnc:exchange-by-pricedb-nearest (get-comm-account-balance account '(01 03 2024) '(28 03 2024)) (xaccAccountGetCommodity commodity) (gnc-dmy2time64 29 03 2024)))


(define (get-account-balance account from to)
  (exact->inexact (gnc:account-get-balance-interval account (apply gnc-dmy2time64 from) (apply gnc-dmy2time64 to) #t)))

(define (get-comm-account-balance account from to)
  ((gnc:account-get-comm-balance-interval account (apply gnc-dmy2time64 from) (apply gnc-dmy2time64 to) #t) 'getmonetary (xaccAccountGetCommodity account) #f))

(get-comm-account-balance (get-account-by-name "Expenses:Food:Groceries") '(01 03 2024) '(28 03 2024))

;; (get-account-by-name "Trading:CURRENCY:ARS")

(export options-generator)
(export document-renderer)
