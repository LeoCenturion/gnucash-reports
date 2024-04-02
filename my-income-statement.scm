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
(define optname-from-date (N_ "Start Date"))

(define (options-generator)
  (let* ((options (gnc:new-options)))

    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

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
(define (get-option pagename optname report-obj)
  (gnc:option-value
   (gnc:lookup-option
    (gnc:report-options report-obj) pagename optname)))

(define (document-renderer report-obj)
  (let*
      ((doc (gnc:make-html-document))
       (expenses-table (make-expenses-table report-obj))
       (income-table (make-income-table report-obj))
       (expenses-classification-table (make-classification-table report-obj)))

    (gnc:html-document-add-object! doc expenses-table)
    (gnc:html-document-add-object! doc (gnc:make-html-text (gnc:html-markup-h1 "Expenses by type")))
    (gnc:html-document-add-object! doc expenses-classification-table)
    (gnc:html-document-add-object! doc (gnc:make-html-text (gnc:html-markup-h1 "Total income")))
    (gnc:html-document-add-object! doc income-table)

    doc))

(define (make-classification-table report-obj)
  (let* ((table (gnc:make-html-table))
         (get-option (lambda (page optname) (get-option page optname report-obj)))
         (report-currency (get-option gnc:pagename-general
                                 optname-report-currency))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))
         (init-date from-date-t64)
         (end-date to-date-t64)
         (sum-accounts (lambda (accs) (reduce gnc:monetary+ 0 (map (lambda (acc-name) (account-balance->monetary acc-name init-date end-date)) accs))))
         (less-variable (append (utilities-account-list)
                                '("Expenses:Food:Groceries")
                                (transportation-account-list)
                                '("Expenses:personal:Hair, Self care, & beauty" "Expenses:personal:Subscriptions" "Expenses:personal:Workout")
                                '("Expenses:Taxes:AGIP")))
         (variable-necessary (append '("Expenses:Home:Insurance" "Expenses:Home:Mobiliario" "Expenses:Home:Repair & Maintenance")
                                     (charity-account-list)
                                     (clothing-account-list)
                                     '("Expenses:personal:Education" "Expenses:personal:Misc")))
         (unnecessary (append '("Expenses:Food:Restaurant-Delivery" "Expenses:personal:Gifts")
                              (recreation-account-list))))
    (gnc:html-table-append-row! table (list "Less Variable" (sum-accounts less-variable)))
    (gnc:html-table-append-row! table (list "Variable Necessary" (sum-accounts variable-necessary)))
    (gnc:html-table-append-row! table (list "Unnecessary" (sum-accounts unnecessary)))
    (gnc:html-table-append-row! table (list "Debt" (sum-accounts (debt-account-list))))
    (gnc:html-table-append-row! table (list "Total" (gnc:monetary+ (sum-accounts  (debt-account-list))
                                                                   (sum-accounts less-variable)
                                                                   (sum-accounts variable-necessary)
                                                                   (sum-accounts unnecessary))))
    table))


(define (make-income-table report-obj)
  (let* ((table (gnc:make-html-table))
         (get-option (lambda (page optname) (get-option page optname report-obj)))
         (report-currency (get-option gnc:pagename-general
                                 optname-report-currency))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))
         (init-date from-date-t64)
         (end-date to-date-t64)
         (sum-accounts (lambda (accs) (reduce gnc:monetary+ 0 (map (lambda (acc-name) (account-balance->monetary acc-name init-date end-date)) accs))))
         (base-income (gnc:monetary-neg (account-balance->monetary "Income:Salary" from-date-t64 to-date-t64)))
         (bonus-income (gnc:monetary-neg (account-balance->monetary "Income:Bonus" from-date-t64 to-date-t64)))
         (reimbursments (gnc:monetary-neg (account-balance->monetary "Income:salesforce reimburstment" from-date-t64 to-date-t64)))
         (deductions (sum-accounts (list "Expenses:Taxes:Deductions" "Expenses:Taxes:Ganancias")))
         (gross-income (gnc:monetary+ base-income bonus-income reimbursments)))

    (gnc:html-table-append-row! table (list "Base income" base-income))
    (gnc:html-table-append-row! table (list "Bonus" bonus-income))
    (gnc:html-table-append-row! table (list "Reimbursment" reimbursments))
    (gnc:html-table-append-row! table (list "Gross Income" gross-income))
    (gnc:html-table-append-row! table (list "Deductions" deductions))
    (gnc:html-table-append-row! table (list "Net Income" (gnc:monetary+ gross-income (gnc:monetary-neg deductions))))


    table))

(define (make-expenses-table report-obj)
  (let*
      ((table (gnc:make-html-table))
       (get-option (lambda (page optname) (get-option page optname report-obj)))
       (report-currency (get-option gnc:pagename-general
                                    optname-report-currency))
       (price-source (get-option gnc:pagename-general
                                 optname-price-source))
       (from-date-t64 (gnc:time64-start-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-from-date))))
       (to-date-t64 (gnc:time64-end-day-time
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general
                                  optname-to-date))))
       (init-date from-date-t64)
       (end-date to-date-t64)
       (exchange-date init-date)
       (exchange-fn (gnc:case-exchange-fn
                     price-source report-currency exchange-date))
       (commodity->currency (lambda (cc) (gnc:sum-collector-commodity cc  report-currency exchange-fn)))

       (all-accounts (append (charity-account-list) (retirement-fund-account-list) (emergency-fund-account-list) (housing-account-list) (utilities-account-list) (groceries-account-list) (transportation-account-list) (clothing-account-list) (medical-health-account-list) (personal-expenses-account-list) (recreation-account-list) (debt-account-list)))

       (sum-accounts (lambda (accs) (reduce gnc:monetary+ 0 (map (lambda (acc-name) (account-balance->monetary acc-name init-date end-date)) accs)))))

    (gnc:html-table-append-row! table (list "Item" "Sub item" "Subtotal" "total" "actual" "difference"))

    (gnc:html-table-append-row! table (list "Charity" "-" "-" (sum-accounts (charity-account-list)) "?" "?"))
    (add-to-table! (charity-account-list) table init-date end-date)

    (let ((total-savings (sum-accounts (append (retirement-fund-account-list) (emergency-fund-account-list)))))
      (gnc:html-table-append-row! table (list "Savings" "-" "-" total-savings "?" "?")))

    (gnc:html-table-append-row! table (list "Retirement fund" "-" "-" "-" "-" "-"))
    (add-to-table! (retirement-fund-account-list) table init-date end-date #:conversion-fn commodity->currency)
    (gnc:html-table-append-row! table (list "Emergency Fund" "-" "-" "?" "?" "?"))
    (add-to-table! (emergency-fund-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Housing" "-" "-" (sum-accounts (housing-account-list)) "?" "?"))
    (add-to-table! (housing-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Utilities" "-" "-" (sum-accounts (utilities-account-list)) "?" "?"))
    (add-to-table! (utilities-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Groceries" "-" "-" (sum-accounts (groceries-account-list)) "?" "?"))
    (add-to-table! (groceries-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Transportation" "-" "-" (sum-accounts (transportation-account-list)) "?" "?"))
    (add-to-table! (transportation-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Clothing" "-" "-" (sum-accounts (clothing-account-list)) "?" "?"))
    (add-to-table! (clothing-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Medical/Health" "-" "-" (sum-accounts (medical-health-account-list)) "?" "?"))
    (add-to-table! (medical-health-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Personal" "-" "-" (sum-accounts (personal-expenses-account-list))  "?" "?"))
    (add-to-table! (personal-expenses-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Recreation" "-" "-" (sum-accounts (recreation-account-list))  "?" "?"))
    (add-to-table! (recreation-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Debt" "-" "-" (sum-accounts (debt-account-list)) "?" "?"))
    (add-to-table! (debt-account-list) table init-date end-date)

    (gnc:html-table-append-row! table (list "Grand Total" "" "" "total" "actual" "difference"))
    (let ((total (gnc:monetary->string (sum-accounts all-accounts))))
      (gnc:html-table-append-row! table (list "" "" ""  total "?")))
    table))

(define*  (account-balance->monetary account-name from to #:key (domestic "Trading:CURRENCY:ARS"))
  (let* ((account-obj (get-account-by-name account-name)))
    (if (gnc:account-is-stock? account-obj)
        (gnc:exchange-by-pricedb-nearest (get-comm-account-balance account-obj from to) (xaccAccountGetCommodity (get-account-by-name domestic)) to)
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
(define (housing-account-list) (list "Expenses:Home:Insurance" "Expenses:Home:Mobiliario" "Expenses:Home:Repair & Maintenance" "Expenses:Taxes:AGIP"))
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

(define (get-account-by-name full-name)
  (let ((func (lambda (acc) (equal? full-name (gnc-account-get-full-name acc)))))
    (find func (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))))

(define (get-account-balance account from_t64 to_t64)
  (exact->inexact (gnc:account-get-balance-interval account from_t64 to_t64 #t)))

(define (get-comm-account-balance account from to)
  ((gnc:account-get-comm-balance-interval account  from to #f) 'getmonetary (xaccAccountGetCommodity account) #f))

(export options-generator)
(export document-renderer)
