(define-module (gnucash reports example prototype-loader))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash utilities))
(gnc:module-load "gnucash/report/report-system" 0)
                                        ; load the renderer module once at the top level to get the symbols

(use-modules (gnucash reports example prototype))
                                        ; get and reload the module
(gnc:debug "hola")
(define (reload-report-module)
  (reload-module (resolve-module '(gnucash reports example prototype))))

                                        ; every time options are generated, reload the meat of the report
(define (options_loader)
  (reload-report-module)
  (options-generator))

                                        ; every time report is rendered, reload the meat of the report
(define (renderer_loader report-obj)
  (reload-report-module)
  (document-renderer report-obj))

(gnc:define-report
 'version 1
 'name (N_ "Prototype 2")
 'report-guid "d895ff06d05d41c0a1beb117124a46d5"
 'menu-tip (N_ "Unstable. Used for Testing.")
 'menu-path (list gnc:menuname-example)
 'options-generator options_loader
 'renderer renderer_loader)

