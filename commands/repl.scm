(library (commands repl)
  (export register-repl-command)
  (import (rnrs base)
	  (only (guile) format getenv resolve-module current-module
		set-current-module save-module-excursion)
	  (only (system repl repl) start-repl)
    (only (system repl server) spawn-server make-tcp-server-socket)
	  (only (ice-9 readline) activate-readline write-history read-history)
	  (only (gnucash gnome-utils) gnc-add-scm-extension)
	  (only (gnucash gnome-utils gnc-menu-extensions) gnc:make-menu-item)
	  (only (gnucash core-utils) G_ N_))

  (define history-filename (format #f "~a/.gnucash_history" (getenv "HOME")))

  (define previous-module #f)

  (define (repl)
    (save-module-excursion
     (lambda ()
       (set-current-module (or previous-module
			       (begin
				 (read-history history-filename)
				 (activate-readline)
				 (resolve-module '(guile-user)))))
       (spawn-server (make-tcp-server-socket #:port 37147))
       (set! previous-module (current-module))
       (write-history history-filename)
       (format #t "Goodbye~%"))))

  (define menuname-tools (N_ "Tools"))

  (define (register-repl-command)
    (gnc-add-scm-extension
     (gnc:make-menu-item
      "Scheme REPL"
      "0155032a4b41443388f6cf8effc98168"
      "Start a Scheme REPL in the tty"
      (list menuname-tools)
      (lambda (window)
	(repl))))))
