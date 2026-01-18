(import scheme)
(cond-expand
 (chicken-4
  (use data-structures extras posix)
  (use ezxdisp chickollect))
 (chicken-5
  (import (chicken condition)
          (chicken fixnum)
          (chicken format)
          (chicken pathname)
          (chicken process signal)
          (chicken process-context)
          (chicken string))
  (import ezxdisp chickollect))
 (else
  (error "Unsupported CHICKEN version.")))

(define bar-height (make-parameter 12))
(define bar-width (make-parameter 800))
(define bar-title (make-parameter "chickollect-x11-bar"))
(define bar-background-color (make-parameter (make-ezx-color 0 0 0)))
(define bar-text-color (make-parameter (make-ezx-color 0.4 0.4 0.4)))
(define collect-hook (make-parameter #f))
(define network-devices (make-parameter '()))

;; Parameters for chickollect
(define date/time-format (make-parameter "%T  %F  w%V"))
(define collect-interval (make-parameter 1))
(define monitors (make-parameter '(date/time cpu memory battery network)))

(define (trunc n)
  (inexact->exact (truncate n)))

(define (trunc* n max-digits)
  (let* ((str (number->string n))
         (len-str (string-length str)))
    (if (<= len-str max-digits)
        str
        (substring str 0 max-digits))))

(define (format-transfer-rate bytes)
  (cond ((zero? bytes)
         "--")
        ((> bytes 1000000)
         (conc (trunc* (/ bytes 1000000.0) 4) "MBps"))
        ((> bytes 1000)
         (conc (trunc (/ bytes 1000.0)) "KBps"))
        (else
         (conc (trunc bytes) "Bps"))))

(define (pad-number n)
  (cond ((fx< n 10)
         (conc "  " n))
        ((fx< n 100)
         (conc " " n))
        (else (->string n))))

(define (format-network-devices sys-data)
  (string-intersperse
   (map (lambda (dev)
          (or (and-let* ((net-conf (alist-ref 'network sys-data))
                         (net-stats (alist-ref dev net-conf))
                         (net-stats)
                         (download (format-transfer-rate (car net-stats)))
                         (upload (format-transfer-rate (cadr net-stats))))
                (apply sprintf (list "~a: ~a v  ~a ^"
                                     dev
                                     download
                                     upload)))
              (symbol->string dev)))
        (network-devices))
   "  |  "))

(define (format-batteries batteries-data)
  (if (null? batteries-data)
      "BATTERY: --"
      (string-append
       "BATTERY: "
       (string-intersperse
        (map (lambda (battery-data)
               (if battery-data
                   (let* ((battery-status (car battery-data))
                          (battery-capacity (cdr battery-data))
                          (battery-status-icon
                           (cond ((eq? battery-status 'Charging) "^")
                                 ((eq? battery-status 'Discharging) "v")
                                 ((eq? battery-status 'Full) "")
                                 ((eq? battery-status 'Unknown) "?")
                                 (else #f))))
                     (if battery-status
                         (conc battery-capacity "%" (or battery-status-icon ""))
                         "--"))
                   "--"))
             batteries-data)
        " "))))

(define (redraw bar sys-data)
  (let* ((bar-data
          (let loop ((monitors (monitors)))
            (if (null? monitors)
                '()
                (let ((monitor (car monitors)))
                  (case monitor
                    ((date/time)
                     (cons (or (alist-ref 'date/time sys-data) "--")
                           (loop (cdr monitors))))
                    ((cpu)
                     (let* ((cpu-stats (alist-ref 'cpu sys-data))
                            (num-cpus (and cpu-stats (length cpu-stats)))
                            (avg-cpu-usage
                             (if cpu-stats
                                 (pad-number (trunc (/ (apply + cpu-stats) num-cpus)))
                                 "--")))
                       (cons (string-append "CPU: " avg-cpu-usage "%")
                             (loop (cdr monitors)))))
                    ((memory)
                     (let ((memory (alist-ref 'memory sys-data)))
                       (append
                        (list (string-append
                               "RAM: " (if memory
                                           (string-append
                                            (pad-number (trunc (car memory)))
                                            "%")
                                           "--"))
                              (string-append
                               "SWAP: " (if memory
                                            (string-append
                                             (pad-number (trunc (cdr memory)))
                                             "%")
                                            "--")))
                        (loop (cdr monitors)))))
                    ((battery)
                     (cons (format-batteries (or (alist-ref 'battery sys-data) '()))
                           (loop (cdr monitors))))
                    ((network)
                     (cons (format-network-devices sys-data)
                           (loop (cdr monitors))))
                    (else (error 'redraw "Invalid monitor" monitor)))))))
         (bar-text (string-intersperse bar-data "  |  ")))
    (ezx-wipe bar)
    (ezx-str-2d bar 2 (fx- (bar-height) 1) bar-text (bar-text-color))
    (ezx-redraw bar)))

(define (init-bar)
  (let ((bar (ezx-init (bar-width) (bar-height) (bar-title))))
    (ezx-set-background bar (bar-background-color))
    bar))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n")
                             args)))
  (exit 1))

(define (usage #!optional exit-code)
  (let* ((port (if (and exit-code (not (zero? exit-code)))
                   (current-error-port)
                   (current-output-port)))
         (prog (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #prog [--width <pixels>] [<config file>]

EOF
))
    (fprintf port msg)
    (when exit-code (exit exit-code))))


(define (main args)
  (let ((conf-file #f)
        (width #f))
    (let loop ((args args))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((member arg '("-h" "-help" "--help"))
                 (usage 0))
                ((string=? arg "--width")
                 (if (null? (cdr args))
                     (die! "--width: Missing argument.")
                     (let* ((width-str (cadr args))
                            (w (string->number width-str)))
                       (if w
                           (set! width w)
                           (die! "--width: Invalid argument: ~a" width-str))
                       (loop (cddr args)))))
                (else
                 (unless conf-file
                   (set! conf-file arg)
                   (loop (cdr args))))))))
    (when conf-file
      (load conf-file))

    ;; Command line options overwrite settings in the conf file
    (when width
      (bar-width width))

    (let ((bar (init-bar)))
      ;; Reload configuration upon receiving SIGHUP
      (set-signal-handler!
       signal/hup
       (lambda (signum)
         (load conf-file)
         (ezx-quit bar)
         (set! bar (init-bar))
         (redraw bar '())))

      (collect-loop
       (lambda (data)
         (when (collect-hook)
           ((collect-hook) data))
         (redraw bar data))
       date/time-format: (date/time-format)
       collect-interval: (collect-interval)
       monitors: (monitors))
      (ezx-quit bar))))

(handle-exceptions exn
  (with-output-to-port (current-error-port)
    (lambda ()
      (print-call-chain)
      (print-error-message exn)))
  (main (command-line-arguments)))
