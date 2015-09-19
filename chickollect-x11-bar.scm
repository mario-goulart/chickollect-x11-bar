(use data-structures extras posix)
(use ezxdisp chickollect)

(define bar-height (make-parameter 12))
(define bar-width (make-parameter 800))
(define bar-title (make-parameter "chickollect-x11-bar"))
(define bar-background-color (make-parameter (make-ezx-color 0 0 0)))
(define bar-text-color (make-parameter (make-ezx-color 0.4 0.4 0.4)))
(define collect-hook (make-parameter #f))
(define network-devices (make-parameter '()))

;; Parameters for chickollect
(define time-format (make-parameter "%T"))
(define date-format (make-parameter "%F"))
(define collect-interval (make-parameter 1))
(define monitors (make-parameter '(memory cpu date time battery network)))

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
         (conc (trunc* (/ bytes 1000000) 4) "MBps"))
        ((> bytes 1000)
         (conc (trunc (/ bytes 1000)) "KBps"))
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
  (string-append
   "BATTERY: "
   (string-intersperse
    (map (lambda (battery-data)
           (let* ((battery-status (car battery-data))
                  (battery-capacity (cdr battery-data))
                  (battery-status-icon
                   (cond ((eq? battery-status 'Charging) "^")
                         ((eq? battery-status 'Discharging) "v")
                         ((eq? battery-status 'Full) "")
                         (else #f))))
             (if battery-status
                 (conc battery-capacity "%" (or battery-status-icon ""))
                 "--")))
         batteries-data)
    " ")))

(define (redraw bar sys-data)
  (let* ((memory (alist-ref 'memory sys-data))
         (ram (trunc (car memory)))
         (swap (trunc (cdr memory)))
         (batteries-data (alist-ref 'battery sys-data))
         (cpu-stats (alist-ref 'cpu sys-data))
         (num-cpus (length cpu-stats))
         (avg-cpu-usage (trunc (/ (apply + cpu-stats) num-cpus)))
         (bar-fmt
          "~a  ~a  |  CPU: ~a%  |  RAM: ~a%  |  SWAP: ~a%  |  ~a  |  ~a")
         (content (sprintf bar-fmt
                           (alist-ref 'time sys-data)
                           (alist-ref 'date sys-data)
                           (pad-number avg-cpu-usage)
                           (pad-number ram)
                           (pad-number swap)
                           (format-batteries batteries-data)
                           (format-network-devices sys-data))))
    (ezx-wipe bar)
    (ezx-str-2d bar 2 (fx- (bar-height) 1) content (bar-text-color))
    (ezx-redraw bar)))

(define (main args)
  (let ((conf-file (and (not (null? args))
                        (car args))))
    (when conf-file
      (load conf-file))
    (let ((bar (ezx-init (bar-width) (bar-height) (bar-title))))

      ;; Reload configuration upon receiving SIGHUP
      (set-signal-handler!
       signal/hup
       (lambda (signum)
         (load conf-file)
         (redraw bar '())))

      (ezx-set-background bar (bar-background-color))
      (collect-loop
       (lambda (data)
         (when (collect-hook)
           ((collect-hook) data))
         (redraw bar data))
       time-format: (time-format)
       date-format: (date-format)
       collect-interval: (collect-interval)
       monitors: (monitors))
      (ezx-quit bar))))

(main (command-line-arguments))
