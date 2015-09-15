(use data-structures extras posix)
(use ezxdisp chickollect)

(define default-height 12)

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

(define (conf-val conf-data field #!optional default)
  (alist-ref field conf-data eqv? default))

(define (format-network-devices sys-data network-devices)
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
        network-devices)
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

(define redraw
  (let ((conf-parsed? #f)
        (height #f)
        (text-color #f)
        (network-devices #f))
    (lambda (bar sys-data conf-data reparse-conf?)
      (when (or reparse-conf? (not conf-parsed?))
        (set! network-devices (conf-val conf-data 'network-devices '(eth0)))
        (set! text-color (apply make-ezx-color
                                (conf-val conf-data 'text-color '(1 1 1))))
        (set! height (conf-val conf-data 'bar-height default-height))
        (set! conf-parsed? #t))
      (unless reparse-conf?
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
                                 (format-network-devices sys-data network-devices))))
          (ezx-wipe bar)
          (ezx-str-2d bar 2 (fx- height 1) content text-color)
          (ezx-redraw bar))))))

(define (read-conf-file conf-file)
  (if conf-file
      (with-input-from-file conf-file read-file)
      '()))

(define (main args)
  (let* ((conf-file (and (not (null? args))
                        (car args)))
         (conf-data (read-conf-file conf-file))
         (width (conf-val conf-data 'bar-width 800))
         (title (conf-val conf-data 'bar-title "chickollect-x11-bar"))
         (height (conf-val conf-data 'bar-height default-height))
         (bg-color (apply make-ezx-color
                          (conf-val conf-data 'background-color '(0 0 0))))
         (bar (ezx-init width height title)))

    ;; Reload configuration upon receiving SIGHUP
    (set-signal-handler! signal/hup
      (lambda (signum)
        (set! conf-data (read-conf-file conf-file))
        (redraw bar '() conf-data #t)))

    (ezx-set-background bar bg-color)
    (collect-loop
     (lambda (data)
       (redraw bar data conf-data #f))
     conf: conf-data)
    (ezx-quit bar)))

(main (command-line-arguments))
