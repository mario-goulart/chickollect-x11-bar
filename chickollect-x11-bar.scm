(use data-structures extras posix)
(use ezxdisp chickollect)

(define default-height 12)

(define config (make-parameter '()))

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

(define (config-val field #!optional default)
  (alist-ref field (config) eqv? default))

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
    (lambda (bar sys-data reparse-conf?)
      (when (or reparse-conf? (not conf-parsed?))
        (set! network-devices (config-val 'network-devices '(eth0)))
        (set! text-color (apply make-ezx-color
                                (config-val 'text-color '(1 1 1))))
        (set! height (config-val 'bar-height default-height))
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

(define (main args)
  (let ((conf-file (and (not (null? args))
                        (car args))))
    (when conf-file
      (load conf-file))
    (let* ((width (config-val 'bar-width 800))
           (title (config-val 'bar-title "chickollect-x11-bar"))
           (height (config-val 'bar-height default-height))
           (bg-color (apply make-ezx-color
                            (config-val 'background-color '(0 0 0))))
           (bar (ezx-init width height title))
           (collect-hook (config-val 'hook)))

      ;; Reload configuration upon receiving SIGHUP
      (set-signal-handler! signal/hup
                           (lambda (signum)
                             (load conf-file)
                             (set! collect-hook (config-val 'hook))
                             (redraw bar '() #t)))

      (ezx-set-background bar bg-color)
      (collect-loop
       (lambda (data)
         (when collect-hook
           (collect-hook data))
         (redraw bar data #f))
       conf: (config))
      (ezx-quit bar))))

(main (command-line-arguments))
