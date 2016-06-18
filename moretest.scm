(use trace)

(load "frp")

;;(trace gochan-broadcast gochan-send gochan-receive)

(define clock
  (make-primitive-signal (current-seconds)))

(define printer
  (make-registered-signal
   (list clock)
   (lambda (state clock)
     (print clock)
     clock)))

(start-signal-graph! printer)

(let loop ()
  (notify-primitive-signal! clock (current-seconds))
  (thread-sleep! 1)
  (loop))
