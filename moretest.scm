(use trace)

(load "frp")

(trace gochan-broadcast gochan-send gochan-receive)

(define clock
  (make-primitive-signal 0))

(define receiver (gochan))
(primitive-emitters-set! clock (list receiver))

(thread-start! (primitive-thread clock))

(print (default-value clock))
(let loop ()
  (notify-primitive-signal! clock (current-seconds))
  (print (gochan-receive receiver))
  (loop))
