(use frp-lowlevel frp)

(define clock (make-primitive-signal (current-seconds)))
(define even-clock (filter even? 0 clock))

(define printer (map (compose print list) clock even-clock))

(start-signal-graph! printer)
(let loop ()
  (notify-primitive-signal! clock (current-seconds))
  (thread-sleep! 1)
  (loop))
