(use frp-lowlevel frp)

(define clock (make-primitive-signal (current-seconds)))

(define printer (map print clock))

(start-signal-graph! printer)
(let loop ()
  (notify-primitive-signal! clock (current-seconds))
  (thread-sleep! 1)
  (loop))
