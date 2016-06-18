(module frp
    (map fold async)

  (import (except scheme map) chicken)
  (use frp-lowlevel)

  (define (map function . signals)
    (make-registered-signal
     signals
     (lambda (state . inputs)
       (apply function inputs))))

  (define (fold function init signal)
    (make-registered-signal (list signal) function init))

  (define (async signal)
    (let* ((primitive (make-primitive-signal (initial-value signal)))
           (receiver (make-registered-signal
                      (list signal)
                      (lambda (state data)
                        (if state (notify-primitive-signal! primitive data) #t)))))
      (start-signal-graph! receiver)
      primitive)))
