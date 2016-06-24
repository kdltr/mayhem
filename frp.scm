(module frp
    (map fold filter merge async)

  (import (except scheme map)
          (only srfi-1 find any)
          chicken)
  (use frp-lowlevel)

  (define (map function . signals)
    (make-registered-signal
     signals
     (lambda (state . inputs)
       (if (any change? inputs)
           (change (apply function (map body-of inputs)))
           (no-change state)))
     (apply function (map initial-value signals))))

  (define (fold function init signal)
    (make-registered-signal
     (list signal)
     (lambda (last input)
       (if (change? input)
           (change (function last (body-of input)))
           (no-change last)))
     init))

  (define (filter pred init signal)
    (make-registered-signal
     (list signal)
     (lambda (last input)
       (if (and (change? input) (pred (body-of input)))
           input
           (no-change last)))
     init))

  (define (merge signal . rest)
    (make-registered-signal
     (cons signal rest)
     (lambda (last . inputs)
       (let ((changed-input (find change? inputs)))
         (if changed-input
             changed-input
             (no-change last))))
     (initial-value signal)))

  (define (async signal)
    (let* ((primitive (make-primitive-signal (initial-value signal)))
           (receiver (make-registered-signal
                      (list signal)
                      (lambda (state input)
                        (when (change? input)
                          (notify-primitive-signal! primitive (body-of input)))
                        (change #t))
                      #f)))
      (start-signal-graph! receiver)
      primitive)))
