(use frp-lowlevel mailbox extras)

(define ticker (make-primitive-signal #t))
(define other (make-primitive-signal 0))


(define tick-receiver (make-mailbox))
(primitive-emitters-set!
 ticker
 (cons tick-receiver (primitive-emitters ticker)))

(start-signal-graph! ticker)
(start-signal-graph! other)

(let loop ()
  (let loop2 ((n (random 50)))
    (notify-primitive-signal! other n)
    (unless (zero? n)
      (loop2 (sub1 n))))

  (notify-primitive-signal! ticker #t)

  (let loop2 ((n 0))
    (let ((msg (mailbox-receive! tick-receiver)))
      (if (equal? msg '(change #t))
          (loop)
          (loop2 (add1 n))))))
