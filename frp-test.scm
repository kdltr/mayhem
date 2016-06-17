(use matchable)
(load "frp")

(use trace)
;;(trace gochan-send gochan-receive)

(define words
  (make-primitive-signal
   (lambda (state message)
     (match message
       (('word w)
        (list 'change w))
       (else
        (list 'no-change state))))))

(define cursor-position
  (make-primitive-signal
   (lambda (state message)
     (match message
       (('cursor-position x y)
        (list 'change (list x y)))
       (else
        (list 'no-change state))))))

(define translate
  (make-registered-signal
   (list words)
   (lambda (state word)
     (thread-sleep! 1)
     (string-append "translated-" word))))

(define word-pairs
  (make-registered-signal
   (list words translate)
   (lambda (state word translated)
     (list word translated))))

(define printer
  (make-registered-signal
   (list word-pairs cursor-position)
   (lambda (state word-pair cursor-position)
     (print (list word-pair cursor-position)))))

(start-signal-graph! printer)

(define words-input (car (receptors words)))
(define cursor-input (car (receptors cursor-position)))

(start-signal-graph! printer)

(thread-start!
 (lambda ()
   (let loop ()
     (gochan-send words-input '(word "hello"))
     (thread-sleep! 1)
     (loop))))

(let loop ()
  (gochan-send cursor-input (list 'cursor-position (random 300) (random 300)))
  (thread-sleep! 0.2)
  (loop))
