(use matchable)
(load "frp")

(use trace)
;;(trace gochan-send gochan-receive)

(define words
  (make-primitive-signal "hello"))

(define cursor-position
  (make-primitive-signal '(0 0)))

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

(thread-start!
 (lambda ()
   (let loop ()
     (notify-primitive-signal! words "hello")
     (thread-sleep! 1)
     (loop))))

(let loop ()
  (notify-primitive-signal! cursor-position (list (random 300) (random 300)))
  (thread-sleep! 0.2)
  (loop))
