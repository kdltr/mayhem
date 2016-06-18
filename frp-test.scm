(use matchable)
(load "frp")

(use trace)
;;(trace gochan-send gochan-receive)

(define words
  (make-primitive-signal ""))

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

(define async-word-pairs
  (make-primitive-signal (initial-value word-pairs)))

(define async-word-pairs-receiver
  (make-registered-signal
   (list word-pairs)
   (lambda (state word-pair)
     (if state
         (notify-primitive-signal! async-word-pairs word-pair)
         #t))))

(define printer
  (make-registered-signal
   (list async-word-pairs cursor-position)
   (lambda (state word-pair cursor)
     (print (list word-pair cursor)))))

(start-signal-graph! printer)
(start-signal-graph! async-word-pairs-receiver)

(print
 (map (o thread-state primitive-thread)
      (list words cursor-position async-word-pairs)))

(print
 (map (o thread-state signal-thread)
      (list translate word-pairs async-word-pairs-receiver printer)))

(define words-thread
  (make-thread
   (lambda ()
     (let loop ((words-list (circular-list "hello" "world" "how" "are" "things" "going")))
       (notify-primitive-signal! words (car words-list))
       (thread-sleep! 1)
       (loop (cdr words-list))))))

(thread-start! words-thread)
(let loop ()
  (notify-primitive-signal! cursor-position (list (random 300) (random 300)))
  (thread-sleep! 0.2)
  (loop))
