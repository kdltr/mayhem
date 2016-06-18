(use matchable frp-lowlevel frp)

(define words (make-primitive-signal ""))
(define cursor-position (make-primitive-signal '(0 0)))

(define translate
  (map (lambda (word) (thread-sleep! 1) (string-append "translated-" word))
       words))
(define word-pairs (async (map list words translate)))
(define printer (map (compose print list) word-pairs cursor-position))


(start-signal-graph! printer)

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
