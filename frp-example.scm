(load "frp")

(define clock
  (make-signal
   (lambda (self children)
     (thread-sleep! 1)
     (for-each (lambda (c) (gochan-send c 2)) children))))

(define counter
  (make-registered-signal
   (list clock)
   (let ((count 1))
     (lambda (self children clock)
       (for-each (lambda (c) (gochan-send c count)) children)
       (set! count (* count (gochan-receive clock)))))))

(define printer
  (make-registered-signal
   (list counter clock)
   (lambda (self children counter clock)
     (printf "~A: ~A\n" (gochan-receive counter) (gochan-receive clock)))))


(use trace)
(trace start-signal-graph!)

(start-signal-graph! printer)
