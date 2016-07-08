;; Signal propagating the date of each frame

(define-record clock-tick time)
(define-type clock-tick clock-tick?)
(define clock
  (frp:map
   (lambda (_) (make-clock-tick (get-time)))
   new-frame))


;; Signal propagating left and right key states

(define-record movement-key direction press)
(define-type movement-key movement-key?)
(define movement-keys
  (frp:map
   (lambda (k)
     (let ((key (cadr k))
           (action (caddr k)))
       (make-movement-key (case key ((38) 'left) ((40) 'right))
                          (case action ((0) #f) ((1) #t)))))
   (frp:filter (lambda (k)
                 (and (pair? k)
                      (not (= (caddr k) 2))
                      (or (= (cadr k) 38) (= (cadr k) 40))))
               '(0 38 0 0) key)))


;; Signal notifying about spacebar presses

(define-record spacebar-pressed)
(define-type spacebar-pressed spacebar-pressed?)
(define spacebar
  (frp:map
   (lambda (_) (make-spacebar-pressed))
   (frp:filter
    (cut equal? <> '(32 65 1 0))
    #f
    key)))
