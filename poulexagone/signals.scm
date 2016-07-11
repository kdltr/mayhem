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
     (let ((key (car k))
           (action (caddr k)))
       (make-movement-key (select key ((+key-a+) 'left) ((+key-d+) 'right))
                          (select action ((+release+) #f) ((+press+) #t)))))
   (frp:filter (lambda (k)
                 (and (pair? k)
                      (not (= (caddr k) +repeat+))
                      (member (car k) (list +key-a+ +key-d+))))
               `(,+key-a+ 0 ,+release+ 0) key)))


;; Signal notifying about spacebar presses

(define-record spacebar-pressed)
(define-type spacebar-pressed spacebar-pressed?)
(define spacebar
  (frp:map
   (lambda (_) (make-spacebar-pressed))
   (frp:filter
    (lambda (k) (and (eq? (car k) +key-space+)
                     (eq? (caddr k) +press+)))
    #f
    key)))

(define-record escape-pressed)
(define-type escape-pressed escape-pressed?)
(define escape
  (frp:map
   (lambda (_) (make-escape-pressed))
   (frp:filter
    (lambda (k) (and (eq? (car k) +key-escape+)
                     (eq? (caddr k) +press+)))
    #f
    key)))
