(use matchable)

;; stub
(define fold-channel void)
(define map-channel void)
(define filter-channel void)
(define clock (void))
(define key-events (void))
(define channel-value void)
;;;

; Utility

(define (xor a b)
  (and (or a b)
       (not (and a b))))

(define (angle->zone a)
  (modulo
    (inexact->exact
      (ceiling (/ (- a (/ pi 6)) (/ pi 3))))
    6))

(define (prev-zone z)
  (modulo (sub1 z) 6))

(define (next-zone z)
  (modulo (add1 z) 6))

; Game logic

(define player-speed (* pi 1))
(define walls-speed 1/4)

; Reuse this with a better frp system
; (define (make-wall zone width)
;   (fold-channel
;     clock
;     (lambda (dt wall)
;       (list (car wall)
;             (max 0 (- (cadr wall) (* dt walls-speed)))
;             (caddr wall)))
;     (list zone 600 width)))
;
; (define walls-clock
;   (filter-channel
;     (fold-channel
;       clock
;       (lambda (dt prev)
;         (if (> (+ dt prev) 500)
;           0
;           (+ dt prev)))
;       0)
;     zero?))

(define (make-wall zone width)
  (list zone 600 width))

(define make-walls
  (let ((last 0))
    (lambda (dt)
      (if (>= (+ last dt) 500)
        (begin
          (set! last 0)
          (map
            (lambda (i)
              (make-wall (random 6) 20))
            (iota (random 5))))
        (begin
          (set! last (+ last dt))
          '())))))

(define (update-wall dt wall)
  (list (car wall)
        (- (cadr wall) (* dt walls-speed))
        (caddr wall)))

(define (update-walls dt walls)
  (map (cut update-wall dt <>) walls))

(define walls
  (fold-channel
   (lambda (dt walls)
     (append (make-walls dt)
             (remove
              (lambda (w)
                (<= (+ (cadr w) (caddr w)) 0))
              (update-walls dt walls))))
   '()
   clock))

(define fps
  (map-channel
    (lambda (c) (if (zero? c)
                  0
                  (* (/ 1 c) 1000)))
    clock))

(define hex-angle (frp:map (lambda (t) (* t (/ pi 2))) time))


(define movements
  (fold-channel
    (lambda (k prev)
      (match k
        (('key 'pressed #\b)  (list #t (second prev)))
        (('key 'pressed #\p)  (list (first prev) #t))
        (('key 'released #\b)  (list #f (second prev)))
        (('key 'released #\p)  (list (first prev) #f))))
    '(#f #f)
    (filter-channel
      (lambda (k)
        (or (equal? k '(key pressed #\b))
            (equal? k '(key released #\b))
            (equal? k '(key pressed #\p))
            (equal? k '(key released #\p))))
      key-events)))

(define (combine-clock-movement clock input)
  (let ((dt (/ clock 1000)))
    (if (apply xor input)
      (* player-speed
         (if (car input) (- dt) dt))
      0)))

(define (side-collisions walls position)
  (let* ((zone (angle->zone position))
         (low-walls (filter (lambda (w)
                              (and (<= (cadr w) (+ hexagon-radius 15))
                                   (>= (+ (cadr w) (caddr w)) (+ hexagon-radius 15))))
                            walls)))
    (list (any (lambda (w) (= (car w) (prev-zone zone))) low-walls)
          (any (lambda (w) (= (car w) (next-zone zone))) low-walls))))

(define (move-player increment walls previous-position)
  (let* ((collisions (side-collisions walls previous-position))
         (old-zone (angle->zone previous-position))
         (position (+ previous-position increment))
         (new-zone (angle->zone position)))
    (cond
      ((and (car collisions) (= new-zone (prev-zone old-zone)))  previous-position)
      ((and (cadr collisions) (= new-zone (next-zone old-zone)))  previous-position)
      (else position))))

(define player-position
  (fold-channel
    move-player
    0
    (map-channel combine-clock-movement clock movements)
    walls))

(define death-collision
  (map-channel
    (lambda (pos walls)
      (filter (lambda (w)
                (and (= (angle->zone pos) (car w))
                     (<= (cadr w) (+ hexagon-radius 15))
                     (>= (+ (cadr w) (caddr w)) (+ hexagon-radius 15))))
              walls))
    player-position
    walls))

