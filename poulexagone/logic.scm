(use matchable)

;; stub
(define fold-channel void)
(define map-channel void)
(define filter-channel void)
(define clock (void))
(define key-events (void))
(define walls (void))
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


;; Game logic

(define player-speed (* pi 1))
(define walls-speed 1/4)

(defstruct gamestate
  last-update
  board-angle
  player-speed
  player-angle
  walls)
(define-type gamestate gamestate?)


;; Per frame update

(define-record clock-tick time)
(define-type clock-tick clock-tick?)
(define clock (frp:map (lambda (_) (make-clock-tick (get-time))) new-frame))
(define-generic (update (gamestate state) (clock-tick tick))
  (let ((dt (- (clock-tick-time tick) (gamestate-last-update state))))
    (update-gamestate state
                      player-angle: (+ (gamestate-player-angle state)
                                       (* dt (gamestate-player-speed state)))
                      board-angle: (clock-tick-time tick)
                      last-update: (clock-tick-time tick)
                      walls: (update-walls dt (gamestate-walls state)))))


;; Movement inputs

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
(define-generic (update (gamestate state) (movement-key key))
  (let ((speed (gamestate-player-speed state))
        (direction (movement-key-direction key))
        (press (movement-key-press key)))
    (update-gamestate state
                      player-speed: (+ speed
                                       (* player-speed
                                          (or (and (eq? direction 'left) (or (and press -1) +1))
                                              (and (eq? direction 'right) (or (and press +1) -1))))))))

;; Walls creation

(define (make-wall zone width)
  (list zone 600 width))

(define-record new-walls walls)
(define-type new-walls new-walls?)
(define new-walls
  (frp:map
   (lambda (_)
     (make-new-walls
      (list-tabulate 5 (lambda (_) (make-wall (random 6) 20)))))
   (frp:every 0.5)))
(define-generic (update (gamestate state) (new-walls new))
  (let ((walls (new-walls-walls new)))
    (update-gamestate state
                      walls: (append walls (gamestate-walls state)))))


(define state
  (frp:fold
   update
   (make-gamestate last-update: (get-time)
                   board-angle: 0
                   player-speed: 0
                   player-angle: 0
                   walls: '())
   (frp:merge
    clock
    movement-keys
    new-walls)))

(define (update-wall dt wall)
  (list (car wall)
        (- (cadr wall) (* dt walls-speed))
        (caddr wall)))

(define (update-walls dt walls)
  (remove
   (lambda (w)
     (<= (+ (cadr w) (caddr w)) 0))
   (map (cut update-wall (* 1000 dt) <>) walls)))


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

