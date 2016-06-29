(use matchable)

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

(define player-speed (* pi 2.5))
(define walls-speed 1/4)

(defstruct gamestate
  last-update
  board-angle
  player-speed
  player-angle
  walls)
(define-type gamestate gamestate?)


;; Per frame update

(define (update-wall* dt wall)
  (let ((new-pos (- (wall-position wall) (* dt walls-speed))))
    (update-wall wall
                 position: (if (> new-pos 0) new-pos 0)
                 height: (if (> new-pos 0)
                             (wall-height wall)
                             (+ (wall-height wall) new-pos)))))

(define (update-walls dt walls)
  (remove
   (lambda (w)
     (<= (+ (wall-position w) (wall-height w)) 0))
   (map (cut update-wall* (* 1000 dt) <>) walls)))

(define (side-collisions walls position)
  (let* ((zone (angle->zone position))
         (low-walls (filter (lambda (w)
                              (and (<= (wall-position w) (+ hexagon-radius 15))
                                   (>= (+ (wall-position w) (wall-height w)) (+ hexagon-radius 15))))
                            walls)))
    (list (any (lambda (w) (= (wall-zone w) (prev-zone zone))) low-walls)
          (any (lambda (w) (= (wall-zone w) (next-zone zone))) low-walls))))

(define (death-collisions pos walls)
  (filter (lambda (w)
            (and (= (angle->zone pos) (wall-zone w))
                 (<= (wall-position w) (+ hexagon-radius 15))
                 (>= (+ (wall-position w) (wall-height w)) (+ hexagon-radius 15))))
          walls))

(define (move-player increment walls previous-position)
  (let* ((collisions (side-collisions walls previous-position))
         (old-zone (angle->zone previous-position))
         (position (+ previous-position increment))
         (new-zone (angle->zone position)))
    (cond
      ((and (car collisions) (= new-zone (prev-zone old-zone)))  previous-position)
      ((and (cadr collisions) (= new-zone (next-zone old-zone)))  previous-position)
      (else position))))

(define-record clock-tick time)
(define-type clock-tick clock-tick?)
(define clock (frp:map (lambda (_) (make-clock-tick (get-time))) new-frame))
(define-generic (update (gamestate state) (clock-tick tick))
  (let* ((dt (- (clock-tick-time tick) (gamestate-last-update state)))
         (new-walls (update-walls dt (gamestate-walls state)))
         (new-position (move-player (* dt (gamestate-player-speed state))
                                    new-walls
                                    (gamestate-player-angle state)))
         (death (pair? (death-collisions new-position new-walls))))
    (update-gamestate state
                      player-angle: new-position
                      board-angle: (clock-tick-time tick)
                      last-update: (clock-tick-time tick)
                      walls: new-walls)))


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

(defstruct wall zone position height)

(define wall-patterns
  (map
   (lambda (p)
     (map (lambda (w) (apply (cut make-wall zone: <> position: <> height: <>) w)) p))
   '(((1 600 20)
      (2 600 20)
      (3 600 20)
      (4 600 20)
      (5 600 20))
     ((1 600 300)))))

(define (random-rotation walls)
  (let ((rot (random 6)))
    (map (lambda (w) (update-wall w zone: (modulo (+ rot (wall-zone w)) 6)))
         walls)))

(define-record new-walls walls)
(define-type new-walls new-walls?)
(define new-walls
  (frp:map
   (lambda (_)
     (make-new-walls
      (random-rotation (list-ref wall-patterns (random (length wall-patterns))))))
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


;; FPS counter

(define fps-counter
  (let ((frame-counter (frp:fold (lambda (prev _) (add1 prev)) 0 new-frame))
        (clock (frp:map (lambda (_) (get-time)) new-frame)))
   (frp:map / frame-counter clock)))
