(use matchable)

;; Utility

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
  walls
  walls-timeout)

(define-type gamestate gamestate?)

(define initial-gamestate
  (make-gamestate last-update: (get-time)
                  board-angle: 0
                  player-speed: 0
                  player-angle: 0
                  walls: '()
                  walls-timeout: 0))


;; State update functions

(define-generic (update (gamestate state) (clock-tick tick))
  (let* ((dt (- (clock-tick-time tick) (gamestate-last-update state)))
         (new-pattern? (>= (clock-tick-time tick) (gamestate-walls-timeout state)))
         (new-pattern (if new-pattern? (random-pattern) '()))
         (new-walls (append new-pattern
                            (update-walls dt (gamestate-walls state))))
         (new-timeout (if new-pattern?
                          (+ (clock-tick-time tick) (pattern-duration new-pattern))
                          (gamestate-walls-timeout state)))
         (new-position (move-player (* dt (gamestate-player-speed state))
                                    new-walls
                                    (gamestate-player-angle state)))
         (death (pair? (death-collisions new-position new-walls))))
    (if death
        (gameover 0)
        (update-gamestate state
                          player-angle: new-position
                          board-angle: (clock-tick-time tick)
                          last-update: (clock-tick-time tick)
                          walls-timeout: new-timeout
                          walls: new-walls))))

(define-generic (update (gamestate state) (movement-key key))
  (let ((speed (gamestate-player-speed state))
        (direction (movement-key-direction key))
        (press (movement-key-press key)))
    (update-gamestate state
                      player-speed: (+ speed
                                       (* player-speed
                                          (or (and (eq? direction 'left) (or (and press -1) +1))
                                              (and (eq? direction 'right) (or (and press +1) -1))))))))


;; State drawing

(define-generic (draw (gamestate state) fps)
  (lambda ()
    (begin-frame!)
    (nvg:save-state! *c*)

    (nvg:translate! *c* cx cy)
    ;; fancy effects
    (nvg:scale! *c* 1 0.8)
    (nvg:rotate! *c* (gamestate-board-angle state))

    (if (even? (floor (gamestate-last-update state)))
        (draw-background background-color-1 background-color-2)
        (draw-background background-color-2 background-color-1))

    ;; walls
    (for-each
     (lambda (w)
       (draw-wall (wall-zone w) (wall-position w) (wall-height w) wall-color))
     (gamestate-walls state))

    ;; player
    (draw-hexagon hexagon-fill-color hexagon-stroke-color)
    (draw-player (gamestate-player-angle state))

    (nvg:restore-state! *c*)

    ;; overlay
    (draw-overlay state fps)
    (end-frame!)))


;; Walls update and collisions

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


;; Walls creation

(defstruct wall zone position height)

(define wall-patterns
  (map
   (lambda (p)
     (map (lambda (w) (apply (cut make-wall zone: <> position: <> height: <>) w))
          p))
   (list
    ;; wall on each but one sides
    '((1 600 60)
      (2 600 60)
      (3 600 60)
      (4 600 60)
      (5 600 60))

    ;; Spiral
    '((0 600 80)
      (3 600 80)
      (1 600 160)
      (4 600 160)
      (2 760 80)
      (5 760 80)
      (3 840 80)
      (0 840 80)
      (1 920 80)
      (4 920 80))

    ;; Alternate
    '((0 600 40)
      (1 600 40)
      (3 600 40)
      (4 600 40)
      (1 680 40)
      (2 680 40)
      (4 680 40)
      (5 680 40)
      (2 760 40)
      (3 760 40)
      (5 760 40)
      (0 760 40)
      (3 840 40)
      (4 840 40)
      (0 840 40)
      (1 840 40)
      (4 920 40)
      (5 920 40)
      (1 920 40)
      (2 920 40)))))

(define (random-rotation walls)
  (let ((rot (random 6)))
    (map (lambda (w) (update-wall w zone: (modulo (+ rot (wall-zone w)) 6)))
         walls)))

(define (random-pattern)
  (random-rotation
   (list-ref wall-patterns (random (length wall-patterns)))))

(define (wall-exterior w)
  (+ (wall-position w) (wall-height w)))

(define (pattern-duration walls)
  (- (/ 1 (* (* 1000 walls-speed)
             (/ 1 (fold (lambda (w prev) (max (wall-exterior w) prev))
                        0
                        walls))))
     ;; some time to prevent waiting for the disappearance of the previous pattern
     1.7))
