(defstruct overstate
  latest-score
  last-update
  background-angle)

(define-type overstate overstate?)

(define (gameover score date angle)
  (make-overstate latest-score: score
                  last-update: date
                  background-angle: angle))


(define-generic (update (overstate state) (spacebar-pressed _))
  (overtrans (overstate-last-update state)
             (overstate-background-angle state)
             initial-gamestate))

(define-generic (update (overstate state) (clock-tick tick))
  (let ((t (clock-tick-time tick)))
    (update-overstate state
                      last-update: t
                      background-angle: (+ (overstate-background-angle state)
                                           (- t (overstate-last-update state))))))

(define-generic (draw (overstate state) fps)
  (lambda ()
    (begin-frame!)
    (nvg:save-state! *c*)
    (nvg:translate! *c* cx (+ cy 400))
    (nvg:scale! *c* 4 4)
    (nvg:rotate! *c* (overstate-background-angle state))
    (draw-background background-color-1 background-color-2)
    (draw-hexagon hexagon-fill-color hexagon-stroke-color)

    (nvg:restore-state! *c*)
    (nvg:begin-path! *c*)
    (nvg:font-size! *c* 24)
    (nvg:font-face! *c* "DejaVu")
    (nvg:fill-color! *c* white)
    (nvg:text! *c* 10 24 "Game over")
    (nvg:text! *c* 10 48 "Press space to restart")
    (end-frame!)))


;; Transition state game -> over

(define (lerp v0 v1 t)
  (+ (* (- 1 t) v0)
     (* t v1)))

(defstruct overtrans
  start
  percent
  angle
  last-update
  next-state)

(define (overtrans start angle next)
  (make-overtrans start: start percent: 0 angle: angle last-update: start next-state: next))

(define-type overtrans overtrans?)

(define-generic (update (overtrans state) (clock-tick tick))
  (let* ((start (overtrans-start state))
         (stop (+ start 0.3))
         (now (clock-tick-time tick))
         (percent (/ (- now start)
                     (- stop start))))
    (if (>= percent 1)
        (let ((next (overtrans-next-state state)))
          (cond ((overstate? next)
                 (update-overstate
                  (overtrans-next-state state)
                  last-update: now
                  background-angle: (overtrans-angle state)))
                ((gamestate? next)
                 (update-gamestate
                  initial-gamestate
                  last-upgrade: now
                  board-angle: (overtrans-angle state)))))
        (update-overtrans
         state
         percent: percent
         last-update: now
         angle: (+ (overtrans-angle state) (- now (overtrans-last-update state)))))))

(define-generic (draw (overtrans state) fps)
  (let ((percent (overtrans-percent state))
        (flip? (gamestate? (overtrans-next-state state))))
    (lambda ()
      (begin-frame!)
      (nvg:translate! *c* cx (if flip?
                                 (lerp (+ cy 400) cy percent)
                                 (lerp cy (+ cy 400) percent)))
      (nvg:scale! *c*
                  (if flip? (lerp 4 1 percent)
                      (lerp 1 4 percent))
                  (if flip? (lerp 4 0.8 percent)
                      (lerp 0.8 4 percent)))
      (nvg:rotate! *c* (overtrans-angle state))
      (draw-background background-color-1 background-color-2)
      (draw-hexagon hexagon-fill-color hexagon-stroke-color)
      (end-frame!)
      )))
