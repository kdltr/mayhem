(defstruct overstate
  latest-score
  board)

(define-type overstate overstate?)

(define (gameover score board)
  (make-overstate latest-score: score
                  board: board))

(define-generic (update (overstate state) (spacebar-pressed _))
  (overtrans (board-last-update (overstate-board state))
             initial-gamestate
             (overstate-board state)))

(define-generic (update (overstate state) (escape-pressed _))
  (exit 0))

(define-generic (update (overstate state) (clock-tick tick))
  (let ((now (clock-tick-time tick)))
    (update-overstate
     state
     board: (update (overstate-board state) now))))

(define-generic (draw (overstate state) fps)
  (lambda ()
    (begin-frame!)
    (nvg:save-state! *c*)
    (nvg:translate! *c* cx (+ cy 400))
    (nvg:scale! *c* 4 4)

    (draw-board (overstate-board state) void
                flip-colors: #f)

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

(define (sinrp v0 v1 t)
  (+ (* (- 1 (sin (* (/ pi 2) t))) v0)
     (* (sin (* (/ pi 2) t)) v1)))

(defstruct overtrans
  start
  percent
  next-state
  board)

(define (overtrans start next board)
  (make-overtrans start: start percent: 0 next-state: next board: board))

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
                 (update-overstate next board: (overtrans-board state)))
                ((gamestate? next)
                 (update-gamestate next board: (overtrans-board state)))))
        (update-overtrans
         state
         percent: percent
         board: (update (overtrans-board state) now)))))

(define-generic (draw (overtrans state) fps)
  (let ((percent (overtrans-percent state))
        (flip? (gamestate? (overtrans-next-state state))))
    (lambda ()
      (begin-frame!)
      (nvg:translate! *c* cx (if flip?
                                 (sinrp (+ cy 400) cy percent)
                                 (sinrp cy (+ cy 400) percent)))
      (nvg:scale! *c*
                  (if flip? (sinrp 4 1 percent)
                      (sinrp 1 4 percent))
                  (if flip? (sinrp 3.8 0.8 percent)
                      (sinrp 0.8 3.8 percent)))
      (let* ((board (overtrans-board state))
             (skew (* (board-speed board) 0.02)))
       (nvg:skew-x! *c*
                    (if flip?
                        (sinrp 0 skew percent)
                        (sinrp skew 0 percent))))

      (draw-board (overtrans-board state) void flip-colors: #f)
      (end-frame!)
      )))
