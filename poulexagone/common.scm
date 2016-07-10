(defstruct board
  last-update
  last-pulse
  pulse
  flip
  angle)

(define-type board board?)

(define-generic (update (board board) now)
  (let ((percent (/ (- now (board-last-pulse board)) 0.5)))
   (update-board
    board
    last-pulse: (if (>= percent 1) now (board-last-pulse board))
    flip: (if (>= percent 1) (not (board-flip board)) (board-flip board))
    pulse: percent
    last-update: now
    angle: (+ (board-angle board)
              (- now (board-last-update board))))))

(define (draw-board board between #!key (flip-colors #t))
  (nvg:rotate! *c* (board-angle board))
  (if (and flip-colors (board-flip board))
      (draw-background background-color-1 background-color-2)
      (draw-background background-color-2 background-color-1))
  (between)
  (nvg:save-state! *c*)
  (when flip-colors
    (nvg:scale! *c*
                (lerp 1 1.1 (board-pulse board))
                (lerp 1 1.1 (board-pulse board))))
  (draw-hexagon hexagon-fill-color hexagon-stroke-color)
  (nvg:restore-state! *c*)
  )
