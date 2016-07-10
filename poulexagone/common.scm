(defstruct board
  last-update
  angle)

(define-type board board?)

(define-generic (update (board board) now)
  (update-board
   board
   last-update: now
   angle: (+ (board-angle board)
             (- now (board-last-update board)))))

(define (draw-board board between #!key (flip-colors #t))
  (nvg:rotate! *c* (board-angle board))
  (if (and flip-colors (even? (floor (board-last-update board))))
      (draw-background background-color-1 background-color-2)
      (draw-background background-color-2 background-color-1))
  (between)
  (draw-hexagon hexagon-fill-color hexagon-stroke-color))
