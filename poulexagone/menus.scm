(defstruct overstate
  latest-score
  background-angle)

(define-type overstate overstate?)

(define-generic (update (overstate state) (any _))
  state)

(define-generic (update (overstate state) (spacebar-pressed _))
  initial-gamestate)

(define (gameover score)
  (make-overstate latest-score: score))

