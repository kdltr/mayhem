(defstruct overstate
  latest-score
  background-angle)

(define-type overstate overstate?)

(define (gameover score)
  (make-overstate latest-score: score))


(define-generic (update (overstate state) (spacebar-pressed _))
  initial-gamestate)

(define-generic (draw (overstate state) fps)
  (lambda ()
    (begin-frame!)
    (nvg:begin-path! *c*)
    (nvg:font-size! *c* 24)
    (nvg:font-face! *c* "DejaVu")
    (nvg:fill-color! *c* white)
    (nvg:text! *c* 10 24 "Game over")
    (nvg:text! *c* 10 48 "Press space to restart")
    (end-frame!)))
