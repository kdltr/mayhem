(import scheme chicken data-structures srfi-18)
(use glfw3 gl (prefix frp frp:) frp-lowlevel frp-glfw mailbox
     nanovg-gl2 matchable vector-lib defstruct)

(init)
(make-window 640 480 "Drag&drop" resizable: #f swap-interval: 0)
(define *c* (create-context))

(define left-click
  (frp:fold
   (lambda (last button)
     (if (= (car button) +mouse-button-left+)
         (cond ((= (cadr button) +press+) #t)
               ((= (cadr button) +release+) #f))
         last))
   #f
   mouse-button))

(define pos+click
  (frp:merge
   (frp:map (cut list 'cursor-position <>) cursor-position)
   (frp:map (cut list 'left-click <> <>) left-click cursor-position)))


(defstruct gamestate squares grabbed-id grab-offset)

(define (in-rect? px py rx ry rw rh)
  (and (< rx px (+ rx rw))
       (< ry py (+ ry rh))))

(define state
  (frp:fold
   (lambda (state m)
     (match m
       (('cursor-position (x y))
        (if (gamestate-grabbed-id state)
            (update-gamestate
             state
             squares: (vector-map (lambda (i sq)
                                    (if (= i (gamestate-grabbed-id state))
                                        (let ((offset (gamestate-grab-offset state)))
                                          (cons* (- x (car offset)) (- y (cadr offset)) (drop sq 2)))
                                        sq))
                                  (gamestate-squares state)))
            state))
       (('left-click #t (x y))
        (let* ((under-mouse-id
                (vector-index
                 (lambda (sq)
                   (in-rect? x y (car sq) (cadr sq) 50 50))
                 (gamestate-squares state)))
               (under-mouse (and under-mouse-id (vector-ref (gamestate-squares state) under-mouse-id))) )
          (update-gamestate state
                            grabbed-id: under-mouse-id
                            grab-offset: (and under-mouse (list (- x (car under-mouse)) (- y (cadr under-mouse)))))))
       (('left-click #f (_ _))
        (update-gamestate state grabbed-id: #f))))
   (make-gamestate
    squares: (vector '(30 30 1 0.5 0.35) '(148 298 0.35 1 0.5))
    grabbed-id: #f
    grab-offset: '(0 0))
   pos+click))


(define scene
  (frp:map
   (lambda (frame wsize fbsize state)
     ;; represent scene as a thunk for now
     (lambda ()
       (gl:Clear (+ gl:COLOR_BUFFER_BIT
                    gl:STENCIL_BUFFER_BIT))
       (gl:LoadIdentity)
       (let* ((width (car wsize))
              (height (cadr wsize))
              (fbwidth (car fbsize))
              (ratio (if (zero? width) 1 (/ fbwidth width))))
         (gl:Viewport 0 0 width height)
         (begin-frame! *c* width height ratio))
       (vector-for-each
        (lambda (i square)
          (begin-path! *c*)
          (apply (cut rectangle! <> <> <> 50 50) *c* (take square 2))
          (fill-color! *c* (apply make-color-rgbf (drop square 2)))
          (fill! *c*))
        (gamestate-squares state))
       (end-frame! *c*)))
   new-frame window-size framebuffer-size state))


(define tick-receiver (make-mailbox))
(primitive-emitters-set!
 new-frame
 (cons tick-receiver (primitive-emitters new-frame)))
(define scene-receiver (make-mailbox))
(emitters-set! scene (list scene-receiver))

(dynamic-wind
    (lambda () (start-signal-graph! scene))
    (lambda ()
      (let loop ()
        (swap-buffers (window))
        (poll-events)
        (notify-primitive-signal! new-frame #t)
        (let loop2 ()
          (let* ((tick-msg (mailbox-receive! tick-receiver))
                 (scene-msg (if tick-msg (mailbox-receive! scene-receiver))))
            (if (equal? '(change #t) tick-msg)
                (begin ((cadr scene-msg)) ;; it's time to render
                       (unless (window-should-close (window))
                         (loop)))
                (begin (poll-events)
                       (loop2)))))))
    (lambda ()
      (set! *c* #f)
      (destroy-window (window))
      (gc #t)))
