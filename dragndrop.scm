(import scheme chicken data-structures srfi-18)
(use glfw3 gl (prefix frp frp:) frp-lowlevel frp-glfw mailbox
     nanovg-gl2 matchable)

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
  (frp:map list cursor-position left-click))


(define (in-rect? px py rx ry rw rh)
  (and (< rx px (+ rx rw))
       (< ry py (+ ry rh))))

(define state
  (frp:fold
   (lambda (states pos+click)
     (let ((cursor-pos (car pos+click))
           (click (cadr pos+click)))
       (let loop ((states states))
         (if (null? states)
             '()
             (cons
              (let ((state (car states)))
                (if (and click (apply in-rect? (append cursor-pos (take state 4))))
                    (append (map (cut - <> 25) cursor-pos)
                            (drop state 2))
                    state))
              (loop (cdr states)))))))
   '((30 30 50 50 1 0.5 0.35)
     (148 298 50 50 0.35 1 0.5))
   pos+click))


(define scene
  (frp:map
   (lambda (frame wsize fbsize states)
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
       (for-each
        (lambda (state)
          (begin-path! *c*)
          (apply rectangle! *c* (take state 4))
          (fill-color! *c* (apply make-color-rgbf (drop state 4)))
          (fill! *c*))
        states)
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
