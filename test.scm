(use srfi-18)
(use glfw3 gl posix)

(foreign-declare #<<EOF
#include <errno.h>
#include <GLFW/glfw3.h>

static int a2b_read = -1;
static int a2b_write = -1;
static int b2a_read = -1;
static int b2a_write = -1;

static GLFWwindow* window = NULL;
static pthread_t pthread;

static void* start_thread(void* args) {
  char buf[1] = {0};
  while (1) {

    if (read(a2b_read, buf, 1) == -1) {
        err(1, NULL);
    }

    glfwMakeContextCurrent(window);
    glfwSwapBuffers(window);
    glfwMakeContextCurrent(NULL);

    write(b2a_write, buf, 1);
  }
}

EOF
)

(define start-thread!
  (foreign-lambda* int ()
    "C_return(pthread_create(&pthread, NULL, start_thread, NULL));"))

(define register-window!
  (foreign-lambda* void ((c-pointer w))
    "window = (GLFWwindow*) w;"))

(define (external-swap-buffers)
  (signal-swapper-thread!)
  (gc #f)
  (wait-swapper-thread!))

(define (signal-swapper-thread!)
  (write-char #\null a2b-write-port)
  (flush-output a2b-write-port))

(define (wait-swapper-thread!)
  (thread-wait-for-i/o! b2a-read #:input)
  (read-char b2a-read-port))

(define-values (a2b-read a2b-write) (create-pipe))
(define-values (b2a-read b2a-write) (create-pipe))

((foreign-lambda* void ((int a) (int b) (int c) (int d))
   "a2b_read = a; a2b_write = b; b2a_read = c; b2a_write = d;")
 a2b-read a2b-write b2a-read b2a-write)

(thread-start!
 (lambda ()
   (let loop ()
     (print "WORKER1: hello")
     (thread-sleep! 1)
     (loop))))

(thread-start!
 (lambda ()
   (thread-sleep! 0.5)
   (let loop ()
     (print "WORKER2: hello")
     (thread-sleep! 1)
     (loop))))

(start-thread!)

(define a2b-write-port (open-output-file* a2b-write))
(define b2a-read-port (open-input-file* b2a-read))

(with-window (600 400 "GLFW3 Test" resizable: #f)
  (register-window! (window))
  (let loop ()
    (poll-events)
    (gl:Clear gl:COLOR_BUFFER_BIT)
    (gl:LoadIdentity)
    (gl:Rotatef (* 100 (get-time)) 0 0 1)
    (gl:Begin gl:QUADS)
    (gl:Vertex2f -0.2 0.2)
    (gl:Vertex2f 0.2 0.2)
    (gl:Vertex2f 0.2 -0.2)
    (gl:Vertex2f -0.2 -0.2)
    (gl:End)
    ;;(swap-buffers (window))
    (let ((ctx (window)))
      (make-context-current #f)
      (external-swap-buffers)
      (make-context-current ctx))
    (unless (window-should-close (window))
      (loop))))
