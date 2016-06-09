#include <stdio.h>
#include <pthread.h>
#include <GLFW/glfw3.h>
#include <GL/gl.h>

static pthread_t thread;

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
static char done = 0;

static GLFWwindow* window;

static void* start_thread(void* args) {
  pthread_mutex_lock(&mutex);
  pthread_cond_signal(&cond);

  while (1) {
    pthread_cond_wait(&cond, &mutex);

    glfwMakeContextCurrent(window);
    glfwSwapBuffers(window);
    glfwMakeContextCurrent(NULL);

    done = 1;
    pthread_cond_signal(&cond);
  }
}

int main(void) {
  double lastTime = glfwGetTime();
  /* Initialize the library */
  if (!glfwInit())
    return -1;

  /* Create a windowed mode window and its OpenGL context */
  glfwWindowHint(GLFW_RESIZABLE, 0);
  glfwWindowHint(GLFW_CONTEXT_RELEASE_BEHAVIOR, GLFW_RELEASE_BEHAVIOR_NONE);
  window = glfwCreateWindow(640, 480, "Hello World", NULL, NULL);
  if (!window) {
    glfwTerminate();
    return -1;
  }

  /* Make the window's context current */
  //pthread_mutex_lock(&mutex);
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);

  pthread_mutex_lock(&mutex);
  pthread_create(&thread, NULL, start_thread, NULL);
  pthread_cond_wait(&cond, &mutex);
  
  /* Loop until the user closes the window */
  while (!glfwWindowShouldClose(window)) {
    double curTime = glfwGetTime();
    printf("FPS: %lf\n", 1 / (curTime - lastTime));
    lastTime = curTime;
    
    /* Render here */
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
    glRotated(100 * glfwGetTime(), 0, 0, 1);
    glBegin(GL_TRIANGLES);
    glVertex2f(0, 0.2);
    glVertex2f(0.2, -0.2);
    glVertex2f(-0.2, -0.2);
    glEnd();
    
    /* Swap front and back buffers */
    glfwMakeContextCurrent(NULL);
    pthread_cond_signal(&cond);
    pthread_unlock_mutex(&mutex);
    printf("Doing stuff...\n");
    usleep(100);
    if (done == 1) {
      done = 0;
    }
    else {
      pthread_cond_wait(&cond, &mutex);
    }
    glfwMakeContextCurrent(window);

    
    /* Poll for and process events */
    glfwPollEvents();
  }
  
  glfwTerminate();
  return 0;
}
