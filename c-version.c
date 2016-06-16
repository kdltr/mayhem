#include <stdio.h>
#include <pthread.h>
#include <GLFW/glfw3.h>
#include <GL/gl.h>

static pthread_t thread;

static int a2b[2];
static int b2a[2];

static GLFWwindow* window;

static void* start_thread(void* args) {
  char buf[1] = {0};

  while (1) {
    read(a2b[0], buf, 1);

    glfwMakeContextCurrent(window);
    glfwSwapBuffers(window);
    glfwMakeContextCurrent(NULL);

    write(b2a[1], buf, 1);
  }
}

int main(void) {
  double lastTime = glfwGetTime();
  char buf[1] = {0};
  
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
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);

  pipe(a2b);
  pipe(b2a);

  pthread_create(&thread, NULL, start_thread, NULL);
  
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
    write(a2b[1], buf, 1);
    printf("Doing stuff...\n");
    //usleep(15000);
    read(b2a[0], buf, 1);
    glfwMakeContextCurrent(window);

    
    /* Poll for and process events */
    glfwPollEvents();
  }
  
  glfwTerminate();
  return 0;
}
