#include <cstdio>
#include <iostream>
#include <math.h>

// clang++ driver.cpp pi.ll -o pi

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int print_int(int X) {
  fprintf(stderr, "%d\n", X);
  return 0;
}

extern "C" DLLEXPORT float print_float(float X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

extern "C" {
int func1();
}

bool essentiallyEqual(float a, float b, float epsilon) {
  return fabs(a - b) <= ((fabs(a) > fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

int main() {
  int res1 = func1();

  if (res1 == 2) {
    printf("PASSED Result is : %d\n\n", res1);
  } else {
    printf("FAILED Result is : %d\n\n", res1);
  }
}
