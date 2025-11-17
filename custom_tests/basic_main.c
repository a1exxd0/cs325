float myFunc(float a, float b) {
  float z[1][2] = {{2.1, 3.2}};

  if (a == b) {
    z[0][0] = 1.0;
    z[0][0] = -z[0][0];
  }

  float c = z[0][0];
  return a + b + c;
}

int main() {
  int x = 5;
  float y = myFunc(1.0, 2.0);
  return (x + 1) - y;
}
