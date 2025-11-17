float myFunc(float a, float b) {
  float z[1] = {2.1};

  if (a == b) {
    z[0] = 1.0;
    z[0] = -z[0];
  }

  float c = z[0];
  return a + b + c;
}

int main() {
  int x = 5;
  float y = myFunc(1.0, 2.0);
  return (x + 1) - y;
}
