struct s {
  int a;
  int c;
};

struct s glob[30];

// Take an array
int foo(float p[]) {
  // Write to a float pointer
  p[3] += 3.3;
  return (int)p[3];
}

// Take a struct field pointer
int bar(int *p) {
  return *p;
}

// Return a struct field pointer
int *baz() {
  int *p = &glob[5].a;
  // Write to a int pointer
  *p += 6;
  return p;
}

// Write to a struct pointer
void g(struct s *p) {
  struct s s1 = {1,2};
  *p = s1;
}

// Return a struct
struct s h() {
  struct s s1 = {3,4};
  return s1;
}
  
int main() {
  float f[20];
  f[3] = 55.2;

  glob[5].a = 33;

  g(glob+7);

  struct s ss, *pp = &ss;
  // Write to a pointer with a function return value
  *pp = h();

  // 55+42+1+4 = 102
  return foo(f) + bar(baz()) + glob[7].a + pp->c;
}
