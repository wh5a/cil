# 1 "no-heap.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "no-heap.c"
struct s {
  int a;
  int c;
};

struct s glob[30];


int foo(float p[]) {
  return (int)p[3];
}


int bar(int *p) {
  return *p;
}


int *baz() {
  int *p = &glob[5].a;

  *p += 6;
  return p;
}


void g(struct s *p) {
  struct s s1 = {1,2};
  *p = s1;
}


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

  *pp = h();


  return foo(f) + bar(baz()) + glob[7].a + pp->c;
}
