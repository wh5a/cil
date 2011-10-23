struct s {
  int a;
  union {
    char b;
    int c;
  };
};

struct s glob;

int main() {
  struct s loc;
  glob.a = 33;
  glob.c = 44;
  loc.a = 55;
  loc.b = 66;
  if (glob.b)
    return glob.a + loc.b;
  else
    return loc.a + loc.b;
}
