/* test support for large constants in enums (gcc-specific) 
   if you ask me, gcc's behaviour is a bit weird, but... */
enum fun {
  NEG = -1, 
  SMALL = 33,
  STRANGE = 44LL,
  LARGE = 22LL << 34
};

long long magic1 = 22LL << 34;
enum fun magic2 = LARGE;

int main()
{
  int ok = 1;

  ok = ok && (enum fun)-1 < 0;

  return ok ? 0 : 2;
}
