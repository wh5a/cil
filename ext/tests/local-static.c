int kk(int x) {
  static int j;
  j++;
  return x;
}

int glob_crit1 = 1, glob_crit2;
int glob3;

int main() {
  int i;
  /* Local statics create some problems for us:
     CIL promotes them to global scope and rename them if necessary.
     CSurf wouldn't know what the variable is renamed to, so proper information cannot be generated.
     Therefore, we require that CIL must preprocess the code before CSurf.
  */
  static int j = 1;
  j += 5;
  i = kk(9);
  glob_crit1++;
  glob_crit2++;
  glob3++;
  
  if (i)
    return kk(5);
  else
    return setuid(i+glob_crit1+glob_crit2+j);
}
