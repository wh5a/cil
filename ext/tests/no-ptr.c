int kk(int x) {
  int j;
  j++;
  return x;
}

int glob_crit1 = 1, glob_crit2;
int glob3;

int main() {
  int i;
  int j = 1;
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
