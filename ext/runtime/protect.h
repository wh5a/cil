#ifndef RUNTIME_PROTECT_H
#define RUNTIME_PROTECT_H

#include <unistd.h>

enum ikind {
  TChar,
  TInt,
  TShort,
  TLong,
  TLongLong,
};

enum fkind {
  TFloat,
  TDouble,
  //  TLongDouble
};

void protInit(void);
void write_long(void *, long long, enum ikind);
void write_double(void *, double, enum fkind);
void write_obj(void *, void *, unsigned int);
void update_stacks(void *, unsigned int);
void mreprotect(void *, unsigned int);
void munprotect(void *, unsigned int);
void check_obj(void *, unsigned int);
void check_stacks(void *, unsigned int);

#endif
