#include "protect.h"
//#include "queue.h"

#include <sys/mman.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <string.h>
#include <stdarg.h>
 
extern int protStart;
extern int protEnd ;

// We can also query Linux using sysconf(), on BSD we cannot(?)
#define PAGESIZE 4096
#define PAGEMASK 0xfffff000

// The difference between the real stack and the shadow stack
// Real stack is allocated at the highest address, so almost always higher than shadow
// Real stack base is randomized by kernel
int shadowStackOffset __attribute__((section(".protected")));

int *globStart __attribute__((section(".protected")));
int *globEnd __attribute__((section(".protected")));

static void panic_syscall(char *s) {
  perror(s);
  abort();
}

static void panic(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  
  abort();
}

struct Range {
  unsigned int start;
  unsigned int end;
};

#define NRANGES 100
struct Range ranges[NRANGES];
int nranges;

static void readMap() {
  int verbose = 0;
  // Kernel code in /usr/src/linux/fs/proc/task_mmu.c
  FILE *f = fopen("/proc/self/maps", "r");
  if (!f)
    panic_syscall("Procfs");
  nranges = 1;
  while (1) {
    unsigned int start, end;
    if (fscanf(f, "%x-%x", &start, &end) == EOF)
      break;
    if (verbose)
      printf("%x %x\n", start, end);
    
    if (start == ranges[nranges-1].end) // merge
      ranges[nranges-1].end = end;
    else if (start > ranges[nranges-1].end) {
      ranges[nranges].start = start;
      ranges[nranges].end = end;
      nranges++;
    }
    else
      panic("ERROR! Memory map is not sorted\n");

    // skip until newline
    while (fgetc(f) != '\n');
  }

  if (nranges > NRANGES)
    panic("Out of bound");
  
  if (verbose) {
    printf("============\n");
    int i;
    for (i = 1; i < nranges; i++)
      printf("%x %x\n", ranges[i].start, ranges[i].end);
  }
}

// We assume stackBase is already randomized by kernel ASLR
static void stackInit(rlim_t r, unsigned int stackBase) {
  unsigned int addr;
  // Two surrounding guard pages
  rlim_t stackSize = r + 2 * PAGESIZE;
  FILE *f = fopen("/dev/urandom","r");

  if (!f)
    panic_syscall("Random number");
  
  while (1) {
    if (fread(&addr, sizeof(addr), 1, f) < 1)
      panic_syscall("fread");
    // round it down to the page boundary
    addr &= PAGEMASK;

    // Is this a good address?
    if (addr==0)
      continue;
    unsigned int addr_end = addr + stackSize;
    int i;
    // Shouldn't overlap with any exisiting mapping
    for (i = 1; i < nranges; i++) {
      if (addr>=ranges[i].start && ranges[i].end>addr)
        continue;
      if (ranges[i].start>=addr && addr_end>ranges[i].end)
        continue;
    }

    void *ptr = mmap((void *)addr, stackSize, PROT_NONE, MAP_FIXED|MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
    if (ptr == MAP_FAILED)
      continue;
    assert(ptr==(void *)addr);
    if (mprotect((void *)(addr+PAGESIZE), r, PROT_READ|PROT_WRITE))
      panic_syscall("mprotect");
    fclose(f);
    break;
  }  
  // set the shadow stack base
  unsigned int s_stackBase = addr + PAGESIZE + r;
  //  printf("\nThe bases of the stack and the shadow stack are %p, %p\n", stackBase, s_stackBase);
  shadowStackOffset = stackBase - s_stackBase;
}


void protInit() {
  // Obtain the stack's maximum size with getrlimit
  // TODO: Instrument setrlimit() calls to adjust the shadow stack.
  // Another idea: Grow and shrink the stack on demand via (mprotect + catching segfault)
  struct rlimit r;
  if (getrlimit(RLIMIT_STACK, &r))
    panic_syscall("getrlimit");

  // Programs can increase soft limits up to hard limits
  // Normally, r.rlim_cur = 8192k (ulimit -s), r.rlim_max = RLIM_INFINITY (ulimit -s -H)

  // protInit should be called from main, so stackBase is the frame pointer of main,
  // or caller of main (when protInit is inlined)
  // TODO: Since we're reading the memory map anyway, we can also get stackBase from there.

  // Readers can also read about CHECK_GETFRAME in ccured/include/ccuredcheck.h
  // GCC Info 5.41 Getting the Return or Frame Address of a Function
  // http://gcc.gnu.org/onlinedocs/gcc-4.1.2/gcc/Return-Address.html
  unsigned int stackBase = (unsigned int) __builtin_frame_address(1);

  /*
    Now create a shadow stack using mmap.
    NOTE: mmap could be dangerous because it can override existing mappings!
    THEREFORE, it is left to us to handle existing mappings.
    Experiments:
    mmap on 0x8048000: no error reported by mmap. simply segfaults afterwards.
    mmap on 0xc000f000: mmap reports an error that it cannot allocate memory
  */
  readMap();
  stackInit(r.rlim_cur, stackBase);

  // Write protect from &protStart to &protEnd
  globStart = &protStart;
  globEnd = &protEnd;
  assert(globEnd > globStart);
  if (mprotect(globStart, globEnd - globStart, PROT_READ))
    panic_syscall("mprotect read-only");
}

void write_obj(void *dest, void *src, unsigned int size) {
  // If dest points to a global, we munprotect, memcpy, mreprotect.
  if ((int *)dest >= globStart && (int *)dest < globEnd) {
    munprotect(dest, size);
    memcpy(dest, src, size);
    mreprotect(dest, size);
  }
  /* Otherwise, we memcpy, update_stacks.
     TODO: handle heaps
  */
  else {
    memcpy(dest, src, size);
    update_stacks(dest, size);
  }
}

#define GENERIC_WRITER(TYPE) do { \
  TYPE *dest = p, value = (TYPE)v; \
  unsigned int size = sizeof(TYPE); \
  if ((int *)dest >= globStart && (int *)dest < globEnd) { \
    munprotect(dest, size); \
    *dest = value; \
    mreprotect(dest, size); \
  } \
  else { \
    *dest = value; \
    update_stacks(dest, size); \
  } \
} while (0)

// write_long and write_double are optimized scalar versions of write_obj
void write_long(void *p, long long v, enum ikind type_tag) {
  switch (type_tag) {
  case TChar:
    GENERIC_WRITER(char);
    break;
  case TInt:
    GENERIC_WRITER(int);
    break;
  case TShort:
    GENERIC_WRITER(short);
    break;
  case TLong:
    GENERIC_WRITER(long);
    break;
  case TLongLong:
    GENERIC_WRITER(long long);
    break;
  }
}

void write_double(void *p, double v, enum fkind type_tag) {
  switch (type_tag) {
  case TFloat:
    GENERIC_WRITER(float);
    break;
  case TDouble:
    GENERIC_WRITER(double);
    break;
  }
}

void update_stacks(void *addr, unsigned int size) {
  char *dest = (char *)addr - shadowStackOffset;
  memcpy(dest, addr, size);
}

void mreprotect(void *addr, unsigned int size) {
  unsigned int start = (unsigned int)addr & PAGEMASK,
    len = (unsigned int)addr + size - start;
  if (mprotect((void *)start, len, PROT_READ))
    panic_syscall("mprotect");
}

void munprotect(void *addr, unsigned int size) {
  unsigned int start = (unsigned int)addr & PAGEMASK,
    len = (unsigned int)addr + size - start;
  if (mprotect((void *)start, len, PROT_READ | PROT_WRITE))
    panic_syscall("mprotect");
}

void check_stacks(void *addr, unsigned int size) {
  char *dest = (char *)addr - shadowStackOffset;
  if (memcmp(dest, addr, size))
    panic("ERROR! %p and %p are supposed to be identical copies.\n", dest, addr);
}

void check_obj(void *p, unsigned int size) {
  if ((int *)p >= globStart && (int *)p < globEnd)
    return;
  check_stacks(p, size);
}
