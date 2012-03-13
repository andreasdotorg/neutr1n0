#include <stdio.h>
#include <stdlib.h>

#ifdef CVF
#include "pthread_w32.h"
#else
#include <pthread.h>
#endif

#include <time.h>
#include <sys/time.h>

extern void decode1_(int *iarg);
extern void a2d_(int *iarg);

int
start_threads_(void)
{
  pthread_t thread1,thread2;
  int iret1,iret2;
  int iarg1 = 1,iarg2 = 2;

  iret1 = pthread_create(&thread1,NULL,(void *)a2d_,&iarg1);
  iret2 = pthread_create(&thread2,NULL,(void *)decode1_,&iarg2);
  return (iret1 | iret2);
}
