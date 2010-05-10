#include <windows.h>
#include <stdio.h>

int ptt_(int *nport, char *unused, int *ntx, int *iptt)
{
  static HANDLE hFile;
  static int open=0, nhold=0;
  char s[10];
  int i3,i4,i5,i6,i9,i00;

  if(*nport==0) {
    *iptt=*ntx;
    return(0);
  }

  nhold=0;
  if(*nport>100) nhold=1;

  if(*ntx && (!open)) {
    sprintf(s,"\\\\.\\COM%d",*nport%100);
    hFile=CreateFile(
		     TEXT(s),
		     GENERIC_WRITE,
		     0,
		     NULL,
		     OPEN_EXISTING,
		     FILE_ATTRIBUTE_NORMAL,
		     NULL
		     );
    if(hFile==INVALID_HANDLE_VALUE) {
      printf("PTT: Cannot open COM port %d.\n",*nport%100);
      return(1);
    }
    open=1;
  }

  if(*ntx && open) {
    EscapeCommFunction(hFile,3);
    EscapeCommFunction(hFile,5);
    *iptt=1;
  }

  else {
    EscapeCommFunction(hFile,4);
    EscapeCommFunction(hFile,6);
    EscapeCommFunction(hFile,9);
    if(nhold==0)  {
      i00=CloseHandle(hFile);
      open=0;
    }
    *iptt=0;
  }
  return(0);
}
