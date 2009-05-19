#include <stdio.h>

void w21_(char *utcdate, int len)
{
  FILE *fp;

  fp=fopen("ALL.TXT","a");
  fprintf(fp,"\nUTC Date: \n-----------\n%s\n",utcdate);
  fclose(fp);
}

void w21a_(int *ih, int *im, int *is, char *mode, char *sending,
	   char *cshort, int len1, int len2, int len3)
{
  FILE *fp;

  fp=fopen("ALL.TXT","a");
  fprintf(fp,"%2.2d%2.2d%2.2d  Transmitting: %6s  %28s  %11s\n",
	  *ih,*im,*is,mode,sending,cshort);
  fclose(fp);
}
