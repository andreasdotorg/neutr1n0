#include <stdio.h>

void azelout_(int *ih, int *im, int *is, float *AzMoon, float *ElMoon,
	     float *AzSun, float *ElSun, float *AzAux, float *ElAux,
	     int *nfreq, float *doppler, double *dfdt, float *doppler00,
	     double *dfdt0)
{
  FILE *fp;

  fp=fopen("azel.dat","w");
  fprintf(fp,"%2.2d:%2.2d:%2.2d,%5.1f,%5.1f,Moon\n",*ih,*im,*is,
	  *AzMoon,*ElMoon);
  fprintf(fp,"%2.2d:%2.2d:%2.2d,%5.1f,%5.1f,Sun\n",*ih,*im,*is,
	  *AzSun,*ElSun);
  fprintf(fp,"%2.2d:%2.2d:%2.2d,%5.1f,%5.1f,Source\n",*ih,*im,*is,
	  *AzAux,*ElAux);
  fprintf(fp,"%4d,%8.1f,%8.2f,%8.1f,%8.2f,Doppler\n",*nfreq,*doppler,
	  *dfdt,*doppler00,*dfdt0);
  fflush(fp);
}
