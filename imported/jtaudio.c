#include <stdio.h>
#include <portaudio.h>
#include <string.h>

void fivehz_(void);
void fivehztx_(void);
void addnoise_(short int *n);

//  Definition of structure pointing to the audio data
typedef struct
{
  double *Tsec;
  double *tbuf;
  int    *iwrite;
  int    *ibuf;
  int    *TxOK;
  int    *ndebug;
  int    *ndsec;
  int    *Transmitting;
  int    *nwave;
  int    *nmode;
  int    *trperiod;
  int     nbuflen;
  int     nfs;
  short  *y1;
  short  *y2;
  short  *iwave;
} paTestData;

typedef struct _SYSTEMTIME
{
  short   Year;
  short   Month;
  short   DayOfWeek;
  short   Day;
  short   Hour;
  short   Minute;
  short   Second;
  short   Millisecond;
} SYSTEMTIME;

#ifdef Win32
  extern void __stdcall GetSystemTime(SYSTEMTIME *st);
#else
  #include <sys/time.h>
  #include <time.h>

  void GetSystemTime(SYSTEMTIME *st){
    struct timeval tmptimeofday;
    struct tm tmptmtime;
    gettimeofday(&tmptimeofday,NULL);
    gmtime_r((const time_t *)&tmptimeofday.tv_sec,&tmptmtime);
    st->Year = (short)tmptmtime.tm_year;
    st->Month = (short)tmptmtime.tm_year;
    st->DayOfWeek = (short)tmptmtime.tm_wday;
    st->Day = (short)tmptmtime.tm_mday;
    st->Hour = (short)tmptmtime.tm_hour;
    st->Minute = (short)tmptmtime.tm_min;
    st->Second = (short)tmptmtime.tm_sec;
    st->Millisecond = (short)(tmptimeofday.tv_usec/1000);
  }
#endif

//  Input callback routine:
static int
SoundIn( void *inputBuffer, void *outputBuffer,
		       unsigned long framesPerBuffer,
		       const PaStreamCallbackTimeInfo* timeInfo, 
		       PaStreamCallbackFlags statusFlags,
		       void *userData )
{
  paTestData *data = (paTestData*)userData;
  short *in = (short*)inputBuffer;
  unsigned int i;
  static int ia=0;
  static int ib=0;
  static int ncall=0;
  static int nsec0=0;
  static double stime0=86400.0;
  int nsec;
  double stime;
  SYSTEMTIME st;

  // Get System time
  GetSystemTime(&st);
  nsec = (int) (st.Hour*3600.0 + st.Minute*60.0 + st.Second);
  stime = nsec + st.Millisecond*0.001 + *data->ndsec*0.1;
  *data->Tsec = stime;
  nsec=(int)stime;
  ncall++;

  // NB: inputBufferAdcTime and currentTime do not work properly.
  /*
  if(nsec!=nsec0) {
    printf("%f %f %f %f\n",stime,timeInfo->inputBufferAdcTime,
	   timeInfo->currentTime,timeInfo->outputBufferDacTime);
  }
  */

  //  if((inputBuffer==NULL) & (ncall>2) & (stime>stime0)) {
  if((statusFlags!=0) & (ncall>2) & (stime>stime0)) {
    if(*data->ndebug) 
      printf("Status flags %d at Tsec = %7.1f s, DT = %7.1f\n",
	       (int)statusFlags,stime,stime-stime0);
    stime0=stime;
  }

  if((statusFlags&1) == 0) {
    //increment buffer pointers only if data available
    ia=*data->iwrite;
    ib=*data->ibuf;
    ib++;                               //Increment ibuf
    if(ib>1024) ib=1; 
    *data->ibuf=ib;
    data->tbuf[ib-1]=stime;
    for(i=0; i<framesPerBuffer; i++) {
      data->y1[ia] = (*in++);
      data->y2[ia] = (*in++);
      ia++;
    }
  }

  if(ia >= data->nbuflen) ia=0;          //Wrap buffer pointer if necessary
  *data->iwrite = ia;                    //Save buffer pointer
  fivehz_();                             //Call fortran routine
  nsec0=nsec;
  return 0;
}

//  Output callback routine:
static int
SoundOut( void *inputBuffer, void *outputBuffer,
		       unsigned long framesPerBuffer,
		       const PaStreamCallbackTimeInfo* timeInfo, 
		       PaStreamCallbackFlags statusFlags,
		       void *userData )
{
  paTestData *data = (paTestData*)userData;
  short *wptr = (short*)outputBuffer;
  unsigned int i,n;
  static short int n2;
  static int ic=0;
  static int TxOKz=0;
  int nsec;
  double stime;
  SYSTEMTIME st;

   // Get System time
  GetSystemTime(&st);
  nsec = (int) (st.Hour*3600.0 + st.Minute*60.0 + st.Second);
  stime = nsec + st.Millisecond*0.001 + *data->ndsec*0.1;
  *data->Tsec = stime;
  nsec=(int)stime;

  if(*data->TxOK && (!TxOKz))  {
    n=nsec/(*data->trperiod);
    //    ic = (int)(stime - *data->trperiod*n) * data->nfs/framesPerBuffer;
    //    ic = framesPerBuffer*ic;

    if(*data->nmode == 3)  {
      ic = 0;
    }
    else  {
      ic = (int)(stime - *data->trperiod*n) * data->nfs;
      ic = ic % *data->nwave;
    }
  }

  TxOKz=*data->TxOK;
  *data->Transmitting=*data->TxOK;

  if(*data->TxOK)  {
    for(i=0 ; i < framesPerBuffer; i++ )  {
      n2=data->iwave[ic];
      addnoise_(&n2);
      *wptr++ = n2;                   //left
      *wptr++ = n2;                   //right
      ic++;

      if(ic >= *data->nwave) {
	/*             FSK441               JT6M                 JT41  */
        if((*data->nmode != 1) && (*data->nmode != 4) && (*data->nmode != 9)) {
          *data->TxOK = 0;
          ic--;
        } else {
          ic = ic % *data->nwave;       //Wrap buffer pointer if necessary
        }
      }
    }
  } else {
    memset((void*)outputBuffer, 0, 2*sizeof(short)*framesPerBuffer);
  }
  fivehztx_();                             //Call fortran routine
  return 0;
}

/*******************************************************************/
int jtaudio_(int *ndevin, int *ndevout, short y1[], short y2[], 
	     int *nbuflen, int *iwrite, short iwave[], 
	     int *nwave, int *nfsample, int *nsamperbuf,
	     int *TRPeriod, int *TxOK, int *ndebug,
 	     int *Transmitting, double *Tsec, int *ngo, int *nmode,
	     double tbuf[], int *ibuf, int *ndsec)
{
  paTestData data;
  PaStream *instream, *outstream;
  PaStreamParameters inputParameters, outputParameters;
  //  PaStreamInfo *streamInfo;

  int nSampleRate = *nfsample;
  int ndevice_in = *ndevin;
  int ndevice_out = *ndevout;
  double dSampleRate = (double) *nfsample;
  PaError err_init, err_open_in, err_open_out, err_start_in, err_start_out;
  PaError err = 0;

  data.Tsec = Tsec;
  data.tbuf = tbuf;
  data.iwrite = iwrite;
  data.ibuf = ibuf;
  data.TxOK = TxOK;
  data.ndebug = ndebug;
  data.ndsec = ndsec;
  data.Transmitting = Transmitting;
  data.y1 = y1;
  data.y2 = y2;
  data.nbuflen = *nbuflen;
  data.nmode = nmode;
  data.nwave = nwave;
  data.iwave = iwave;
  data.nfs = nSampleRate;
  data.trperiod = TRPeriod;

  err_init = Pa_Initialize();                      // Initialize PortAudio

  if(err_init) {
    printf("Error initializing PortAudio.\n");
    printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_init), err_init);

    Pa_Terminate();  // I don't think we need this but...

    return(-1);
  }

  //  printf("Opening device %d for input, %d for output...\n",ndevice_in,ndevice_out);

  inputParameters.device = ndevice_in;
  inputParameters.channelCount = 2;
  inputParameters.sampleFormat = paInt16;
  inputParameters.suggestedLatency = 0.2;
  inputParameters.hostApiSpecificStreamInfo = NULL;

// Test if this configuration actually works, so we do not run into an ugly assertion
  err_open_in = Pa_IsFormatSupported(&inputParameters, NULL, dSampleRate);

  if (err_open_in == 0) {
    err_open_in = Pa_OpenStream(
		       &instream,       //address of stream
		       &inputParameters,
		       NULL,
		       dSampleRate,            //Sample rate
		       2048,            //Frames per buffer
		       paNoFlag,
		       (PaStreamCallback *)SoundIn,  //Callback routine
		       (void *)&data);  //address of data structure

    if(err_open_in) {   // We should have no error here usually
      printf("Error opening input audio stream:\n");
      printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_open_in), err_open_in);

      err = 1;
    } else {
      //      printf("Successfully opened audio input.\n");
    }
  } else {
    printf("Error opening input audio stream.\n");
    printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_open_in), err_open_in);

    err = 1;
  }

  outputParameters.device = ndevice_out;
  outputParameters.channelCount = 2;
  outputParameters.sampleFormat = paInt16;
  outputParameters.suggestedLatency = 0.2;
  outputParameters.hostApiSpecificStreamInfo = NULL;

// Test if this configuration actually works, so we do not run into an ugly assertion
  err_open_out = Pa_IsFormatSupported(NULL, &outputParameters, dSampleRate);

  if (err_open_out == 0) {
    err_open_out = Pa_OpenStream(
		       &outstream,      //address of stream
		       NULL,
		       &outputParameters,
		       dSampleRate,            //Sample rate
		       2048,            //Frames per buffer
		       paNoFlag,
		       (PaStreamCallback *)SoundOut,  //Callback routine
		       (void *)&data);  //address of data structure

    if(err_open_out) {     // We should have no error here usually
      printf("Error opening output audio stream!\n");
      printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_open_out), err_open_out);

      err += 2;
    } else {
      //      printf("Successfully opened audio output.\n");
    }
  } else {
    printf("Error opening output audio stream.\n");
    printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_open_out), err_open_out);

    err += 2;
  }

  // if there was no error in opening both streams start them
  if (err == 0) {
    err_start_in = Pa_StartStream(instream);             //Start input stream

    if(err_start_in) {
      printf("Error starting input audio stream!\n");
      printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_start_in), err_start_in);

      err += 4;
    }

    err_start_out = Pa_StartStream(outstream);             //Start output stream

    if(err_start_out) {
      printf("Error starting output audio stream!\n");
      printf("\tErrortext: %s\n\tNumber: %d\n",Pa_GetErrorText(err_start_out), err_start_out);

      err += 8;
    } 
  }

  if (err == 0) printf("Audio streams running normally.\n******************************************************************\n");

  while( Pa_IsStreamActive(instream) && (*ngo != 0) && (err == 0) )  {
    //    printf("CPU: %.1f\%\n",100*Pa_GetStreamCpuLoad(instream));
    Pa_Sleep(200);
  }

  Pa_AbortStream(instream);              // Abort stream
  Pa_CloseStream(instream);             // Close stream, we're done.
  Pa_AbortStream(outstream);              // Abort stream
  Pa_CloseStream(outstream);             // Close stream, we're done.

  Pa_Terminate();

  return(err);
}


int padevsub_(int *idevin, int *idevout)
{
  int numdev,ndefin,ndefout;
  int nchin[41], nchout[41];
  int      i, devIdx;
  int      numDevices;
  const PaDeviceInfo *pdi;
  PaError  err;

  Pa_Initialize();
  numDevices = Pa_GetDeviceCount();
  numdev = numDevices;

  if( numDevices < 0 )  {
    err = numDevices;
    Pa_Terminate();
    return err;
  }

  if ((devIdx = Pa_GetDefaultInputDevice()) > 0) {
    ndefin = devIdx;
  } else {
    ndefin = 0;
  }

  if ((devIdx = Pa_GetDefaultOutputDevice()) > 0) {
    ndefout = devIdx;
  } else {
    ndefout = 0;
  }

  printf("\nAudio     Input    Output     Device Name\n");
  printf("Device  Channels  Channels\n");
  printf("------------------------------------------------------------------\n");

  for( i=0; i < numDevices; i++ )  {
    pdi = Pa_GetDeviceInfo(i);
//    if(i == Pa_GetDefaultInputDevice()) ndefin = i;
//    if(i == Pa_GetDefaultOutputDevice()) ndefout = i;
    nchin[i]=pdi->maxInputChannels;
    nchout[i]=pdi->maxOutputChannels;
    printf("  %2d       %2d        %2d       %s\n",i,nchin[i],nchout[i],pdi->name);
  }

  printf("\nUser requested devices:   Input = %2d   Output = %2d\n",
  	 *idevin,*idevout);
  printf("Default devices:          Input = %2d   Output = %2d\n",
  	 ndefin,ndefout);
  if((*idevin<0) || (*idevin>=numdev)) *idevin=ndefin;
  if((*idevout<0) || (*idevout>=numdev)) *idevout=ndefout;
  if((*idevin==0) && (*idevout==0))  {
    *idevin=ndefin;
    *idevout=ndefout;
  }
  printf("Will open devices:        Input = %2d   Output = %2d\n",
  	 *idevin,*idevout);

  Pa_Terminate();

  return 0;
}

