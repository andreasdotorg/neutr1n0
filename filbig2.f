      subroutine filbig2(dat,nmax,f0,newdat,c4a,n4)

C  Filter and downsample complex data for X and Y polarizations,
C  stored in array dat(nmax).  Output is downsampled from 11025 or
C  12000 Hz to 375 Hz, and the low-pass filter has f_cutoff = 100 Hz, 
C  f_stop = 187.5 Hz, ripple=0.1 dB, Atten=50 dB.

      parameter (NFFT1=1323000,NFFT2=45000)
      real dat(nmax)                             !Input data
      complex ca(NFFT1)                          !FFTs of input
      complex c4a(NFFT2)                         !Output data
      real*8 df
C Impulse response of filter (one side)
      real halfpulse(4)
!Filter (complex; imag = 0)
      complex cfilt(NFFT2)                       
      real rfilt(NFFT2)                          !Filter (real)
      integer*8 plan1,plan3,plan5
      logical first
      include 'fftw3.f'
      equivalence (rfilt,cfilt)
      data first/.true./
      data halfpulse/0.758314821007,0.205883390004,-0.123207383552,
     +     0.045847258328/
      save

      first=.true.
      if(nmax.lt.0) go to 900
      if(first) then
         npatience=FFTW_ESTIMATE
!         npatience=FFTW_MEASURE
C  Plan the FFTs just once
         call sfftw_plan_dft_1d_(plan1,NFFT1,ca,ca,
     +        FFTW_FORWARD,npatience)                      !BACK
         call sfftw_plan_dft_1d_(plan3,NFFT2,c4a,c4a,
     +        FFTW_BACKWARD,npatience)                     !FOR
         call sfftw_plan_dft_1d_(plan5,NFFT2,cfilt,cfilt,
     +        FFTW_FORWARD,npatience)                      !BACK

C  Convert impulse response to filter function
         do i=1,NFFT2
            cfilt(i)=0.
         enddo
         fac=1.0/NFFT1                          !### Scale here, if needed
         cfilt(1)=fac*halfpulse(1)
         do i=2,4
            cfilt(i)=fac*halfpulse(i)
            cfilt(NFFT2+2-i)=fac*halfpulse(i)
         enddo
         call sfftw_execute_(plan5)

         base=cfilt(NFFT2/2+1)
         do i=1,NFFT2
            rfilt(i)=real(cfilt(i))-base
         enddo

         df=11025.d0/NFFT1
         first=.false.
      endif

C  When new data comes along, we need to compute a new "big FFT"
C  If we just have a new f0, continue with the existing ca and cb.

      if(newdat.ne.0) then
         nz=min(nmax,NFFT1)
         do i=1,nz
            ca(i)=dat(i)
         enddo
         if(nmax.lt.NFFT1) then
            do i=nmax+1,NFFT1
               ca(i)=0.
            enddo
         endif
         call sfftw_execute_(plan1)
         newdat=0
      endif

C  NB: f0 is the frequency at which we want our filter centered.
C      i0 is the bin number in ca and cb closest to f0.

      i0=nint(f0/df) + 1
      nh=NFFT2/2
      do i=1,nh                                !Copy data into c4a and c4b,
         j=i0+i-1                              !and apply the filter function
         c4a(i)=rfilt(i)*ca(j)
      enddo
      do i=nh+1,NFFT2
         j=i0+i-1-NFFT2
         if(j.lt.1) j=j+NFFT1                  !### $$$ ###
         c4a(i)=rfilt(i)*ca(j)
      enddo
      n4=min(int(nmax*375.0/11025.0),NFFT2)
!      c4a(

C  Do the short reverse transform, to go back to time domain.
      print*,'B',NFFT1,NFFT2,plan3
      call sfftw_execute_(plan3)
      print*,'C',n4,plan3
!      go to 999

 900  call sfftw_destroy_plan_(plan1)
      call sfftw_destroy_plan_(plan3)
      call sfftw_destroy_plan_(plan5)
      print*,'Destroying FFTW plans'

 999  return
      end
