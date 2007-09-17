      subroutine genmet(mode,mettab)

C  Generate metric table for a soft-decision convolutional decoder.
C  Assumes gaussian noise on a BPSK channel.  Works from first principles
C  by evaluating the normal probability function and then computing
C  the log-likelihood for every possible received sysmol value.
C  Symbols are offset binary, with 128 corresponding to an erased symbol.

C  Original C version by Phil Karn, KA9Q.

      character*6 mode
      real bias                         !bias for integer table
      integer scale                     !scale factor for integer table
      integer mettab(0:255,0:1)         !Metric table (RxSymbol,TxSymbol)

      bias=-0.5
      if(mode(1:3).eq.'JT2') then
C  DBPSK
         open(19,file='dmet_20_-2_2.dat',status='old')
         scale=20
      else if(mode(1:3).eq.'JT4') then
C  Nocoherent 2FSK
         open(19,file='dmet_10_-1_3.dat',status='old')
         scale=10
      endif

      do i=0,255
         read(19,*) junk,d0,d1
         mettab(i,0)=nint(scale*(d0-bias))
         mettab(i,1)=nint(scale*(d1-bias))
      enddo

      return
      end

