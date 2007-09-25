      subroutine decode24(dat,npts,dtx,dfx,flip,ndepth,neme,
     +  mycall,hiscall,hisgrid,mode,mode4,nafc,decoded,ncount,
     +  deepmsg,qual)

C  Decodes JT65 data, assuming that DT and DF have already been determined.

      real dat(npts)                        !Raw data
      real s2(77,126)
      real s3(64,63)
      real ftrack(126)
      character decoded*22,deepmsg*22
      character mycall*12,hiscall*12,hisgrid*6
      character*72 c72
      real*8 dt,df,phi,f0,dphi,twopi
      complex c0,c1
      integer*1 i1,symbol(207)
      integer*1 data1(13)                   !Decoded data (8-bit bytes)
      integer   data4a(9)                   !Decoded data (8-bit bytes)
      integer   data4(12)                   !Decoded data (6-bit bytes)
      integer amp,delta
      integer mettab(0:255,0:1)             !Metric table
      integer fano
      integer npr2(207)
      logical first
      double complex cz
      include 'avecom.h'
      integer*1 sym0
      common/tst99/ sym0(216)
      equivalence (i1,i4)
      data first/.true./
      data npr2/
     +  0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,1,1,0,0,
     +  0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,1,0,1,0,0,0,
     +  1,0,0,1,0,0,1,1,1,1,1,0,0,0,1,0,1,0,0,0,1,1,1,1,0,1,1,0,0,1,
     +  0,0,0,1,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,0,1,0,1,0,1,1,0,1,0,1,
     +  0,1,1,1,0,0,1,0,1,1,0,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,
     +  0,1,1,1,0,1,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,1,0,0,0,1,1,1,1,1,
     +  1,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,1,1,1,1,0,1,0,1/
      save

      rewind 41
      rewind 42
      rewind 43

      if(first) then
         call genmet(mode,mettab)
         twopi=8*atan(1.d0)
         dt=2.d0/11025          !Sample interval (2x downsampled data)
         df=11025.d0/2520.d0
         nsym=206
         amp=15
         first=.false.
      endif

      istart=nint(dtx/dt)              !Start index for synced FFTs

C  Should amp be adjusted according to signal strength?

C  Compute soft symbols using differential BPSK demodulation
      c0=0.                                !### C0=1 ???
      k=istart
      fac=1.e-4
      phi=0.d0

      do j=1,nsym+1
         if(flip.gt.0.0) then
            f0=1270.46 + dfx + npr2(j)*df
         else
            f0=1270.46 + dfx + (1-npr2(j))*df
         endif
         dphi=twopi*dt*f0
         c1=0.
         phi=0.d0                          !### ??? ###  CHECK THIS ###
         do i=1,1260
            k=k+1
            phi=phi+dphi
            cz=dcmplx(cos(phi),-sin(phi))
            if(k.le.npts) c1=c1 + dat(k)*cz
         enddo
         c1=fac*c1
         rsym=amp*(real(c1)*real(c0) + aimag(c1)*aimag(c0))
         c0=c1
         r=rsym+128.
         if(r.gt.255.0) r=255.0
         if(r.lt.0.0) r=0.0
         i4=nint(r)
         if(j.ge.1) symbol(j)=i1
         i4a=i4
         i1=sym0(j)
         write(41,3090) j,rsym,i4a,i4
 3090    format(i3,f9.1,2i6)
      enddo
      call flush(41)

      nbits=72+31
      delta=100
      limit=100000
      ncycles=0
      call interleave24(symbol(2),-1)         !Remove the interleaving

      do iter=1,2
         if(iter.eq.2) then
            do i=2,207
               i1=symbol(i)
               i4=255-i4
               symbol(i)=i1
            enddo
         endif
         ncount=fano(metric,ncycles,data1,symbol(2),nbits,mettab,
     +        delta,limit)
         if(ncount.ge.0) go to 100
      enddo

 100     do i=1,9
         i1=data1(i)
         data4a(i)=i4
      enddo
      write(c72,1100) (data4a(i),i=1,9)
 1100 format(9b8.8)
      read(c72,1102) data4
 1102 format(12b6)

      decoded='                      '
      if(ncount.ge.0) call unpackmsg(data4,decoded)

!      call extract(s3,nadd,ncount,decoded)     !Extract the message
      qual=0.
      deepmsg='                      '
!      if(ndepth.ge.1) call deep65(s3,mode65,neme,nchallenge,
!     +    flip,mycall,hiscall,hisgrid,deepmsg,qual)


C  Save symbol spectra for possible decoding of average.

      return
      end
