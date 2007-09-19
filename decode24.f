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

C  Shoule amp be adjusted according to signal strength?

C  Compute soft symbols using differential BPSK demodulation
      c0=0.                                !### C0=1 ???
      k=istart
      fac=1.e-4

      do j=1,nsym+1
         f0=1270.46 + dfx + npr2(j)*df
         dphi=twopi*dt*f0
         c1=0.
         phi=0.d0                          !### ??? ###
         do i=1,1260
            k=k+1
            phi=phi+dphi
            cz=dcmplx(cos(phi),-sin(phi))
            c1=c1 + dat(k)*cz
         enddo
         c1=fac*c1
         rsym=amp*(real(c1)*real(c0) + imag(c1)*imag(c0))
         ang=atan2(imag(c1),real(c1))
         r2=amp*abs(c1)*abs(c0)*sin(ang-ang0)
         ndang=nint(57.1957795131d0*(ang-ang0))
         if(ndang.le.-180) ndang=ndang+360
         if(ndang.gt.180) ndang=ndang-360
!         hist(ndang)=hist(ndang)+1
         write(41,3090) j,ang,ndang,rsym,r2
 3090    format(i3,f10.3,i6,2f10.3)
         ang0=ang
         c0=c1
         r=rsym+128.
         if(r.gt.255.0) r=255.0
         if(r.lt.0.0) r=0.0
         i4=nint(r)
         if(j.ge.1) symbol(j)=i1
      enddo

      do i=1,nsym
         j=0
         if(symbol(i).lt.0) j=1
         i1=symbol(i)
         if(i4.lt.0) i4=i4+256
         write(42,3091) i,i4,j
 3091    format(3i6)
      enddo      

      nbits=72+31
      delta=100
      limit=100000
      ncycles=0
!      symbol(207)=-128
      do i=1,207
         i1=symbol(i)
         ierr=0
         if(i4.ge.128 .and. sym0(i).eq.0) ierr=i4-127
         if(i4.lt.128 .and. sym0(i).eq.1) ierr=i4-128
         write(43,3009) i,sym0(i),i4,ierr
 3009    format(4i5)
      enddo

!      sum=0.
!      do i=1,206
!         i1=symbol(i+1)
!         sum=sum + i4
!      enddo
!      ave=sum/206.
!      sq=0.
!      do i=1,206
!         i1=symbol(i+1)
!         sq=sq + (i4-ave)**2
!      enddo
!      rms=sqrt(sq/205.0)
!      print*,ave,rms

      ncount=fano(metric,ncycles,data1,symbol(2),nbits,mettab,
     +     delta,limit)
!      print*,'Decode24  ncount:',ncount,ncycles

      write(c72,1100) (data1(i),i=1,9)
 1100 format(9b8.8)
!      print*,c72
      read(c72,1102) data4
 1102 format(12b6)
!      write(*,3001) (data1(i),i=1,9),(data4(i),i=1,12)
! 3001 format('Decode24:'9(1x,z2),2x,12(1x,z2))

      decoded='                      '
      if(ncount.ge.0) call unpackmsg(data4,decoded)
!      print*,decoded

!      call extract(s3,nadd,ncount,decoded)     !Extract the message
      qual=0.
      deepmsg='                      '
!      if(ndepth.ge.1) call deep65(s3,mode65,neme,nchallenge,
!     +    flip,mycall,hiscall,hisgrid,deepmsg,qual)


C  Save symbol spectra for possible decoding of average.

      return
      end
