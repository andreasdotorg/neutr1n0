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
      real*8 dt,df,phi,f0,dphi,twopi
      complex c0,c1
      integer*1 i1,symbol(207)
      integer amp
      integer npr2(207)
      double complex cz
      include 'avecom.h'
      equivalence (i1,i4)
      data npr2/
     +  0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,1,1,0,0,
     +  0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,1,0,1,0,0,0,
     +  1,0,0,1,0,0,1,1,1,1,1,0,0,0,1,0,1,0,0,0,1,1,1,1,0,1,1,0,0,1,
     +  0,0,0,1,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,0,1,0,1,0,1,1,0,1,0,1,
     +  0,1,1,1,0,0,1,0,1,1,0,1,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,
     +  0,1,1,1,0,1,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,1,0,0,0,1,1,1,1,1,
     +  1,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,1,0,1,1,1,1,0,1,0,1/
      save

      twopi=8*atan(1.d0)
      dt=2.d0/11025                    !Sample interval (2x downsampled data)
      df=11025.d0/2520.d0
      istart=nint(dtx/dt)              !Start index for synced FFTs
      nsym=206
      amp=10

C  Compute soft symbols using differential BPSK demodulation
      c0=0.
      k=istart
      fac=1.e-4

      print*,dtx,dfx,df,flip,ndepth

      do j=1,nsym
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
      print*,npts,k,istart

      do i=1,nsym
         j=0
         if(symbol(i).lt.0) j=1
         i1=symbol(i)
         if(i4.lt.0) i4=i4+256
         write(42,3091) i,i4,j
 3091    format(3i6)
      enddo      

!      call extract(s3,nadd,ncount,decoded)     !Extract the message
      qual=0.
      if(ndepth.ge.1) call deep65(s3,mode65,neme,nchallenge,
     +    flip,mycall,hiscall,hisgrid,deepmsg,qual)

      if(ncount.lt.0) decoded='                      '

C  Save symbol spectra for possible decoding of average.
!      do j=1,63
!         k=mdat(j)
!         if(flip.lt.0.0) k=mdat2(j)
!         call move(s2(8,k),ppsave(1,j,nsave),64)
!      enddo

      return
      end
