      subroutine decodecw(dat,jz,cfile6,DFTolerance,NFreeze,
     +        MouseDF,mycall,hiscall,hisgrid)

      parameter (NFFT=16384,NH=NFFT/2)
      parameter (NMAX=150*11025)
      real dat(jz)
      character cfile6*6
      character mycall*12,hiscall*12,hisgrid*6
      integer DFTolerance
      integer nparm(5,30)
      real s(NH),s1(NH)
      real x(65536)
      real*8 dt,f0,phi,dhpi,twopi
      complex c(NMAX),z,cx(32768)
      equivalence (x,cx)

      twopi=8.d0*atan(1.d0)
      nblk=jz/nfft
      call zero(s,nh)
      k=0
      do n=1,nblk
         do i=1,nfft
            k=k+1
            x(i)=dat(k)
         enddo
         call ps(x,nfft,s1)
         call add(s,s1,s,nh)
      enddo

      df=11025.0/nfft
      do i=1,nh
         freq=df*(i-1)
         sx=-10
         if(s(i).gt.0.0) sx=10.0*log10(s(i))
         write(71,3001) freq,s(i),sx
 3001    format(3f10.3)
      enddo

      dt=1.d0/11025.d0
      f0=800.d0
      dphi=twopi*f0*dt
      phi=0.d0
      fac=1.e-4
      do i=1,jz
         phi=phi+dphi
         c(i)=fac*dat(i)*cmplx(cos(phi),-sin(phi))
      enddo

      nstep=nint(0.003d0/dt)
      nsum=nint(0.06d0/dt)
      iz=jz/nstep
      do i=1,iz
         k=nstep*(i-1)
         z=0.
         do n=1,nsum
            k=k+1
            z=z+c(k)
         enddo
         x(i)=real(z)**2 + imag(z)**2
      enddo
      do i=iz+1,65536
         x(i)=0.
      enddo
      call xfft(x,65536)
      dt2=dt*nstep
      df2=(1.d0/dt2)/65536
      ia=15.0/df2
      ib=18.333/df2
      ymax=-99.
      do i=ia,ib
         y=real(cx(i))**2 + imag(cx(i))**2
         yy=-20.
         if(y.gt.0.0) yy=10.0*log10(y)
         write(72,3002) (i-1)*df2,y,yy
 3002    format(f10.3,e12.3,f10.3)
         if(y.gt.ymax) then
            ymax=y
            ipk=i-1
         endif
      enddo

      fdit=df2*ipk
      bits=1.2*fdit*966.6667/20.0
      nbits=2*nint(0.5*bits)
      print*,bits,nbits,fdit,ipk
      call msgparms(nbits,nparm,kz)
      do k=1,kz
         write(*,3003) k,(nparm(i,k),i=1,5)
 3003    format(6i5)
      enddo

      return
      end

