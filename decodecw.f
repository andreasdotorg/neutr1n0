      subroutine decodecw(dat,jz,cfile6,DFTolerance,NFreeze,
     +        MouseDF,mycall,hiscall,hisgrid)

      parameter (NFFT=4096,NH=NFFT/2)
      parameter (NMAX=150*11025)
      real dat(jz)
      character cfile6*6,rpt*3
      character mycall*12,hiscall*12,hisgrid*6
      integer DFTolerance
      integer nparm(5,30),id(300)
      real s(NH),s1(NH)
      real tmp(2600)
      real f1(300),f2(300)
      real x(65536)
      real y2(32)
      real*8 dt,f0,phi,dhpi,twopi
      complex c(NMAX),z,cx(0:32768)
      equivalence (x,cx)

      twopi=8.d0*atan(1.d0)
      rewind 71
      rewind 72
      rewind 73

C  Examine the RF spectrum.  This might be needed to find f0.
      nblk=jz/nfft                       !Number of non-overlapping blocks
      call zero(s,nh)                    !Zero the accumulating array
      k=0
      do n=1,nblk                        !Compute spectra
         do i=1,nfft
            k=k+1
            x(i)=dat(k)
         enddo
         call ps(x,nfft,s1)
         call add(s,s1,s,nh)             !Add spectrum into s().
      enddo
      call smooth(s,NH)                  !Smooth it once, 1-2-1

      df=11025.0/nfft
      do i=1,nh
         freq=df*(i-1)
         sx=-10
         if(s(i).gt.0.0) sx=10.0*log10(s(i))
         write(71,3001) freq,s(i),sx
 3001    format(3f10.3)
      enddo

C  Mix signal to baseband
      dt=1.d0/11025.d0
      f0=800.d0                          !Assumed f0, for now
      dphi=twopi*f0*dt
      phi=0.d0
      fac=1.e-5
      do i=1,jz
         phi=phi+dphi
         c(i)=fac*dat(i)*cmplx(cos(phi),-sin(phi))
      enddo

C  Compute oversampled symbol spectra
      nstep=nint(0.003d0/dt)             !Step size, in samples
      nsum=nint(0.06d0/dt)               !Nominal symbol length
      iz=jz/nstep
      do i=1,iz
         k=nstep*(i-1)
         z=0.
         do n=1,nsum                     !Integrate and dump
            k=k+1
            z=z+c(k)
         enddo
         x(i)=real(z)**2 + aimag(z)**2    !Convert to power
      enddo

      nfft2=2**(int(log(float(iz))/log(2.0)) + 1)
      nh2=nfft2/2
      dt2=dt*nstep
      df2=(1.d0/dt2)/nfft2
      do i=iz+1,nfft2
         x(i)=0.
      enddo
      call xfft(x,nfft2)                 !Compute specurum
      do i=1,nh2                         !Convert to power
         x(i)=real(cx(i))**2 + aimag(cx(i))**2
      enddo
      call smooth(x,nh2)                 !Smooth once

      ia=15.0/df2
      ib=18.333/df2
      ymax=-99.
      do i=ia,ib                         !Do two-harmonic summing
         ih=i/2
         if(2*ih.eq.i) then
            y=x(ih) + x(i)
         else
            y=0.5*(x(ih)+x(ih+1)) + x(i)
         endif
         write(72,3002) (i-1)*df2,y
 3002    format(f10.3,f12.1)
         if(y.gt.ymax) then
            ymax=y
            ipk=i
         endif
      enddo

      fdit=df2*ipk
      bits=1.2*fdit*966.6667/20.0
      nbits=2*nint(0.5*bits)
      b4=58.0*20.0/1.2
      wpm=20.0*nbits/b4
      tdit=1.2d0/wpm                     !Key-down dit time, seconds
      fdit=1.0/tdit
!      print*,bits,nbits,fdit,wpm

C  OK, now we (probably) know the actual keying rate.  Re-compute the
C  symbol spectra.

      tstep=tdit/16.0                    !Oversample by 16 x
      isym=tdit*11025.0
      t=-tstep
      do k=1,41000
         t=t+tstep
         ia=t*11025.0 + 1.0
         ib=ia+isym
         if(ib.gt.jz) go to 10
         z=0.
         do i=ia,ib
            z=z+c(i)
         enddo
         x(k)=real(z)**2 + aimag(z)**2    !Convert to power
      enddo

 10   kz=k-1

C  Fold at nibble (2-symbol) length.
      nnib=kz/32
      call zero(y2,32)
      do i=1,32*nnib
         j=mod(i-1,32) + 1
         y2(j)=y2(j)+x(i)
      enddo

      ymax=-99.
      do i=1,32
         y2(i)=y2(i)/nnib
         if(y2(i).gt.ymax) then
            ymax=y2(i)
            ipk=i
         endif
         write(73,3003) i,y2(i)
 3003    format(i2,f10.6)
      enddo

C  Final downsampling, phased at symbol peaks
      nsym=2*nnib
      do i=1,nsym
         j=(i-1)*16 + ipk
         x(i)=x(j)
      enddo

C  Get median of lower half, then normalize
      call pctile(x,tmp,nsym,25,base)
      i50=nint(0.25*nsym)
      i84=nint(0.42*nsym)
      sigma=tmp(i84)-tmp(i50)
      do i=1,nsym
         x(i)=(x(i)-base)/sigma
      enddo

!      call msgparms(nbits,nparm,kz)

      qbest=-99.
      do k=1,kz
         ntype=nparm(1,k)
         nr1=nparm(2,k)
         nz1=nparm(3,k)
         nr2=nparm(4,k)
         nz2=nparm(5,k)
         call foldcw(x,nsym,ntype,nr1,nz1,nr2,nz2,f1,f2,qual,
     +        xm21,xm31,xm41,xm22,xm32,xm42)
         if(qual.gt.qbest) then
            kpk=k
            qbest=qual
         endif
      enddo

      ntype=nparm(1,kpk)
      nr1=nparm(2,kpk)
      nz1=nparm(3,kpk)
      nr2=nparm(4,kpk)
      nz2=nparm(5,kpk)
      rpt='   '
      if(ntype.eq.3) rpt='OOO'
      if(ntype.eq.2 .and. nz2.eq.28) rpt='RO '
      if(ntype.eq.2 .and. nz2.eq.34) rpt='RRR'
      if(ntype.eq.2 .and. nz2.eq.36) rpt='73 '
      
      call foldcw(x,nsym,ntype,nr1,nz1,nr2,nz2,f1,f2,qual,
     +     xm21,xm31,xm41,xm22,xm32,xm42)

      write(81,3200) nsym,nbits,ntype,nr1,nz1,nr2,nz2,qual,
     +     xm21,xm31,xm41,xm22,xm32,xm42
 3200 format(7i5/7f10.3)
      if(nz1.gt.0) write(81,3201) (f1(i),i=1,nz1)
 3201 format(10f8.3)
      if(nz2.gt.0) write(81,3201) (f2(i),i=1,nz2)

      write(*,3100) nbits,nsym,ntype,nr1,nz1,nr2,nz2,qual,xm41,xm42,rpt
 3100 format(7i5,3f7.2,2x,a3)

      write(80,3100) nbits,nsym,ntype,nr1,nz1,nr2,nz2,qual,xm41,xm42,rpt
      if(ntype.eq.2) then
         do i=1,nz2
            id(i)=0
            if(f2(i).gt.0.0) id(i)=1
         enddo
         write(80,3101) (id(i),i=1,nz2)
 3101    format(70i1)
      else
         do i=1,nz1
            id(i)=0
            if(f1(i).gt.0.0) id(i)=1
         enddo
         write(80,3101) (id(i),i=1,nz1)
         if(ntype.eq.3) then
            do i=1,nz1
               id(i)=0
               if(f2(i).gt.0.0) id(i)=1
            enddo
            write(80,3101) (id(i),i=1,nz2)
         endif
      endif

      return
      end
