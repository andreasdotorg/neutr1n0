      subroutine gencw(msg,wpm0,freqcw,samfac,TRPeriod,iwave,nwave)

C  Generates array iwave() containing an audio signal corresponding
C  to an EME CW message.

      parameter (NMAX=150*11025)
      character*22 msg,word12,word3,s1,s2,s3
      integer*2 iwave(NMAX)
      integer TRPeriod
      integer*1 idat(5000),idat1(460),idat2(200),idat3(200)
      real*8 dt,t,twopi,pha,dpha,tdit,samfac
      data twopi/6.283185307d0/

      call msgtype(msg,ntype,nrpt1,nrpt2,s1,s2)
      call morse(s1,idat1,nz1)         !Encode string 1
      nz2=0
      if(ntype.ge.2) call morse(s2,idat2,nz2)   !Encode string 2
      s3='KK'
      call morse(s3,idat3,nz3)         !Encode 'KK'

      b4=58.0*wpm0/1.2
      b3=0.75*b4
      b1=0.25*b4

      nr1=1
      nr2=0
      if(nrpt1.eq.100) then
         nr1=nint((b4-nz3)/nz1)
      else if(nrpt1.eq.1) then
         nr2=nint((b4-nz1-nz3)/nz2)
      else if(nrpt1.eq.75) then
         nr1=nint(b3/nz1)
         nr2=nint((b4-nr1*nz1-nz3)/nz2)
      endif
      nbits=nr1*nz1 + nr2*nz2 + nz3
      
      j=0
      do n=1,nr1
         do i=1,nz1
            j=j+1
            idat(j)=idat1(i)
         enddo
      enddo
      if(nr2.gt.0) then
         do n=1,nr2
            do i=1,nz2
               j=j+1
               idat(j)=idat2(i)
            enddo
         enddo
      endif

      do i=1,nz3
         j=j+1
         idat(j)=idat3(i)
      enddo
      jz=j
      do i=jz+1,5000
         idat(j)=0
      enddo

      wpm=wpm0 * nbits/b4
      tdit=1.2d0/wpm                   !Key-down dit time, seconds
      dt=1.d0/(11025.d0*samfac)
!      write(*,3001) msg,ntype,nr1,nz1,nr2,nz2,nz3,nbits,wpm,b4,1.d6*dt
! 3001 format(a22,i3,6i5,f7.2,f8.2,f9.3)
      nwave=jz*tdit/dt
      pha=0.
      dpha=twopi*freqcw*dt
      t=0.
      s=0.
      u=wpm/(11025*0.03)
      j0=1
      nsign=1
      do i=1,nwave
         t=t+dt
         pha=pha+dpha
         j=nint(t/tdit) + 1
         s=s + u*(idat(j)-s)
         if(idat(j0).eq.0 .and. idat(j).ne.0) nsign=-nsign
         iwave(i)=nsign*nint(s*32767.d0*sin(pha))
         j0=j
      enddo
      do i=nwave+1,NMAX
         iwave(i)=0
      enddo
      nwave=nwave+11025

      return
      end

