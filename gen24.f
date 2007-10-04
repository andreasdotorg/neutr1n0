      subroutine gen24(message,mode,mode4,samfac,ntxdf,iwave,nwave,
     +    sendingsh,msgsent,nmsg)

C  Encodes a JT2 or JT4 message into a wavefile.

      parameter (NMAX=60*11025)     !Max length of wave file
      character*22 message          !Message to be generated
      character*22 msgsent          !Message as it will be received
      character*3 cok               !'   ' or 'OOO'
      character*6 mode
      real*8 t,dt,phi,f,f0,dfgen,dphi,pi,twopi,samfac,tsymbol
      integer*2 iwave(NMAX)         !Generated wave file
      integer sendingsh
      integer dgen(13)
      integer*1 data0(13),symbol(216)
      logical first
      include 'prcom2.f'
      common/tst99/ symbol
      data first/.true./
      save

      nsym=207                               !Symbols per transmission
      if(first) then
         do i=1,nsym
            pr2(i)=2*npr2(i)-1
         enddo
         pi=4.d0*atan(1.d0)
         twopi=2.d0*pi
         first=.false.
      endif

      call chkmsg(message,cok,nspecial,flip)
      call packmsg(message,dgen)  !Pack 72-bit message into 12 six-bit symbols
      call entail(dgen,data0)
      call unpackmsg(dgen,msgsent)
!      write(*,3001) (data0(i),i=1,9),(dgen(i),i=1,12)
! 3001 format('Gen24:   ',9(1x,z2),2x,12(1x,z2))
!      print*,msgsent
      nbytes=(72+31+7)/8
      call encode(data0,nbytes,symbol(2))    !Convolutional encoding
      symbol(1)=0                            !Reference phase
      sendingsh=0
      if(iand(dgen(10),8).ne.0) sendingsh=-1 !Plain text flag
      call interleave24(symbol(2),1)         !Apply JT2/JT4 interleaving

C  Set up necessary constants
      tsymbol=2520.d0/11025.d0
      dt=1.0/(samfac*11025.0)
      f0=118*11025.d0/1024 + ntxdf
      dfgen=11025.0/2520                     !4.375 Hz
      t=0.d0
      phi=0.d0
      j0=0
      ndata=(nsym*11025.d0*samfac*tsymbol)/2
      ndata=2*ndata
      if(mode(1:3).eq.'JT2') then                 !JT2 mode
         ss=1.0
         s=0.0
         u=0.04
         do i=1,ndata
            t=t+dt
            j=int(t/tsymbol) + 1                  !Symbol number, 1-207
            if(j.ne.j0) then
               f=f0 + npr2(j)*dfgen
               if(flip.lt.0.0) f=f0 + (1-npr2(j))*dfgen
               dphi=twopi*dt*f
               if(symbol(j).gt.0) ss=-ss
               j0=j
            endif
            phi=phi+dphi
            s=s + u*(ss-s)
            iwave(i)=32767.0 * s * sin(phi)
         enddo
      else                                        !JT4x mode
         do i=1,ndata
            t=t+dt
            j=int(t/tsymbol) + 1                  !Symbol number, 1-207
            if(j.ne.j0) then
               f=f0 + (npr2(j)+2*symbol(j)-1.5) * dfgen * mode4
               if(flip.lt.0.0) f=f0+((1-npr2(j))+2*symbol(j)-1.5)*dfgen
               dphi=twopi*dt*f
               j0=j
            endif
            phi=phi+dphi
            iwave(i)=32767.0*sin(phi)
         enddo
      endif

      do j=1,5512                !Put another 0.5 sec of silence at end
         i=i+1
         iwave(i)=0
      enddo
      nwave=i

      if(flip.lt.0.0) then
         do i=22,1,-1
            if(msgsent(i:i).ne.' ') goto 10
         enddo
 10      msgsent=msgsent(1:i)//' OOO'
      endif
      do i=22,1,-1
         if(msgsent(i:i).ne.' ') goto 20
      enddo
 20   nmsg=i

!      write(*,3002) (symbol(i),i=1,207)
! 3002 format(70i1)

      return
      end

