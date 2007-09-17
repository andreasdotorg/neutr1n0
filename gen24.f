      subroutine gen24(message,mode,samfac,ntxdf,iwave,nwave,
     +    sendingsh,msgsent,nmsg)

C  Encodes a JT2 or JT4 message into a wavefile.

      parameter (NMAX=60*11025)     !Max length of wave file
      character*22 message          !Message to be generated
      character*22 msgsent          !Message as it will be received
      character*3 cok               !'   ' or 'OOO'
      character*6 mode
      real*8 t,dt,phi,f,f0,dfgen,dphi,twopi,samfac,tsymbol
      integer*2 iwave(NMAX)         !Generated wave file
      integer sendingsh
      integer dgen(13)
      integer*1 data0(13),symbol(208)
      logical first
      include 'prcom2.f'
      data twopi/6.283185307d0/,first/.true./
      save

      mode4=10
      nsym=207                               !Symbols per transmission
      if(first) then
         do i=1,nsym
            pr2(i)=2*npr2(i)-1
         enddo
         first=.false.
      endif

      call chkmsg(message,cok,nspecial,flip)
      call packmsg(message,dgen)  !Pack 72-bit message into 12 six-bit symbols
      call entail(dgen,data0)
      nbytes=(72+31+7)/8
      call encode(dgen,nbytes,symbol)        !Convolutional encoding
      sendingsh=0
      if(iand(dgen(10),8).ne.0) sendingsh=-1 !Plain text flag
C###      call interleave63(sent,1) !Apply interleaving
      tsymbol=2520.d0/11025.d0

C  Set up necessary constants
      dt=1.0/(samfac*11025.0)
      f0=118*11025.d0/1024 + ntxdf
      dfgen=11025.0/2520                     !4.375 Hz
      t=0.d0
      phi=0.d0
      j0=0
      ndata=(nsym*11025.d0*samfac*tsymbol)/2
      ndata=2*ndata
      if(mode(1:3).eq.'JT2') then
         do i=1,ndata
            t=t+dt
            j=int(t/tsymbol) + 1                  !Symbol number, 1-207
            if(j.ne.j0) then
               f=f0 + npr2(j)*dfgen
               if(flip.lt.0.0) f=f0 + (1-npr2(j))*dfgen
               dphi=twopi*dt*f
               sig=1.0
               if(symbol(j).gt.0) sig=-1.0
               j0=j
            endif
            phi=phi+dphi
            iwave(i)=32767.0*sin(phi)*sig
         enddo
      else
         do i=1,ndata
            t=t+dt
            j=int(t/tsymbol) + 1                  !Symbol number, 1-207
            if(j.ne.j0) then
               f=f0 + (npr2(j) + 2*symbol(j)) * dfgen * mode4
               if(flip.lt.0.0) f=f0 + ((1-npr2(j)) + 2*symbol(j))*dfgen
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
      call unpackmsg(dgen,msgsent)
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

      return
      end

