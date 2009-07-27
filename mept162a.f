      subroutine mept162a(c2,jz,f0,cfile6,ndiag,minsync,mousedf,ndftol,
     +    ndwspr,ccfblue,ccfred)

C  Orchestrates the process of finding, synchronizing, and decoding 
C  WSPR signals.

      character*22 message
      character*8 cfile6
      real ccfblue(-5:540)
      real ccfred(-224:224)
      logical first
      real ps(-256:256)
      real sstf(5,275)
      real a(5)
      complex c2(jz)
      complex c2a(0:65535)
      complex c3(45000),c4(45000)
      data first/.true./
      save

C  Mix 1500 Hz +/- 100 Hz to baseband, and downsample by 1/32
!      call mix162(id,npts,c2,jz,ps)
      c2a(0:jz-1)=c2
      c2a(jz:)=0.
      nfft0=65536
      call fourt(c2a,65536,1,-1,1,0.0)
      do k=-256,256
         i=128*k
         sq=0.
         do n=-64,63
            j=i+n
            if(j.lt.0) j=j+nfft0
            sq=sq + real(c2a(j))**2 + aimag(c2a(j))**2
         enddo
         ps(k)=1.e-8*sq
      enddo

C  Compute pixmap.dat
!      call spec162(c2,jz)

C  Look for sync patterns, get DF and DT
      call sync162(c2,jz,mousedf,ndftol,ccfblue,ccfred,ps,sstf,kz)

      idtmax=64
      idtstep=1
      if(ndwspr.eq.0) then
         idtstep=8
      endif
      do k=1,kz
         snrsync=sstf(1,k)
         snrx=sstf(2,k)
         dtx=sstf(3,k)
         dfx=sstf(4,k)
         drift=sstf(5,k)
         ndf=nint(f0-1500.0+dfx)
         a(1)=-dfx
         a(2)=-0.5*drift
         a(3)=0.
         call twkfreq(c2,c3,jz,a)                    !Remove drift

         nsync=nint(snrsync-4.0)
         nsnrx=nint(snrx)
         if(nsnrx.lt.-33) nsnrx=-33
         if(nsync.lt.0) nsync=0
         message='                      '
         if(nsync.ge.minsync .and. nsnrx.ge.-30) then      !### -31 dB limit?
            dt=1.0/375
            do idt=0,idtmax,idtstep
               do isign=-1,1,2
                  ii=idt*isign
                  i1=nint((dtx+2.0)/dt) + ii !Start index for synced symbols
                  if(i1.ge.1) then
                     c4(1:jz-i1+1)=c3(i1:jz)
                     c4(jz-i1+2:)=0.
                  else
                     c4(:-i1+1)=0.
                     jzz=min(jz,45000)
                     c4(-i1+2:jzz)=c3(1:i1+jzz-1)
                     if(jzz.lt.45000) c4(jzz:45000)=0.
                  endif
                  call decode162(c4,45000,ndwspr,message,ncycles,metric,
     +                           nerr)
                  if(message(1:6).ne.'      ' .and.
     +               message(1:6).ne.'000AAA') go to 23
               enddo
            enddo

 23         nf1=nint(-a(2))
            if(ndiag.ne.0) then
               call cs_lock('mept162a')
               write(11,1012) cfile6,nsync,nsnrx,dtx,ndf,nf1,message,
     +              ii,ncycles/81
               write(21,1012) cfile6,nsync,nsnrx,dtx,ndf,nf1,message,
     +              ii,ncycles/81
            else
               write(11,1012) cfile6,nsync,nsnrx,dtx,ndf,nf1,message
               write(21,1012) cfile6,nsync,nsnrx,dtx,ndf,nf1,message
            endif
 1012       format(a6,i4,i4,f5.1,i6,i3,2x,a22,15x,i4,i6)
            i1=index(message,' ')
            call cs_unlock
         endif
      enddo

      return
      end
