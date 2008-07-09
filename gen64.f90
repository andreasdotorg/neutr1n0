subroutine gen64(message,mode64,samfac,ntxdf,iwave,nwave,  &
     sendingsh,msgsent,nmsg)

! Encodes a JT64 message into a wavefile.

  parameter (NMAX=60*11025)     !Max length of wave file
  character*22 message          !Message to be generated
  character*22 msgsent          !Message as it will be received
  real*8 t,dt,phi,f,f0,dfgen,dphi,twopi,samfac,tsymbol
  integer*2 iwave(NMAX)  !Generated wave file
  integer sent(63)
  integer sendingsh
  integer ic6(6)
  integer isync(63)
  data ic6/0,1,4,3,5,2/,idum/-1/
  data twopi/6.283185307d0/
  save

! Set up the JT64 sync pattern
! Insert the 6x6 Costas array 3 times at low-frequency edge.
  isync=-1
  do n=1,3
     i0=11
     if(n.eq.2) i0=28
     if(n.eq.3) i0=46
     do i=1,6
        isync(i0+i)=ic6(i)
     enddo
  enddo

  nspecial=0
  if(nspecial.eq.0) then
!         call wqencode(message,ntype,data0)

! Must do rs_init for RS(45,9)
!         call rs_encode(dgen,sent)

! Temporary: correct sync plus random data
     do i=1,63
        if(isync(i).lt.0) then
           call random_number(x)
           sent(i)=63.99999*x
        else
           sent(i)=isync(i)
        endif
     enddo

     tsymbol=8192.d0/11025.d0
     nsym=63                            !Symbols per transmission
  else
     tsymbol=16384.d0/11025.d0
     nsym=32
     sendingsh=1                         !Flag for shorthand message
  endif

! Set up necessary constants
  dt=1.0/(samfac*11025.0)
  f0=118*11025.d0/1024 + ntxdf
  dfgen=mode64*11025.0/4096.0
  t=0.d0
  phi=0.d0
  k=0
  j0=0
  ndata=(nsym*11025.d0*samfac*tsymbol)/2
  ndata=2*ndata
  do i=1,ndata
     t=t+dt
     j=int(t/tsymbol) + 1                    !Symbol number, 1-63
     if(j.ne.j0) then
        f=f0
        if(nspecial.ne.0) f=f0+10*nspecial*dfgen
        if(nspecial.eq.0) then
           k=k+1
           f=f0+(sent(k))*dfgen
        endif
        dphi=twopi*dt*f
        j0=j
     endif
     phi=phi+dphi
     iwave(i)=32767.0*sin(phi)
  enddo

  do j=1,5512                !Put another 0.5 sec of silence at end
     i=i+1
     iwave(i)=0
  enddo
  nwave=i
!      call unpackmsg(dgen,msgsent)
!      if(flip.lt.0.0) then
!         do i=22,1,-1
!            if(msgsent(i:i).ne.' ') goto 10
!         enddo
! 10      msgsent=msgsent(1:i)//' OOO'
!      endif
  msgsent=message
  do i=22,1,-1
     if(msgsent(i:i).ne.' ') goto 20
  enddo
20 nmsg=i

  return
end subroutine gen64

