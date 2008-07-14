subroutine genwspr(message,samfacout,ntxdf,iwave,nwave,nbad,msgsent)

!  Encode a WSPR message and generate the corresponding wavefile.

  parameter (NMAX=120*12000)     !Max length of wave file
  parameter (MAXSYM=176)
  character*22 message           !Message to be generated
  integer*2 iwave(NMAX)          !Generated wave file
  real*8 samfacout,fsample
  integer*1 symbol(MAXSYM)
  integer*1 data0(11)
  integer npr3(162)
  logical first
  real*8 t,dt,phi,f,f0,dfgen,dphi,pi,twopi,tsymbol
  character*22 msgsent           !Message sent
  data npr3/                                    &
       1,1,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0, &
       0,1,0,1,1,1,1,0,0,0,0,0,0,0,1,0,0,1,0,1, &
       0,0,0,0,0,0,1,0,1,1,0,0,1,1,0,1,0,0,0,1, &
       1,0,1,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0,0,1, &
       0,0,1,0,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,0, &
       0,0,0,0,1,0,0,1,0,0,1,1,1,0,1,1,0,0,1,1, &
       0,1,0,0,0,1,1,1,0,0,0,0,0,1,0,1,0,0,1,1, &
       0,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,1,1,0, &
       0,0/

  data first/.true./,idum/0/
  save

  if(first) then
     pi=4.d0*atan(1.d0)
     twopi=2.d0*pi
     fsample=11025.d0
     first=.false.
  endif

  call wqencode(message,ntype,data0)
  nbytes=(50+31+7)/8
  call encode232(data0,nbytes,symbol,MAXSYM)  !Convolutional encoding
  call inter_mept(symbol,1)                   !Apply interleaving
  call wqdecode(data0,msgsent,ntype2)
  nbad=0
  if(ntype2.ne.ntype .or. msgsent.ne.message) nbad=-1
  if(ntype2.eq.ntype .and. index(msgsent,'<...>').gt.0) then
     nbad=0
     msgsent=message
  endif

  tsymbol=8192.d0/12000.d0
  
  dt=1.d0/(fsample*samfacout)
  f0=1500 + ntxdf
  dfgen=12000.d0/8192.d0                     !1.4649 Hz
  nsigs=1
  snrdb=99.
  if(snrdb.eq.10.0) nsigs=10
  nwave=111*fsample
  if(snrdb.le.10.0) nwave=NMAX

  do isig=1,nsigs
     if(nsigs.eq.1) snr=10.0**(0.05*(snrdb-1))   !Bandwidth correction?
     fac=3000.0
     if(snr.gt.1.0) fac=3000.0/snr
     if(nsigs.eq.10) then
        snr=10.0**(0.05*(-20-isig-1))
        f0=1390 + 20*isig
     endif
!     t=-2.d0 - 0.1*(isig-1)
     t=-0.2d0
     phi=0.d0
     j0=0

     do i=1,nwave
        t=t+dt
        j=int(t/tsymbol) + 1                          !Symbol number
        sig=0.
        if(j.ge.1 .and. j.le.162) then
           if(j.ne.j0) then
              f=f0 + dfgen*(npr3(j)+2*symbol(j)-1.5)
              j0=j
              dphi=twopi*dt*f
           endif
           sig=0.9999
        endif
        phi=phi+dphi
        if(snrdb.gt.50.0) then
           n=32767.0*sin(phi)           !Normal transmission, signal only
        else
           if(isig.eq.1) then
              n=fac*(gran(idum) + sig*snr*sin(phi))
           else
              n=iwave(i) + fac*sig*snr*sin(phi)
           endif
           if(n.gt.32767) n=32767
           if(n.lt.-32767) n=-32767
        endif
        iwave(i)=n
     enddo
  enddo
  return
end subroutine genwspr
