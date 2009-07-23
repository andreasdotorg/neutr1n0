! Fortran logical units used in WSJT6
!
!   10  wave files read from disk
!   11  decoded.txt
!   12  decoded.ave
!   13  tsky.dat
!   14  azel.dat
!   15  debug.txt
!   16  c:/wsjt.reg 
!   17  wave files written to disk
!   18  test file to be transmitted (wsjtgen.f90)
!   19  dmet_* files
!   20  prefixes.txt
!   21  ALL.TXT
!   22  kvasd.dat
!   23  CALL3.TXT

!------------------------------------------------ ftn_init
subroutine ftn_init

  character*1 cjunk
  include 'gcom1.f90'
  include 'gcom2.f90'
  include 'gcom3.f90'
  include 'gcom4.f90'
  character*12 csub0
  common/mtxcom/ltrace,mtx,mtxstate,csub0

  call cs_init
  call cs_lock('ftn_init')
  i=ptt(nport,pttport,0,iptt)                       !Clear the PTT line
  addpfx='    '

  do i=80,1,-1
     if(AppDir(i:i).ne.' ') goto 1
  enddo
1 iz=i
  lenappdir=iz
  call pfxdump(appdir(:iz)//'/prefixes.txt')

  do i=80,1,-1
     if(AzElDir(i:i).ne.' ') goto 2
  enddo
2 iz2=i

#ifdef CVF
  open(11,file=appdir(:iz)//'/decoded.txt',status='unknown',               &
       share='denynone',err=910)
#else
  open(11,file=appdir(:iz)//'/decoded.txt',status='unknown',               &
       err=910)
#endif
  endfile 11

#ifdef CVF
  open(12,file=appdir(:iz)//'/decoded.ave',status='unknown',               &
       share='denynone',err=920)
#else
  open(12,file=appdir(:iz)//'/decoded.ave',status='unknown',               &
       err=920)
#endif
  endfile 12

#ifdef CVF
  open(14,file=appdir(:iz)//'/azel.dat',status='unknown',                 &
       share='denynone',err=930)
#else
  open(14,file=appdir(:iz)//'/azel.dat',status='unknown',                 &
       err=930)
#endif

#ifdef CVF
  open(15,file=appdir(:iz)//'/debug.txt',status='unknown',                 &
       share='denynone',err=940)
#else
  open(15,file=appdir(:iz)//'/debug.txt',status='unknown',                 &
       err=940)
#endif

#ifdef CVF
  open(21,file=appdir(:iz)//'/ALL.TXT',status='unknown',                   &
       access='append',share='denynone',err=950)
#else
  open(21,file=appdir(:iz)//'/ALL.TXT',status='unknown',                   &
       position='append',err=950)
#endif

#ifdef CVF
  open(22,file=appdir(:iz)//'/kvasd.dat',access='direct',recl=1024,        &
       status='unknown',share='denynone')
#else
  open(22,file=appdir(:iz)//'/kvasd.dat',access='direct',recl=1024,        &
       status='unknown')
#endif

  call cs_unlock
  return

910 print*,'Error opening DECODED.TXT'
  stop
920 print*,'Error opening DECODED.AVE'
  stop
930 print*,'Error opening azel.dat'
  stop
940 print*,'Error opening DEBUG.TXT'
  stop
950 print*,'Error opening ALL.TXT'
  stop

end subroutine ftn_init
