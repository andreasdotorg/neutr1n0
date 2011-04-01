program t75

! Tests experimental ISCAT decoder

  parameter (NMAX=512*1024)
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  character arg*12                        !Command-line argument
  character cfile6*6                      !File time
  character msg*28,msgsent*28
  character*40 infile
  real ccfblue(-5:540)                        !blue line
  integer dftolerance
  real ccf(-5:540)        !X-cor function in JT65 mode (blue line)
  real psavg(450)         !Average spectrum of the whole file
  integer*2 iwave(NMAX)
  real x(12)

  nargs=iargc()
  if(nargs.ne.2) then
     print*,'Usage: t75 infile nrec'
     go to 999
  endif
  call getarg(1,infile)
  call getarg(2,arg)
  read(arg,*) nrec
  open(74,file=infile,form='unformatted',status='old')

  MinSigdB=-20
  DFTolerance=400
  NFreeze=0
  MouseDF=0
  mousebutton=0
  mode4=2

  if(nrec.le.0) then
     msg='W8WN K1JT'
     nmsg=9
     call geniscat(msg,nmsg,mode4,1.d0,iwave,jz,msgsent)
     jz=jz/2                                               !15-sec 
     do i=1,jz
        call random_number(x)
        xs=sum(x)-6.0
        dat(i)=100.0*(0.1*iwave(i)/32768.0 + xs)
     enddo
     call iscat(dat,jz,cfile6,MinSigdB,DFTolerance,     &
          NFreeze,MouseDF,mousebutton,mode4,ccf,psavg)
  else
     do irec=1,nrec
        read(74,end=999) jz,cfile6,(dat(j),j=1,jz)
        if(irec.ne.nrec .and. nrec.ne.999) cycle
        call iscat(dat,jz,cfile6,MinSigdB,DFTolerance,     &
             NFreeze,MouseDF,mousebutton,mode4,ccf,psavg)
     enddo
  endif

999 end program t75
