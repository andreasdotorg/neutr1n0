program t74

! Tests experimental Diana decoder

  parameter (NMAX=512*1024)
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  character arg*12                        !Command-line argument
  character cfile6*6                      !File time
  character msg*28
  character*40 infile
  real ccfblue(-5:540)                        !blue line
  integer dftolerance

  nargs=iargc()
  if(nargs.ne.2) then
     print*,'Usage: t74 infile nrec'
     go to 999
  endif
  call getarg(1,infile)
  call getarg(2,arg)
  read(arg,*) nrec
  open(74,file=infile,form='unformatted',status='old')

  MinSigdB=0
  DFTolerance=400
  NFreeze=1
  MouseDF=0

  do irec=1,nrec
     read(74,end=999) jz,cfile6,(dat(j),j=1,jz)
     if(irec.ne.nrec .and. nrec.ne.999) go to 900

     call diana(dat,jz,cfile6,MinSigdB,DFTolerance,NFreeze,      &
          MouseDF,ccfblue,ccfred)
900  continue
  enddo

999 end program t74
