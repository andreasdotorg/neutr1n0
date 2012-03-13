program techo

  parameter (NDZ=28672)
  parameter (NSYNC=24030)
  parameter (LAGMAX=NDZ-NSYNC)
  parameter (NFFT=32768,NH=NFFT/2)

  real d(NDZ)
  complex cd(NFFT)
  complex c(NFFT)
  complex csync(NSYNC)
  real s(NH)
  real s2(NFFT)
  character infile*40,arg*12
  character*24 fname
  real*8 dt,pha,dpha,twopi,f,df
  integer ic27(27)
  data ic27/1,3,7,15,2,5,11,23,18,8,17,6,13,27,26,24,20,12,25,22,   &
       16,4,9,19,10,21,14/

  nargs=iargc()
  if(nargs.ne.3) then
     print*,'Usage: techo <infile> nrec nw'
     go to 999
  endif

  call getarg(1,infile)
  call getarg(2,arg)
  read(arg,*) nrec
  call getarg(3,arg)
  read(arg,*) nw

  twopi=8*atan(1.d0)
  dt=1.d0/11025.d0
  df=11025.d0/890.d0

  pha=0.d0
  f=1500.d0
  k=0
  do j=1,27
     if(nw.ne.0) f=1500.d0 + (ic27(j)-14)*df
     dpha=twopi*f*dt
     do i=1,890
        pha=pha+dpha
        k=k+1
        csync(k)=cmplx(cos(pha),-sin(pha))
     enddo
  enddo

  open(26,file=infile,form='unformatted',status='old')

  npts=NDZ
  df1=11025.0/NFFT
  fac=1.e-4

  do irec=1,999
     read(26,end=999) fname,ntime,dop0,doppler,d
     if(irec.lt.nrec) cycle
     if(irec.gt.nrec) go to 999
     print*,fname,ntime,dop0,doppler,irec,nrec
     d=fac+d
     call analytic(d,npts,NFFT,s,cd)
     cd(NDZ+1:)=0.

     sbest=0.
     do lag=0,LAGMAX,10
        do i=1,NSYNC
           c(i)=fac*cd(i+lag)*csync(i)
        enddo
        c(NSYNC+1:)=0.
        call four2a(c,NFFT,1,-1,1)
        smax=0.
        do i=1,NFFT
           s2(i)=real(c(i))**2 + aimag(c(i))**2
           if(s2(i).gt.smax) then
              smax=s2(i)
              ipk=i
           endif
        enddo
        write(14,3002) lag,smax
3002    format(i6,e15.3)
        if(smax.gt.sbest) then
           sbest=smax
           ibest=ipk
           lagbest=lag
           rewind 13
           do i=1,NFFT
              f=(i-1)*df1
              if(i.gt.NFFT/2) f=(i-NFFT-1)*df1
              if(abs(f).lt.200.0) write(13,3001) f,s2(i),db(s2(i))
3001          format(3f12.3)
           enddo
        endif
     enddo
  call flush(13)
  call flush(14)
  fbest=(ibest-1)*df1
  if(ibest.gt.NH+1)   fbest=(ibest-1-NFFT)*df1
  print*,lagbest,fbest,sbest
  enddo

999 end program techo
