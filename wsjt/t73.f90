program t73

! Tests experimental JT6M decoder

  parameter (NMAX=512*1024)
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  complex cdat(NMAX)                      !Analytic form of signal
  complex z
  character arg*12                        !Command-line argument
  character cfile6*6                      !File time
  character frag*28                       !Message fragment to be matched
!  character msg*40
!  complex cfrag(2100)                     !Complex waveform of message fragment
  real s(NMAX)
  real sym(646)
  real fsym(0:2)
  real ccf(-20000:20000)
  integer dftolerance
  real*8 twopi8,dt8,f08
  common/scratch/work(NMAX)

  nargs=iargc()
  if(nargs.ne.2) then
     print*,'Usage: t73 nfile frag'
     go to 999
  endif
  call getarg(1,arg)
  read(arg,*) nfile
  call getarg(2,frag)
  open(73,file='dat.73',form='unformatted',status='old')

  do i=28,1,-1                          !Get length of fragment
     if(frag(i:i).ne.' ') go to 10
  enddo
10 nfrag=i

  nsps=512
  twopi8=8.d0*atan(1.d0)
  dt8=1.d0/11025.d0
  f08=50*11025.d0/nsps
  f0=f08
  dftolerance=40
  ipk=0
  
  do ifile=1,nfile
     read(73,end=999) jz,nz,cfile6,(dat(j),j=1,jz)
     if(ifile.ne.nfile .and. nfile.ne.999) go to 900

!     do i=1,jz
!        j=(i-1)/512
!        dat(i)=0.
!        if(mod(j,3).eq.1) dat(i)=sin(twopi8*i*dt8*f08)
!     enddo

     npts=jz
     xn=log(float(npts))/log(2.0)
     n=xn
     if(xn-n .gt.0.001) n=n+1
     nfft1=2**n
     nh1=nfft1/2
     nq1=nfft1/4
     df1=11025.0/nfft1
     ia=nint(11025.0/(3.0*nsps*df1))
     ib=nint(dftolerance/df1)
     i0=nint(f0/df1)

     call analytic(dat,npts,nfft1,s,cdat)        !Convert to analytic signal

     do i=-ib,ib
        ccf(i)=0.46*s(i0+i) + 0.27*(s(i0+i-ia)+s(i0+i+ia))
     enddo

     nadd=nint(1.0/df1)
     call smo(ccf(-ib),2*ib+1,work,nadd)

     ccfmax=0.
     do i=-ib,ib
        if(ccf(i).gt.ccfmax) then
           ccfmax=ccf(i)
           ipk=i
        endif
        freq=i*df1
        write(13,3001) i,freq,1000.0*ccf(i)
3001    format(i8,2f12.3)
     enddo

     dfx=ipk*df1

! DF is known, now find symbol sync.
     call tweak1(cdat,npts,-f0-dfx,cdat)

     nrpt=npts/1536-1
     ymax=0.
     jpk=0
     do j=0,1535
        y=0.
        do n=0,nrpt-1
           k=n*1536 + j
           z=0.
           do i=1,512
              k=k+1
              z=z+cdat(k)
           enddo
           y=y + abs(z)
        enddo
        write(14,3002) j,y
3002    format(i8,f12.3)
        if(y.gt.ymax) then
           ymax=y
           i1=j
        endif
     enddo

! OK, we have DF and starting sample, i1.  Compute symbol amplitudes.
        nsym=(npts-i1+1)/(3*512)
        nsym=3*nsym
        fsym=0.
        k=i1-1
        do j=1,nsym
           z=0.
           do i=1,512
              k=k+1
              z=z+cdat(k)
           enddo
           sym(j)=abs(z)
           i=mod(j-1,3)
           fsym(i)=fsym(i)+sym(j)
           write(16,3005) j,sym(j)
3005       format(i4,f12.3)
        enddo
        fsmin=min(fsym(0),fsym(1),fsym(2))
        print*,fsym/fsmin

     print*,ifile,f0,dfx,jpk

900  continue
  enddo

999 end program t73
