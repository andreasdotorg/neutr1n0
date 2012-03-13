subroutine ping441(dat,jz,nz,MinSigdB,MinWidth,pick,pingdat,nping)

! Decode Multi-Tone FSK441 mesages.

  real dat(jz)                !Raw audio data
  logical pick

  real sigdb(3100)             !Detected signal in dB, sampled at 20 ms
  real work(3100)
  integer indx(3100)
  real pingdat(3,100)

  slim=MinSigdB
  wmin=0.001*MinWidth * (19.95/20.0)
  dt=1.0/11025.0

! Find signal power at suitable intervals to search for pings.
  istep=221
  dtbuf=istep/11025.
  do n=1,nz
     s=0.
     ib=n*istep
     ia=ib-istep+1
     do i=ia,ib
        s=s+dat(i)**2
     enddo
     sigdb(n)=s/istep
  enddo

!#####################################################################
  if(.not.pick) then
! Remove initial transient from sigdb
     call indexx(nz,sigdb,indx)
     imax=0
     do i=1,50
        if(indx(i).gt.50) go to 10
        imax=max(imax,indx(i))
     enddo
10   do i=1,50
        if(indx(nz+1-i).gt.50) go to 20
        imax=max(imax,indx(nz+1-i))
     enddo
20   imax=imax+6            !Safety margin
     base1=sigdb(indx(nz/2))
     do i=1,imax
        sigdb(i)=base1
     enddo
  endif
!##################################################################

  call smooth(sigdb,nz)

! Remove baseline and one dB for good measure.
  call pctile (sigdb,work,nz,50,base1)
  do i=1,nz
     sigdb(i)=dB(sigdb(i)/base1) - 1.0
  enddo

  call ping(sigdb,nz,dtbuf,slim,wmin,pingdat,nping)

  return
end subroutine ping441
