subroutine chk441(dat,jz,tstart,width,nfreeze,mousedf,             &
     dftolerance,pick,nok)

! Experimental FSK441 decoder

  parameter (NMAX=512*1024)
  parameter (MAXFFT=8192)
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  logical pick
  complex cdat(NMAX)                      !Analytic form of signal
  character frag*28,frag0*29              !Message fragment to be matched
  complex cfrag(2100)                     !Complex waveform of message fragment
  complex z
  integer itone(84)                       !Generated tones for msg fragment
  real s(NMAX)
  real ccf(-6000:6000)
  integer dftolerance
  common/scratch/work(NMAX)
  save frag0,cfrag,ndits

  frag=' '
  if(frag.ne.frag0) then
     do i=28,1,-1                          !Get length of message fragment
        if(frag(i:i).ne.' ') go to 10
     enddo
10   nfrag=i
     if(nfrag.eq.0) nfrag=1
     call abc441(frag,nfrag,itone,ndits)
     call gen441(itone,ndits,cfrag)        !Generate complex waveform
     frag0=frag
  endif

  ccf0=3.0
  sb0=0.75
  if(pick) then
     ccf0=2.1
     sb0=0.60
  endif


  nsps=25                                  !Initialize variables
  nsam=nsps*ndits
  dt=1.0/11025.0
  i0=(tstart-0.02)/dt
  if(i0.lt.1) i0=1
  npts=nint((width+0.02)/dt)+1
  npts=min(npts,jz+1-i0)
  npts=min(npts,22050)                     !Max ping length 2 s
  xn=log(float(npts))/log(2.0)
  n=xn
  if(xn-n .gt.0.001) n=n+1
  nfft1=2**n
  df1=11025.0/nfft1
  nok=0
  sbest=0.

  call analytic(dat(i0),npts,nfft1,s,cdat)    !Convert to analytic signal

!  call len441(cdat,npts,lenacf,nacf)          !Do ACF to find message length

  ia=nint(dftolerance/df1)
  i0=0
  if(nfreeze.ne.0) i0=nint(mousedf/df1)
  ccfmax=0.
  do i=-ia,ia                                 !Find DF
     ccf(i)=s(i0+i+nint(882.0/df1)) + s(i0+i+nint(1323.0/df1)) +        &
          s(i0+i+nint(1764.0/df1)) + s(i0+i+nint(2205.0/df1))
  enddo
  ccf(:-ia-1)=0.
  ccf(ia+1:)=0.
  nadd=2*nint(5.0/df1)+1
  call smo(ccf(-ia),2*ia+1,work,nadd)         !Smooth CCF by nadd

  do i=-ia,ia                                 !Find max of smoothed CCF
     if(ccf(i).gt.ccfmax) then
        ccfmax=ccf(i)
        ipk=i0+i
        dfx=ipk*df1
     endif
  enddo

  ic=min(nint(220/df1),ia)                    !Baseline range +/- 220 Hz
  call pctile(ccf(ipk-ic),work,2*ic+1,50,base)
  ccfmax=ccfmax/base
  if(ccfmax.lt.ccf0) go to 800                !Reject non-FSK441 signals

! We seem to have an FSK441 ping, and we know DF; now find DT.
  call tweak1(cdat,npts,-dfx,cdat)            !Mix to standard frequency

! Look for best match to "frag", find its DT
  do i=1,npts-nsam
     z=0.
     a=0.
     do j=1,nsam
        a=a + abs(cdat(j+i-1))
        z=z + cdat(j+i-1)*cfrag(j)
     enddo
     ss=abs(z)/a
!     ss=abs(z)
     if(ss.gt.sbest) then
        sbest=ss
     endif
  enddo
  if(sbest.lt.sb0) go to 800            !Skip if not decodable FSK441 data
  nok=1

800 continue
  return
end subroutine chk441
