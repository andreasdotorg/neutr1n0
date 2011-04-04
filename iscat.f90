subroutine iscat(dat,npts0,cfile6,MinSigdB,DFTolerance,NFreeze,MouseDF,    &
     mousebutton,mode4,nafc,psavg)

! Decode an ISCAT signal

  parameter (NMAX=34*11025)
  parameter (NSZ=4*1400)
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  complex cdat(368640)
  complex cdat0(368640)
  character cfile6*6                      !File time
  character c42*42
  character msg*29,msg1*29
  character csync*1
  real x(NSZ),x2(NSZ)
  complex c(288)
  real s0(288,NSZ)
  real fs0(288,108)                        !108 = 96 + 3*4
  real fs1(0:41,30)
  real savg(288)
  real b(288)
  real ccf(1:96)
  real psavg(72)                          !Average spectrum of whole file
  integer dftolerance
  integer icos(4)
  data icos/0,1,3,2/
  data nsync/4/,nlen/2/,ndat/18/
  data c42/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ /.?@-'/

  nfft1=184320
  if(npts0.gt.nfft1) nfft1=2*nfft1
  nfft2=9*nfft1/32
  fac=2.0/nfft1
  do i=1,npts0/2
     cdat(i)=fac*cmplx(dat(2*i-1),dat(2*i))
  enddo
  cdat(npts0/2+1:nfft1/2)=0.
  call four2a(cdat,nfft1,1,-1,0)               !Forward r2c FFT
  call four2a(cdat,nfft2,1,1,1)                !Inverse c2c FFT
! Now cdat() is the downsampled analytic signal.  
! New sample rate = fsample = BW = 11025 * (9/32) = 3100.78125 Hz
! NB: npts, nsps, etc., are all reduced by 9/32

  npts=npts0*9.0/32.0
  fsample=3100.78125
  nsps=144/mode4
  nsym=npts/nsps
  nblk=nsync+nlen+ndat
  nfft=2*nsps                          !FFTs at twice the symbol length
  kstep=nsps/4                         !Step by 1/4 symbol
  df=fsample/nfft
  fac=1.0/1000.0                       !Somewhat arbitrary
  savg=0.
  cdat0(:npts)=cdat(:npts)

!  do idf1=-3,3
!     f0=0.
!     f1=5.0*idf1
!     call tweak2(cdat0,npts,fsample,f0,f1,cdat)

  ia=1-kstep
  do j=1,4*nsym                                   !Compute symbol spectra
     ia=ia+kstep
     ib=ia+nsps-1
     if(ib.gt.npts) exit
     c(1:nsps)=fac*cdat(ia:ib)
     c(nsps+1:nfft)=0.
     call four2a(c,nfft,1,-1,1)
     do i=1,nfft
        s0(i,j)=real(c(i))**2 + aimag(c(i))**2
        savg(i)=savg(i) + s0(i,j)                 !Also avg specrtum
     enddo
  enddo

  jsym=j-1
  jh=jsym/2
  savg=savg/jsym
  do i=1,nfft
     x(1:jsym)=s0(i,1:jsym)
     call pctile(x,x2,jsym,30,b(i))           !Baseline level for each freq bin
  enddo
  b(1:10)=b(11)

  do i=1,71                                   !Compute spectrum in dB, for plot
     if(mode4.eq.1) then
        psavg(i)=2*db(savg(4*i)+savg(4*i-1)+savg(4*i-2)+savg(4*i-3)) + 1.0
     else
        psavg(i)=2*db(savg(2*i)+savg(2*i-1)) + 7.0
     endif
  enddo

!### Not sure about this ? ###
  do j=1,jsym                                    !Normalize the symbol spectra
     s0(1:nfft,j)=s0(1:nfft,j)/b(1:nfft)
  enddo

  fs0=0.
  jb=(jsym-4*nblk+1)/4
  jb=4*jb
  do j=1,jb                                  !Fold s0 into fs0, modulo 4*nblk 
     k=mod(j-1,4*nblk)+1
     do i=1,nfft
        fs0(i,k)=fs0(i,k) + s0(i,j)
     enddo
  enddo

  do j=1,12
     fs0(1:nfft,96+j)=fs0(1:nfft,j)
  enddo

  i0=27
  if(mode4.eq.1) i0=95                  !i0 is bin of nominal lowest tone freq
  ia=i0-600/df                          !Set search range in frequency...
  ib=i0+600/df
  if(nfreeze.eq.1) then
     ia=i0+(mousedf-dftolerance)/df
     ib=i0+(mousedf+dftolerance)/df
  endif
  if(ia.lt.1) ia=1
  if(ib.gt.nfft-3) ib=nfft-3

  smax=0.
  ipk=1
  jpk=1
  do j=0,4*nblk-1                            !Find the sync pattern: lags 0-95
     do i=ia,ib                              !Search over specified frequency range
        ss=0.
        do n=1,4                             !Sum over 4 sync tones
           k=j+4*n-3
           ss=ss + fs0(i+2*icos(n),k)
        enddo
        if(ss.gt.smax) then
           smax=ss
           ipk=i                             !Frequency offset, DF
           jpk=j+1                           !Time offset, DT
        endif
     enddo
  enddo

  ref=fs0(ipk+2,jpk)  + fs0(ipk+4,jpk)    + fs0(ipk+6,jpk)   +     &
      fs0(ipk,jpk+4)  + fs0(ipk+4,jpk+4)  + fs0(ipk+6,jpk+4) +     &
      fs0(ipk,jpk+8)  + fs0(ipk+2,jpk+8)  + fs0(ipk+4,jpk+8) +     &
      fs0(ipk,jpk+12) + fs0(ipk+2,jpk+12) + fs0(ipk+6,jpk+12)
  ref=ref/3.0

  kk=0
  do j=0,4*nblk-1
     ss=0.
     do n=1,4
        k=j+4*n-3
        ss=ss + fs0(ipk+2*icos(n),k)
     enddo
     kk=kk+1
     ccf(kk)=ss/ref
  enddo

  xsync=smax/ref
  nsig=nint(db(smax/ref - 1.0) -15.0)
  if(mode4.eq.1) nsig=nsig-5
  if(nsig.lt.-20) nsig=-20
  ndf0=nint(df*(ipk-i0))
  if(nsig.lt.MinSigdB) then
     msglen=0
     worst=1.
     avg=1.
     go to 800
  endif

  smax=0.
  ja=jpk+16
  if(ja.gt.4*nblk) ja=ja-4*nblk
  jb=jpk+20
  if(jb.gt.4*nblk) jb=jb-4*nblk
  do i=ipk,ipk+60,2                         !Find User's message length
     ss=fs0(i,ja) + fs0(i+10,jb)
     if(ss.gt.smax) then
        smax=ss
        ipk2=i
     endif
  enddo

  msglen=(ipk2-ipk)/2
  if(msglen.lt.2 .or. msglen.gt.29) msglen=3
  fs1=0.
  jb=(jsym-4*nblk+1)/4
  jb=4*jb
  k=0
  n=0
  do j=jpk,jsym,4                         !Fold information symbols into fs1
     k=k+1
     if(mod(k-1,nblk)+1.gt.6) then
        n=n+1
        m=mod(n-1,msglen)+1
        do i=0,41
           fs1(i,m)=fs1(i,m) + s0(ipk+2*i,j)
        enddo
     endif
  enddo

! Read out the message:
  msg1='                            '
  mpk=0
  worst=9999.
  sum=0.
  do m=1,msglen
     smax=0.
     smax2=0.
     do i=0,41
        if(fs1(i,m).gt.smax) then
           smax=fs1(i,m)
           ipk3=i
        endif
     enddo
     do i=0,41
        if(fs1(i,m).gt.smax2 .and. i.ne.ipk3) smax2=fs1(i,m)
     enddo
     rr=smax/smax2
     sum=sum + rr
     if(rr.lt.worst) worst=rr
     if(ipk3.eq.40) mpk=m
     msg1(m:m)=c42(ipk3+1:ipk3+1)
  enddo

  avg=sum/msglen
  if(mpk.eq.1) then
     msg=msg1(2:)
  else if(mpk.lt.msglen) then
     msg=msg1(mpk+1:msglen)//msg1(1:mpk-1)
  else
     msg=msg1(1:msglen-1)
  endif

800 continue
  nworst=10.0*(worst-1.0)
  navg=10.0*(avg-1.0)
  if(nworst.gt.10) nworst=10
  if(navg.gt.10) navg=10
  isync=xsync
  if(navg.le.0) msg=' '
  csync=' '
  if(isync.ge.1) csync='*'

  call cs_lock('iscat')
  write(11,1020) cfile6,nsig,ndf0,csync,msg,msglen,nworst,navg
  write(21,1020) cfile6,nsig,ndf0,csync,msg,msglen,nworst,navg
1020 format(a6,i5,i5,1x,a1,2x,a28,i4,2i3)
  call match('VK7MO',msg(1:msglen-1),nstart1,nmatch1)
  call match('VK3HZ',msg(1:msglen-1),nstart2,nmatch2)

  write(*,1021) cfile6,nsig,ndf0,csync,msg,xsync,msglen,nworst,navg,       &
       nmatch1,nmatch2
1021 format(a6,i5,i5,1x,a1,2x,a28,f5.1,5i3)
  call cs_unlock

!  enddo

  return
end subroutine iscat
