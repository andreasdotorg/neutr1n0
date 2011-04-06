subroutine synciscat(dat,npts0,s0,jsym,df,MinSigdB,DFTolerance,NFreeze,   &
     MouseDF,mousebutton,mode4,nafc,psavg,xsync,nsig,ndf0,msglen,ipk,jpk,idf)

! Synchronize an ISCAT signal

  parameter (NMAX=34*11025)
  parameter (NSZ=4*1400)
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  complex cdat(368640)
  complex cdat0(368640)
  real x(NSZ),x2(NSZ)
  complex c(288)
  real s0(288,NSZ)
  real fs0(288,108)                        !108 = 96 + 3*4
  real savg(288)
  real b(288)
  real ccf(1:96)
  real psavg(72)                          !Average spectrum of whole file
  integer dftolerance
  integer icos(4)
  data icos/0,1,3,2/
  data nsync/4/,nlen/2/,ndat/18/

! To silence compiler warnings:
  nsigbest=-999
  ndf0best=0
  msglenbest=0
  ipkbest=0
  jpkbest=0
  ipk2=0
  idfbest=mousebutton

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

  npts=npts0*9.0/32.0                  !Downsampled data length
  fsample=3100.78125                   !New sample rate
  nsps=144/mode4
  nsym=npts/nsps
  nblk=nsync+nlen+ndat
  nfft=2*nsps                          !FFTs at twice the symbol length,
  kstep=nsps/4                         !  stepped by 1/4 symbol
  df=fsample/nfft
  fac=1.0/1000.0                       !Somewhat arbitrary
  savg=0.
  cdat0(:npts)=cdat(:npts)

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
        savg(i)=savg(i) + s0(i,j)                 !Accumulate avg spectrum
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

  idfmax=0
  idfstep=1
  jb=(jsym-4*nblk+1)/4
  jb=4*jb
  idftop=60
  if(nafc.ne.0) then
     idfmax=idftop
     iimax=idftop*(jb/2)/2584.0
     if(iimax.eq.0) then
        idfmax=0
     else
        idfstep=nint(float(idftop)/iimax)
        idfmax=nint(float(idftop)/idfstep)
        idfmax=idfstep*idfmax
     endif
  endif
  xsyncbest=0.
  do idf=-idfmax,idfmax,idfstep
     fs0=0.
     do j=1,jb                             !Fold s0 into fs0, modulo 4*nblk 
        k=mod(j-1,4*nblk)+1
        ii=nint(idf*float(j-jb/2)/2584.0)
        ia=max(1-ii,1)
        ib=min(nfft-ii,nfft)
        do i=ia,ib
           fs0(i,k)=fs0(i,k) + s0(i+ii,j)
        enddo
     enddo

     do j=1,12
        fs0(1:nfft,96+j)=fs0(1:nfft,j)
     enddo

     i0=27
     if(mode4.eq.1) i0=95                  !bin of nominal lowest tone
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
     do j=0,4*nblk-1                            !Find sync pattern: lags 0-95
        do i=ia,ib                              !Search specified freq range
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
     if(nsig.lt.MinSigdB) go to 900

     smax=0.
     ja=jpk+16
     if(ja.gt.4*nblk) ja=ja-4*nblk
     jj=jpk+20
     if(jj.gt.4*nblk) jj=jj-4*nblk
     do i=ipk,ipk+60,2                         !Find User's message length
        ss=fs0(i,ja) + fs0(i+10,jj)
        if(ss.gt.smax) then
           smax=ss
           ipk2=i
        endif
     enddo
     
     msglen=(ipk2-ipk)/2
     if(msglen.lt.2 .or. msglen.gt.29) msglen=3

     if(xsync.gt.xsyncbest) then
        xsyncbest=xsync
        nsigbest=nsig
        ndf0best=ndf0
        msglenbest=msglen
        ipkbest=ipk
        jpkbest=jpk
        idfbest=idf
     endif
     fdot=idf*df/30.0
!     write(*,3001) idf,fdot,xsync,nsig,ndf0,msglen
!3001 format(i5,2f8.1,3i5)
  enddo

  xsync=xsyncbest
  nsig=nsigbest
  ndf0=ndf0best
  msglen=msglenbest
  ipk=ipkbest
  jpk=jpkbest
  idf=idfbest

900 return
end subroutine synciscat
