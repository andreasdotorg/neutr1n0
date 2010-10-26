subroutine diana(dat,npts,cfile6,MinSigdB,DFTolerance,NFreeze,       &
     MouseDF,ccfblue,ccfred)

! Decode a Diana signal

  parameter (NMAX=512*1024)
  parameter (NSZ=646)                     !Quarter-symbols in 30 s
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  character cfile6*6                      !File time
  character c42*42
  character msg*28
  real s0(1024,NSZ)
  real fs0(1024,96)                       !Folded-for-sync symbol spectra
  real ccfblue(-5:540)
  real ccfred(-224:224)
  integer dftolerance
  data nsps/2048/,nsync/4/,nlen/2/,ndat/18/

  nsym=npts/nsps                      !Total symbol intervals in file
  nblk=nsync+nlen+ndat                !Frame size
  nfft=4096
  nq=nfft/4
  df=11025.0/nfft
  kstep=nsps/4

  call specdiana(dat,npts,s0,jsym,fs0)   !Get symbol spectra, fold for sync

  call syncdiana(fs0,kstep,nfreeze,mousedf,dftolerance,syncx,     &
     ipk,jpk,dfx,dtx,msglen,ccfblue,ccfred)     !Get sync: DF, DT, msglen
  
  msg=' '
  nsnr=-25
  jsync=syncx
  if(jsync.ge.MinSigdB .and. msglen.gt.0) call decdiana(s0,jsym,ipk,jpk,  &
       msglen,msg,nsnr)                               !Decode message
  jdf=nint(dfx)
  nwidth=0

  call cs_lock('iscat')
!  write(*,1020) cfile6,jsync,nsnr,dtx,jdf,nwidth,msg
  write(11,1020) cfile6,jsync,nsnr,dtx,jdf,nwidth,msg
  write(21,1020) cfile6,jsync,nsnr,dtx,jdf,nwidth,msg
1020 format(a6,i3,i5,f5.1,i5,i3,7x,a28)
  call cs_unlock

  return
end subroutine diana
