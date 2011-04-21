subroutine diana(dat,npts,cfile6,MinSigdB,DFTolerance,NFreeze,       &
     MouseDF,nafc,ccfblue,ccfred)

! Decode a Diana signal

  parameter (NMAX=512*1024)
  parameter (NSZ=646)                     !Quarter-symbols in 30 s
  real dat(NMAX)                          !Raw signal, 30 s at 11025 sps
  character cfile6*6                      !File time
  character msg*28
  real s0(1024,NSZ)
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

! Get symbol spectra, fold for sync
  call specdiana(dat,npts,s0,jsym)

! Get sync: DF, DT, msglen
  call syncdiana(s0,jsym,kstep,nfreeze,mousedf,dftolerance,nafc,xsync,   &
     ipk,jpk,idfpk,dfx,dtx,msglen,msg,nsnr,nworst,navg,ccfblue,ccfred)

  jdf=nint(dfx)
  nfdot=nint(idfpk*df)
  jsync=xsync

  call cs_lock('iscat')
!  write(*,1020) cfile6,jsync,nsnr,dtx,jdf,nfdot,msg,msglen,nworst,navg
  write(11,1020) cfile6,jsync,nsnr,dtx,jdf,nfdot,msg,msglen,nworst,navg
  write(21,1020) cfile6,jsync,nsnr,dtx,jdf,nfdot,msg,msglen,nworst,navg
1020 format(a6,i3,i5,f5.1,i5,i4,7x,a28,i5,2i3)
  call cs_unlock

  return
end subroutine diana
