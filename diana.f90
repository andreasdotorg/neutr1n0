subroutine diana(cdat,npts,cfile6,MinSigdB,DFTolerance,NFreeze,       &
     MouseDF,nafc,ccfblue,ccfred)

! Decode a Diana signal

  parameter (NSZ=646)                     !Quarter-symbols in 30 s
  complex cdat(93024)                     !Raw signal, 30 s at 11025*9/32 sps
  character cfile6*6                      !File time
  character msg*28
  real s0(1152,NSZ)
  real ccfblue(-5:540)
  real ccfred(-224:224)
  integer dftolerance
  data nsps/576/,nsync/4/,nlen/2/,ndat/18/

  nsym=npts/nsps                      !Total symbol intervals in file
  nblk=nsync+nlen+ndat                !Frame size
  df=11025.0/4096.0
  kstep=nsps/4

! Get symbol spectra, fold for sync
  call specdiana(cdat,npts,s0,jsym)

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
