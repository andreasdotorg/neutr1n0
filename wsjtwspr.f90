subroutine wsjtwspr(dat,jz,cfile6,ndiag)

  real dat(jz)
  character cfile6*6
  complex c2(45000)
  include 'gcom2.f90'

  f0=1270.46 + mousedf
  newdat=1
  if(nagain.eq.1) newdat=0
  call filbig2(dat,jz,f0,newdat,c2,n4)
  call mept162a(c2,n4,f0,cfile6,ndiag,MinSyncdB,mousedf,DFTolerance)

  return
end subroutine wsjtwspr
