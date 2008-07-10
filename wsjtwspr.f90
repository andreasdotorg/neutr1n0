subroutine wsjtwspr(dat,jz,cfile6,ccfblue,ccfred,ndiag)

  real dat(jz)
  real ccfblue(-5:540)
  real ccfred(-224:224)
  character cfile6*6
  complex c2(45000)
  include 'gcom2.f90'

  f0=1500 + mousedf
  newdat=1
  if(nagain.eq.1) newdat=0
  call filbig2(dat,jz,f0,newdat,c2,n4)
  call mept162a(c2,n4,f0,cfile6,ndiag,minsigdb,mousedf,DFTolerance,ndwspr, &
       ccfblue,ccfred)
  return
end subroutine wsjtwspr
