subroutine wsjtwspr(dat,jz,cfile6,ndiag,minsync)

  real dat(jz)
  character cfile6*6
  complex c2(45000)
  include 'gcom2.f90'

  f0=1500.0
  newdat=1
  call filbig2(dat,jz,f0,newdat,c2,n4)
  call mept162a(c2,n4,cfile6,ndiag,minsync)

  return
end subroutine wsjtwspr
