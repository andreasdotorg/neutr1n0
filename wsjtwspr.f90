subroutine wsjtwspr(dat,jz)

  real dat(jz)
  complex c2(45000)
  include 'gcom2.f90'

  f0=1500.0
  newdat=1
  call filbig2(dat,jz,f0,newdat,c2,n4)
  call mept162a(c2,n4)

  return
end subroutine wsjtwspr
