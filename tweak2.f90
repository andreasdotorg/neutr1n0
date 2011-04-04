subroutine tweak2(ca,jz,fsample,f0,f1,cb)

! Shift frequency of analytic signal ca, with output to cb

  complex ca(jz),cb(jz)
  real*8 twopi,dphi
  complex*16 w,wstep
  data twopi/0.d0/
  save twopi

  if(twopi.eq.0.d0) twopi=8.d0*atan(1.d0)
  w=1.d0
  x0=0.5*(jz+1)
  do i=1,jz
     x=(i-x0)/fsample
     if(mod(i,100).eq.1) then
        dphi=(f0 + f1*x) * (twopi/fsample)
        wstep=cmplx(cos(dphi),sin(dphi))
     endif
     w=w*wstep
     cb(i)=w*ca(i)
  enddo

  return
end subroutine tweak2
