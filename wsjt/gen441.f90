subroutine gen441(itone,ndits,cfrag)

  integer itone(84)
  complex cfrag(2100)

! Generate iwave
  twopi=8*atan(1.0)
  dt=1.0/11025.0
  k=0
  NSPD=25
  df=11025.0/NSPD
  pha=0.
  do m=1,ndits
     freq=(itone(m)+1)*df
     dpha=twopi*freq*dt
     do i=1,NSPD
        k=k+1
        pha=pha+dpha
        cfrag(k)=cmplx(cos(pha),-sin(pha))
     enddo
  enddo

  return
end subroutine gen441
