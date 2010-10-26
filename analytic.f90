subroutine analytic(d,npts,nfft1,s,c)

! Convert real data to a complex ("analytic") signal

  parameter (NMAX=512*1024)
  real d(npts)
  real s(NMAX)
  complex c(NMAX)

  nh=nfft1/2
  fac=2.0/nfft1
  do i=1,npts
     c(i)=fac*d(i)
  enddo
  c(npts+1:nfft1)=0.
  call four2a(c,nfft1,1,-1,1)

  do i=1,nh
     s(i)=real(c(i))**2 + aimag(c(i))**2
  enddo

  c(1)=0.5*c(1)
  c(nh+2:nfft1)=0.
  call four2a(c,nfft1,1,1,1)

  return
end subroutine analytic
