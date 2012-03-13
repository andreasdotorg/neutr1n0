subroutine specdiana(cdat,npts,s0,jsym)

  parameter (NSZ=646)
  complex cdat(93024)                     !Raw signal, 30 s at 11025*9/32 sps
  complex ct(1152)
  real x(1152),x2(1152)
  real savg(1152)
  real s0(1152,NSZ)                       !Symbol spectra at 1/4-symbol steps
  equivalence (x,c)

  nblk=24
  nsps=576                            !2048*9/32
  nsym=npts/nsps                      !Total symbol intervals in file
  nfft=2*nsps                         !Do FFTs at twice the symbol length
  kstep=nsps/4                        !Step by 1/4 symbol
  nq=nfft/4
  fac=1.0/1000.0                      !Somewhat arbitrary
  savg=0.

  ia=1-kstep
  do j=1,4*nsym                       !Compute symbol spectra
     ia=ia+kstep
     ib=ia+nsps-1
     if(ib.gt.npts) exit
     ct(1:nsps)=fac*cdat(ia:ib)
     ct(nsps+1:nfft)=0.
     call four2a(ct,nfft,1,-1,1)
     do i=1,nfft
        s0(i,j)=real(ct(i))**2 + aimag(ct(i))**2
        savg(i)=savg(i) + s0(i,j)
     enddo
  enddo
  jsym=4*nsym
  savg=savg/jsym

  do i=1,nfft                                 !Normalize the symbol spectra
     fac=1.0/savg(i)
     if(i.lt.11) fac=1.0/savg(11)
     do j=1,jsym
        s0(i,j)=fac*s0(i,j)
     enddo
  enddo

  return
end subroutine specdiana
