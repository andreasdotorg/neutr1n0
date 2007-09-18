      subroutine ps24(dat,nfft,s)

      parameter (NMAX=2520+2)
      parameter (NHMAX=NMAX/2-1)
      real dat(nfft)
      real s(NHMAX)
      real work(2*NMAX)
      complex c(0:NMAX)

      nh=nfft/2
      do i=1,nh
         c(i-1)=dat(i)/128.0       !### Why 128 ??
      enddo
      do i=nh+1,nfft
         c(i-1)=0.
      enddo

      call fourt(c,nfft,1,-1,0,work)

      fac=1.0/nfft
      do i=1,nh
         s(i)=fac*(real(c(i))**2 + aimag(c(i))**2)
      enddo

      return
      end
