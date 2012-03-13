      subroutine gentone(x,n,k,samfac)

      real*4 x(512)
      real*8 dt,f,twopi,samfac
      
      twopi=8*datan(1.d0)
      dt=1.d0/(samfac*11025.d0)
      f=(n+51)*11025.d0/512.d0
      do i=1,512
         x(i)=sin(twopi*i*dt*f)
      enddo
      k=k+512

      return
      end
