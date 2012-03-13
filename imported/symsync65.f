      subroutine symsync65(c5,n5,k0,s,flip,pr,nsps,kpk,ccf,smax)

      complex c5(n5)
      real s(n5),pr(126),ccf(-128:128)
      complex z

      kh=nsps/2
      z=0.
      do i=1,nsps
         z=z + c5(i)
      enddo
      s(1)=real(z)**2 + aimag(z)**2
      smax=s(1)
      do i=nsps+1,n5
         z=z + c5(i) - c5(i-nsps)
         s(i-nsps+1)=real(z)**2 + aimag(z)**2
         smax=max(s(i-nsps+1),smax)
      enddo
      iz=n5-nsps+1

      smax=0.
      do k=-kh,kh
         sum=0.
         do i=1,126
            j=nsps*(i-1)+k+k0
            if(j.ge.1 .and. j.le.iz) sum=sum + flip*pr(i)*s(j)
         enddo
         ccf(k)=sum
         if(sum.gt.smax) then
            smax=sum
            kpk=k
         endif
      enddo

      return
      end
