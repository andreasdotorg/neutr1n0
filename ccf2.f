      subroutine ccf2(ss,nz,lag1,lag2,ccfbest,lagpk)

      parameter (LAGMAX=200)
      real ss(nz)
      real ccf(-LAGMAX:LAGMAX)
      real pr(162)
      logical first

C  The WSPR pseudo-random sync pattern:
      integer npr(162)
      data npr/
     +       1,1,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,
     +       0,1,0,1,1,1,1,0,0,0,0,0,0,0,1,0,0,1,0,1,
     +       0,0,0,0,0,0,1,0,1,1,0,0,1,1,0,1,0,0,0,1,
     +       1,0,1,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0,0,1,
     +       0,0,1,0,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,0,
     +       0,0,0,0,1,0,0,1,0,0,1,1,1,0,1,1,0,0,1,1,
     +       0,1,0,0,0,1,1,1,0,0,0,0,0,1,0,1,0,0,1,1,
     +       0,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,1,1,0,
     +       0,0/
      data first/.true./
      save first,pr,nsym

      if(first) then
         nsym=162
         do i=1,nsym
            pr(i)=2*npr(i)-1
         enddo
      endif

      ccfbest=0.

      do lag=lag1,lag2
         x=0.
         do i=1,nsym
            j=16*i + lag
            if(j.ge.1 .and. j.le.nz) x=x+ss(j)*pr(i)
         enddo
         ccf(lag)=x
         if(x.gt.ccfbest) then
            ccfbest=x
            lagpk=lag
         endif
      enddo

      sum=0.
      nsum=0
      do i=lag1,lag2
         if(abs(i-lagpk).gt.16) then
            sum=sum+ccf(i)
            nsum=nsum+1
         endif
      enddo
      ave=sum/nsum

      sq=0.
      do i=lag1,lag2
         if(abs(i-lagpk).gt.16) sq=sq + (ccf(i)-ave)**2
      enddo
      rms=sqrt(sq/(nsum-1))
      ccfbest=(ccfbest-ave)/rms

      return
      end
