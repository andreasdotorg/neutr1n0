      subroutine foldcw(x,nsym,ntype,nr1,nz1,nr2,nz2,f1,f2,qual)

      real x(nsym)
      real f1(300),f2(300),tmp(300)
      integer hist1(-10:30),hist2(-10:30)

      rewind 74
      rewind 75
      rewind 76

      call zero(f1,200)
      call zero(f2,50)
      q1=0.
      q2=0.

C  Fold at period nz1
      do i=1,nr1*nz1
         j=mod(i-1,nz1) + 1
         f1(j)=f1(j) + x(i)
      enddo
      do i=1,nz1
         f1(i)=f1(i)/nr1
      enddo

C  Get median, re-normalize, and compute histogram
      call pctile(f1,tmp,nz1,25,b1)
      call pctile(f1,tmp,nz1,75,b2)
      q1=b2-b1
      bmid=0.5*(b1+b2)
      call zero(hist1,41)
      do i=1,nz1
         f1(i)=(f1(i)-bmid)/(b2-b1)
         write(75,3005) i,f1(i)
 3005    format(i3,f10.3)
         j=nint(5.0*f1(i))
         if(j.lt.-10) j=-10
         if(j.gt.30) j=30
         hist1(j)=hist1(j)+1
      enddo

      if(nr2*nz2.gt.0) then
C  Fold at period nz2
         k=nr1*nz1
         do i=1,nr2*nz2
            k=k+1
            j=mod(i-1,nz2) + 1
            f2(j)=f2(j) + x(k)
         enddo
         do i=1,nz2
            f2(i)=f2(i)/nr2
         enddo
         
C  Get median, re-normalize, and compute histogram
         call pctile(f2,tmp,nz2,25,b3)
         call pctile(f2,tmp,nz2,75,b4)
         q2=b4-b3
         bmid=0.5*(b3+b4)
         call zero(hist2,41)
         do i=1,nz2
            f2(i)=(f2(i)-bmid)/(b4-b3)
            write(76,3005) i,f2(i)
            j=nint(5.0*f2(i))
            if(j.lt.-10) j=-10
            if(j.gt.30) j=30
            hist2(j)=hist2(j)+1
         enddo
      endif

      do i=-10,30
         write(74,3004) i,hist1(i),hist2(i)
 3004    format(i3,2i5)
      enddo
      write(74,3004) 31,0

      qual=q1
      if(ntype.eq.2) qual=q2
      
      return
      end
