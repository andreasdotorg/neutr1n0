      subroutine unpack50(dat,n1,n2)

      integer*1 dat(11),i1
      equivalence (i1,i4)

      i4=0
      i1=dat(1)
      n1=ishft(i4,20)
      i1=dat(2)
      n1=n1 + ishft(i4,12)
      i1=dat(3)
      n1=n1 + ishft(i4,4)
      i1=dat(4)
      n1=n1 + iand(ishft(i4,-4),15)

      n2=ishft(iand(i4,15),18)
      i1=dat(5)
      n2=n2 + ishft(i4,10)
      i1=dat(6)
      n2=n2 + ishft(i4,2)
      i1=dat(7)
      n2=n2 + iand(ishft(i4,-6),3)

      return
      end

