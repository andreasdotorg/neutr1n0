      subroutine unpack50(dat,n1,n2)

      integer*1 dat(11)

      i4=iand(dat(1),255)
      n1=ishft(i4,20)
      i4=iand(dat(2),255)
      n1=n1 + ishft(i4,12)
      i4=iand(dat(3),255)
      n1=n1 + ishft(i4,4)
      i4=iand(dat(4),255)
      n1=n1 + iand(ishft(i4,-4),15)

      n2=ishft(iand(i4,15),18)
      i4=iand(dat(5),255)
      n2=n2 + ishft(i4,10)
      i4=iand(dat(6),255)
      n2=n2 + ishft(i4,2)
      i4=iand(dat(7),255)
      n2=n2 + iand(ishft(i4,-6),3)

      return
      end

