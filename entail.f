      subroutine entail(dgen,data0)

C  Move 72-bit packed data from 6-bit to 8-bit symbols and add a zero tail.
      integer dgen(12)
      integer*1 data0(13),i1
      equivalence (i1,i4)

      i4=0
      k=0
      m=0
      do i=1,12
         n=dgen(i)
         do j=1,6
            k=k+1
            i4=i4+i4+iand(1,ishft(n,j-6))
            if(k.eq.8) then
               m=m+1
               data0(m)=i1
               k=0
            endif
         enddo
      enddo
      do m=10,13
         data0(m)=0
      enddo

      return
      end
