      subroutine inter_mept(id,ndir)

C  Interleave (ndir=1) or de-interleave (ndir=-1) the array id.

      integer*1 id(0:161),itmp(0:161)
      integer j0(0:161)
      character*8 c0,c1
      logical first
      data first/.true./
      save

      if(first) then
         k=-1
         do i=0,255
            m=i
            n=iand(m,1)
            n=2*n + iand(m/2,1)
            n=2*n + iand(m/4,1)
            n=2*n + iand(m/8,1)
            n=2*n + iand(m/16,1)
            n=2*n + iand(m/32,1)
            n=2*n + iand(m/64,1)
            n=2*n + iand(m/128,1)
            if(n.le.161) then
               k=k+1
               j0(k)=n
            endif
         enddo
         first=.false.
      endif

      if(ndir.eq.1) then
         do i=0,161
            itmp(j0(i))=id(i)
         enddo
      else
         do i=0,161
            itmp(i)=id(j0(i))
         enddo
      endif

      do i=0,161
         id(i)=itmp(i)
      enddo

      return
      end
