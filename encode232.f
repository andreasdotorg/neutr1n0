      subroutine encode232(dat,nbytes,symbol,nsym)

C  Convolutional encoder for a K=32, r=1/2 code.

      integer*1 dat(nbytes)             !User data, packed 8 bits per byte
      integer*1 symbol(nsym)            !Channel symbols, one bit per byte
      include 'conv232.f'

      nstate=0
      k=0
      do j=1,nbytes
         do i=7,0,-1
            nstate=ior(ishft(nstate,1),iand(ishft(dat(j),-i),1))
            n=iand(nstate,npoly1)
            n=ieor(n,ishft(n,-16))
            k=k+1
            symbol(k)=partab(iand(ieor(n,ishft(n,-8)),255))
            n=iand(nstate,npoly2)
            n=ieor(n,ishft(n,-16))
            k=k+1
            symbol(k)=partab(iand(ieor(n,ishft(n,-8)),255))
         enddo
      enddo

      return
      end
