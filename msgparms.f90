subroutine msgparms(nbits,nparm,kz)
  integer nparm(5,30)

  k=0
!  ntype=1
  do nr1=5,15
     do nz1=56,174,2
        nb=nr1*nz1 + 28
        if(nb.eq.nbits) then
           k=k+1
           nparm(1,k)=1
           nparm(2,k)=nr1
           nparm(3,k)=nz1
           nparm(4,k)=0
           nparm(5,k)=0
           if(k.eq.30) go to 900
        endif
     enddo
  enddo

!  ntype=2
  do nz1=56,168,2
     do nr2=20,32
        if(nz1 + nr2*28 + 28.eq.nbits) then
           k=k+1
           nparm(1,k)=2
           nparm(2,k)=1
           nparm(3,k)=nz1
           nparm(4,k)=nr2
           nparm(5,k)=28
           if(k.eq.30) go to 900
        endif
        if(nz1 + nr2*34 + 28.eq.nbits) then
           k=k+1
           nparm(1,k)=2
           nparm(2,k)=1
           nparm(3,k)=nz1
           nparm(4,k)=nr2
           nparm(5,k)=34
           if(k.eq.30) go to 900
        endif
        if(nz1 + nr2*36 + 28.eq.nbits) then
           k=k+1
           nparm(1,k)=2
           nparm(2,k)=1
           nparm(3,k)=nz1
           nparm(4,k)=nr2
           nparm(5,k)=36
           if(k.eq.30) go to 900
        endif
     enddo
  enddo

!  ntype=3
  do nr1=4,11
     do nz1=56,174,2
        do nr2=4,6
           nb=nr1*nz1 + nr2*46 + 28
           if(nb.eq.nbits) then
              k=k+1
              nparm(1,k)=3
              nparm(2,k)=nr1
              nparm(3,k)=nz1
              nparm(4,k)=nr2
              nparm(5,k)=46
              if(k.eq.30) go to 900
           endif
        enddo
     enddo
  enddo
  go to 910
900 print*,'Reached 30 entries in nparm()'
910 kz=k
  return
end subroutine msgparms
