subroutine dtrim(d2,jz)

! Remove any transient data at start of record.

  integer*2 d2(jz)
!  real dat(jz),dat2(jz)
  real ssq(1000)

  sumsq=0.
  nz=jz/1000
  k=0
  do i=1,1000
     sq=0.
     do n=1,nz
        k=k+1
        x=d2(k)
        sq=sq + x*x
     enddo
     ssq(i)=sq
     sumsq=sumsq+sq
  enddo
  avesq=sumsq/1000.0

  ichk=11025/nz
  do i=ichk,1,-1
     if(ssq(i).lt.avesq/3.0 .or. ssq(i).gt.3.0*avesq) go to 10
  enddo
  i=0

10 continue
  ia=(i+1)*nz
  if(i.eq.1) ia=1
  if(ia.gt.1) d2(1:ia)=0               !Zero the bad data

  return
end subroutine dtrim
