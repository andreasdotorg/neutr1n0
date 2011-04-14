subroutine dtrim(dat,jz,dat2,jz2)
  real dat(jz),dat2(jz)
  real ssq(1000)

  sumsq=0.
  nz=jz/1000
  k=0
  do i=1,1000
     sq=0.
     do n=1,nz
        k=k+1
        sq=sq + dat(k)**2
     enddo
     ssq(i)=sq
     sumsq=sumsq+sq
  enddo
  avesq=sumsq/1000.0
  do i=1,1000
     if(ssq(i).ge.0.1*avesq) go to 10
  enddo
  i=0

10 ia=(i+1)*nz
  if(i.eq.1) ia=1
  jz2=jz-ia+1
  dat2(:jz2)=dat(ia:)

  return
end subroutine dtrim
