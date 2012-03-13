subroutine len441(cdat,npts,msglen,nacf)

! Determine length of the user message in a JTMS ping.

  complex cdat(npts)
  real acf(2250)
  real acf2(2250)
  real tmp(2250)

  msglen=0                              !Use ACF to find msg length
  acf=0.
  acf2=0.
  ja=151
  jb=min(75*28,npts-225)
  do j=ja,jb
     z=0.
     do i=1,npts-j
        z=z + cdat(i)*conjg(cdat(i+j))
     enddo
     acf(j)=abs(z)/(npts-j)
  enddo

!  rewind 53
  jstep=50
  smax=0.
  do j=ja+jstep,jb-jstep

! Find local base level; update every 10 bins.
     if(mod(j-ja-jstep,10).eq.0) then
        call pctile(acf(j-jstep),tmp,2*jstep+1,45,base)
     endif
     acf2(j)=acf(j)/base - 1.0
     if(acf2(j).gt.smax) then
        smax=acf2(j)
        jpk=j
     endif
!     write(53,5001) j,j/75.0,acf2(j)
!5001 format(i8,2f12.3)
  enddo
!  call flush(53)
  
  np=nint(jpk/75.0)
  chk=(jpk/75.0)/np

  smax2=0.
  do m=4,2,-1
     if(mod(np,m).eq.0) then
        i0=nint(np*75.0/m)
        smax2=0.
        do i=-10,10
           if(acf2(i0+i).gt.smax2) smax2=acf2(i0+i)
        enddo
        if(smax2/smax.gt.0.6) go to 10
     endif
  enddo
  m=1
10 msglen=np/m
  nacf=smax-4.0
  if(nacf.lt.0) nacf=0
  if(nacf.gt.10) nacf=10
  if(chk.lt.0.99 .or. chk.gt.1.01 .or. nacf.le.0) msglen=0
!  print*,np,msglen,nacf,chk,smax,smax2

  return
end subroutine len441
