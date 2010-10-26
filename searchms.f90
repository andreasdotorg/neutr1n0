subroutine searchms(cdat,npts,msg,ndi,rmax)

  parameter (NMAX=30*11025)     !Max length of wave file
  complex cdat(npts)
  complex cwave(NMAX)
  character*28 msg            !User message

  call genms(msg,1.d0,iwave,cwave,1,kz)

  r=0.
  rmax=0.
  do i1=1,npts-kz
     z=0.
     ss=0.
     do i=1,kz
        ss=ss + abs(cdat(i+i1-1))
        z=z + cdat(i+i1-1)*conjg(cwave(i))
     enddo
     r=abs(z)/ss
!     write(53,3001) i1,r,abs(z)
!3001 format(i8,2f12.3)
     if(r.gt.rmax) then
        rmax=r
        i1pk=i1
     endif
  enddo     

  nch=i1pk/56.0
  ndi=i1pk - 56*nch
  if(ndi.gt.8) ndi=ndi-56
!  print*,'Z',i1pk,ndi,msg
!  call flush(53)

  return
end subroutine searchms
