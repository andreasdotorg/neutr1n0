subroutine getfile(fname,len)

#ifdef CVF
  use dflib
#endif

  parameter (NDMAX=120*11025)
  character*(*) fname
  include 'gcom1.f90'
  include 'gcom2.f90'
  include 'gcom4.f90'


  integer*1 d1(NDMAX)
  integer*1 hdr(44),n1
  integer*2 d2(NDMAX)
  integer*2 nfmt2,nchan2,nbitsam2,nbytesam2
  character*4 ariff,awave,afmt,adata
  common/hdr/ariff,lenfile,awave,afmt,lenfmt,nfmt2,nchan2, &
     nsamrate,nbytesec,nbytesam2,nbitsam2,adata,ndata,d2
  equivalence (ariff,hdr),(n1,n4),(d1,d2)

1 if(ndecoding.eq.0) go to 2
#ifdef CVF
  call sleepqq(100)
#else
  call usleep(100*1000)
#endif

  go to 1

2 do i=len,1,-1
     if(fname(i:i).eq.'/' .or. fname(i:i).eq.'\\') go to 10
  enddo
  i=0
10 filename=fname(i+1:)
  ierr=0

#ifdef CVF
  open(10,file=fname,form='binary',status='old',err=998)
  read(10,end=998) hdr
#else
  call rfile2(fname,hdr,44+2*NDMAX,nr)
#endif

  call check_endian
  if(nbitsam2.eq.8) then
     if(ndata.gt.NDMAX) ndata=NDMAX

#ifdef CVF
     call rfile(10,d1,ndata,ierr)
     if(ierr.ne.0) go to 999
#endif

     do i=1,ndata
        n4=d1(i)
        if (n4.lt.0) n4=256+n4
        d2c(i)=250*n4
     enddo
     jzc=ndata

  else if(nbitsam2.eq.16) then
     if(ndata.gt.2*NDMAX) ndata=2*NDMAX
#ifdef CVF
     call rfile(10,d2c,ndata,ierr)
     jzc=ndata/2
     if(ierr.ne.0) go to 999
#else
     jzc=ndata/2
     do i=1,jzc
        d2c(i)=d2(i)
     enddo
#endif
  endif

  ndiskdat=1
  mousebutton=0
  close(10)
  return

#ifdef CVF
998 ierr=1001
999 close(10)
    return
#endif

end subroutine getfile

subroutine check_endian

  parameter (NDMAX=120*11025)

  integer*1 d1(NDMAX)
  integer*1 hdr(44),n1
  integer*2 d2(NDMAX)
  integer*2 nfmt2,nchan2,nbitsam2,nbytesam2
  integer*2 iswap_short
  character*4 ariff,awave,afmt,adata
  common/hdr/ariff,lenfile,awave,afmt,lenfmt,nfmt2,nchan2, &
     nsamrate,nbytesec,nbytesam2,nbitsam2,adata,ndata,d2
  equivalence (ariff,hdr),(n1,n4),(d1,d2)

  if (nfmt2.eq.1) return             ! correct endianess for this CPU
  write(*,1000)
1000 format('Converting file to big-endian',i10)
  lenfile = iswap_int(lenfile)
  lenfmt = iswap_int(lenfmt)
  nfmt2 = iswap_short(nfmt2)
  nchan2 = iswap_short(nchan2)
  nsamrate = iswap_int(nsamrate)
  nbytesec = iswap_int(nbytesec)
  nbytesam2 = iswap_short(nbytesam2)
  nbitsam2 = iswap_short(nbitsam2)
  ndata = iswap_int(ndata)
  if (nbitsam2.eq.8) return           ! header converted.   Data are bytes

  do i=1,ndata/2
    d2(i) = iswap_short(d2(i))
  enddo


  return
end subroutine check_endian

integer function iswap_int(idat)

  itemp1 = ior(ishft(idat,24), iand(ishft(idat,8), z'00ff0000'))
  itemp0 = ior(iand(ishft(idat,-8), z'0000ff00'), iand(ishft(idat,-24),z'000000ff'))
  iswap_int = ior(itemp1,itemp0)
  
end function iswap_int

integer*2 function iswap_short(idat)

  integer*2 idat,m2
  data m2/255/

  iswap_short = ior(ishft(idat,8), iand(ishft(idat,-8), m2))

end function iswap_short
