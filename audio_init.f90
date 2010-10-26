subroutine audio_init(ndin,ndout)
!f2py threadsafe

  integer start_threads

  include 'gcom1.f90'
  include 'gcom2.f90'

  nmode=1
  if(mode(1:4).eq.'JT65') then
     nmode=2
     if(mode(5:5).eq.'A') mode65=1
     if(mode(5:5).eq.'B') mode65=2
     if(mode(5:5).eq.'C') mode65=4
  endif
  if(mode(1:4).eq.'Echo') nmode=3
  if(mode(1:2).eq.'CW') nmode=5
  if(mode(1:3).eq.'JT4') nmode=7
  if(mode(1:4).eq.'JTMS') nmode=8
  if(mode(1:5).eq.'ISCAT') nmode=9

  ndevin=ndin
  ndevout=ndout
  TxOK=0
  Transmitting=0
  nfsample=11025
  nspb=1024
  nbufs=2048
  nmax=nbufs*nspb
  nwave=60*nfsample
  ngo=1
  f0=800.0
  do i=1,nwave
     iwave(i)=nint(32767.0*sin(6.283185307*i*f0/nfsample))
  enddo

  ierr=start_threads()

  return
end subroutine audio_init
