subroutine a2d(iarg)

! Start the PortAudio streams for audio input and output.
  include 'gcom1.f90'
  include 'gcom2.f90'

! This call does not normally return, as the background portion of
! JTaudio goes into a test-and-sleep loop.

  idevin=iarg                                !Silence compiler warning
  idevin=ndevin
  idevout=ndevout
  call padevsub(idevin,idevout)

  ierr=jtaudio(idevin,idevout,y1,y2,NMAX,iwrite,iwave,nwave,    &
       11025,NSPB,TRPeriod,TxOK,ndebug,Transmitting,            &
       Tsec,ngo,nmode,tbuf,ibuf,ndsec)
  if(ierr.ne.0) then
     write(*,1005) ierr
1005 format('Error ',i2,' in JTaudio, you will only be able to work offline.')
  else
     write(*,1006) 
1006 format('Audio streams terminated normally.')
  endif
  return
end subroutine a2d
