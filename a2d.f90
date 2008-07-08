!---------------------------------------------------- a2d
subroutine a2d(iarg)

! Start the PortAudio streams for audio input and output.
  integer nchin(0:20),nchout(0:20)
  include 'gcom1.f90'
  include 'gcom2.f90'

! This call does not normally return, as the background portion of
! JTaudio goes into a test-and-sleep loop.

  idevin=iarg                                !Silence compiler warning
  idevin=ndevin
  idevout=ndevout
  call padevsub(numdevs,ndefin,ndefout,nchin,nchout)
  
  write(*,1000) idevin,idevout
1000 format(/'User requested devices:   Input =',i2,'   Output =',i2)
  write(*,1002) ndefin,ndefout
1002 format(/'Default devices:          Input =',i2,'   Output =',i2)
  if(idevin.lt.0 .or. idevin.ge.numdevs) idevin=ndefin
  if(idevout.lt.0 .or. idevout.ge.numdevs) idevout=ndefout
  if(idevin.eq.0 .and. idevout.eq.0) then
     idevin=ndefin
     idevout=ndefout
  endif
  write(*,1004) idevin,idevout
1004 format( 'Will open devices:        Input =',i2,'   Output =',i2)

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
