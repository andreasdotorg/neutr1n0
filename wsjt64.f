      subroutine wsjt64(dat,npts,cfile6,NClearAve,MinSigdB,
     +  DFTolerance,NFreeze,NAFC,mode64,Nseg,MouseDF,NAgain,
     +  ndepth,nchallenge,neme,idf,idfsh,mycall,hiscall,hisgrid,
     +  lumsg,lcum,nspecial,ndf,nstest,dfsh,
     +  snrsh,NSyncOK,ccfblue,ccfred,ndiag,nwsh)

C  Orchestrates the process of decoding JT64 messages, using data that
C  have been 2x downsampled.  The search for shorthand messages has
C  already been done.

      real dat(npts)                        !Raw data
      integer DFTolerance
      logical first
      logical lcum
      character decoded*22,cfile6*6,special*5,cooo*3
      character*22 deepmsg
      character*67 ave1,ave2
      character*1 csync
      character*12 mycall
      character*12 hiscall
      character*6 hisgrid
      real ccfblue(-5:540),ccfred(-224:224)
      integer itf(2,9)
      include 'avecom.h'
      data first/.true./,ns10/0/,ns20/0/
      data itf/0,0, 1,0, -1,0, 0,-1, 0,1, 1,-1, 1,1, -1,-1, -1,1/
      save

      if(first) then
         nsave=0
         first=.false.
         ave1=' '
         ave2=' '
      endif

      naggressive=0
      if(ndepth.ge.2) naggressive=1
      nq1=3
      nq2=6
      if(naggressive.eq.1) nq1=1

      if(NClearAve.ne.0) then
         nsave=0                        !Clear the averaging accumulators
         ns10=0
         ns20=0
         ave1=' '
         ave2=' '
      endif
      if(MinSigdB.eq.99 .or. MinSigdB.eq.-99) then
         ns10=0                         !For Include/Exclude ?
         ns20=0
      endif

C  Attempt to synchronize: look for sync tone, get DF and DT.
      call sync64(dat,npts,DFTolerance,NFreeze,MouseDF,
     +    mode64,dtx,dfx,snrx,snrsync,ccfblue,ccfred,flip,width)
      write(*,3002) snrsync,dtx,dfx
 3002 format('Sync:',f6.1,'  DT:',f6.1,'   DF:',f6.1)

      csync=' '
      decoded='                      '
      deepmsg='                      '
      special='     '
      cooo='   '
      ncount=-1             !Flag for RS decode of current record
      ncount1=-1            !Flag for RS Decode of ave1
      ncount2=-1            !Flag for RS Decode of ave2
      NSyncOK=0
      nqual1=0
      nqual2=0

      if(nsave.lt.MAXAVE .and. (NAgain.eq.0 .or. NClearAve.eq.1)) 
     +  nsave=nsave+1
      if(nsave.le.0) go to 900          !Prevent bounds error

      nflag(nsave)=0                    !Clear the "good sync" flag
      iseg(nsave)=Nseg                  !Set the RX segment to 1 or 2
      nsync=nint(snrsync-3.0)
      nsnr=nint(snrx)
      if(nsnr.lt.-30 .or. nsync.lt.0) nsync=0
      nsnrlim=-32

C  Good Sync takes precedence over a shorthand message:
      if(nsync.ge.MinSigdB .and. nsnr.ge.nsnrlim .and.
     +   nsync.ge.nstest) nstest=0

      if(nstest.gt.0) then
         dfx=dfsh
         nsync=nstest
         nsnr=snrsh
         dtx=1.
         ccfblue(-5)=-999.0
         if(nspecial.eq.1) special='ATT  '
         if(nspecial.eq.2) special='RO   '
         if(nspecial.eq.3) special='RRR  '
         if(nspecial.eq.4) special='73   '
         NSyncOK=1              !Mark this RX file as good (for "Save Decoded")
         if(NFreeze.eq.0 .or. DFTolerance.ge.200) special(5:5)='?'
         width=nwsh
         idf=idfsh
         go to 200
      endif

      print*,'A:',nsync,nsnr
      if(nsync.lt.MinSigdB .or. nsnr.lt.nsnrlim) go to 200

C  If we get here, we have achieved sync!

C### From here onward, code from wsjt65.f was deleted.  Must restore
C### and modify.

 200  continue

 900  continue

      return
      end
