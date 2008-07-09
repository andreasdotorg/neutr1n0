      subroutine sync64(dat,jz,DFTolerance,NFreeze,MouseDF,
     +  mode64,dtx,dfx,snrx,snrsync,ccfblue,ccfred1,flip,width)

C  Synchronizes JT64 data, finding the best-fit DT and DF.  
C  NB: at this stage, submodes ABC are processed in the same way.

      parameter (NP2=30*11025)         !Size of data array
      parameter (NFFTMAX=4096)         !Max length of FFTs
      parameter (NHMAX=NFFTMAX/2)      !Max length of power spectra
      parameter (NSMAX=160)            !Max number of half-symbol steps
      integer DFTolerance              !Range of DF search
      real dat(jz)
      real psavg(NHMAX)                !Average spectrum of whole record
      real s2(NHMAX,NSMAX)             !2d spectrum, stepped by half-symbols
      real ccfblue(-5:540)             !CCF with pseudorandom sequence

C  The value 450 is empirical:
      real ccfred(-450:450)            !Peak of ccfblue, as function of freq
      real ccfred1(-224:224)           !Peak of ccfblue, as function of freq
      real ccf64(-224:224)
      integer ic6(6)
      integer isync(63)
      data ic6/0,1,4,3,5,2/,idum/-1/
      save

C  Do FFTs of symbol length, stepped by half symbols.  Note that we have
C  already downsampled the data by factor of 2.
      nsym=63
      nfft=4096
      nsteps=2*jz/nfft - 1
      nh=nfft/2
      df=0.5*11025.0/nfft
! Set up the JT64 sync pattern
      isync=-1
      do n=1,3
         i0=11
         if(n.eq.2) i0=28
         if(n.eq.3) i0=46
         do i=1,6
            isync(i0+i)=ic6(i)
         enddo
      enddo

C  Compute power spectrum for each step and get average
      call zero(psavg,nh)
      do j=1,nsteps
         k=(j-1)*nh + 1
         call limit(dat(k),nfft)
         call ps(dat(k),nfft,s2(1,j))
         if(mode64.eq.4) call smooth(s2(1,j),nh)
         call add(psavg,s2(1,j),psavg,nh)
      enddo

!      call flat1(psavg,s2,nh,nsteps,NHMAX,NSMAX)        !Flatten the spectra
!      do i=1,nh
!         write(42,4001) i*df,psavg(i)
! 4001    format(2f10.3)
!      enddo

C  Find the best frequency channel for CCF
      famin=3.
      fbmax=2700.

      fa=famin
      fb=fbmax
      if(NFreeze.eq.1) then
         fa=max(famin,1270.46+MouseDF-DFTolerance)
         fb=min(fbmax,1270.46+MouseDF+DFTolerance)
      else
         fa=max(famin,1270.46+MouseDF-600)
         fb=min(fbmax,1270.46+MouseDF+600)
      endif
      ia=fa/df
      ib=fb/df
      i0=nint(1270.46/df)
      syncbest=-1.e30
      call zero(ccfred,745)
      nsync=18

C### Code from here to (almost) end of sync65 was deleted.  Must replace
C### with code modified for JT64.

      do i=ia,ib
         do lag=-20,20
            sum=0.
            do j=1,nsym
               if(isync(j).ge.0) then
                  j0=2*j -1 + lag
                  if(j0.ge.1 .and. j0.le.nsteps) then
                     sum=sum + s2(2*isync(j)+i,j0)
                  endif
               endif
            enddo
            ccf64(lag)=sum/nsync
            if(ccf64(lag).gt.syncbest) then
               ipk=i
               syncbest=ccf64(lag)
            endif
         enddo
      enddo

! Once more, at the best frequency
      i=ipk
      syncbest=-1.e30
      rewind 41

      dtstep=0.37151927
      do lag=-20,20
         sum=0.
         do j=1,nsym
            if(isync(j).ge.0) then
               j0=2*j - 1 + lag
               if(j0.ge.1 .and. j0.le.nsteps) then
                  sum=sum + s2(2*isync(j)+i,j0)
               endif
            endif
         enddo
         ccf64(lag)=sum/nsync
            if(ccf64(lag).gt.syncbest) then
               lagpk=lag
               syncbest=ccf64(lag)
            endif
         write(41,3001) lag,dtstep*lag,ccf64(lag)
 3001    format(i5,2f10.3)
         ccfred1(lag)=ccf64(lag)
      enddo

      snrsync=syncbest
      dtx=dtstep*lagpk
      dfx=(ipk-i0)*df

      return
      end

