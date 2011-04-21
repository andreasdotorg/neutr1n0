subroutine syncdiana(s0,jsym,kstep,nfreeze,mousedf,dftolerance,nafc,xsync,  &
     ipk,jpk,idfpk,dfx,dtx,msglen,msg,nsnr,nworst,navg,ccfblue,ccfred)

  parameter (NSZ=646)                     !Quarter-symbols in 30 s
  real s0(1024,NSZ)
  real fs0(1024,96)                       !Folded-for-sync spectra
  real ccfblue(-5:540)
  real ccfred(-224:224)
  character msg*28,msgbest*28
  integer dftolerance
  integer isync(4)
  data isync/8,16,32,24/

  df=11025.0/4096.0
  nblk=24
  i0=2*236
  xsyncbest=0.
  bigworst=0.
  ipk=9999
  jpk=9999
  nq=1024
  jb=(jsym-4*nblk+1)/4
  jb=4*jb

  idfmax=0
  if(nafc.eq.1) idfmax=10
  do idf=-idfmax,idfmax,2

     fs0=0.
     do j=1,jb                           !Fold s0 into fs0, modulo 4*nblk
        k=mod(j-1,4*nblk)+1
        ii=nint(idf*float(j-jb/2)/float(jb))
        i1=max(1,1-ii)
        i2=min(nq,nq-ii)
        fs0(i1:i2,k)=fs0(i1:i2,k) + s0(i1+ii:i2+ii,j)
     enddo

     ia=nint(-600.0/df)
     ib=nint(600.0/df)
     if(nfreeze.eq.1) then
        ia=nint((mousedf-dftolerance)/df)
        ib=nint((mousedf+dftolerance)/df)
     endif

     smax=0.
     do i=ia,ib                          !Search over DF range
        sm1=0.
        do j=0,4*nblk-1                  !Find sync pattern, lags 0-95
           ss=0.
           do n=1,4                      !Sum the four sync tones
              k=j+4*n-3
              if(k.gt.4*nblk) k=k-4*nblk
              ss=ss + fs0(i0+i+2*isync(n),k)
           enddo
           if(ss.gt.sm1) then
              sm1=ss
              jpk1=j+1
           endif
!           if(abs(i).le.224) ccfred(i)=sm1
        enddo
        if(sm1.gt.smax) then
           smax=sm1
           ipk=i0+i                   !Frequency offset, DF
           jpk=jpk1
           idfpk=idf
        endif
     enddo

     ref=fs0(ipk+2,jpk) + fs0(ipk+4,jpk) + fs0(ipk+6,jpk)
     j=jpk+4
     if(j.gt.96) j=j-96
     ref=ref + fs0(ipk,j) + fs0(ipk+4,j) + fs0(ipk+6,j)
     j=jpk+8
     if(j.gt.96) j=j-96
     ref=ref + fs0(ipk,j) + fs0(ipk+2,j) + fs0(ipk+4,j)
     j=jpk+12
     if(j.gt.96) j=j-96
     ref=ref + fs0(ipk,j) + fs0(ipk+2,j) + fs0(ipk+6,j)

     ref=ref/3.0                         !Reference level near (DF,DT)
     xsync=smax/ref - 1.0

     smax=0.
     j1=jpk+16
     if(j1.gt.4*nblk) j1=j1-4*nblk
     j2=jpk+20
     if(j2.gt.4*nblk) j2=j2-4*nblk
     do i=ipk+2,ipk+56,2                         !Find User's message length
        ss=fs0(i,j1) + fs0(i+10,j2)
        if(ss.gt.smax) then
           smax=ss
           ipk2=i
        endif
     enddo
     msglen=(ipk2-ipk)/2

     call decdiana(s0,jsym,ipk,jpk,idfpk,msglen,msg,snrx,worst,avg)

     if(worst.gt.bigworst) then
        bigworst=worst
        bigavg=avg
        xsyncbest=xsync
        ipkbest=ipk
        jpkbest=jpk
        idfpkbest=idfpk
        msglenbest=msglen
        msgbest=msg
        snrbest=snrx
     endif

  enddo

  worst=bigworst
  avg=bigavg
  xsync=xsyncbest
  ipk=ipkbest
  jpk=jpkbest
  idfpk=idfpkbest
  msglen=msglenbest
  msg=msgbest
  snrx=snrbest

  dfx=(ipk-i0)*df
  dtx=jpk*kstep/11025.0 - 1.4
  nsnr=nint(snrx)
  if(nsnr.le.-27) then
     nsnr=-27
     msg=' '
  endif

  nworst=10.0*(worst-1.0)
  navg=10.0*(avg-1.0)
  if(nworst.gt.10) nworst=10
  if(navg.gt.10) navg=10
  if(navg.le.0) msg=' '

! Final computation of fs0, using idfpk
  fs0=0.
  do j=1,jb                           !Fold s0 into fs0, modulo 4*nblk
     k=mod(j-1,4*nblk)+1
     ii=nint(idfpk*float(j-jb/2)/float(jb))
     i1=max(1,1-ii)
     i2=min(nq,nq-ii)
     fs0(i1:i2,k)=fs0(i1:i2,k) + s0(i1+ii:i2+ii,j)
  enddo

! Compute ccfred
  do i=ia,ib                          !Search over DF range
     sm1=0.
     do j=0,4*nblk-1                  !Find sync pattern, lags 0-95
        ss=0.
        do n=1,4                      !Sum the four sync tones
           k=j+4*n-3
           if(k.gt.4*nblk) k=k-4*nblk
           ss=ss + fs0(i0+i+2*isync(n),k)
        enddo
        if(ss.gt.sm1) then
           sm1=ss
           jpk1=j+1
        endif
        if(abs(i).le.224) ccfred(i)=sm1
     enddo
  enddo
  ccfred=0.5*ccfred/ref - 1.0

! Compute ccfblue using idfpk and ipk
  do j=0,4*nblk-1
     ss=0.
     do n=1,4
        k=j+4*n-3
        if(k.gt.4*nblk) k=k-4*nblk
        ss=ss + fs0(ipk+2*isync(n),k)
     enddo
     jj=mod(j+80,96) - 5
     ccfblue(jj)=0.5*(ss/ref - 1.0)
!     write(41,3001) j,jj,ccfblue(jj)
!3001 format(2i6,f10.3)
  enddo
!  call flush(41)
!  rewind 41

  return
end subroutine syncdiana
