subroutine iscat(cdat,npts,t2,cfile6,MinSigdB,DFTolerance,NFreeze,   &
     MouseDF,mousebutton,mode4,nafc,psavg)

! Decode an ISCAT signal

  parameter (NMAX=34*11025)
  parameter (NSZ=4*1400)
  character cfile6*6                      !File time
  character c42*42
  character msg*29,msg1*29
  character csync*1
  real s0(288,NSZ)
  real fs1(0:41,30)
  real psavg(72)                          !Average spectrum of whole file
  integer dftolerance
  integer icos(4)
  data icos/0,1,3,2/
  data nsync/4/,nlen/2/,ndat/18/
  data c42/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ /.?@-'/

! Compute symbol spectra and establish sync:
  call synciscat(cdat,npts,s0,jsym,df,MinSigdB,DFTolerance,NFreeze,    &
       MouseDF,mousebutton,mode4,nafc,psavg,xsync,nsig,ndf0,msglen,    &
       ipk,jpk,idf,df1)

  if(nsig.lt.MinSigdB .or. xsync.le.1.0) then
     msglen=0
     worst=1.
     avg=1.
     xsync=0.
     ndf0=0
     go to 100
  endif

  ipk3=0                                  !Silence compiler warning
  nblk=nsync+nlen+ndat
  fs1=0.
  jb=(jsym-4*nblk+1)/4
  jb=4*jb
  k=0
  n=0
  do j=jpk,jsym,4                         !Fold information symbols into fs1
     k=k+1
     if(mod(k-1,nblk)+1.gt.6) then
        n=n+1
        m=mod(n-1,msglen)+1
        ii=nint(idf*float(j-jb/2)/float(jb))
        do i=0,41
           iii=ii+ipk+2*i
           if(iii.ge.1 .and. iii.le.288) fs1(i,m)=fs1(i,m) + s0(iii,j)
        enddo
     endif
  enddo

! Read out the message:
  msg1='                            '
  mpk=0
  worst=9999.
  sum=0.
  do m=1,msglen
     smax=0.
     smax2=0.
     do i=0,41
        if(fs1(i,m).gt.smax) then
           smax=fs1(i,m)
           ipk3=i
        endif
     enddo
     do i=0,41
        if(fs1(i,m).gt.smax2 .and. i.ne.ipk3) smax2=fs1(i,m)
     enddo
     rr=smax/smax2
     sum=sum + rr
     if(rr.lt.worst) worst=rr
     if(ipk3.eq.40) mpk=m
     msg1(m:m)=c42(ipk3+1:ipk3+1)
  enddo

  avg=sum/msglen
  if(mpk.eq.1) then
     msg=msg1(2:)
  else if(mpk.lt.msglen) then
     msg=msg1(mpk+1:msglen)//msg1(1:mpk-1)
  else
     msg=msg1(1:msglen-1)
  endif

100 nworst=10.0*(worst-1.0)
  navg=10.0*(avg-1.0)
  if(nworst.gt.10) nworst=10
  if(navg.gt.10) navg=10
  isync=xsync
  if(navg.le.0) msg=' '
  csync=' '
  if(isync.ge.1) csync='*'
  nfdot=nint(idf*df1)

  call cs_lock('iscat')
  write(11,1020) cfile6,isync,nsig,t2,ndf0,nfdot,csync,msg,msglen,nworst,navg
  write(21,1020) cfile6,isync,nsig,t2,ndf0,nfdot,csync,msg,msglen,nworst,navg
1020 format(a6,2i4,f5.1,i5,i4,1x,a1,2x,a28,i4,2i3)
  call cs_unlock

  return
end subroutine iscat
