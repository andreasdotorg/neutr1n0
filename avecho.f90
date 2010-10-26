subroutine avecho(fname,ntime,y1,ibuf0,ntc,necho,nfrit,ndither,      &
     dlatency,nsave,f1,nsum,nclearave,ss1,ss2)

  parameter (NBSIZE=1024*2048)
  character*24 fname
  integer*2 y1(NBSIZE)                   !Buffer for Rx data
  real d(28672)                          !Real audio data
  real s1(600)      !Avg spectrum relative to initial Doppler echo freq
  real s2(600)      !Avg spectrum with Dither and changing Doppler removed
  real ss1(-224:224)
  real ss2(-224:224)
  real tmp(600)
  integer nsum      !Number of integrations
  real dop0         !Doppler shift for initial integration (Hz)
  real doppler      !Doppler shift for current integration (Hz)
  real s(8192)
  real x(32770)
  complex c(0:16384)
  equivalence (x,c)
  common/echo/xdop(2),techo,ElMoon,mjd
  save s1,s2,dop0

  if(ibuf0.lt.1) print*,'IBUF0:',ibuf0
  sq=0.
  k=2048*(ibuf0-1)  
  do i=1,14*2048
     k=k+1
     if(k.gt.NBSIZE) k=k-NBSIZE
     d(i)=y1(k)
     sq=sq + d(i)*d(i)
  enddo

  sigdB=db(sq/(14*2048)) - 58.5
  if(sigdB.lt.-99.0) sigdB=-99.0

  if(nclearave.ne.0) nsum=0
  nclearave=0
  if(nsum.eq.0) then
     dop0=2.0*xdop(1)       !Remember the initial Doppler
     s1=0.
     s2=0.
  endif

  doppler=2.0*xdop(1)
!  if(nsave.ne.0) write(26) fname,ntime,dop0,doppler,d(1:28672)

  dt=1.0/11025.0
  df=11025.0/32768.0
  istart=1
  nz=14*2048 + 1 - istart
  x(1:28672)=d(istart:istart+28671)
  x(28673:)=0.0
  call xfft(x,32768)

  fac=1.e-9
  do i=1,8192
     s(i)=fac * (real(c(i))**2 + aimag(c(i))**2)
  enddo

  fnominal=1500.0           !Nominal audio frequency w/o doppler or dither
  ia=nint((fnominal+dop0-nfrit)/df)
  ib=nint((f1+doppler-nfrit)/df)
  if(ia.lt.600 .or. ib.lt.600) go to 900
  if(ia.gt.7590 .or. ib.gt.7590) go to 900

  nsum=nsum+1
  u=1.0/nsum
  if(ntc.lt.1) ntc=1
  if(nsum.gt.10*ntc) u=1.0/(10*ntc)
  do i=1,600
     s1(i)=(1.0-u)*s1(i) + u*s(ia+i-300)  !Center at initial doppler freq
     s2(i)=(1.0-u)*s2(i) + u*s(ib+i-300)  !Center at expected echo freq
     j=i-300
     if(abs(j).le.224) then
        ss1(j)=s1(i)
        ss2(j)=s2(i)
     endif
  enddo
  if(nsave.ne.0) then
     call cs_lock('avecho')
     open(25,file=fname,status='unknown')
     do i=1,600
        write(25,3001) (i-300)*df,s1(i),s2(i)
3001    format(f10.3,2f12.3)
     enddo
     call flush(25)
     close(25)
     call cs_unlock
  endif

  call pctile(s2,tmp,600,50,x0)
  call pctile(s2,tmp,600,84,x1)
  rms=x1-x0
  peak=-1.e30
  do i=1,600
     if(s2(i).gt.peak) then
        peak=s2(i)
        ipk=i
     endif
  enddo

  s2half=0.5*(peak-x0) + x0

  ia=ipk
  ib=ipk
  do i=1,100
     if((ipk-i).lt.1) go to 11
     ia=ipk-i
     if(s2(ia).le.s2half) goto 11
  enddo
11 do i=1,100
     if((ipk+i).gt.600) go to 21
     ib=ipk+i
     if(s2(ib).le.s2half) goto 21
  enddo
21 width=df*(ib-ia-1)

  exchsig=-99.
  if(x0.gt.0.0) echosig=10.0*log10(peak/x0 - 1.0) - 35.7
  echodop=df*(ipk-300)
  snr=0.
  if(rms.gt.0.0) snr=(peak-x0)/rms

  NQual=(snr-2.5)/2.5
  if(nsum.lt.12)  NQual=(snr-3)/3
  if(nsum.lt.8)   NQual=(snr-3)/4
  if(nsum.lt.4)   NQual=(snr-4)/5
  if(nsum.lt.2)   NQual=0
  if(NQual.lt.0)  NQual=0
  if(NQual.gt.10) NQual=10

  call cs_lock('avecho')
  rewind 11
  write(11,1010) nsum,sigdB,echosig,echodop,width,NQual
1010 format(i4,f6.1,f7.1,f8.1,f6.1,i4)
  write(21,1010) nsum,sigdB,echosig,echodop,width,NQual
  call flush(11)
  call flush(21)
  call cs_unlock

900 return
end subroutine avecho
