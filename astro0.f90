subroutine astro0(nyear,month,nday,uth8,nfreq,grid,cauxra,cauxdec,       &
     AzSun8,ElSun8,AzMoon8,ElMoon8,AzMoonB8,ElMoonB8,ntsky,ndop,ndop00,  &
     dbMoon8,RAMoon8,DecMoon8,HA8,Dgrd8,sd8,poloffset8,xnr8,dfdt,dfdt0,  &
     RaAux8,DecAux8,AzAux8,ElAux8,width1,width2,w501,w502,xlst8)

!f2py threadsafe
!f2py intent(in) nyear,month,nday,uth8,nfreq,grid,cauxra,cauxdec
!f2py intent(out) AzSun8,ElSun8,AzMoon8,ElMoon8,AzMoonB8,ElMoonB8,ntsky,ndop,ndop00,dbMoon8,RAMoon8,DecMoon8,HA8,Dgrd8,sd8,poloffset8,xnr8,dfdt,dfdt0,RaAux8,DecAux8,AzAux8,ElAux8,width1,width2,w501,w502,xlst8

  parameter (DEGS=57.2957795130823d0)
  character grid*6
  character*9 cauxra,cauxdec
  real*8 AzSun8,ElSun8,AzMoon8,ElMoon8,AzMoonB8,ElMoonB8,AzAux8,ElAux8
  real*8 dbMoon8,RAMoon8,DecMoon8,HA8,Dgrd8,xnr8,dfdt,dfdt0,dt
  real*8 sd8,poloffset8,day8,width1,width2,w501,w502,xlst8
  include 'gcom2.f90'
  data uth8z/0.d0/,imin0/-99/
  save

  call cs_lock('astro0a')
  auxra=0.
  i=index(cauxra,':')
  if(i.eq.0) then
     read(cauxra,*,err=1,end=1) auxra
  else
     read(cauxra(1:i-1),*,err=1,end=1) ih
     read(cauxra(i+1:i+2),*,err=1,end=1) im
     read(cauxra(i+4:i+5),*,err=1,end=1) is
     auxra=ih + im/60.0 + is/3600.0
  endif
1 auxdec=0.
  i=index(cauxdec,':')
  if(i.eq.0) then
     read(cauxdec,*,err=2,end=2) auxdec
  else
     read(cauxdec(1:i-1),*,err=2,end=2) id
     read(cauxdec(i+1:i+2),*,err=2,end=2) im
     read(cauxdec(i+4:i+5),*,err=2,end=2) is
     auxdec=abs(id) + im/60.0 + is/3600.0
     if(cauxdec(1:1).eq.'-') auxdec=-auxdec
  endif

2 nmode=1
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

  uth=uth8
  call cs_unlock

  call astro(nyear,month,nday,uth,nfreq,hisgrid,2,nmode,1,    &
       AzSun,ElSun,AzMoon,ElMoon,ntsky,doppler00,doppler,            &
       dbMoon,RAMoon,DecMoon,HA,Dgrd,sd,poloffset,xnr,auxra,auxdec,  &
       AzAux,ElAux,day,xlon2,xlat2,xlst)
  AzMoonB8=AzMoon
  ElMoonB8=ElMoon
  call astro(nyear,month,nday,uth,nfreq,grid,1,nmode,1,       &
       AzSun,ElSun,AzMoon,ElMoon,ntsky,doppler00,doppler,            &
       dbMoon,RAMoon,DecMoon,HA,Dgrd,sd,poloffset,xnr,auxra,auxdec,  &
       AzAux,ElAux,day,xlon1,xlat1,xlst)

  day8=day
  xlst8=xlst
  call tm2(day8,xlat1,xlon1,xl1,b1)
  call tm2(day8,xlat2,xlon2,xl2,b2)
  call tm2(day8+1.d0/1440.0,xlat1,xlon1,xl1a,b1a)
  call tm2(day8+1.d0/1440.0,xlat2,xlon2,xl2a,b2a)
  fghz=0.001*nfreq
  dldt1=DEGS*(xl1a-xl1)
  dbdt1=DEGS*(b1a-b1)
  dldt2=DEGS*(xl2a-xl2)
  dbdt2=DEGS*(b2a-b2)
  rate1=2.0*sqrt(dldt1**2 + dbdt1**2)
  width1=0.5*6741*fghz*rate1
  rate2=sqrt((dldt1+dldt2)**2 + (dbdt1+dbdt2)**2)
  width2=0.5*6741*fghz*rate2

  fbend=0.7
  a2=0.0045*log(fghz/fbend)/log(1.05)
  if(fghz.lt.fbend) a2=0.0
  f50=0.19 * (fghz/fbend)**a2
  if(f50.gt.1.0) f50=1.0
  w501=f50*width1
  w502=f50*width2

  RaAux8=auxra
  DecAux8=auxdec
  AzSun8=AzSun
  ElSun8=ElSun
  AzMoon8=AzMoon
  ElMoon8=ElMoon
  dbMoon8=dbMoon
  RAMoon8=RAMoon/15.0
  DecMoon8=DecMoon
  HA8=HA
  Dgrd8=Dgrd
  sd8=sd
  poloffset8=poloffset
  xnr8=xnr
  AzAux8=AzAux
  ElAux8=ElAux
  ndop=nint(doppler)
  ndop00=nint(doppler00)

  if(uth8z.eq.0.d0) then
     uth8z=uth8-1.d0/3600.d0
     dopplerz=doppler
     doppler00z=doppler00
  endif
     
  dt=60.0*(uth8-uth8z)
  if(dt.le.0) dt=1.d0/60.d0
  dfdt=(doppler-dopplerz)/dt
  dfdt0=(doppler00-doppler00z)/dt
  uth8z=uth8
  dopplerz=doppler
  doppler00z=doppler00

  imin=60*uth8
  isec=3600*uth8

  if(isec.ne.isec0 .and. ndecoding.eq.0) then
     call cs_lock('astro0b')
     ih=uth8
     im=mod(imin,60)
     is=mod(isec,60)
     do i=80,1,-1
        if(AzElDir(i:i).ne.' ') goto 700
     enddo
700  jz=i
     open(14,file=AzElDir(:jz)//'/azel.dat',status='unknown',err=930)
     write(14,1010,err=800) ih,im,is,AzMoon,ElMoon,                  &
        ih,im,is,AzSun,ElSun,                                        &
        ih,im,is,AzAux,ElAux,                                        &
        nfreq,doppler,dfdt,doppler00,dfdt0
1010 format(i2.2,':',i2.2,':',i2.2,',',f5.1,',',f5.1,',Moon'/        &
            i2.2,':',i2.2,':',i2.2,',',f5.1,',',f5.1,',Sun'/         &
            i2.2,':',i2.2,':',i2.2,',',f5.1,',',f5.1,',Source'/      &
            i5,',',f8.1,',',f8.2,',',f8.1,',',f8.2,',Doppler')
     close(14)
800  isec0=isec
     elmoon2=elmoon
     call cs_unlock
  endif
  return

930 print*,'Error opening azel.dat'
  stop

end subroutine astro0
