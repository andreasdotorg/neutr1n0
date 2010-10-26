subroutine syncdiana(fs0,kstep,nfreeze,mousedf,dftolerance,syncx,     &
     ipk,jpk,dfx,dtx,msglen,ccfblue,ccfred)

  real fs0(1024,96)                       !Folded-for-sync spectra
  real ccfblue(-5:540)
  real ccfred(-224:224)
  integer dftolerance
  integer isync(4)
  data isync/8,16,32,24/

  df=11025.0/4096.0
  nblk=24
  i0=2*236
  smax=0.
  ipk=9999
  jpk=9999

  ia=-600.0/df
  ib=600.0/df
  if(nfreeze.eq.1) then
     ia=(mousedf-dftolerance)/df
     ib=(mousedf+dftolerance)/df
  endif

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
     if(sm1.gt.smax) then
        smax=sm1
        ipk=i0+i                   !Frequency offset, DF
        jpk=jpk1
     endif
  enddo

  dfx=(ipk-i0)*df
  dtx=jpk*kstep/11025.0 - 1.4

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
  ccfred=0.5*ccfred/ref - 1.0
  syncx=smax/ref - 1.0

  kk=0
  do j=0,4*nblk-1                     !Compute ccfblue
     ss=0.
     do n=1,4
        k=j+4*n-3
        if(k.gt.4*nblk) k=k-4*nblk
        ss=ss + fs0(ipk+2*isync(n),k)
     enddo
     jj=mod(j+80,96) - 5
     ccfblue(jj)=0.5*(ss/ref - 1.0)
  enddo

  smax=0.
  ja=jpk+16
  if(ja.gt.4*nblk) ja=ja-4*nblk
  jb=jpk+20
  if(jb.gt.4*nblk) jb=jb-4*nblk
  do i=ipk+2,ipk+56,2                         !Find User's message length
     ss=fs0(i,ja) + fs0(i+10,jb)
     if(ss.gt.smax) then
        smax=ss
        ipk2=i
     endif
  enddo
  msglen=(ipk2-ipk)/2

  return
end subroutine syncdiana
