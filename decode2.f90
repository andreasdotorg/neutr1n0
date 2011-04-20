subroutine decode2

! Get data and parameters from gcom, then call the decoders

  character fnamex*24
  
  include 'gcom1.f90'
  include 'gcom2.f90'
  include 'gcom3.f90'
  include 'gcom4.f90'

! ndecoding  data  Action
!--------------------------------------
!    0             Idle
!    1       d2a   Standard decode, full file
!    2       y1    Mouse pick, top half
!    3       y1    Mouse pick, bottom half
!    4       d2c   Decode recorded file
!    5       d2a   Mouse pick, main window

  lenpick=22050                !Length of FSK441 mouse-picked region
  istart=1.0 + 11025*0.001*npingtime - lenpick/2
  if(npingtime2.ge.npingtime+1000) then
     lenpick=11025*0.001*(npingtime2-npingtime)
     istart=1.0 + 11025*0.001*npingtime
  endif
  if(istart.lt.2) istart=2

  if(ndecoding.eq.1) then
! Normal decoding at end of Rx period (or at t=53s in JT65)
     istart=1
     call decode3(d2a,jza,istart,fnamea)
  else if(ndecoding.eq.2) then

! Mouse pick, top half of waterfall
     if(mode(1:5).eq.'ISCAT' .and. MouseButton.eq.3) then
        lenpick=istart
        istart=1
     endif
! The following is empirical:
     k=2048*ibuf0 + istart - 11025*mod(tbuf(ibuf0),dble(trperiod)) -3850
     if(k.le.0)      k=k+NRxMax
     if(k.gt.NrxMax) k=k-NRxMax
     nt=ntime/86400
     nt=86400*nt + tbuf(ibuf0)
     if(receiving.eq.0) nt=nt-trperiod
     call get_fname(hiscall,iyr,imo,ida,nt,lauto,fnamex)
     do i=1,lenpick
        k=k+1
        if(k.gt.NrxMax) k=k-NRxMax
        d2b(i)=dgain*y1(k)
     enddo
     call decode3(d2b,lenpick,istart,fnamex)
  else if(ndecoding.eq.3) then

!Mouse pick, bottom half of waterfall
     if(mode(1:5).eq.'ISCAT' .and. MouseButton.eq.3) then
        lenpick=istart
        istart=1
     endif
     ib0=ibuf0-161
     if(lauto.eq.1 .and. mute.eq.0 .and. transmitting.eq.1) ib0=ibuf0-323
     if(ib0.lt.1) ib0=ib0+1024
     k=2048*ib0 + istart - 11025*mod(tbuf(ib0),dble(trperiod)) - 3850
     if(k.le.0)      k=k+NRxMax
     if(k.gt.NrxMax) k=k-NRxMax
     nt=ntime/86400
     nt=86400*nt + tbuf(ib0)
     call get_fname(hiscall,iyr,imo,ida,nt,lauto,fnamex)
     do i=1,lenpick
        k=k+1
        if(k.gt.NrxMax) k=k-NRxMax
        d2b(i)=dgain*y1(k)
     enddo
     call decode3(d2b,lenpick,istart,fnamex)

  else if(ndecoding.eq.4) then
! Recorded file
     jzz=jzc
     if(mousebutton.eq.0) istart=1
     if(mousebutton.gt.0) then
        if(mode(1:5).eq.'ISCAT' .and. abs(npingtime2-npingtime).lt.1000)   &
             lenpick=lenpick*2.24
        jzz=lenpick
        if(abs(npingtime2-npingtime).lt.1000) then
           istart=istart + 3300 - jzz/2
           if(istart.lt.2) istart=2
           if(istart+jzz.gt.jzc) istart=jzc-jzz
        endif
     endif

     if(mode(1:5).eq.'ISCAT' .and. mousebutton.eq.3) then
        lenpick=11.025*npingtime
        if(lenpick.gt.jzz) then
           lenpick=jzz
           npingtime=jzz/11.025
        endif
        if(lenpick.lt.24586) lenpick=24586
        istart=1
        call decode3(d2c,lenpick,istart,filename)
     else
        call decode3(d2c(istart),jzz,istart,filename)
     endif

  else if(ndecoding.eq.5) then
! Mouse pick, main window (but not from recorded file)
     istart=istart - 1512
     if(istart.lt.2) istart=2
     if(istart+lenpick.gt.jza) istart=jza-lenpick
     if(mode(1:5).eq.'ISCAT' .and. MouseButton.eq.3) then
        lenpick=istart
        istart=2
        call decode3(d2a,lenpick,istart,fnamea)
     else
        call decode3(d2a(istart),lenpick,istart,fnamea)
     endif
  endif
  fnameb=fnamea

  return

end subroutine decode2
