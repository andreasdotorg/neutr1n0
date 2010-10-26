subroutine gendiana(msg,msglen,samfac,iwave,nwave,msgsent,sendingsh)

! Generate waveform for Diana mode.

  parameter (NMAX=30*11025,NSZ=126,NSPS=2048)
  character msg*28,msgsent*28
  integer*2 iwave(NMAX)
  integer imsg(28)
  integer itone(NSZ)
  character c42*42
  real*8 twopi,dt,f0,f,df,pha,dpha,samfac
  integer isync(4)                              !Sync pattern
  integer sendingsh
  data isync/8,16,32,24/
  data nsync/4/,nlen/2/,ndat/18/
  data c42/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ /.?+-'/

  twopi=8.d0*atan(1.d0)
  df=11025.d0/NSPS                     !5.383 Hz
  dt=1.d0/(samfac*11025.d0)
  f0=236*df                            !1270.46 Hz
  nsym=126                             !Total symbols in whole transmission

  nblk=nsync+nlen+ndat
  k=0
  kk=1
  do i=1,msglen                        !Define tone sequence for user message
     imsg(i)=index(c42,msg(i:i))-1     !Get character index from c42
     if(imsg(i).lt.0) imsg(i)=36       !Default char is <space>
  enddo

  do i=1,nsym
     j=mod(i-1,nblk)+1
     if(j.le.nsync) then
        itone(i)=isync(j)
     else if(j.gt.nsync .and. j.le.nsync+nlen) then
        itone(i)=msglen
        if(j.ge.nsync+2) then
           n=msglen + 5*(j-nsync-1)
           if(n.gt.41) n=n-42
           itone(i)=n
        endif
     else
        k=k+1
        kk=mod(k-1,msglen)+1
        irpt=(i-1)/nblk
        itone(i)=mod(imsg(kk) + 7*irpt,42)
     endif
  enddo
  msgsent=msg

  k=0
  pha=0.
  do m=1,nsym                                    !Generate iwave
     f=f0 + itone(m)*df
     dpha=twopi*f*dt
     do i=1,NSPS
        k=k+1
        pha=pha+dpha
        iwave(k)=nint(32767.0*sin(pha))
     enddo
  enddo
  nwave=k

  return
end subroutine gendiana
