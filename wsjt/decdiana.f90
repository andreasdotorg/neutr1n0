subroutine decdiana(s0,jsym,ipk,jpk,idfpk,msglen,msg,snrx,worst,avg)

  parameter (NSZ=646)
  real s0(1152,NSZ)
  real fs1(0:41,28)
  real tmp(1176)
  real tmp2(1176)
  character msg*28
  character c42*42
  data c42/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ /.?+-'/

  nblk=24
  jb=jsym
  fs1=0.
  k=0
  n=0
  do j=jpk,jpk+4*125,4                !Fold from s0 into fs1, modulo msglen
     k=k+1
     km=mod(k-1,nblk)+1
     if(km.gt.6) then    !Use only message symbols
        n=n+1
        m=mod(n-1,msglen)+1
        iblk=(j-jpk)/(4*nblk)         !iblk runs from 0 to 4
        ioffset=7*iblk
        ii=nint(idfpk*float(j-jb/2)/float(jb))
        do i=0,41
           iii=i+ioffset
           if(iii.ge.42) iii=iii-42
           i3=ipk + 2*iii + ii
           fs1(i,m)=fs1(i,m) + s0(i3,j)
        enddo
     endif
  enddo

  k=0
  do j=1,msglen
     do i=0,41
        k=k+1
        tmp(k)=fs1(i,j)
     enddo
  enddo
  kz=k
  call pctile(tmp,tmp2,kz,47,base)
  fs1=fs1/base - 1.0

  msg='                            '
  worst=9999.
  sum1=0.
  sum2=0.
  do m=1,msglen                           !Read out message contents
     smax1=0.
     do i=0,41
        if(fs1(i,m).gt.smax1) then         !Find highest peak
           smax1=fs1(i,m)
           ipk3=i
        endif
     enddo
     sum1=sum1+smax1

     smax2=0.
     do i=0,41
        if(fs1(i,m).gt.smax2 .and. i.ne.ipk3) then  !Find 2nd highest peak
           smax2=fs1(i,m)
        endif
     enddo
     sum2=sum2+smax2
     rr=smax1/smax2                       !Reliability indicator
     if(rr.lt.worst) worst=rr
     msg(m:m)=c42(ipk3+1:ipk3+1)          !Insert decoded char in msg
  enddo
  
  ave1=sum1/msglen                        !Average signal
  ave2=sum2/msglen                        !Average second-best
  avg=sum1/sum2                        !Average reliability indicator
  snrx=db(ave1) - 26.7 + 1.0              !Last number is empirical

  return
end subroutine decdiana
