      subroutine msgtype(msg,ntype,nrpt1,nrpt2,s1,s2)
      character*22 msg,s1,s2

      i1=-99
      i2=-99
      i3=-99
      i4=-99

      i1=index(msg,'[')
      if(i1.le.0) then
         s1=msg
         s2='                      '
         nrpt1=100
         nrpt2=0
         go to 100
      else if(i1.eq.1) then
         i2=index(msg,']')
         s1=msg(i1+1:i2-1)
         nrpt1=100
         s2=msg(i2+2:)
         nrpt2=0
         if(s2(1:1).ne.' ') nrpt2=1
      else if(i1.gt.1) then
         s1=msg(1:i1-2)
         nrpt1=1
         i2=index(msg,']')
         s2=msg(i1+1:i2-1)
         nrpt2=100
      endif
      i3=index(s2,'[')
      if(i3.le.0) go to 100
      i4=index(s2,']')
      s2=s2(2:i4-1)
      nrpt2=100
      if(nrpt1.eq.100 .and. nrpt2.eq.100) then
         nrpt1=75
         nrpt2=25
      endif

 100  ntype=1
      if(nrpt1.eq.1 .and. nrpt2.eq.100) ntype=2
      if(nrpt1.eq.75 .and. nrpt2.eq.25) ntype=3

      return
      end
