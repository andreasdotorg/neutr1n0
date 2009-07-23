subroutine get_fname(hiscall,iyr,imo,ida,ntime,lauto,fname)

  character hiscall*12,fname*24,tag*7
  integer ntime

  nsec=mod(ntime,86400)
  ihr=nsec/3600
  imin=mod(nsec/60,60)
  isec=mod(nsec,60)
  call cs_lock('get_fname')
  write(fname,1000) iyr-2000,imo,ida,ihr,imin,isec
1000 format('_',3i2.2,'_',3i2.2,'.WAV')
  call cs_unlock
  tag=hiscall(1:7)
  i=index(hiscall,'/')
  if(i.ge.5) tag=hiscall(1:i-1)
  if(i.ge.2.and.i.le.4) tag=hiscall(i+1:)
  if(hiscall(1:1).eq.' ' .or. lauto.eq.0) tag='Mon'
  i=index(tag,' ')
  fname=tag(1:i-1)//fname
  
  return
end subroutine get_fname
