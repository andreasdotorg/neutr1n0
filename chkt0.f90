subroutine chkt0(nplain,naddon,ndiff)

!f2py threadsafe
!f2py intent(out) nplain,naddon,ndiff
  integer dgen(12)
  character*22 t0msg2
  include 'gcom2.f90'

  call cs_lock('chkt0')
  call packmsg(t0msg,dgen)
  call unpackmsg(dgen,t0msg2)
  if(index(t0msg,'/').gt.0) then
     naddon=1
  else
     naddon=0
  endif
  if(iand(dgen(10),8).ne.0) then
     nplain=1
     naddon=0
  else
     nplain=0
  endif
  if(t0msg2.ne.t0msg) then
     ndiff=1
  else
     ndiff=0
  endif
  call cs_unlock

  return
end subroutine chkt0
