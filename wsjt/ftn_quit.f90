subroutine ftn_quit
!f2py threadsafe
  call four2a(a,-1,-1,1,1)
  call cs_destroy
  return
end subroutine ftn_quit
