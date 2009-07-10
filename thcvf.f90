subroutine cs_init(mtx)
  use dfmt
  type (RTL_CRITICAL_SECTION) ncrit1
  mtx=loc(ncrit1)
  call InitializeCriticalSection(mtx)
  return
end subroutine cs_init

subroutine cs_destroy(mtx)
  use dfmt
  type (RTL_CRITICAL_SECTION) ncrit1
  call DeleteCriticalSection(mtx)
  return
end subroutine cs_destroy

subroutine th_create(sub)
  use dfmt
  external sub
  ith=CreateThread(0,0,sub,0,0,id)
  return
end subroutine th_create

subroutine th_exit
  use dfmt
  call ExitThread(ncode)
  return
end subroutine th_exit

subroutine cs_lock(mtx)
  use dfmt
  call EnterCriticalSection(mtx)
  return
end subroutine cs_lock

subroutine cs_unlock(mtx)
  use dfmt
  call LeaveCriticalSection(mtx)
  return
end subroutine cs_unlock
