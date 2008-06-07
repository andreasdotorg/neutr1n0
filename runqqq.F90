subroutine runqqq(fname,cmnd,iret)

#ifdef CVF
  use dflib
#endif
  integer system

  character*(*) fname,cmnd

#ifdef CVF
  iret=runqq(fname,cmnd)
#else
  iret=system('./KVASD_g95 -q > dev_null')
#endif

  return
end subroutine runqqq

subroutine flushqqq(lu)

#ifdef CVF
  use dfport
#endif

  call flush(lu)

  return
end subroutine flushqqq

subroutine sleepqqq(n)
#ifdef CVF
  use dflib
      call sleepqq(n)
#else
      call usleep(n*1000)
#endif

  return

end subroutine sleepqqq
