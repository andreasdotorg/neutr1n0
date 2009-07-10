subroutine cs_init(mtx)
  call fthread_mutex_init(mtx)
  return
end subroutine cs_init

subroutine cs_destroy(mtx)
  call fthread_mutex_destroy(mtx)
  return
end subroutine cs_destroy

subroutine th_create(sub)
  call fthread_create(sub,id)
  return
end subroutine th_create

subroutine th_exit
  call fthread_exit
  return
end subroutine th_exit

subroutine cs_lock(mtx)
  call fthread_mutex_lock(mtx)
  return
end subroutine cs_lock

subroutine cs_unlock(mtx)
  call fthread_mutex_unlock(mtx)
  return
end subroutine cs_unlock
