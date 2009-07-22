subroutine ftn_quit
  call four2a(a,-1,1,1,1)
  call filbig2(dat,-1,f0,newdat2,c4a,n4)
  call cs_destroy
  return
end subroutine ftn_quit
