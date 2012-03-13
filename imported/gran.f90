real function gran(idum)
  real r(12)
  if(idum.lt.0) then
     call random_seed
     idum=0
  endif
  call random_number(r)
  gran=sum(r)-6.0
end function gran
