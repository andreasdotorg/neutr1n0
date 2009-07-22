subroutine azdist0(MyGrid,HisGrid,utch,nAz,nEl,nDmiles,nDkm,nHotAz,nHotABetter)
  character*6 MyGrid,HisGrid
  real*8 utch
!f2py intent(in) MyGrid,HisGrid,utch
!f2py intent(out) nAz,nEl,nDmiles,nDkm,nHotAz,nHotABetter

  call cs_lock('azdist0')
  if(hisgrid(5:5).eq.' ' .or. ichar(hisgrid(5:5)).eq.0) hisgrid(5:5)='m'
  if(hisgrid(6:6).eq.' ' .or. ichar(hisgrid(6:6)).eq.0) hisgrid(6:6)='m'
  call azdist(MyGrid,HisGrid,utch,nAz,nEl,nDmiles,nDkm,nHotAz,nHotABetter)
  call cs_unlock

  return
end subroutine azdist0
