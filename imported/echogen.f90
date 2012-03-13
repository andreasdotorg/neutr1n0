subroutine echogen(mode_echo,dither,iwave,nwave,f1)

  parameter (NMAX=44100)            !Length of wave file, 4.0 seconds
  real dither                       !Amount to dither f1
  integer*2 iwave(NMAX)             !Wave file to be generated
  integer nwave                     !Length of wave file
  real f1                           !Generated audio frequency
  real*8 dt,pha,dpha,twopi,f,df
  integer ic27(27)
  data ic27/1,3,7,15,2,5,11,23,18,8,17,6,13,27,26,24,20,12,25,22,   &
       16,4,9,19,10,21,14/

  twopi=8*atan(1.d0)
  dt=1.d0/11025.d0
  df=11025.d0/890.d0

  if(mode_echo.ne.0) then
     pha=0.d0
     k=0
     do j=1,27
        f=1500.d0 + (ic27(j)-14)*df
        dpha=twopi*f*dt
        do i=1,890
           pha=pha+dpha
           k=k+1
           iwave(k)=nint(32767.0*sin(pha))
        enddo
     enddo
     do i=1,20
        pha=pha+dpha
        k=k+1
        iwave(k)=nint(32767.0*sin(pha))
        if(abs(iwave(k)).lt.3000) go to 10
     enddo
10   iwave(k+1:)=0
     f1=1500.0
  else
     call random_number(r)
     f1=1500 + dither*(r-0.5)          !Define the TX frequency
     dpha=twopi*dt*f1
     pha=0.
     do i=1,NMAX
        pha=pha+dpha
        iwave(i)=nint(32767.0*sin(pha))
     enddo
  endif
  nwave=NMAX

  return
end subroutine echogen
