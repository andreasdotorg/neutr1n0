      subroutine pfxdump(fname)
      character*(*) fname
      include 'pfx.f'

      open(11,file=fname,status='unknown')
      write(11,1001) 
 1001 format(
     +  'Supported Suffixes:    /0 /1 /2 /3 /4 /5 /6 /7 /8 /9 /A /P')
      write(11,1002) 
 1002 format(/'Supported Add-On DXCC Prefixes:')
      write(11,1003) pfx
 1003 format(15a6)
      close(11)

      return
      end
