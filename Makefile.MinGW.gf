# Makefile for Windows
# !include <dfinc.mak>   #Some definitions for Compaq Visual Fortran
CC = /mingw/bin/gcc
#FC = /mingw/bin/g95
FC = gfortran

# FFLAGS = -O2
CFLAGS = -I. -fbounds-check

all:    JT65code.exe WSJT7.EXE

OBJS1 = JT65code.o nchar.o grid2deg.o packmsg.o packtext.o \
	packcall.o packgrid.o unpackmsg.o unpacktext.o \
	unpackcall.o unpackgrid.o deg2grid.o packdxcc.o \
	chkmsg.o getpfx1.o getpfx2.o k2grid.o grid2k.o \
	interleave63.o graycode.o set.o igray.o \
	init_rs.o encode_rs.o decode_rs.o \
	wrapkarn.o

JT65code.exe: $(OBJS1)
	$(FC) $(FFLAGS) -o JT65code.exe $(OBJS1)

OBJS2C   = init_rs.o encode_rs.o decode_rs.o jtaudio.o fano.o \
	tab.o nhash.o

F2PYONLY = ftn_init ftn_quit audio_init spec getfile azdist0 astro0 chkt0

SRCS2F90 = a2d.f90 abc441.F90 astro0.F90 audio_init.F90 azdist0.f90 \
	blanker.f90 decode1.F90 decode2.f90 decode3.F90 ftn_init.F90 \
	ftn_quit.f90 get_fname.F90 getfile.F90 horizspec.f90 hscroll.f90 \
	pix2d.f90 pix2d65.f90 rfile.f90 savedata.F90 spec.f90 \
	wsjtgen.F90 runqqq.F90 fivehz.F90 msgparms.f90 chkt0.f90 \
	genwspr.f90 wqencode.f90 wqdecode.f90 packpfx.f90 unpackpfx.f90 \
	packname.f90 unpackname.f90 packtext2.f90 unpacktext2.f90 \
	packprop.f90 unpackprop.f90 hash.f90 wsjtwspr.f90 gen64.f90

SRCS2F77 = wsjt1.f avesp2.f bzap.f spec441.f spec2d.f mtdecode.f \
	stdecode.f indexx.f s2shape.f flat2.f gen65.f gen24.f entail.f \
	genmet.f wsjt24.f sync24.f ps24.f fourt.f xcor24.f decode24.f\
	chkmsg.f gen6m.f interleave24.f \
	gentone.f syncf0.f syncf1.f synct.f decode6m.f avemsg6m.f \
	set.f flatten.f db.f pctile.f sort.f ssort.f ps.f smooth.f ping.f \
	longx.f peakup.f sync.f detect.f avemsg65.f decode65.f demod64a.f \
	encode65.f extract.f chkhist.f flat1.f four2.f gencw.f \
	gencwid.f msgtype.f getpfx1.f \
	getpfx2.f getsnr.f graycode.f grid2k.f interleave63.f k2grid.f \
	limit.f lpf1.f deep65.f morse.f nchar.f packcall.f packgrid.f \
	packmsg.f packtext.f setup65.f short65.f slope.f spec2d65.f \
	sync65.f unpackcall.f unpackgrid.f unpackmsg.f unpacktext.f \
	xcor.f xfft.f xfft2.f wsjt65.f astro.f azdist.f coord.f dcoord.f \
	deg2grid.f dot.f ftsky.f geocentric.f GeoDist.f grid2deg.f \
	moon2.f MoonDop.f sun.f toxyz.f pfxdump.f \
	ftpeak65.f fil651.f fil652.f fil653.f symsync65.f \
	rfile2.f encode232.f inter_mept.f pack50.f unpack50.f \
	filbig2.F mept162a.f twkfreq.f sync162.f decode162.f \
	ps162.f fchisq.f fano232.f ccf2.f wsjt64.f sync64.f


SRCS2C   = resample.c ptt.c igray.c wrapkarn.c start_portaudio.c \
	cutil.c w21.c azelout.c
WSJT7.EXE: WsjtMod/Audio.pyd wsjt.spec
	c:/python25/python c:/python25/pyinstaller-1.3/Build.py wsjt.spec
	mv wsjt.exe WSJT7.EXE

WsjtMod/Audio.pyd: $(OBJS2C) $(SRCS2F90) $(SRCS2F77) $(SRCS2C) 
	c:/python25/python c:/python25/scripts/f2py.py -c -I. \
	--quiet --fcompiler=$(FC) \
	--opt="-fbounds-check -O2 -DUSE_PORTAUDIO -fno-range-check" \
	--compiler=mingw32 \
	$(OBJS2C) \
	libportaudio.a libfftw3f.a libsamplerate.a libpthreadGC2.a -lwinmm  \
	-m Audio \
	only: $(F2PYONLY) : \
	$(SRCS2F90) $(SRCS2F77) $(SRCS2C)
	mv Audio.pyd WsjtMod/Audio.pyd

wsjt.spec: wsjt.py WsjtMod/astro.py WsjtMod/g.py WsjtMod/options.py \
	WsjtMod/palettes.py WsjtMod/smeter.py WsjtMod/specjt.py
	c:/python25/python c:/python25/pyinstaller-1.3/makespec.py --icon \
	   wsjt.ico --tk --onefile wsjt.py

jtaudio.o: jtaudio.c
	$(CC) $(CFLAGS) -c -DWin32 jtaudio.c

init_rs.o: init_rs.c
	$(CC) $(CFLAGS) -c -DBIGSYM=1 init_rs.c
encode_rs.o: encode_rs.c
	$(CC) $(CFLAGS) -c -DBIGSYM=1 encode_rs.c
decode_rs.o: decode_rs.c
	$(CC) $(CFLAGS) -c -DBIGSYM=1 decode_rs.c

.PHONY : clean

clean:
	rm *.o JT65code.exe wsjt7.exe WsjtMod/Audio.pyd
