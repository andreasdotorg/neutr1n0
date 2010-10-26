#------------------------------------------------------ astro
from Tkinter import *
import Pmw
import g

def done():
    g.astro_geom0=root.geometry()
    root.withdraw()

root=Toplevel()
root.withdraw()
root.protocol('WM_DELETE_WINDOW',done)
if g.Win32: root.iconbitmap("wsjt.ico")
root.title("Astronomical data")
frame=Frame(root)
frame.pack()

def astro2(t):
    root.geometry(t)
    root.deiconify()
    root.focus_set()

def update():
    t1= "           Az      El\n" 
    t2= "Moon:    %6.2f  %6.2f\n" % (g.AzMoon,g.ElMoon)
    t3= "Moon/DX: %6.2f  %6.2f\n" % (g.AzMoonB,g.ElMoonB)
    t4= "Sun:     %6.2f  %6.2f\n" % (g.AzSun,g.ElSun)
    t4a="Source:  %6.2f  %6.2f\n\n" % (g.AzAux,g.ElAux)
    t5= "             DX    Self\n"
    t6= "Dop:    %7d %7d\n" % (g.ndop,g.ndop00)
    t7= "df/dt:  %7.2f %7.2f\n" % (g.dfdt,g.dfdt0)
    t7aa="Spread: %7.1f %7.1f\n" % (g.w2,g.w1)
    t7ab="w50:    %7.1f %7.1f\n\n" % (g.w502,g.w501)
    t7a="            RA      DEC\n"
    irah=int(g.RAMoon)
    iram=int(60.0*(g.RAMoon-irah))
    t7b="Moon:     %2.2d:%2.2d  %6.2f\n" % (irah,iram,g.DecMoon)
    irah=int(g.RaAux)
    iram=int(60.0*(g.RaAux-irah))
    t7c="Source:   %2.2d:%2.2d  %6.2f\n\n" % (irah,iram,g.DecAux)
    t8= "Freq:%5d  Tsky:%6d\n" % (g.nfreq,g.ntsky)
    if g.nfreq==2: t8="Freq:  1.8 Tsky:%6d\n" % (g.ntsky)
    if g.nfreq==4: t8="Freq:  3.5 Tsky:%6d\n" % (g.ntsky)
    t9= "MNR: %5.1f  Dgrd:%6.1f\n" % (g.MaxNR,g.Dgrd)
    t10="DPol: %4d  SD:%8.2f\n\n" % (g.poloffset,g.sd)
    t11="LST (h):  %6.3f\n" % (g.xlst,)

    t=t1+t2+t3+t4+t4a+t5+t6+t7+t7aa+t7ab+t7a+t7b+t7c+t8+t9+t10+t11
    lab1.configure(text=t)
    g.astro_geom=root.geometry()
    frame.after(1000,update)

g2font=g.g2font
lab1=Label(frame,font=g2font,justify=LEFT,bg="#66FFFF",
           relief=RIDGE,bd=4,anchor=N)
lab1.pack(ipadx=4)

frame.after(1000,update)
