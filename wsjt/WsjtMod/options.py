#------------------------------------------------------ options
from Tkinter import *
import Pmw
import g

def done():
    root.withdraw()

root=Toplevel()
root.withdraw()
root.protocol('WM_DELETE_WINDOW',done)
if g.Win32: root.iconbitmap("wsjt.ico")
root.title("Options")

def options2(t):
    root.geometry(t)
    root.deiconify()
    root.focus_set()

#-------------------------------------------------------- Create GUI widgets
g1=Pmw.Group(root,tag_text="Station parameters")
MyCall=StringVar()
MyGrid=StringVar()
#RxDelay=StringVar()
#TxDelay=StringVar()
HighPri=IntVar()
IDinterval=IntVar()
ComPort=IntVar()
PttPort=StringVar()
ndevin=IntVar()
ndevout=IntVar()
DevinName=StringVar()
DevoutName=StringVar()
samfacin=DoubleVar()
samfacout=DoubleVar()
Template1=StringVar()
Template2=StringVar()
Template3=StringVar()
Template4=StringVar()
Template5=StringVar()
Template6=StringVar()
addpfx=StringVar()
auxra=StringVar()
auxdec=StringVar()
azeldir=StringVar()
##necho=IntVar()
##dlatency=DoubleVar()
ntc=IntVar()
fRIT=IntVar()
dither=IntVar()
temp=StringVar()
wind=StringVar()
pwr=StringVar()
ant=StringVar()
mytag=IntVar()
histag=IntVar()
genmsg=IntVar()

ntc.set(1)

def resetgen():
    defaults()
    genmsg.set(1)

def defaults():
    t=''
    if mytag.get()==1: t=' %S'
    if histag.get()==1: t=' %H'
    if (ireport.get()==0 and iregion.get()==0):
        tx1.delete(0,END)
        tx1.insert(0,'%T %M')
        tx2.delete(0,END)
        tx2.insert(0,'%T %M %R')
        tx3.delete(0,END)
        tx3.insert(0,'R%R'+t)
        tx4.delete(0,END)
        tx4.insert(0,'RRR'+t)
        tx5.delete(0,END)
        tx5.insert(0,'73'+t)
        tx6.delete(0,END)
        tx6.insert(0,'CQ %M')
    elif (ireport.get()==1 and iregion.get()==0):
        tx1.delete(0,END)
        tx1.insert(0,'%T %M')
        tx2.delete(0,END)
        tx2.insert(0,'%T %M %G')
        tx3.delete(0,END)
        tx3.insert(0,'RR %G'+t)
        tx4.delete(0,END)
        tx4.insert(0,'RRR'+t)
        tx5.delete(0,END)
        tx5.insert(0,'73'+t)
        tx6.delete(0,END)
        tx6.insert(0,'CQ %M')

    elif (ireport.get()==0 and iregion.get()==1):
        tx1.delete(0,END)
        tx1.insert(0,'%T %M')
        tx2.delete(0,END)
        tx2.insert(0,'%T %M %R %R')
        tx3.delete(0,END)
        tx3.insert(0,'%T %M R%R R%R')
        tx4.delete(0,END)
        tx4.insert(0,'RRRR RRRR %M')
        tx5.delete(0,END)
        tx5.insert(0,'73 %M')
        tx6.delete(0,END)
        tx6.insert(0,'CQ %M')

    elif (ireport.get()==1 and iregion.get()==1):
        tx1.delete(0,END)
        tx1.insert(0,'%T %M')
        tx2.delete(0,END)
        tx2.insert(0,'%T %M %G')
        tx3.delete(0,END)
        tx3.insert(0,'%T %M RR %G')
        tx4.delete(0,END)
        tx4.insert(0,'RRRR RRRR %M')
        tx5.delete(0,END)
        tx5.insert(0,'73 %M')
        tx6.delete(0,END)
        tx6.insert(0,'CQ %M')

#------------------------------------------------------ setMyTag
def setMyTag(event=NONE):
    if(mytag.get()==1): histag.set(0)

#------------------------------------------------------ setHisTag
def setHisTag(event=NONE):
    if(histag.get()==1): mytag.set(0)

mycall=Pmw.EntryField(g1.interior(),labelpos=W,label_text='My Call:',
        value='K1JT',entry_textvariable=MyCall,entry_width=12)
mygrid=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Grid Locator:',
        value='FN20qi',entry_textvariable=MyGrid,entry_width=12)
##rxdelay=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Rx Delay (s):',
##        value='0.2',entry_textvariable=RxDelay)
##txdelay=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Tx Delay (s):',
##        value='0.2',entry_textvariable=TxDelay)
idinterval=Pmw.EntryField(g1.interior(),labelpos=W,label_text='ID Interval (m):',
        value=10,entry_textvariable=IDinterval,entry_width=12)
comport=Pmw.EntryField(g1.interior(),labelpos=W,label_text='PTT Port:',
        value='/dev/ttyS0',entry_textvariable=PttPort,entry_width=12)
audioin=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Audio In:',
        value='0',entry_textvariable=DevinName,entry_width=12)
audioout=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Audio Out:',
        value='0',entry_textvariable=DevoutName,entry_width=12)
ratein=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Rate In:',
        value=1.0000,entry_textvariable=samfacin,entry_width=12)
rateout=Pmw.EntryField(g1.interior(),labelpos=W,label_text='Rate Out:',
        value=1.0000,entry_textvariable=samfacout,entry_width=12)

#widgets = (mycall, mygrid, rxdelay,txdelay,idinterval,comport,audioin,audioout)
widgets = (mycall, mygrid,idinterval,comport,audioin,audioout,ratein,rateout)
for widget in widgets:
    widget.pack(fill=X,expand=1,padx=10,pady=2)

Pmw.alignlabels(widgets)
mycall.component('entry').focus_set()

f0=Frame(g1.interior(),width=100,height=15)
ndtr=IntVar()
Label(f0,text='PTT line:  ').pack(side=LEFT)
rb7=Radiobutton(f0,text='DTR',value=1,variable=ndtr)
rb8=Radiobutton(f0,text='RTS',value=0,variable=ndtr)
rb7.pack(anchor=W,side=LEFT,padx=2,pady=2)
rb8.pack(anchor=W,side=LEFT,padx=2,pady=2)
f0.pack()

f1=Frame(g1.interior(),width=100,height=15)
mileskm=IntVar()
Label(f1,text='Distance unit:').pack(side=LEFT)
rb5=Radiobutton(f1,text='mi',value=0,variable=mileskm)
rb6=Radiobutton(f1,text='km',value=1,variable=mileskm)
rb5.pack(anchor=W,side=LEFT,padx=2,pady=2)
rb6.pack(anchor=W,side=LEFT,padx=2,pady=2)
f1.pack()

g2=Pmw.Group(root,tag_text="Message templates for FSK441, ISCAT")
f2=Frame(g2.interior(),width=100,height=20)
f2a=Frame(f2,width=50,height=20,bd=2,relief=GROOVE)
f2a.pack(side=LEFT,padx=6,pady=6,fill=Y)
f2b=Frame(f2,width=50,height=20,bd=2,relief=GROOVE)
f2b.pack(side=LEFT,padx=6,pady=6)
iregion=IntVar()
rb4=Radiobutton(f2a,text='EU',value=1,variable=iregion)
rb4.pack(anchor=W,side=LEFT,padx=2,pady=2)
rb3=Radiobutton(f2a,text='NA',value=0,variable=iregion)
rb3.pack(anchor=W,side=LEFT,padx=2,pady=2)

ireport=IntVar()
rb1=Radiobutton(f2b,text='Report',value=0,variable=ireport)
rb2=Radiobutton(f2b,text='Grid',value=1,variable=ireport)
rb1.grid(column=0,row=0)
rb2.grid(column=1,row=0)
cb1=Checkbutton(f2b,text='My tag',variable=mytag,command=setMyTag)
cb1.grid(column=0,row=2)
cb2=Checkbutton(f2b,text='His tag',variable=histag,command=setHisTag)
cb2.grid(column=1,row=2)

f2.pack()

f3=Frame(g2.interior(),width=100,height=20)
Button(f3,text="   Reset   ",command=defaults).pack(side=LEFT,padx=6,pady=6)
Button(f3,text="Reset and Gen Msgs",command=resetgen).pack(side=LEFT,padx=6,pady=6)
f3.pack()

tx1=Pmw.EntryField(g2.interior(),labelpos=W,label_text='Tx 1:',
                   entry_textvariable=Template1)
tx2=Pmw.EntryField(g2.interior(),labelpos=W,label_text='Tx 2:',
                   entry_textvariable=Template2)
tx3=Pmw.EntryField(g2.interior(),labelpos=W,label_text='Tx 3:',
                   entry_textvariable=Template3)
tx4=Pmw.EntryField(g2.interior(),labelpos=W,label_text='Tx 4:',
                   entry_textvariable=Template4)
tx5=Pmw.EntryField(g2.interior(),labelpos=W,label_text='Tx 5:',
                   entry_textvariable=Template5)
tx6=Pmw.EntryField(g2.interior(),labelpos=W,label_text='Tx 6:',
                   entry_textvariable=Template6)

messages=(tx1,tx2,tx3,tx4,tx5,tx6)
for m in messages:
    m.pack(fill=X,expand=1,padx=10,pady=2)

#g3=Pmw.Group(root)
g3=Pmw.Group(root,tag_text="Miscellaneous")
temp_prefix=Pmw.EntryField(g3.interior(),labelpos=W,label_text='DXCC prefix:',
    entry_width=9,entry_textvariable=addpfx)
aux_ra=Pmw.EntryField(g3.interior(),labelpos=W,label_text='Source RA:',
    entry_width=9,entry_textvariable=auxra)
aux_dec=Pmw.EntryField(g3.interior(),labelpos=W,label_text='Source DEC:',
    entry_width=9,entry_textvariable=auxdec)
azeldir_entry=Pmw.EntryField(g3.interior(),labelpos=W,label_text='AzElDir:',
    entry_width=9,value=g.appdir,entry_textvariable=azeldir)
ntc_entry=Pmw.EntryField(g3.interior(),labelpos=W,label_text='Echo Avg (m):',
    entry_width=9,entry_textvariable=ntc)
##necho_entry=Pmw.EntryField(g3.interior(),labelpos=W,label_text='Echo waveform:',
##    entry_width=9,entry_textvariable=necho)
fRIT_entry=Pmw.EntryField(g3.interior(),labelpos=W,label_text='RIT (Hz):',
    entry_width=9,entry_textvariable=fRIT)
dither_entry=Pmw.EntryField(g3.interior(),labelpos=W,label_text='Dither (Hz):',
    entry_width=9,entry_textvariable=dither)
##dlatency_entry=Pmw.EntryField(g3.interior(),labelpos=W,label_text='Latency (s):',
##    entry_width=9,entry_textvariable=dlatency)

widgets = (temp_prefix,aux_ra,aux_dec,azeldir_entry,ntc_entry, \
           fRIT_entry,dither_entry)
for widget in widgets:
    widget.pack(padx=10,pady=2)
Pmw.alignlabels(widgets)

g1.pack(side=LEFT,fill=BOTH,expand=1,padx=6,pady=6)
g2.pack(side=LEFT,fill=BOTH,expand=1,padx=6,pady=6)
g3.pack(side=LEFT,fill=BOTH,expand=1,padx=6,pady=6)

