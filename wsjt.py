#!/usr/bin/env python
#------------------------------------------------------------------- WSJT
# $Date$ $Revision$
#
from Tkinter import *
from tkFileDialog import *
from WsjtMod import Pmw
import tkMessageBox
from WsjtMod import g
import os,time
from WsjtMod import Audio
from math import log10
try:
    from numpy.oldnumeric import zeros
#    print "importing from numpy"
except: 
    from Numeric import zeros
#    print "importing from Numeric"
import dircache
import Image,ImageTk  #, ImageDraw
from WsjtMod.palettes import colormapblue, colormapgray0, colormapHot, \
     colormapAFMHot, colormapgray1, colormapLinrad, Colormap2Palette
from types import *
import array
import thread
import webbrowser

root = Tk()
Version="9.1 r" + "$Rev$"[6:-1]
print "******************************************************************"
print "WSJT Version " + Version + ", by K1JT"
print "Revision date: " + \
      "$Date$"[7:-1]
print "Run date:   " + time.asctime(time.gmtime()) + " UTC"

#See if we are running in Windows
g.Win32=0
if sys.platform=="win32":
    g.Win32=1
    try:
        root.option_readfile('wsjtrc.win')
    except:
        pass
else:
    try:
        root.option_readfile('wsjtrc')
    except:
        pass
root_geom=""

#------------------------------------------------------ Global variables
appdir=os.getcwd()
Audio.gcom2.appdir=(appdir+(' '*80))[:80]
Audio.ftn_init()
addpfx0=""
first=1
g.appdir=appdir
isync=0
isync441=1
isync_iscat=-20
isync65=1
isync_save=0
itol=5                                       #Default tol=400 Hz
ntol=(10,25,50,100,200,400,600)              #List of available tolerances
idsec=0
#irdsec=0
lauto=0
ltxdf=0
altmsg=0
cmap0="Linrad"
fileopened=""
font1='Helvetica'
hiscall=""
hisgrid=""
isec0=-99
jtol=IntVar()
k2txb=IntVar()
kb8rq=IntVar()
loopall=0
mode=StringVar()
mode.set("")
mrudir=os.getcwd()
MyCall0=""
nafc=IntVar()
naz=0
ndepth=IntVar()
nel=0
ncall=0
ncwtrperiod=120
ndmiles=0
ndkm=0
ndebug=IntVar()
neme=IntVar()
nfreeze=IntVar()
nhotaz=0
nhotabetter=0
nmeas=0
nlowbeacon=IntVar()
nlowbeacon.set(0)
nmonitor=IntVar()
nopen=0
nshrx=IntVar()
noshjt65=IntVar()
noshjt65all=IntVar()
nsync=IntVar()
nzap=IntVar()
qdecode=IntVar()
setseq=IntVar()
ShOK=IntVar()
slabel="Sync   "
textheight=7
ToRadio0=""
tx6alt=""
txsnrdb=99.
TxFirst=IntVar()
xypm=[]
green=zeros(500,'f')
im=Image.new('P',(500,120))
im.putpalette(Colormap2Palette(colormapLinrad),"RGB")
pim=ImageTk.PhotoImage(im)
balloon=Pmw.Balloon(root)

g.freeze_decode=0
g.mode=""
g.ndevin=IntVar()
g.ndevout=IntVar()
g.DevinName=StringVar()
g.DevoutName=StringVar()
#------------------------------------------------------ showspecjt
def showspecjt(event=NONE):
    if g.showspecjt==0: g.showspecjt=1

#------------------------------------------------------ restart
def restart():
    Audio.gcom2.nrestart=1
    Audio.gcom2.mantx=1

#------------------------------------------------------ restart2
def restart2():
    Audio.gcom2.shok=ShOK.get()
    Audio.gcom2.nrestart=1

#------------------------------------------------------ toggle_freeze
def toggle_freeze(event=NONE):
    nfreeze.set(1-nfreeze.get())

#------------------------------------------------------ toggle_zap
def toggle_zap(event=NONE):
    nzap.set(1-nzap.get())

#------------------------------------------------------ btx (1-6)
def btx1(event=NONE):
    ntx.set(1)
    Audio.gcom2.txmsg=(tx1.get()+(' '*28))[:28]
    Audio.gcom2.ntxreq=1
    restart()
def btx2(event=NONE):
    ntx.set(2)
    Audio.gcom2.txmsg=(tx2.get()+(' '*28))[:28]
    Audio.gcom2.ntxreq=2
    restart()
def btx3(event=NONE):
    ntx.set(3)
    Audio.gcom2.txmsg=(tx3.get()+(' '*28))[:28]
    Audio.gcom2.ntxreq=3
    restart()
def btx4(event=NONE):
    ntx.set(4)
    Audio.gcom2.txmsg=(tx4.get()+(' '*28))[:28]
    Audio.gcom2.ntxreq=4
    restart()
def btx5(event=NONE):
    ntx.set(5)
    Audio.gcom2.txmsg=(tx5.get()+(' '*28))[:28]
    Audio.gcom2.ntxreq=5
    restart()
def btx6(event=NONE):
    ntx.set(6)
    Audio.gcom2.txmsg=(tx6.get()+(' '*28))[:28]
    Audio.gcom2.ntxreq=6
    restart()

#------------------------------------------------------ quit
def quit(event=NONE):
    root.destroy()

#------------------------------------------------------ testmsgs
def testmsgs():
    for m in (tx1, tx2, tx3, tx4, tx5, tx6):
        m.delete(0,99)
    tx1.insert(0,"@A")
    tx2.insert(0,"@B")
    tx3.insert(0,"@C")
    tx4.insert(0,"@D")
    tx5.insert(0,"@1000")
    tx6.insert(0,"@2000")

#------------------------------------------------------ textsize
def textsize():
    global textheight
    if textheight <= 9:
        textheight=21
    else:
        if mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4':
            textheight=7
        else:
            textheight=9
    text.configure(height=textheight)

#------------------------------------------------------ logqso
def logqso(event=NONE):
    t=time.strftime("%Y-%b-%d,%H:%M",time.gmtime())
    tf=str(g.nfreq)
    if g.nfreq==2: tf="1.8"
    if g.nfreq==4: tf="3.5"
    t=t+","+ToRadio.get()+","+HisGrid.get()+","+tf+","+g.mode+"\n"
    t2="Please confirm making the following entry in WSJT.LOG:\n\n" + t
    result=tkMessageBox.askyesno(message=t2)
    if result:
        f=open(appdir+'/WSJT.LOG','a')
        f.write(t)
        f.close()
    
#------------------------------------------------------ monitor
def monitor(event=NONE):
    bmonitor.configure(bg='green')
    Audio.gcom2.monitoring=1

#------------------------------------------------------ stopmon
def stopmon(event=NONE):
    global loopall
    loopall=0
    bmonitor.configure(bg='gray85')
    Audio.gcom2.monitoring=0

#------------------------------------------------------ dbl_click_text
def dbl_click_text(event):
    t=text.get('1.0',END)           #Entire contents of text box
    t1=text.get('1.0',CURRENT)      #Contents from start to mouse pointer
    if mode.get()=='Diana':
        report.delete(0,END)
        report.insert(0,'OOO')
    dbl_click_call(t,t1,'OOO',event)

#------------------------------------------------------ dbl_click3_text
def dbl_click3_text(event):
    if mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4':
        t=text.get('1.0',END)           #Entire contents of text box
        t1=text.get('1.0',CURRENT)      #Contents from start to mouse pointer
        n=t1.rfind("\n")
        rpt=t1[n+12:n+15]
        if rpt[0:1] == " ": rpt=rpt[1:]
        if rpt[:1]=='-' and len(rpt)==2: rpt=rpt[0:1]+'0'+rpt[1:2]
        dbl_click_call(t,t1,rpt,event)

    elif mode.get()[:5]=='ISCAT' or mode.get()=='Diana':
        t=text.get('1.0',END)           #Entire contents of text box
        t1=text.get('1.0',CURRENT)      #Contents from start to mouse pointer
        n=t1.rfind("\n")
        rpt=t1[n+12:n+15]
        if mode.get()=='Diana':
            rpt=t1[n+12:n+16]
        if rpt[0:1] == " ": rpt=rpt[1:]
        report.delete(0,END)
        report.insert(0,rpt)
        dbl_click_call(t,t1,rpt,event)

    elif mode.get()=='FSK441' or mode.get()=='JTMS':
        t=text.get('1.0',END)           #Entire contents of text box
        t1=text.get('1.0',CURRENT)      #Contents from start to mouse pointer
        n=t1.rfind("\n")
        rpt=t1[n+21:n+23]
        report.delete(0,END)
        report.insert(0,rpt)
        dbl_click_call(t,t1,rpt,event)


#------------------------------------------------------ dbl_click_ave
def dbl_click_ave(event):
    t=avetext.get('1.0',END)           #Entire contents of text box
    t1=avetext.get('1.0',CURRENT)      #Contents from start to mouse pointer
    dbl_click_call(t,t1,'OOO',event)
#------------------------------------------------------ dbl_click_call
def dbl_click_call(t,t1,rpt,event):
    global hiscall
    i=len(t1)                       #Length to mouse pointer
    i1=t1.rfind(' ')+1              #index of preceding space
    i2=i1+t[i1:].find(' ')          #index of next space
    hiscall=t[i1:i2]                #selected word, assumed as callsign
    if hiscall[0:1]=='<' and hiscall [i2-i1-1:]=='>':
        hiscall=hiscall[1:i2-i1-1]
    ToRadio.delete(0,END)
    ToRadio.insert(0,hiscall)
    i3=t1.rfind('\n')+1             #start of selected line
    if i>6 and i2>i1:
        try:
            nsec=60*int(t1[i3+2:i3+4]) + int(t1[i3+4:i3+6])
        except:
            nsec=0
        if setseq.get(): TxFirst.set((nsec/Audio.gcom1.trperiod)%2)
        lookup()
        GenStdMsgs()
        if (mode.get()[:4]=='JT65' or \
                mode.get()[:3]=='JT4') and rpt <> "OOO":
            n=tx1.get().rfind(" ")
            t2=tx1.get()[0:n+1]
            tx2.delete(0,END)
            tx2.insert(0,t2+rpt)
            tx3.delete(0,END)
            tx3.insert(0,t2+"R"+rpt)
            tx4.delete(0,END)
            tx4.insert(0,t2+"RRR")
            tx5.delete(0,END)
            tx5.insert(0,t2+"73")

        if t[i3:i1].find(' CQ ')>=0:
            ntx.set(1)
        else:
            ntx.set(2)
        if event.num==3 and not lauto: toggleauto()

def textkey(event=NONE):
    text.configure(state=DISABLED)
def avetextkey(event=NONE):
    avetext.configure(state=DISABLED)

#------------------------------------------------------ force_decode
def force_decode(event=NONE):
    Audio.gcom2.nforce=1
    if event.keysym == 'd': Audio.gcom2.ntx2=0
    if event.keysym == 'D': Audio.gcom2.ntx2=1
    decode()

#------------------------------------------------------ decode
def decode(event=NONE):
    if Audio.gcom2.ndecoding==0:        #If already busy, ignore request
        Audio.gcom2.nagain=1
        Audio.gcom2.npingtime=0         #Decode whole record
        n=1
        Audio.gcom2.mousebutton=0
        if Audio.gcom2.ndecoding0==4: n=4
        Audio.gcom2.ndecoding=n         #Standard decode, full file (d2a)

#------------------------------------------------------ decode_include
def decode_include(event=NONE):
    global isync,isync_save
    isync_save=isync
    isync=-99
    Audio.gcom2.minsigdb=-99
    decode()

#------------------------------------------------------ decode_exclude
def decode_exclude(event=NONE):
    global isync,isync_save
    isync_save=isync
    isync=99
    Audio.gcom2.minsigdb=99
    decode()

#------------------------------------------------------ openfile
def openfile(event=NONE):
    global mrudir,fileopened,nopen
    nopen=1                         #Work-around for "click feedthrough" bug
    try:
        os.chdir(mrudir)
    except:
        pass
    fname=askopenfilename(filetypes=[("Wave files","*.wav *.WAV")])
    if fname:
        Audio.getfile(fname,len(fname))
        if Audio.gcom2.ierr: print 'Error ',Audio.gcom2.ierr, \
           'when trying to read file',fname
        mrudir=os.path.dirname(fname)
        fileopened=os.path.basename(fname)
    os.chdir(appdir)
 
#------------------------------------------------------ opennext
def opennext(event=NONE):
    global ncall,fileopened,loopall,mrudir
    if fileopened=="" and ncall==0:
        openfile()
        ncall=1
    else:
# Make a list of *.wav files in mrudir
        la=os.listdir(mrudir)
        la.sort()
        lb=[]
        for i in range(len(la)):
            j=la[i].find(".wav") + la[i].find(".WAV")
            if j>0: lb.append(la[i])
        for i in range(len(lb)):
            if lb[i]==fileopened:
                break
        if i<len(lb)-1:
            fname=mrudir+"/"+lb[i+1]
#            if not lauto: stopmon()
            Audio.getfile(fname,len(fname))
            if Audio.gcom2.ierr: print 'Error ',Audio.gcom2.ierr, \
               'when trying to read file',fname
            mrudir=os.path.dirname(fname)
            fileopened=os.path.basename(fname)
        else:
            t="No more *.wav files in this directory."
	    tkMessageBox.showwarning(message=t)
            ncall=0
            loopall=0

#------------------------------------------------------ decodeall
def decodeall(event=NONE):
    global loopall
    loopall=1
    opennext()

#------------------------------------------------------ astro1
def astro1(event=NONE):
    astro.astro2(g.astro_geom0)

#------------------------------------------------------ options1
def options1(event=NONE):
    options.options2(root_geom[root_geom.index("+"):])

#------------------------------------------------------ txmute
def txmute(event=NONE):
    Audio.gcom1.mute=1-Audio.gcom1.mute
    if Audio.gcom1.mute:
        lab7.configure(bg='red',fg='black')
    else:
        lab7.configure(bg='gray85',fg='gray85')

#------------------------------------------------------ savelast
def savelast(event=NONE):
    Audio.gcom2.nsavelast=1

#------------------------------------------------------ stub
def stub(event=NONE):
    MsgBox("Sorry, this function is not yet implemented.")

#------------------------------------------------------ MsgBox
def MsgBox(t):
    tkMessageBox._show(message=t)

#------------------------------------------------------ txstop
def txstop(event=NONE):
    if lauto: toggleauto()
    Audio.gcom1.txok=0
    Audio.gcom2.mantx=0
    
#------------------------------------------------------ lookup
def lookup(event=NONE):
    global hiscall,hisgrid
    hiscall=ToRadio.get().upper().strip()
    ToRadio.delete(0,END)
    ToRadio.insert(0,hiscall)
    s=whois(hiscall)
    balloon.bind(ToRadio,s[:-1])
    hisgrid=""
    if s:
        i1=s.find(',')
        i2=s.find(',',i1+1)
        hisgrid=s[i1+1:i2]
        hisgrid=hisgrid[:2].upper()+hisgrid[2:4]+hisgrid[4:6].lower()
    if len(hisgrid)==4: hisgrid=hisgrid+"mm"
    if len(hisgrid)==5: hisgrid=hisgrid+"m"
    HisGrid.delete(0,99)
    HisGrid.insert(0,hisgrid)

def lookup_gen(event):
    lookup()
    GenStdMsgs()

#-------------------------------------------------------- addtodb
def addtodb():
    global hiscall
    if HisGrid.get()=="":
        MsgBox("Please enter a valid grid locator.")
    else:
        modified=0
        hiscall=ToRadio.get().upper().strip()
        hisgrid=HisGrid.get().strip()
        hc=hiscall
        NewEntry=hc + "," + hisgrid
	result=tkMessageBox.askyesno(message="Is this station known to be active on EME?")
        if result:
            NewEntry=NewEntry + ",EME,,"
        else:
            NewEntry=NewEntry + ",,,"
        try:
            f=open(appdir+'/CALL3.TXT','r')
            s=f.readlines()
        except:
            print 'Error opening CALL3.TXT'
            s=""
        f.close()
        hc2=""
        stmp=[]
        for i in range(len(s)):
            hc1=hc2
            if s[i][:2]=="//":
                stmp.append(s[i])
            else:
                i1=s[i].find(",")
                hc2=s[i][:i1]
                if hc>hc1 and hc<hc2:
                    stmp.append(NewEntry+"\n")
                    modified=1
                elif hc==hc2:
                    t=s[i] + "\n\n is already in CALL3.TXT\nDo you wish to replace this entry?"
		    result=tkMessageBox.askyesno(message=t)
                    if result:
                        i1=s[i].find(",")
                        i2=s[i].find(",",i1+1)
                        i3=s[i].find(",",i2+1)
                        i4=len(NewEntry)
                        s[i]=NewEntry[:i4-1] + s[i][i3+1:]
                        modified=1
                stmp.append(s[i])
        if hc>hc1 and modified==0:
            stmp.append(NewEntry+"\n")
        try:
            f=open(appdir+'/CALL3.TMP','w')
            f.writelines(stmp)
            f.close()
        except:
            print 'Error in opening or writing to CALL3.TMP'

        if modified:
            if os.path.exists("CALL3.OLD"): os.remove("CALL3.OLD")
            os.rename("CALL3.TXT","CALL3.OLD")
            os.rename("CALL3.TMP","CALL3.TXT")

#------------------------------------------------------ setrpt
def setrpt(event):
#    report.delete(0,END)
    report.focus_set()

#-------------------------------------------------------- clrToRadio
def clrToRadio(event):
    ToRadio.delete(0,END)
    HisGrid.delete(0,99)
    ToRadio.focus_set()
    if kb8rq.get():
        ntx.set(6)
        nfreeze.set(0)

#------------------------------------------------------ whois
def whois(hiscall):
    whodat=""
    try:
        f=open(appdir+'/CALL3.TXT','r')
        s=f.readlines()
        f.close()
    except:
        print 'Error when searching CALL3.TXT, or no such file present'
        s=""
    for i in range(len(s)):
        if s[i][:2] != '//':
            i1=s[i].find(',')
            if s[i][:i1] == hiscall:
                return s[i]
    return ""

#------------------------------------------------------ cleartext
def cleartext():
    f=open(appdir+'/decoded.txt',mode='w')
    f.truncate(0)                           #Delete contents of decoded.txt
    f.close()
    f=open(appdir+'/decoded.ave',mode='w')
    f.truncate(0)                           #Delete contents of decoded.ave
    f.close()

#------------------------------------------------------ ModeFSK441
def ModeFSK441(event=NONE):
    global slabel,isync,isync441,textheight,itol
    if g.mode != "FSK441":
        if lauto: toggleauto()
        mode.set("FSK441")
        cleartext()
        Audio.gcom1.trperiod=30
        lab2.configure(text='FileID            T      Width    dB  Rpt       DF')
        lab1.configure(text='Time (s)',bg="green")
        lab4.configure(fg='black')
        lab5.configure(fg='black')
        lab6.configure(bg="green")
        isync=isync441
        slabel="S      "
        f5b1.grid(column=0,row=0,padx=2,sticky='EW')
        lsync.configure(text=slabel+str(isync))
        iframe4b.pack_forget()
        textheight=9
        text.configure(height=textheight)
        bclravg.pack_forget()
        binclude.pack_forget()
        bexclude.pack_forget()
        cbfreeze.grid_forget()
        cbafc.grid_forget()
        lsync.grid(column=0,row=0,padx=8,sticky='EW')
        ltol.grid(column=0,row=1,padx=8,sticky='EW')
        cbzap.grid(column=1,row=0,padx=2,sticky='W')
        nfreeze.set(0)
        shrx.grid(column=1,row=1,sticky='W',padx=2)
        shmsg.grid(column=1,row=0,sticky='W',padx=2)
        report.grid(column=1,row=1,sticky='W',padx=7)
        labreport.grid(column=0,row=1,sticky='E',padx=0)
        if ltxdf: toggletxdf()
        graph2.configure(bg='black')
        btxdf.grid_forget()
        report.delete(0,END)
        report.insert(0,'26')
        itol=4
        inctol()
        ntx.set(1)
        GenStdMsgs()
        erase()

#------------------------------------------------------ ModeJT65
def ModeJT65():
    global slabel,isync,isync65,textheight,itol
    cleartext()
    lab2.configure(text='FileID      Sync      dB        DT       DF    W')
    lab4.configure(fg='gray85')
    lab5.configure(fg='gray85')
    Audio.gcom1.trperiod=60
    iframe4b.pack(after=iframe4,expand=1, fill=X, padx=4)
    textheight=7
    text.configure(height=textheight)
    isync=isync65
    slabel="Sync   "
    f5b1.grid(column=0,row=0,padx=2,sticky='EW')
    lsync.configure(text=slabel+str(isync))
    lsync.grid(column=0,row=0,padx=2,sticky='EW')
    ltol.grid(column=0,row=1,padx=2,sticky='EW')
    cbzap.grid(column=1,row=0,padx=2,sticky='W')
    btxstop.pack_forget()
    bclravg.pack(side=LEFT,expand=1,fill=X)
    binclude.pack(side=LEFT,expand=1,fill=X)
    bexclude.pack(side=LEFT,expand=1,fill=X)
    btxstop.pack(side=LEFT,expand=1,fill=X)
    cbfreeze.grid(column=1,row=2,padx=2,sticky='W')
    cbafc.grid(column=1,row=1,padx=2,sticky='W')
    if ltxdf: toggletxdf()
    btxdf.grid_forget()
    shmsg.grid_forget()
    shrx.grid_forget()
    report.grid_forget()
    labreport.grid_forget()
    graph2.configure(bg='#66FFFF')
    itol=4
    inctol()
    nfreeze.set(0)
    ntx.set(1)
    GenStdMsgs()
    erase()
#    graph2.pack_forget()

#------------------------------------------------------ ModeJT65A
def ModeJT65A(event=NONE):
    if g.mode != "JT65A":
        if lauto: toggleauto()
        ModeJT65()
        mode.set("JT65A")
        btxdf.grid(column=1,row=0,sticky='EW',padx=4)


#------------------------------------------------------ ModeJT65B
def ModeJT65B(event=NONE):
    if g.mode != "JT65B":
        if lauto: toggleauto()
        mode.set("JT65B")
        ModeJT65()

#------------------------------------------------------ ModeJT65C
def ModeJT65C(event=NONE):
    if g.mode != "JT65C":
        if lauto: toggleauto()
        mode.set("JT65C")
        ModeJT65()

#------------------------------------------------------ ModeJTMS
def ModeJTMS(event=NONE):
    if g.mode != "JTMS":
        if lauto: toggleauto()
    ModeFSK441()
    cbfreeze.grid(column=0,row=2,padx=4,sticky='W')
    mode.set("JTMS")
    
#------------------------------------------------------ ModeISCAT_A
def ModeISCAT_A(event=NONE):
    ModeISCAT_B()
    mode.set("ISCAT-A")
    Audio.gcom2.mode4=1
    
#------------------------------------------------------ ModeISCAT_B
def ModeISCAT_B(event=NONE):
    global isync,isync_iscat
    if g.mode != "ISCAT-B":
        if lauto: toggleauto()
        cleartext()
        ModeFSK441()
        mode.set("ISCAT-B")
        lab2.configure(text='FileID       Sync    dB      DT       DF       F1')
        isync=isync_iscat
        lsync.configure(text=slabel+str(isync))
        cbafc.grid(column=1,row=1,padx=2,sticky='W')
        cbfreeze.grid(column=1,row=2,padx=2,sticky='W')
        itol=3
        ltol.configure(text='Tol    '+str(ntol[itol]))
        inctol()
        nfreeze.set(0)
        report.delete(0,END)
        report.insert(0,'-15')
        shrx.grid_forget()
        shmsg.grid_forget()
        ntx.set(1)
        Audio.gcom2.mousedf=0
        Audio.gcom2.mode4=2
        GenStdMsgs()
        erase()        

#------------------------------------------------------ ModeDiana
def ModeDiana(event=NONE):
    global isync,isync_iscat
    if g.mode != "Diana":
        if lauto: toggleauto()
        ModeJT65()
        Audio.gcom1.trperiod=30
        mode.set("Diana")
        isync=1
        lsync.configure(text=slabel+str(isync))
        report.delete(0,END)
        report.insert(0,'-20')

#------------------------------------------------------ ModeCW
def ModeCW(event=NONE):
    if g.mode != "CW":
        if lauto: toggleauto()
        cleartext()
        ModeJT65B()
        mode.set("CW")
        Audio.gcom1.trperiod=ncwtrperiod
        iframe4b.pack_forget()
        text.configure(height=9)
        cbfreeze.grid_forget()
        cbafc.grid_forget()
        lsync.grid_forget()
        ltol.grid_forget()
        cbzap.grid_forget()
        f5b1.grid_forget()
        if ltxdf: toggletxdf()
        ntx.set(1)
        GenStdMsgs()
        erase()

#------------------------------------------------------ ModeJT4
def ModeJT4():
    global slabel,isync,isync65,textheight,itol
    ModeJT65()
    bclravg.pack_forget()
    binclude.pack_forget()
    bexclude.pack_forget()


#------------------------------------------------------ ModeJT4A
def ModeJT4A():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4A")
    Audio.gcom2.mode4=1
    btxdf.grid(column=1,row=0,sticky='EW',padx=4)

#------------------------------------------------------ ModeJT4B
def ModeJT4B():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4B")
    Audio.gcom2.mode4=2

#------------------------------------------------------ ModeJT4C
def ModeJT4C():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4C")
    Audio.gcom2.mode4=4

#------------------------------------------------------ ModeJT4D
def ModeJT4D():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4D")
    Audio.gcom2.mode4=9

#------------------------------------------------------ ModeJT4E
def ModeJT4E():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4E")
    Audio.gcom2.mode4=18

#------------------------------------------------------ ModeJT4F
def ModeJT4F():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4F")
    Audio.gcom2.mode4=36

#------------------------------------------------------ ModeJT4G
def ModeJT4G():
    global slabel,isync,isync65,textheight,itol
    ModeJT4()
    mode.set("JT4G")
    Audio.gcom2.mode4=72

#------------------------------------------------------ ModeEcho
def ModeEcho(event=NONE):
    ModeCW()
    mode.set("Echo")
    if lauto: toggleauto()
    lab2.configure(text='     N      Level         Sig              DF         Width     Az           El          Q')
    Audio.gcom1.trperiod=6
    tx1.delete(0,99)
    tx2.delete(0,99)
    tx3.delete(0,99)
    tx4.delete(0,99)
    tx5.delete(0,99)
    tx6.delete(0,99)

#------------------------------------------------------ ModeMeasure
def ModeMeasure(event=NONE):
    ModeEcho()
    mode.set("Measure")

#------------------------------------------------------ msgpos
def msgpos():
    g=root_geom[root_geom.index("+"):]
    t=g[1:]
    x=int(t[:t.index("+")])          # + 70
    y=int(t[t.index("+")+1:])        # + 70
    return "+%d+%d" % (x,y)    

#------------------------------------------------------ about
def about(event=NONE):
    global Version
    about=Toplevel(root)
    about.geometry(msgpos())
    if g.Win32: about.iconbitmap("wsjt.ico")
    t="WSJT Version " + Version + ", by K1JT"
    Label(about,text=t,font=(font1,16)).pack(padx=20,pady=5)
    t="""
WSJT is a weak signal communications program.  It supports
these operating modes:

  1. FSK441 -  meteor scatter
  2. ISCAT    -  for tropo- and iono-scatter, weak Es/F2 on 50 MHz
                       (replaces JT6M)
  3. JT65      -  for HF, EME, and troposcatter
  4. JT4        -  HF, microwave beacons, 10 GHz EME and rainscatter
  5. CW        -  15 WPM Morse code, messages structured for EME
  6. Echo      -  EME Echo testing

Copy (c) 2001-2010 by Joseph H. Taylor, Jr., K1JT, with
contributions from additional authors.  WSJT is Open Source 
software, licensed under the GNU General Public License (GPL).
Source code and programming information may be found at 
http://developer.berlios.de/projects/wsjt/.
"""
    Label(about,text=t,justify=LEFT).pack(padx=20)
    t="Revision date: " + \
      "$Date$"[7:-1]
    Label(about,text=t,justify=LEFT).pack(padx=20)
    about.focus_set()

#------------------------------------------------------ shortcuts
def shortcuts(event=NONE):
    scwid=Toplevel(root)
    scwid.geometry(msgpos())
    if g.Win32: scwid.iconbitmap("wsjt.ico")
    t="""
F1	List keyboard shortcuts
Shift+F1	List special mouse commands
Ctrl+F1	About WSJT
F2	Options
F3	Tx Mute
F4	Clear "To Radio"
Alt+F4      Exit program
F5	What message to send?
Shift+F5	Examples of minimal JT65 QSOs
F6	Open next file in directory
Shift+F6	Decode all wave files in directory
F9	Online Supplement to User's Guide
F10	Show SpecJT
Shift+F10  Show astronomical data
F11	Decrement Freeze DF
F12	Increment Freeze DF
Alt+1 to Alt+6  Tx1 to Tx6
Alt+A	Toggle Auto On/Off
Alt+C	Clear average
Alt+D	Decode
Ctrl+D	Force Decode 
Shift+Ctrl+D  Force Decode, no JT65 shorthands 
Alt+E	Erase
Alt+F	Toggle Freeze
Alt+G	Generate standard messages
Ctrl+G	Generate alternate JT65/JT4 Messages
Alt+I	Include
Alt+L	Lookup
Ctrl+L	Lookup, then Generate Standard Messages
Alt+M	Monitor
Alt+O	Tx Stop
Alt+Q	Log QSO
Alt+R	Enter report
Alt+S	Stop Monitoring or Decoding
Alt+V	Save Last
Alt+X	Exclude
Alt+Z	Toggle Zap
"""
    Label(scwid,text=t,justify=LEFT).pack(padx=20)
    scwid.focus_set()

#------------------------------------------------------ mouse_commands
def mouse_commands(event=NONE):
    scwid=Toplevel(root)
    scwid.geometry(msgpos())
    if g.Win32: scwid.iconbitmap("wsjt.ico")
    t="""
Click on          Action
--------------------------------------------------------
Waterfall        FSK441, JTMS: click to decode region
                      JT65: Click to set DF for Freeze
                       Double-click to Freeze and Decode

Main screen,     FSK441, JTMS, ISCAT: click to decode ping
graphics area    JT65: Click to set DF for Freeze
                           Double-click to Freeze and Decode

Main screen,     Double-click puts callsign in Tx messages
text area           Right-double-click also sets Auto ON

Sync, S,         Left/Right click to increase/decrease
Tol, ...
"""
    Label(scwid,text=t,justify=LEFT).pack(padx=20)
    scwid.focus_set()

#------------------------------------------------------ what2send
def what2send(event=NONE):
    screenf5=Toplevel(root)
    screenf5.geometry(root_geom[root_geom.index("+"):])
    if g.Win32: screenf5.iconbitmap("wsjt.ico")
    t="""
To optimize your chances of completing a valid QSO using WSJT,
use the following standard procedures and *do not* exchange pertinent
information by other means (e.g., internet, telephone, ...) while the
QSO is in progress!

FSK441, JTMS, or ISCAT:   If you have received
    ... less than both calls from the other station, send both calls.
    ... both calls, send both calls and your signal report.
    ... both calls and signal report, send R and your report.
    ... R plus signal report, send RRR.
    ... RRR, the QSO is complete.  However, the other station may not
know this, so it is conventional to send 73 to signify that you are done.

(Outside of North America, the customary procedures may be slightly
different.)


JT65, JT4:   If you have received
    ... less than both calls, send both calls and your grid locator.
    ... both calls, send both calls, your grid locator, and OOO.
    ... both calls and OOO, send RO.
    ... RO, send RRR.
    ... RRR, the QSO is complete.  However, the other station may not
know this, so it is conventional to send 73 to signify that you are done.

(Sending grid locators is conventional in JT65, but numerical signal
reports may be substituted.)
"""
    Label(screenf5,text=t,justify=LEFT).pack(padx=20)
    screenf5.focus_set()

#------------------------------------------------------ minimal_qso
def minimal_qso(event=NONE):
    screenf5s=Toplevel(root)
    screenf5s.geometry(root_geom[root_geom.index("+"):])
    if g.Win32: screenf5s.iconbitmap("wsjt.ico")
    t="""
The following are recommended sequences for  minimal QSOs
using the standard JT65/JT4 messages:

Station #1                            Station #2
----------------------------------------------------------
CQ K1JT FN20
                                            K1JT DL3XYZ JO61
DL3XYZ K1JT FN20 OOO
                                            RO
RRR
                                            73
----------------------------------------------------------
CQ K1JT FN20
                                            K1JT VK7ABC QE37
VK7ABC K1JT -22
                                            K1JT VK7ABC R-23
VK7ABC K1JT RRR
                                            TNX JOE 73
"""
    Label(screenf5s,text=t,justify=LEFT).pack(padx=20)
    screenf5s.focus_set()

#------------------------------------------------------ usersguide
def usersguide(event=NONE):
    url='http://physics.princeton.edu/pulsar/K1JT/WSJT_User_600.pdf'
    thread.start_new_thread(browser,(url,))

#------------------------------------------------------ wsjt9supp
def wsjt9supp(event=NONE):
    url='http://physics.princeton.edu/pulsar/K1JT/WSJT_9.0_Supplement.pdf'
    thread.start_new_thread(browser,(url,))

#------------------------------------------------------- browser
def browser(url):
    webbrowser.open(url)

#------------------------------------------------------ prefixes
def prefixes(event=NONE):
    pfx=Toplevel(root)
    pfx.geometry(msgpos())
    if g.Win32: pfx.iconbitmap("wsjt.ico")
    f=open(appdir+'/prefixes.txt','r')
    s=f.readlines()
    t2=""
    for i in range(3):
        t2=t2+s[i]
    t=""
    for i in range(len(s)-3):
        t=t+s[i+3]
    t=t.split()
    t.sort()
    t1=""
    n=0
    for i in range(len(t)):
        t1=t1+t[i]+"  "
        n=n+len(t[i])+2
        if n>60:
            t1=t1+"\n"
            n=0
    t1=t1+"\n"
    if options.addpfx.get().lstrip():
        t1=t1+"\nOptional prefix:  "+(options.addpfx.get().lstrip()+'    ')[:8]
    t2=t2+"\n"+t1
    Label(pfx,text=t2,justify=LEFT).pack(padx=20)
    pfx.focus_set()

#------------------------------------------------------ azdist
def azdist():
    if len(HisGrid.get().strip())<4:
        labAz.configure(text="")
        labHotAB.configure(text="",bg='gray85')
        labDist.configure(text="")
    else:
        if mode.get()[:4]=='JT65' or \
               mode.get()[:3]=='JT4' or mode.get()[:2]=="CW":
            labAz.configure(text="Az: %d" % (naz,))
            labHotAB.configure(text="",bg='gray85')
        else:
            labAz.configure(text="Az: %d   El: %d" % (naz,nel))
            if nhotabetter:
                labHotAB.configure(text="Hot A: "+str(nhotaz),bg='#FF9900')
            else:
                labHotAB.configure(text="Hot B: "+str(nhotaz),bg='#FF9900')
        if options.mileskm.get()==0:
            labDist.configure(text=str(ndmiles)+" mi")
        else:
            labDist.configure(text=str(int(1.609344*ndmiles))+" km")
    
#------------------------------------------------------ incsync
def incsync(event):
    global isync
    if isync<10:
        isync=isync+1
        lsync.configure(text=slabel+str(isync))

#------------------------------------------------------ decsync
def decsync(event):
    global isync
    if isync>-30:
        isync=isync-1
        lsync.configure(text=slabel+str(isync))

#------------------------------------------------------ inctol
def inctol(event=NONE):
    global itol
    maxitol=5
    if mode.get()[:4]=='JT65': maxitol=6
    if itol<maxitol: itol=itol+1
    ltol.configure(text='Tol    '+str(ntol[itol]))

#------------------------------------------------------ dectol
def dectol(event):
    global itol
    if itol>0 : itol=itol-1
    ltol.configure(text='Tol    '+str(ntol[itol]))

#------------------------------------------------------ incdsec
def incdsec(event):
    global idsec
    idsec=idsec+5
    bg='red'
    if idsec==0: bg='white'
    ldsec.configure(text='Dsec  '+str(0.1*idsec),bg=bg)
    Audio.gcom1.ndsec=idsec

#------------------------------------------------------ decdsec
def decdsec(event):
    global idsec
    idsec=idsec-5
    bg='red'
    if idsec==0: bg='white'
    ldsec.configure(text='Dsec  '+str(0.1*idsec),bg=bg)
    Audio.gcom1.ndsec=idsec

#------------------------------------------------------ toggle_shift
##def toggle_shift(event):
##    Audio.gcom2.nadd5=1-Audio.gcom2.nadd5
##    if Audio.gcom2.nadd5:
##        bg='red'
##        lshift.configure(text='Shift 5.0',bg=bg)
##    else:
##        bg='white'
##        lshift.configure(text='Shift 0.0',bg=bg)

#------------------------------------------------------ inctrperiod
def inctrperiod(event):
    global ncwtrperiod
    if mode.get()=="CW":
        if ncwtrperiod==120: ncwtrperiod=150
        if ncwtrperiod==60:  ncwtrperiod=120
        Audio.gcom1.trperiod=ncwtrperiod
    elif mode.get()=="FSK441" or mode.get()=="JTMS" or mode.get()[:5]=="ISCAT":
        if Audio.gcom1.trperiod==15: Audio.gcom1.trperiod=30

#------------------------------------------------------ dectrperiod
def dectrperiod(event):
    global ncwtrperiod
    if mode.get()[:2]=="CW":
        if ncwtrperiod==120: ncwtrperiod=60
        if ncwtrperiod==150: ncwtrperiod=120
        Audio.gcom1.trperiod=ncwtrperiod
    elif mode.get()=="FSK441" or mode.get()=="JTMS" or mode.get()[:5]=="ISCAT":
        if Audio.gcom1.trperiod==30: Audio.gcom1.trperiod=15

#------------------------------------------------------ erase
def erase(event=NONE):
    graph1.delete(ALL)
    if mode.get()[:4]!="JT65" and mode.get()[:2]!="CW" and \
            mode.get()[:3]!='JT4' and mode.get()!='Diana':
        graph2.delete(ALL)
    text.configure(state=NORMAL)
    text.delete('1.0',END)
    text.configure(state=DISABLED)
    avetext.configure(state=NORMAL)
    avetext.delete('1.0',END)
    avetext.configure(state=DISABLED)
    lab3.configure(text=" ")
    Audio.gcom2.decodedfile="                        "
#------------------------------------------------------ clear_avg
def clear_avg(event=NONE):
    avetext.configure(state=NORMAL)
    avetext.delete('1.0',END)
    avetext.configure(state=DISABLED)
    f=open(appdir+'/decoded.ave',mode='w')
    f.truncate(0)                           #Delete contents of decoded.ave
    f.close()
    Audio.gcom2.nclearave=1

#------------------------------------------------------ delwav
def delwav():
    t="Are you sure you want to delete\nall *.WAV files in the RxWav directory?"
    result=tkMessageBox.askyesno(message=t)
    if result:
# Make a list of *.wav files in RxWav
        la=dircache.listdir(appdir+'/RxWav')
        lb=[]
        for i in range(len(la)):
            j=la[i].find(".wav") + la[i].find(".WAV")
            if j>0: lb.append(la[i])
# Now delete them all.
        for i in range(len(lb)):
            fname=appdir+'/RxWav/'+lb[i]
            os.remove(fname)

#------------------------------------------------------ del_all
def del_all():
    Audio.gcom1.ns0=-999999

#------------------------------------------------------ toggleauto
def toggleauto(event=NONE):
    global lauto
    lauto=1-lauto
    Audio.gcom2.lauto=lauto
    if lauto and mode.get()!='Echo':
        monitor()
    else:
        Audio.gcom1.txok=0
        Audio.gcom2.mantx=0
    if lauto==0: auto.configure(text='Auto is OFF',bg='gray85',relief=RAISED)
    if lauto==1: auto.configure(text='Auto is  ON',bg='red',relief=SOLID)
    
#------------------------------------------------------ toggletxdf
def toggletxdf(event=NONE):
    global ltxdf
    ltxdf=1-ltxdf
    if ltxdf:
        Audio.gcom2.ntxdf=Audio.gcom2.mousedf
        t="  TxDF =%4d" % (int(Audio.gcom2.mousedf),)
        btxdf.configure(text=t,bg='red',relief=SOLID)
    else:
        Audio.gcom2.ntxdf=0
        t="  TxDF =%4d" % 0
        btxdf.configure(text=t,bg='gray85',relief=RAISED)
    if Audio.gcom1.transmitting:
        txstop()

#----------------------------------------------------- dtdf_change
# Readout of graphical cursor location
def dtdf_change(event):
    if mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4' or mode.get()=='Diana':
        if event.y<40 and Audio.gcom2.nspecial==0:
            lab1.configure(text='Time (s)',bg="#33FFFF")   #light blue
            if mode.get()=='Diana':
                t="%.1f" % (4.458*event.x/500.0-0.6,)
            else:
                t="%.1f" % (12.0*event.x/500.0-2.0,)
            lab6.configure(text=t,bg="#33FFFF")
        elif (event.y>=40 and event.y<95) or \
                 (event.y<95 and Audio.gcom2.nspecial>0):
            lab1.configure(text='DF (Hz)',bg='red')
            idf=Audio.gcom2.idf
            t="%d" % int(idf+1200.0*event.x/500.0-600.0,)
            lab6.configure(text=t,bg="red")
        else:
            lab1.configure(text='Time (s)',bg='green')
            if mode.get()=='Diana':
                t="%.1f" % (event.x*30.0/500.0,)
            else:
                t="%.1f" % (53.0*event.x/500.0,)
            lab6.configure(text=t,bg="green")
    elif mode.get()=='Echo':
        lab1.configure(text='DF (Hz)',bg='red')
##        t="%d" % int((event.x-250),)
        t="%d" % int(0.3365*(event.x-250),)
        lab6.configure(text=t,bg="red")
    else:
        t="%.1f" % (event.x*30.0/500.0,)
        lab6.configure(text=t,bg='green')

#---------------------------------------------------- mouse_click_g1
def mouse_click_g1(event):
    global nopen
    if not nopen:
        if mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4':
            Audio.gcom2.mousedf=int(Audio.gcom2.idf+(event.x-250)*2.4)
        else:
            if Audio.gcom2.ndecoding==0:              #If decoder is busy, ignore
                Audio.gcom2.nagain=1
                Audio.gcom2.mousebutton=event.num     #Left=1, Right=3
                Audio.gcom2.npingtime=int(195+60*event.x) #Time (ms) of mouse-picked ping
                if mode.get()[:5]!='ISCAT' or event.num==3:
                    if Audio.gcom2.ndecoding0==4:
                        Audio.gcom2.ndecoding=4           #Decode from recorded file
                    elif Audio.gcom2.ndecoding0==1:
                        Audio.gcom2.ndecoding=5        #Decode data in main screen
    nopen=0

#------------------------------------------------------ double-click_g1
def double_click_g1(event):
    if (mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4') and \
        Audio.gcom2.ndecoding==0:
        g.freeze_decode=1
    
#------------------------------------------------------ mouse_up_g1
def mouse_up_g1(event):
    if mode.get()[:5]=='ISCAT' and Audio.gcom2.ndecoding==0:
        Audio.gcom2.nagain=1
        Audio.gcom2.mousebutton=event.num          #Left=1, Right=3
        Audio.gcom2.npingtime2=int(195+60*event.x) #Time (ms) of mouse-picked ping
        if Audio.gcom2.ndecoding0==4:
            Audio.gcom2.ndecoding=4           #Decode from recorded file
        elif Audio.gcom2.ndecoding0==1:
            Audio.gcom2.ndecoding=5           #Decode data in main screen

#------------------------------------------------------ right_arrow
def right_arrow(event=NONE):
    n=5*int(Audio.gcom2.mousedf/5)
    if n!=0: n=n+5
    if n==Audio.gcom2.mousedf: n=n+5
    Audio.gcom2.mousedf=n

#------------------------------------------------------ left_arrow
def left_arrow(event=NONE):
    n=5*int(Audio.gcom2.mousedf/5)
    if n==Audio.gcom2.mousedf: n=n-5
    Audio.gcom2.mousedf=n

#------------------------------------------------------ GenStdMsgs
def GenStdMsgs(event=NONE):
    global altmsg,MyCall0,addpfx0,ToRadio0
    t=ToRadio.get().upper().strip()
    ToRadio.delete(0,99)
    ToRadio.insert(0,t)
    if k2txb.get()!=0: ntx.set(1)
    Audio.gcom2.hiscall=(ToRadio.get()+(' '*12))[:12]
    for m in (tx1, tx2, tx3, tx4, tx5, tx6):
        m.delete(0,99)
    if mode.get()=="FSK441" or mode.get()[:5]=="ISCAT" or \
       mode.get()=='JTMS' or mode.get()=='Diana':
        r=report.get()
        tx1.insert(0,setmsg(options.tx1.get(),r))
        tx2.insert(0,setmsg(options.tx2.get(),r))
        tx3.insert(0,setmsg(options.tx3.get(),r))
        tx4.insert(0,setmsg(options.tx4.get(),r))
        tx5.insert(0,setmsg(options.tx5.get(),r))
        tx6.insert(0,setmsg(options.tx6.get(),r))
        if tx3.get()=='ROOO':
            tx3.delete(0,END)
            tx3.insert(0,'RO')
    elif mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4':
        if options.MyCall.get()!= MyCall0 or \
               options.addpfx.get()!= addpfx0 or ToRadio.get()!=ToRadio0:
            MyCall0=options.MyCall.get()
            addpfx0=options.addpfx.get()
            ToRadio0=ToRadio.get()
            t0=("SM5BSZ "+options.MyCall.get()).upper()
            Audio.gcom2.t0msg=(t0+' '*22)[:22]
            nplain,naddon,ndiff=Audio.chkt0()
            if nplain==1:
                MsgBox("Bad 'MyCall' or bad prefix/suffix?\nPlease check on Setup | Options screen.")
                options1()
            t0=("SM5BSZ "+ToRadio0).upper()
            Audio.gcom2.t0msg=(t0+' '*22)[:22]
            nplain,naddon,ndiff=Audio.chkt0()
            if nplain==1:
                MsgBox("Bad callsign in 'To Radio'?\nPlease check.")
            
        t0=(ToRadio.get() + " "+options.MyCall.get()).upper()
        Audio.gcom2.t0msg=(t0+' '*22)[:22]
        nplain,naddon,ndiff=Audio.chkt0()
        if nplain==0 and naddon==0 and ndiff==0:
            t0=t0 + " "+options.MyGrid.get()[:4]
        tx1.insert(0,t0.upper())
        tx2.insert(0,tx1.get()+" OOO")
        tx3.insert(0,"RO")
        tx4.insert(0,"RRR")
        tx5.insert(0,"73")

        t0="CQ " + options.MyCall.get().upper()
        Audio.gcom2.t0msg=(t0+' '*22)[:22]
        nplain,naddon,ndiff=Audio.chkt0()
        if nplain==0 and naddon==0 and ndiff==0:
            t0=t0 + " "+options.MyGrid.get()[:4]
        tx6.insert(0,t0.upper())
        altmsg=0
    elif mode.get()[:2]=="CW":
        tx1.insert(0,"[" + ToRadio.get() + " " +options.MyCall.get() + "]")
        tx2.insert(0,tx1.get()+" [OOO]")
        tx3.insert(0,ToRadio.get() + " " + options.MyCall.get()+" [RO]")
        tx4.insert(0,ToRadio.get() + " " + options.MyCall.get()+" [RRR]")
        tx5.insert(0,ToRadio.get() + " " + options.MyCall.get()+" [73]")
        tx6.insert(0,"[CQ " + options.MyCall.get() + "]")
    
#------------------------------------------------------ GenAltMsgs
def GenAltMsgs(event=NONE):
    global altmsg,tx6alt
    t=ToRadio.get().upper().strip()
    ToRadio.delete(0,99)
    ToRadio.insert(0,t)
    if k2txb.get()!=0: ntx.set(1)
    Audio.gcom2.hiscall=(ToRadio.get()+(' '*12))[:12]
    if (mode.get()[:4]=='JT65' or mode.get()=='Diana' or \
        mode.get()[:3]=='JT4') and ToRadio.get().find("/") == -1 and \
               options.MyCall.get().find("/") == -1:
        for m in (tx1, tx2, tx3, tx4, tx5, tx6):
            m.delete(0,99)
        t=ToRadio.get() + " "+options.MyCall.get()
        tx1.insert(0,t.upper())
        tx2.insert(0,tx1.get()+" OOO")
        tx3.insert(0,tx1.get()+" RO")
        tx4.insert(0,tx1.get()+" RRR")
        tx5.insert(0,"TNX 73 GL ")
        tx6.insert(0,tx6alt.upper())
        altmsg=1

#------------------------------------------------------ setmsg
def setmsg(template,r):
    msg=""
    t=options.MyCall.get()
    n=len(t)
    MySuffix=t[n-3:]
    n2=MySuffix.find('/')
    if(n2>=0): MySuffix=t[n-5:n-2]
    if MySuffix[0:1].isdigit()or MySuffix[1:2].isdigit():
        MySuffix=MySuffix[1:]
    t=ToRadio.get()
    n=len(t)
    HisSuffix=t[n-3:]
    n2=HisSuffix.find('/')
    if(n2>=0): HisSuffix=t[n-5:n-2]
    if HisSuffix[0:1].isdigit()or HisSuffix[1:2].isdigit():
        HisSuffix=HisSuffix[1:]
    npct=0
    for i in range(len(template)):
        if npct:
            if template[i]=="M": msg=msg+options.MyCall.get().upper().strip()
            if template[i]=="T": msg=msg+ToRadio.get().upper().strip()
            if template[i]=="R": msg=msg+r
            if template[i]=="G": msg=msg+options.MyGrid.get()[:4]
            if template[i]=="L": msg=msg+options.MyGrid.get()
            if template[i]=="S": msg=msg+MySuffix
            if template[i]=="H": msg=msg+HisSuffix
            npct=0
        else:
            npct=0
            if template[i]=="%":
                npct=1
            else:
                msg=msg+template[i]            
    return msg.upper()

#------------------------------------------------------ plot_echo
def plot_echo():
    "Plot red and blue curves for Echo mode."
    graph1.delete(ALL)
    y1=[]
    y2=[]
    for i in range(446):        #Find ymax for magenta/orange curves
        ss1=Audio.gcom2.ss1[i+1]
        y1.append(ss1)
        ss2=Audio.gcom2.ss2[i+1]
        y2.append(ss2)
    ymax=max(y1+y2)
    yfac=0.5
    if ymax>80.0/yfac: yfac=80.0/ymax
    xy1=[]
    xy2=[]
    fac=500.0/446.0
    for i in range(446):        #Make xy list for magenta/orange curves
        x=i*fac
        ss1=Audio.gcom2.ss1[i+1]
        n=int(90.0-yfac*ss1) + 20
        xy1.append(x)
        xy1.append(n)
        ss2=Audio.gcom2.ss2[i+1]
        n=int(90.0-yfac*ss2)
        xy2.append(x)
        xy2.append(n)
    graph1.create_line(xy1,fill='#33FFFF')            #Light blue
    graph1.create_line(xy2,fill="red")

#------------------------------------------------------ plot_meas
def plot_meas(nmeas,db):
    global xypm
    graph1.delete(ALL)
    if nmeas<2:
        xypm=[]
    xypm.append(nmeas)
    n=int(60.0-3.0*db)
    xypm.append(n)
    if nmeas>=2:
        graph1.create_line(xypm,fill="green")

#------------------------------------------------------ plot_large
def plot_large():
    "Plot the green, red, and blue curves."
    ngreen=Audio.gcom2.ngreen
    graph1.delete(ALL)
    y=[]
    if ngreen>0:
        for i in range(ngreen):             #Find ymax for green curve
            green=Audio.gcom2.green[i]
            y.append(green)
        ymax=max(y)
        if ymax<1: ymax=1
        yfac=4.0
        if ymax>75.0/yfac: yfac=75.0/ymax
        xy=[]
        for i in range(ngreen):             #Make xy list for green curve
            green=Audio.gcom2.green[i]
            n=int(105.0-yfac*green)
            if mode.get()=='Diana':
                xy.append(2*i)
            else:
                xy.append(i)
            xy.append(n)
        graph1.create_line(xy,fill="green")

        if Audio.gcom2.nspecial==0 or mode.get()[:3]=='JT4':
            y=[]
            for i in range(446):                #Find ymax for red curve
                psavg=Audio.gcom2.psavg[i+1]
                y.append(psavg)
            ymax=max(y)
            yfac=30.0
            if ymax>85.0/yfac: yfac=85.0/ymax
            xy=[]
            fac=500.0/446.0
            for i in range(446):                #Make xy list for red curve
                x=i*fac
                if mode.get()[:3]=='JT4':
                    x=i*500.0/548.571 + 47                      #empirical
                psavg=Audio.gcom2.psavg[i+1]
                n=int(90.0-yfac*psavg)
                xy.append(x)
                xy.append(n)
            graph1.create_line(xy,fill="red")
        else:
            y1=[]
            y2=[]
            for i in range(446):        #Find ymax for magenta/orange curves
                ss1=Audio.gcom2.ss1[i+1]
                y1.append(ss1)
                ss2=Audio.gcom2.ss2[i+1]
                y2.append(ss2)
            ymax=max(y1+y2)
            yfac=30.0
            if ymax>85.0/yfac: yfac=85.0/ymax
            xy1=[]
            xy2=[]
            fac=500.0/446.0
            for i in range(446):        #Make xy list for magenta/orange curves
                x=i*fac
                ss1=Audio.gcom2.ss1[i+1]
                n=int(90.0-yfac*ss1)
                xy1.append(x)
                xy1.append(n)
                ss2=Audio.gcom2.ss2[i+1]
                n=int(90.0-yfac*ss2) - 20
                xy2.append(x)
                xy2.append(n)
            graph1.create_line(xy1,fill="magenta")
            graph1.create_line(xy2,fill="orange")

            x1 = 250.0 + fac*Audio.gcom2.ndf/2.6916504
            x2 = x1 + Audio.gcom2.mode65*Audio.gcom2.nspecial*10*fac
            graph1.create_line([x1,85,x1,95],fill="yellow")
            graph1.create_line([x2,85,x2,95],fill="yellow")
            t="RO"
            if Audio.gcom2.nspecial==3: t="RRR"
            if Audio.gcom2.nspecial==4: t="73"
            graph1.create_text(x2+3,93,anchor=W,text=t,fill="yellow")

        if Audio.gcom2.ccf[0] != -9999.0:
            y=[]
            iz=65
            if mode.get()=='Diana':
                iz=96
            fac=500.0/iz
            for i in range(iz):             #Find ymax for blue curve
                ccf=Audio.gcom2.ccf[i]
                y.append(ccf)
            ymax=max(y)
            yfac=40.0
            if ymax>55.0/yfac: yfac=55.0/ymax
            xy2=[]
            for i in range(iz):             #Make xy list for blue curve
                x=(i+0.5)*fac
                if mode.get()[:3]=='JT4':
                    x=(i+0.5)*500.0/105.0 + 15      #15 is empirical
                ccf=Audio.gcom2.ccf[i]
                n=int(60.0-yfac*ccf)
                xy2.append(x)
                xy2.append(n)
            graph1.create_line(xy2,fill='#33FFFF')

#  Put in the tick marks
        for i in range(13):
            x=int(i*41.667)
            j2=115
            if i==1 or i==6 or i==11: j2=110
            graph1.create_line([x,j2,x,125],fill="red")
            if Audio.gcom2.nspecial==0:
#                x=int((i-0.8)*41.667)
                j1=9
                if i==2 or i==7 or i==12: j1=14
                graph1.create_line([x,0,x,j1],fill="#33FFFF")  #light blue
            else:
                graph1.create_line([x,0,x,125-j2],fill="red")

#------------------------------------------------------ plot_small
def plot_small():
    graph2.delete(ALL)
    xy=[]
    xy2=[]
    df=11025.0/256.0
    fac=150.0/3500.0
    for i in range(81):
        x=int(i*df*fac)
        xy.append(x)
        psavg=Audio.gcom2.psavg[i]
        n=int(150.0-2*psavg)
        xy.append(n)
        if mode.get()=='FSK441' or mode.get()=="JTMS":
            ps0=Audio.gcom2.ps0[i]
            n=int(150.0-2*ps0)
            xy2.append(x)
            xy2.append(n)
    graph2.create_line(xy,fill="magenta")
    if mode.get()=='FSK441' or mode.get()=="JTMS":
        graph2.create_line(xy2,fill="red")
        for i in range(4):
            x=(i+2)*441*fac
            graph2.create_line([x,0,x,20],fill="yellow")
    for i in range(7):
        x=i*500*fac
        ytop=110
        if i%2: ytop=115
        graph2.create_line([x,120,x,ytop],fill="white")

###------------------------------------------------------ plot_yellow
##def plot_yellow():
##    nz=int(Audio.gcom2.ps0[215])
##    if nz>10:
##        y=[]
##        for i in range(nz):             #Find ymax for yellow curve
##            n=Audio.gcom2.ps0[i]
##            y.append(n)
##        ymax=max(y)
##        fac=1.0
##        if ymax>60: fac=60.0/ymax
##        xy2=[]
##        for i in range(nz):
##            x=int(2.34*i)
##            y=fac*Audio.gcom2.ps0[i] + 8
##            n=int(85.0-y)
##            xy2.append(x)
##            xy2.append(n)
##        graph1.create_line(xy2,fill="yellow")

#------------------------------------------------------ update
def update():
    global root_geom,isec0,naz,nel,ndmiles,ndkm,nhotaz,nhotabetter,nopen, \
           im,pim,cmap0,isync,isync441,isync_iscat,isync65,               \
           isync_save,idsec,first,itol,txsnrdb,tx6alt,nmeas
    
    utc=time.gmtime(time.time()+0.1*idsec)
    isec=utc[5]

    if isec != isec0:                           #Do once per second
        isec0=isec
        t=time.strftime('%Y %b %d\n%H:%M:%S',utc)
        Audio.gcom2.utcdate=t[:12]
        Audio.gcom2.iyr=utc[0]
        Audio.gcom2.imo=utc[1]
        Audio.gcom2.ida=utc[2]
        Audio.gcom2.ihr=utc[3]
        Audio.gcom2.imi=utc[4]
        Audio.gcom2.isc=utc[5]
        ldate.configure(text=t)
        root_geom=root.geometry()
        utchours=utc[3]+utc[4]/60.0 + utc[5]/3600.0
        naz,nel,ndmiles,ndkm,nhotaz,nhotabetter=Audio.azdist0( \
            options.MyGrid.get().upper(),HisGrid.get().upper(),utchours)
        azdist()
        g.nfreq=nfreq.get()

        if tx1.get()[0:2]=='GO' and mode.get()=='Echo':
            try:
                nmin=int(tx1.get()[3:5])
            except:
                nmin=10
            if isec==0 and (utc[4]%nmin)==0 and lauto==0:
                toggleauto()
            if isec==4 and (utc[4]%nmin)==1 and lauto==1:
                toggleauto()
                Audio.gcom2.nsumecho=0
        
        if Audio.gcom2.ndecoding==0:
            g.AzSun,g.ElSun,g.AzMoon,g.ElMoon,g.AzMoonB,g.ElMoonB,g.ntsky, \
                g.ndop,g.ndop00,g.dbMoon,g.RAMoon,g.DecMoon,g.HA8,g.Dgrd,  \
                g.sd,g.poloffset,g.MaxNR,g.dfdt,g.dfdt0,g.RaAux,g.DecAux, \
                g.AzAux,g.ElAux,g.w1,g.w2,g.w501,g.w502,g.xlst = Audio.astro0( \
                    utc[0],utc[1],utc[2],utchours,nfreq.get(),            \
                    options.MyGrid.get().upper(),       \
                    options.auxra.get()+(' '*9)[:9],    \
                    options.auxdec.get()+(' '*9)[:9])

            if len(HisGrid.get().strip())<4:
                g.ndop=g.ndop00
                g.dfdt=g.dfdt0

            if (ntx.get()==1 and noshjt65.get()) or noshjt65all.get():
                Audio.gcom2.ntx2=1
            else:
                Audio.gcom2.ntx2=0

        if mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4'\
               or mode.get()[:2]=='CW' or mode.get()=='Echo' \
               or mode.get()=='Diana' or mode.get()=='Measure':
            graph2.delete(ALL)
            graph2.create_text(80,13,anchor=CENTER,text="Moon",font=g2font)
            graph2.create_text(13,37,anchor=W, text="Az: %6.2f" % g.AzMoon,font=g2font)
            graph2.create_text(13,61,anchor=W, text="El: %6.2f" % g.ElMoon,font=g2font)
            graph2.create_text(13,85,anchor=W, text="Dop:%6d" % g.ndop,font=g2font)
            graph2.create_text(13,109,anchor=W,text="Dgrd:%5.1f" % g.Dgrd,font=g2font)

        if mode.get()=='Measure' and Audio.gcom2.monitoring:
            nmeas=nmeas+1
            if nmeas>500: nmeas=1
            db=20.0*log10(g.rms/770.0+0.01)
            t="%5d  %6.1f\n" % (nmeas,db,)
            text.configure(state=NORMAL)
            text.insert(END,t)
            text.see(END)
            plot_meas(nmeas,db)
        else:
            nmeas=0

    if (mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4' \
        or mode.get()=='Diana') and g.freeze_decode:
        itol=2
        ltol.configure(text='Tol    '+str(50))
        Audio.gcom2.dftolerance=50
        nfreeze.set(1)
        Audio.gcom2.nfreeze=1
        if Audio.gcom2.monitoring:
            Audio.gcom2.ndecoding=1
            Audio.gcom2.nagain=0
        else:
            Audio.gcom2.ndecoding=4
            Audio.gcom2.nagain=1
        g.freeze_decode=0

    n=-99
    g.rms=g.rms+0.001
    if g.rms > 0:
        n=int(20.0*log10(g.rms/770.0+0.01))
    else:
        print "RMS noise:", g.rms, " out of range."
    t="Rx noise:%3d dB" % (n,)
    if n>=-10 and n<=10:
        msg4.configure(text=t,bg='gray85')
    else:
        msg4.configure(text=t,bg='red')

    t=g.ftnstr(Audio.gcom2.decodedfile)
#    i=t.rfind(".")
    i=g.rfnd(t,".")
    t=t[:i]
    lab3.configure(text=t)
    if mode.get() != g.mode or first:
        if mode.get()=="FSK441":
            msg2.configure(bg='#FFFF00')
        elif mode.get()[:4]=="JT65":
            msg2.configure(bg='#00FFFF')
        elif mode.get()=="CW":
            msg2.configure(bg='#00FF00')
        elif mode.get()[:5]=="ISCAT":
            msg2.configure(bg='#CCFFFF')
        elif mode.get()[:5]=="Diana":
            msg2.configure(bg='#CCFF00')
        elif mode.get()[:4]=="JTMS":
            msg2.configure(bg='#CC4444')
        elif mode.get()[:3]=="JT4":
            msg2.configure(bg='#88FF88')
        elif mode.get()=="Echo":
            msg2.configure(bg='#FF0000')
        elif mode.get()=="Measure":
            msg2.configure(bg='#FF8800')
        g.mode=mode.get()
        if first and mode.get()!='Echo': GenStdMsgs()
        first=0

    if options.genmsg.get():
        GenStdMsgs()
        options.genmsg.set(0)

    samfac_in=Audio.gcom1.mfsample/110250.0
    samfac_out=Audio.gcom1.mfsample2/110250.0
    xin=1
    xout=1
    try:
        xin=samfac_in/options.samfacin.get()
        xout=samfac_out/options.samfacout.get()
        if xin<0.999 or xin>1.001 or xout<0.999 or xout>1.001:
            lab8.configure(text="%6.4f   %6.4f" \
                % (options.samfacin.get(),options.samfacout.get()), \
                fg='black',bg='red')
        else:
            lab8.configure(fg='gray85',bg='gray85')
    except:
        pass

    msg1.configure(text="%6.4f %6.4f" % (samfac_in,samfac_out))
    t=mode.get()
    msg2.configure(text=t)
    t="Freeze DF:%4d" % (int(Audio.gcom2.mousedf),)
    if abs(int(Audio.gcom2.mousedf))>600:
        msg3.configure(text=t,fg='black',bg='red')
    else:
        msg3.configure(text=t,fg='black',bg='gray85')    
    bdecode.configure(bg='gray85',activebackground='gray95')
    if (sys.platform == 'darwin'):
        bdecode.configure(text='Decode')
    if Audio.gcom2.ndecoding:       #Set button bg=light_blue while decoding
        bdecode.configure(bg='#66FFFF',activebackground='#66FFFF')
        if (sys.platform == 'darwin'):
           bdecode.configure(text='*Decode*')
    msg5.configure(text="T/R Period: %d s" % (Audio.gcom1.trperiod,))
    if mode.get()=="CW": color='white'
    elif mode.get()=='FSK441' or mode.get()=='JTMS' or mode.get()[:5]=='ISCAT':
        if(Audio.gcom1.trperiod==15): color='yellow'
        else: color='white'
    else:
        color='gray85'
    msg5.configure(bg=color)

    tx1.configure(bg='white')
    tx2.configure(bg='white')
    tx3.configure(bg='white')
    tx4.configure(bg='white')
    tx5.configure(bg='white')
    if len(tx5.get())>13 and \
            (mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4'):
        Audio.gcom2.t0msg=(tx5.get()+' '*22)[:22]
        nplain,naddon,ndiff=Audio.chkt0()
        if nplain==1: tx5.configure(bg='pink')
    tx6.configure(bg='white')
    if tx6.get()[:1]=='#':
        try:
            txsnrdb=float(tx6.get()[1:])
            if txsnrdb>-99.0 and txsnrdb<40.0:
                Audio.gcom1.txsnrdb=txsnrdb
                tx6.configure(bg='orange')
        except:
            txsnrdb=99.0
    else:
        txsnrdb=99.0
        Audio.gcom1.txsnrdb=txsnrdb
    if Audio.gcom2.monitoring and not Audio.gcom1.transmitting:
        bmonitor.configure(bg='green')
        if (sys.platform == 'darwin'):
           bmonitor.configure(text='*Monitor*')
    else:
        bmonitor.configure(bg='gray85')    
        if (sys.platform == 'darwin'):
           bmonitor.configure(text='Monitor')    
    if Audio.gcom1.transmitting:
        nmsg=int(Audio.gcom2.nmsg)
        t=g.ftnstr(Audio.gcom2.sending)
        if mode.get()=='Echo':
            t='ECHO TEST'
            nmsg=9
            Audio.gcom2.ntxnow=0
        t="Txing:  "+t[:nmsg]
        bgcolor='yellow'
        if Audio.gcom2.sendingsh==1:  bgcolor='#66FFFF'    #Shorthand (lt blue)
        if Audio.gcom2.sendingsh==-1: bgcolor='red'        #Plain Text
        if Audio.gcom2.sendingsh==2: bgcolor='pink'        #Test file
        if txsnrdb<90.0: bgcolor='orange'                  #Simulation mode
        if Audio.gcom2.ntxnow==1: tx1.configure(bg=bgcolor)
        elif Audio.gcom2.ntxnow==2: tx2.configure(bg=bgcolor)
        elif Audio.gcom2.ntxnow==3: tx3.configure(bg=bgcolor)
        elif Audio.gcom2.ntxnow==4: tx4.configure(bg=bgcolor)
        elif Audio.gcom2.ntxnow==5: tx5.configure(bg=bgcolor)
        elif Audio.gcom2.ntxnow==6: tx6.configure(bg=bgcolor)
    else:
        bgcolor='green'
        t='Receiving'
    msg7.configure(text=t,bg=bgcolor)

    if Audio.gcom2.ndecdone==1 or g.cmap != cmap0:
        if Audio.gcom2.ndecdone==1:
            if isync==-99 or isync==99:
                isync=isync_save
                Audio.gcom2.minsigdb=isync
            try:
                f=open(appdir+'/decoded.txt',mode='r')
                lines=f.readlines()
                f.close()
            except:
                lines=""
            text.configure(state=NORMAL)
            for i in range(len(lines)):
                text.insert(END,lines[i])
            text.see(END)
#            text.configure(state=DISABLED)

            if mode.get()[:4]=='JT65':
                try:
                    f=open(appdir+'/decoded.ave',mode='r')
                    lines=f.readlines()
                    f.close()
                except:
                    lines[0]=""
                    lines[1]=""
                avetext.configure(state=NORMAL)
                avetext.delete('1.0',END)
                if len(lines)>1:
                    avetext.insert(END,lines[0])
                    avetext.insert(END,lines[1])
#                avetext.configure(state=DISABLED)
            Audio.gcom2.ndecdone=2
        
        if g.cmap != cmap0:
            im.putpalette(g.palette)
            cmap0=g.cmap

        if mode.get()=='Echo':
            plot_echo()
        elif mode.get()[:4]=='JT65' or mode.get()[:3]=='JT4' or \
                 mode.get()=='Diana':
            plot_large()
        else:
            im.putdata(Audio.gcom2.b)
            pim=ImageTk.PhotoImage(im)          #Convert Image to PhotoImage
            graph1.delete(ALL)
# NB: top two lines are probably invisible ...
            graph1.create_image(0,0,anchor='nw',image=pim)
            t=g.filetime(g.ftnstr(Audio.gcom2.decodedfile))
            graph1.create_text(100,80,anchor=W,text=t,fill="white")
            if mode.get()[:5]=='ISCAT' and Audio.gcom2.npingtime>0:
                if Audio.gcom2.npingtime2-Audio.gcom2.npingtime >= 1000:
                    x1=(Audio.gcom2.npingtime - 195)/60.0
                    x2=(Audio.gcom2.npingtime2 - 195)/60.0
                else:
                    if Audio.gcom2.mousebutton==3:
                        x1=2
                        x2=(Audio.gcom2.npingtime - 195)/60.0
                    else:
                        x1=(Audio.gcom2.npingtime  - 195 - 1000)/60.0
                        x2=(Audio.gcom2.npingtime2 - 195 + 1000)/60.0
                graph1.create_line([x1,90,x2,90],fill="yellow")
                graph1.create_line([x1,85,x1,95],fill="yellow")
                graph1.create_line([x2,85,x2,95],fill="yellow")
                Audio.gcom2.npingtime=0
                Audio.gcom2.npingtime2=0
            plot_small()
        if loopall: opennext()
        nopen=0

# Save some parameters
    g.mode=mode.get()
    g.report=report.get()
    if mode.get()=='FSK441' or mode.get()=='JTMS': isync441=isync
    elif mode.get()[:5]=="ISCAT": isync_iscat=isync
    elif mode.get()[:4]=='JT65': isync65=isync
    Audio.gcom1.txfirst=TxFirst.get()
    try:
        Audio.gcom1.samfacin=options.samfacin.get()
    except:
        Audio.gcom1.samfacin=1.0
    try:
        Audio.gcom1.samfacout=options.samfacout.get()
    except:
        Audio.gcom1.samfacout=1.0
#    if Audio.gcom1.samfacin>1.01: Audio.gcom1.samfacin=1.01
# ... etc.
    Audio.gcom2.mycall=(options.MyCall.get()+(' '*12))[:12]
    try:
        Audio.gcom2.hiscall=(ToRadio.get()+(' '*12))[:12]
    except:
        MsgBox("Bad callsign: "+ToRadio.get()+".\nPlease re-enter.")
        clrToRadio(None)
    Audio.gcom2.hisgrid=(HisGrid.get()+(' '*6))[:6]
    Audio.gcom4.addpfx=(options.addpfx.get().lstrip().upper()+(' '*8))[:8]
    Audio.gcom2.ntxreq=ntx.get()
    tx=(tx1,tx2,tx3,tx4,tx5,tx6)
    Audio.gcom2.txmsg=(tx[ntx.get()-1].get()+(' '*28))[:28]
    Audio.gcom2.mode=(mode.get()+(' '*6))[:6]
    Audio.gcom2.shok=ShOK.get()
    Audio.gcom2.nsave=nsave.get()
    Audio.gcom2.nzap=nzap.get()
    Audio.gcom2.ndebug=ndebug.get()
    Audio.gcom2.minsigdb=isync
    Audio.gcom2.nafc=nafc.get()
    Audio.gcom2.nfreeze=nfreeze.get()
    Audio.gcom2.dftolerance=ntol[itol]
    Audio.gcom2.neme=neme.get()
    Audio.gcom2.ndepth=ndepth.get()
    Audio.gcom2.ndtr=options.ndtr.get()
    if mode.get()=='CW':
        Audio.gcom2.ntdecode=56
    else:
        if qdecode.get():
            Audio.gcom2.ntdecode=48
        else:
            Audio.gcom2.ntdecode=52
##    if mode.get()=='Diana':
##        Audio.gcom2.ntdecode=26

    try:
        Audio.gcom2.idinterval=options.IDinterval.get()
    except:
        Audio.gcom2.idinterval=0
#    Audio.gcom1.rxdelay=float('0'+options.RxDelay.get())
#    Audio.gcom1.txdelay=float('0'+options.TxDelay.get())
    Audio.gcom2.nslim2=isync-4
    if nshrx.get()==0 and (mode.get()=='FSK441' or mode.get()=='JTMS'):
            Audio.gcom2.nslim2=99
    try:
        Audio.gcom2.nport=int(options.PttPort.get())
    except:
        Audio.gcom2.nport=0

    Audio.gcom2.pttport=(options.PttPort.get() + (' '*80))[:80]

    try:
        Audio.gcom2.ntc=options.ntc.get()
        if int(options.ntc.get()<=0): Audio.gcom2.ntc=1
        Audio.gcom2.nfrit=options.fRIT.get()
        Audio.gcom2.ndither=options.dither.get()
##        Audio.gcom2.necho=options.necho.get()
##        Audio.gcom2.dlatency=options.dlatency.get()
    except:
        pass

    if options.iregion.get():
        options.cb1.configure(state=DISABLED)
        options.cb2.configure(state=DISABLED)
    else:
        options.cb1.configure(state=NORMAL)
        options.cb2.configure(state=NORMAL)

    if altmsg: tx6alt=tx6.get()
    Audio.gcom2.nlowbeacon=nlowbeacon.get()
# Queue up the next update
    ldate.after(100,update)
    
#------------------------------------------------------ Top level frame
frame = Frame(root)

#------------------------------------------------------ Menu Bar
if (sys.platform != 'darwin'):
   mbar = Frame(frame)
   mbar.pack(fill = X)
else:
   mbar = Menu(root)
   root.config(menu=mbar)

# Tearoff menus make less sense under darwin
use_tearoff = (sys.platform != 'darwin')

#------------------------------------------------------ File Menu
if (sys.platform != 'darwin'):
   filebutton = Menubutton(mbar, text = 'File')
   filebutton.pack(side = LEFT)
   filemenu = Menu(filebutton, tearoff=0)
   filebutton['menu'] = filemenu
else:
   filemenu = Menu(mbar, tearoff=0)
filemenu.add('command', label = 'Open', command = openfile, \
             accelerator='Ctrl+O')
filemenu.add('command', label = 'Open next in directory', command = opennext, \
             accelerator='F6')
filemenu.add('command', label = 'Decode remaining files in directory', \
             command = decodeall, accelerator='Shift+F6')
filemenu.add_separator()
filemenu.add('command', label = 'Delete all *.WAV files in RxWav', \
             command = delwav)
filemenu.add_separator()
filemenu.add('command', label = 'Erase ALL.TXT', command = del_all)
filemenu.add_separator()
filemenu.add('command', label = 'Exit', command = quit, accelerator='Alt+F4')

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="File", menu=filemenu)

#------------------------------------------------------ Setup menu
if (sys.platform != 'darwin'):
   setupbutton = Menubutton(mbar, text = 'Setup')
   setupbutton.pack(side = LEFT)
   setupmenu = Menu(setupbutton, tearoff=0)
   setupbutton['menu'] = setupmenu
else:   
   setupmenu = Menu(mbar, tearoff=0)
setupmenu.add('command', label = 'Options', command = options1, \
              accelerator='F2')
setupmenu.add_separator()
setupmenu.add('command', label = 'Toggle size of text window', command=textsize)
setupmenu.add('command', label = 'Generate messages for test tones', command=testmsgs)
setupmenu.add_separator()
setupmenu.add_checkbutton(label = 'F4 sets Tx6',variable=kb8rq)
setupmenu.add_checkbutton(label = 'Double-click on callsign sets TxFirst',
                          variable=setseq)
setupmenu.add_checkbutton(label = 'Gen Msgs sets Tx1',variable=k2txb)
setupmenu.add_separator()
setupmenu.add_checkbutton(label = 'Monitor ON at startup',variable=nmonitor)
setupmenu.add_checkbutton(label = 'Low-Duty Beacon Mode',variable=nlowbeacon)
setupmenu.add_separator()
setupmenu.add_checkbutton(label = 'Enable diagnostics',variable=ndebug)
if (sys.platform == 'darwin'):
    mbar.add_cascade(label="Setup", menu=setupmenu)

#------------------------------------------------------ View menu
if (sys.platform != 'darwin'):
    viewbutton=Menubutton(mbar,text='View')
    viewbutton.pack(side=LEFT)
    viewmenu=Menu(viewbutton,tearoff=0)
    viewbutton['menu']=viewmenu
else:    
    viewmenu=Menu(mbar,tearoff=0)
viewmenu.add('command', label = 'SpecJT', command = showspecjt, \
             accelerator='F10')
viewmenu.add('command', label = 'Astronomical data', command = astro1, \
             accelerator='Shift+F10')

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="View", menu=viewmenu)

#------------------------------------------------------ Mode menu
if (sys.platform != 'darwin'):
    modebutton = Menubutton(mbar, text = 'Mode')
    modebutton.pack(side = LEFT)
    modemenu = Menu(modebutton, tearoff=0)
    modebutton['menu'] = modemenu
else:    
    modemenu = Menu(mbar, tearoff=use_tearoff)

# To enable menu item 0:
# modemenu.entryconfig(0,state=NORMAL)
# Can use the following to retrieve the state:
# state=modemenu.entrycget(0,"state")

modemenu.add_radiobutton(label = 'FSK441', variable=mode,command = ModeFSK441, state=NORMAL)
modemenu.add_radiobutton(label = 'ISCAT-A', variable=mode, command = ModeISCAT_A)
modemenu.add_radiobutton(label = 'ISCAT-B', variable=mode, command = ModeISCAT_B)
modemenu.add_radiobutton(label = 'JT65A', variable=mode, command = ModeJT65A)
modemenu.add_radiobutton(label = 'JT65B', variable=mode, command = ModeJT65B)
modemenu.add_radiobutton(label = 'JT65C', variable=mode, command = ModeJT65C)
modemenu.add_radiobutton(label = 'JT4A', variable=mode, command = ModeJT4A)
modemenu.add_radiobutton(label = 'JT4B', variable=mode, command = ModeJT4B)
modemenu.add_radiobutton(label = 'JT4C', variable=mode, command = ModeJT4C)
modemenu.add_radiobutton(label = 'JT4D', variable=mode, command = ModeJT4D)
modemenu.add_radiobutton(label = 'JT4E', variable=mode, command = ModeJT4E)
modemenu.add_radiobutton(label = 'JT4F', variable=mode, command = ModeJT4F)
modemenu.add_radiobutton(label = 'JT4G', variable=mode, command = ModeJT4G)
modemenu.add_radiobutton(label = 'CW', variable=mode, command = ModeCW)
modemenu.add_radiobutton(label = 'Echo', variable=mode, command = ModeEcho)
modemenu.add_radiobutton(label = 'Measure', variable=mode, command = ModeMeasure)

try:
    f=open(appdir+'/experimental','r')
    modemenu.add_separator()
    modemenu.add_radiobutton(label = 'JTMS', variable=mode, command = ModeJTMS)
    modemenu.add_radiobutton(label = 'Diana', variable=mode, command = ModeDiana)
except:
    pass

try:
    f=open(appdir+'/txboth','r')
    Audio.gcom2.ntxboth=1
except:
    Audio.gcom2.ntxboth=0

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="Mode", menu=modemenu)

#------------------------------------------------------ Decode menu
if (sys.platform != 'darwin'):
    decodebutton = Menubutton(mbar, text = 'Decode')
    decodebutton.pack(side = LEFT)
    decodemenu = Menu(decodebutton, tearoff=use_tearoff)
    decodebutton['menu'] = decodemenu
else:    
    decodemenu = Menu(mbar, tearoff=use_tearoff)
##decodemenu.ISCAT=Menu(decodemenu,tearoff=0)
##decodemenu.ISCAT.add_checkbutton(label='Exhaustive',variable=iscat_ex)
decodemenu.JT65=Menu(decodemenu,tearoff=0)
decodemenu.JT65.add_checkbutton(label='Only EME calls in Deep Search',variable=neme)
decodemenu.JT65.add_checkbutton(label='No Shorthand decodes',variable=noshjt65all)
decodemenu.JT65.add_checkbutton(label='No Shorthands if Tx 1',variable=noshjt65)
decodemenu.JT65.add_checkbutton(label='Quick Decode',variable=qdecode)
decodemenu.JT65.add_separator()
decodemenu.JT65.add_radiobutton(label = 'No Deep Search',
                                variable=ndepth, value=0)
decodemenu.JT65.add_radiobutton(label = 'Normal Deep Search',
                                variable=ndepth, value=1)
decodemenu.JT65.add_radiobutton(label = 'Aggressive Deep Search',
                                variable=ndepth, value=2)
decodemenu.JT65.add_radiobutton(label ='Include Average in Aggressive Deep Search',
                                variable=ndepth, value=3)

##decodemenu.add_cascade(label = 'ISCAT',menu=decodemenu.ISCAT)
decodemenu.add_cascade(label = 'JT65',menu=decodemenu.JT65)

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="Decode", menu=decodemenu)

#------------------------------------------------------ Save menu
if (sys.platform != 'darwin'):
    savebutton = Menubutton(mbar, text = 'Save')
    savebutton.pack(side = LEFT)
    savemenu = Menu(savebutton, tearoff=use_tearoff)
    savebutton['menu'] = savemenu
else:    
    savemenu = Menu(mbar, tearoff=use_tearoff)
nsave=IntVar()
savemenu.add_radiobutton(label = 'None', variable=nsave,value=0)
savemenu.add_radiobutton(label = 'Save decoded', variable=nsave,value=1)
savemenu.add_radiobutton(label = 'Save if Auto On', variable=nsave,value=2)
savemenu.add_radiobutton(label = 'Save all', variable=nsave,value=3)
nsave.set(0)

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="Save", menu=savemenu)

#------------------------------------------------------ Band menu
if (sys.platform != 'darwin'):
    bandbutton = Menubutton(mbar, text = 'Band')
    bandbutton.pack(side = LEFT)
    bandmenu = Menu(bandbutton, tearoff=use_tearoff)
    bandbutton['menu'] = bandmenu
else:    
    bandmenu = Menu(mbar, tearoff=use_tearoff)
nfreq=IntVar()
bandmenu.add_radiobutton(label = '1.8', variable=nfreq,value=2)
bandmenu.add_radiobutton(label = '3.5', variable=nfreq,value=4)
bandmenu.add_radiobutton(label = '7', variable=nfreq,value=7)
bandmenu.add_radiobutton(label = '10', variable=nfreq,value=10)
bandmenu.add_radiobutton(label = '14', variable=nfreq,value=14)
bandmenu.add_radiobutton(label = '18', variable=nfreq,value=18)
bandmenu.add_radiobutton(label = '21', variable=nfreq,value=21)
bandmenu.add_radiobutton(label = '24', variable=nfreq,value=24)
bandmenu.add_radiobutton(label = '28', variable=nfreq,value=28)
bandmenu.add_radiobutton(label = '50', variable=nfreq,value=50)
bandmenu.add_radiobutton(label = '70', variable=nfreq,value=70)
bandmenu.add_radiobutton(label = '144', variable=nfreq,value=144)
bandmenu.add_radiobutton(label = '222', variable=nfreq,value=222)
bandmenu.add_radiobutton(label = '432', variable=nfreq,value=432)
bandmenu.add_radiobutton(label = '1296', variable=nfreq,value=1296)
bandmenu.add_radiobutton(label = '2304', variable=nfreq,value=2304)
bandmenu.add_radiobutton(label = '3456', variable=nfreq,value=3456)
bandmenu.add_radiobutton(label = '5760', variable=nfreq,value=5760)
bandmenu.add_radiobutton(label = '10368', variable=nfreq,value=10368)
nfreq.set(144)

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="Band", menu=bandmenu)

#------------------------------------------------------ Help menu
if (sys.platform != 'darwin'):
   helpbutton = Menubutton(mbar, text = 'Help')
   helpbutton.pack(side = LEFT)
   helpmenu = Menu(helpbutton, tearoff=0)
   helpbutton['menu'] = helpmenu
else:   
   helpmenu = Menu(mbar, tearoff=0)
helpmenu.add('command',label="Online User's Guide",command=usersguide)
helpmenu.add('command',label="Online WSJT9 Supplement", \
             command=wsjt9supp,accelerator='F9')
helpmenu.add('command', label = 'Keyboard shortcuts', command = shortcuts, \
             accelerator='F1')
helpmenu.add('command', label = 'Special mouse commands', \
             command = mouse_commands, accelerator='Shift+F1')
helpmenu.add('command', label = 'What message to send?', \
             command = what2send, accelerator='F5')
helpmenu.add('command', label = 'Examples of minimal JT65 QSOs', \
             command = minimal_qso, accelerator='Shift+F5')
helpmenu.add('command', label = 'Available suffixes and add-on prefixes', \
             command = prefixes)
helpmenu.add('command', label = 'About WSJT', command = about, \
             accelerator='Ctrl+F1')

if (sys.platform == 'darwin'):
    mbar.add_cascade(label="Help", menu=helpmenu)

#------------------------------------------------------ Graphics areas
iframe1 = Frame(frame, bd=1, relief=SUNKEN)
graph1=Canvas(iframe1, bg='black', width=500, height=120,cursor='crosshair')
Widget.bind(graph1,"<Motion>",dtdf_change)
Widget.bind(graph1,"<Button-1>",mouse_click_g1)
Widget.bind(graph1,"<Double-Button-1>",double_click_g1)
Widget.bind(graph1,"<ButtonRelease-1>",mouse_up_g1)
Widget.bind(graph1,"<Button-3>",mouse_click_g1)
graph1.pack(side=LEFT)
graph2=Canvas(iframe1, bg='black', width=150, height=120,cursor='crosshair')
graph2.pack(side=LEFT)
g2font=graph2.option_get("font","font")
if g2font!="": g.g2font=g2font
iframe1.pack(expand=1, fill=X, padx=4)

#------------------------------------------------------ Labels under graphics
iframe2a = Frame(frame, bd=1, relief=FLAT, height=15)
lab1=Label(iframe2a, text='Time (s)')
lab1.place(x=250, y=6, anchor=CENTER)
lab3=Label(iframe2a, text=' ')
lab3.place(x=400,y=6, anchor=CENTER)
lab4=Label(iframe2a, text='1             2            3')
lab4.place(x=593,y=6, anchor=CENTER)
iframe2a.pack(expand=1, fill=X, padx=1)
iframe2 = Frame(frame, bd=1, relief=FLAT,height=15)
lab2=Label(iframe2, text='FileID     Sync     dB        DT        DF      W')
lab2.place(x=3,y=6, anchor='w')
lab5=Label(iframe2, text='Freq (kHz)')
lab5.place(x=580,y=6, anchor=CENTER)
lab6=Label(iframe2a,text='0.0',bg='green')
lab6.place(x=40,y=6, anchor=CENTER)
lab7=Label(iframe2a,text='F3',fg='gray85')
lab7.place(x=495,y=6, anchor=CENTER)
lab8=Label(iframe2a,text='1.0000  1.0000',fg='gray85')
lab8.place(x=135,y=6, anchor=CENTER)
iframe2.pack(expand=1, fill=X, padx=4)

#-------------------------------------------------------- Decoded text
iframe4 = Frame(frame, bd=1, relief=SUNKEN)
text=Text(iframe4, height=6, width=80)
text.bind('<Double-Button-1>',dbl_click_text)
text.bind('<Double-Button-3>',dbl_click3_text)
text.bind('<Key>',textkey)

root.bind_all('<F1>', shortcuts)
root.bind_all('<Shift-F1>', mouse_commands)
root.bind_all('<Control-F1>', about)
root.bind_all('<F2>', options1)
root.bind_all('<F3>', txmute)
root.bind_all('<F4>', clrToRadio)
root.bind_all('<Alt-F4>', quit)
root.bind_all('<F5>', what2send)
root.bind_all('<Shift-F5>', minimal_qso)
root.bind_all('<F6>', opennext)
root.bind_all('<Shift-F6>', decodeall)
root.bind_all('<F9>', wsjt9supp)
root.bind_all('<F10>', showspecjt)
root.bind_all('<Shift-F10>', astro1)
root.bind_all('<F11>', left_arrow)
root.bind_all('<F12>', right_arrow)

root.bind_all('<Alt-Key-1>',btx1)
root.bind_all('<Alt-Key-2>',btx2)
root.bind_all('<Alt-Key-3>',btx3)
root.bind_all('<Alt-Key-4>',btx4)
root.bind_all('<Alt-Key-5>',btx5)
root.bind_all('<Alt-Key-6>',btx6)

root.bind_all('<Alt-a>',toggleauto)
root.bind_all('<Alt-A>',toggleauto)
root.bind_all('<Alt-c>',clear_avg)
root.bind_all('<Alt-C>',clear_avg)
root.bind_all('<Alt-d>',decode)
root.bind_all('<Alt-D>',decode)
root.bind_all('<Control-d>',force_decode)
root.bind_all('<Control-D>',force_decode)
root.bind_all('<Alt-e>',erase)
root.bind_all('<Alt-E>',erase)
root.bind_all('<Alt-f>',toggle_freeze)
root.bind_all('<Alt-F>',toggle_freeze)
root.bind_all('<Alt-g>',GenStdMsgs)
root.bind_all('<Alt-G>',GenStdMsgs)
root.bind_all('<Control-g>', GenAltMsgs)
root.bind_all('<Control-G>', GenAltMsgs)
root.bind_all('<Alt-i>',decode_include)
root.bind_all('<Alt-I>',decode_include)
root.bind_all('<Alt-l>',lookup)
root.bind_all('<Alt-L>',lookup)
root.bind_all('<Control-l>',lookup_gen)
root.bind_all('<Control-L>',lookup_gen)
root.bind_all('<Alt-m>',monitor)
root.bind_all('<Alt-M>',monitor)
root.bind_all('<Alt-o>',txstop)
root.bind_all('<Alt-O>',txstop)
root.bind_all('<Control-o>',openfile)
root.bind_all('<Control-O>',openfile)
root.bind_all('<Alt-q>',logqso)
root.bind_all('<Alt-Q>',logqso)
root.bind_all('<Alt-r>',setrpt)
root.bind_all('<Alt-R>',setrpt)
root.bind_all('<Alt-s>',stopmon)
root.bind_all('<Alt-S>',stopmon)
root.bind_all('<Alt-v>',savelast)
root.bind_all('<Alt-V>',savelast)
root.bind_all('<Alt-x>',decode_exclude)
root.bind_all('<Alt-X>',decode_exclude)
root.bind_all('<Alt-z>',toggle_zap)
root.bind_all('<Alt-Z>',toggle_zap)

text.pack(side=LEFT, fill=X, padx=1)
sb = Scrollbar(iframe4, orient=VERTICAL, command=text.yview)
sb.pack(side=RIGHT, fill=Y)
text.configure(yscrollcommand=sb.set)
iframe4.pack(expand=1, fill=X, padx=4)
iframe4b = Frame(frame, bd=1, relief=SUNKEN)
avetext=Text(iframe4b, height=2, width=80)
avetext.bind('<Double-Button-1>',dbl_click_ave)
avetext.bind('<Key>',avetextkey)
avetext.pack(side=LEFT, fill=X, padx=1)
iframe4b.pack(expand=1, fill=X, padx=4)

#------------------------------------------------------- Button Bar
iframe4c = Frame(frame, bd=1, relief=SUNKEN)
blogqso=Button(iframe4c, text='Log QSO',underline=4,command=logqso,
                padx=1,pady=1)
bstop=Button(iframe4c, text='Stop',underline=0,command=stopmon,
                padx=1,pady=1)
bmonitor=Button(iframe4c, text='Monitor',underline=0,command=monitor,
                padx=1,pady=1)
##bsavelast=Button(iframe4c, text='Save',underline=2,command=savelast,
##                padx=1,pady=1)
bdecode=Button(iframe4c, text='Decode',underline=0,command=decode,
                padx=1,pady=1)
berase=Button(iframe4c, text='Erase',underline=0,command=erase,
                padx=1,pady=1)
bclravg=Button(iframe4c, text='Clear Avg',underline=0,command=clear_avg,
                padx=1,pady=1)
binclude=Button(iframe4c, text='Include',underline=0,
                command=decode_include,padx=1,pady=1)
bexclude=Button(iframe4c, text='Exclude',underline=1,
                command=decode_exclude,padx=1,pady=1)
btxstop=Button(iframe4c,text='TxStop',underline=4,command=txstop,
                padx=1,pady=1)

blogqso.pack(side=LEFT,expand=1,fill=X)
bstop.pack(side=LEFT,expand=1,fill=X)
bmonitor.pack(side=LEFT,expand=1,fill=X)
##bsavelast.pack(side=LEFT,expand=1,fill=X)
bdecode.pack(side=LEFT,expand=1,fill=X)
berase.pack(side=LEFT,expand=1,fill=X)
bclravg.pack(side=LEFT,expand=1,fill=X)
binclude.pack(side=LEFT,expand=1,fill=X)
bexclude.pack(side=LEFT,expand=1,fill=X)
btxstop.pack(side=LEFT,expand=1,fill=X)
iframe4c.pack(expand=1, fill=X, padx=4)

#-----------------------------------------------------General control area
iframe5 = Frame(frame, bd=1, relief=FLAT,height=180)

#------------------------------------------------------ "Other station" info
f5a=Frame(iframe5,height=170,bd=2,relief=GROOVE)
labToRadio=Label(f5a,text='To radio:', width=9, relief=FLAT)
labToRadio.grid(column=0,row=0)
ToRadio=Entry(f5a,width=12)
ToRadio.insert(0,'W8WN')
ToRadio.grid(column=1,row=0,pady=3)
ToRadio.bind('<Return>',lookup)
bLookup=Button(f5a, text='Lookup',underline=0,command=lookup,padx=1,pady=1)
bLookup.grid(column=2,row=0,sticky='EW',padx=4)
labGrid=Label(f5a,text='Grid:', width=9, relief=FLAT)
labGrid.grid(column=0,row=1)
HisGrid=Entry(f5a,width=12)
HisGrid.grid(column=1,row=1,pady=1)
bAdd=Button(f5a, text='Add',command=addtodb,padx=1,pady=1)
bAdd.grid(column=2,row=1,sticky='EW',padx=4)
labHotAB=Label(f5a,bg='#FFCCFF',text='HotA: 247')
labHotAB.grid(column=0,row=2,sticky='EW',padx=4,pady=3)
labAz=Label(f5a,text='Az 257  El 15',width=11)
labAz.grid(column=1,row=2)
labDist=Label(f5a,text='16753 km')
labDist.grid(column=2,row=2)

#------------------------------------------------------ Date and Time
ldate=Label(f5a, bg='black', fg='yellow', width=11, bd=4,
        text='2005 Apr 22\n01:23:45', relief=RIDGE,
        justify=CENTER, font=(font1,16))
ldate.grid(column=0,columnspan=2,row=3,rowspan=2,padx=2,pady=2)

ldsec=Label(f5a, bg='white', fg='black', text='Dsec  0.0', width=8, relief=RIDGE)
ldsec.grid(column=2,row=4,ipadx=3,padx=2,pady=0)

f5a.pack(side=LEFT,expand=1,fill=BOTH)

#------------------------------------------------------ Rx parameters
f5b=Frame(iframe5,bd=0)
f5b1=Frame(f5b,bd=2,relief=GROOVE)
f5b1.grid(column=0,row=0,padx=2,sticky='EW')

lsync=Label(f5b1, bg='white', fg='black', text='Sync   1', width=8, relief=RIDGE)
lsync.grid(column=0,row=0,padx=2,sticky='EW')
Widget.bind(lsync,'<Button-1>',incsync)
Widget.bind(lsync,'<Button-3>',decsync)
cbzap=Checkbutton(f5b1,text='Zap',underline=0,variable=nzap)
cbzap.grid(column=1,row=0,padx=2,sticky='W')
shrx=Checkbutton(f5b1,text='Rx ST',variable=nshrx,command=restart2)
shrx.grid(column=1,row=2,sticky='W',padx=2)
cbfreeze=Checkbutton(f5b1,text='Freeze',underline=0,variable=nfreeze)
cbfreeze.grid(column=0,row=2,padx=2,sticky='W')
cbafc=Checkbutton(f5b1,text='AFC',variable=nafc)
cbafc.grid(column=1,row=1,padx=2,sticky='W')
##lspace=Label(f5b1, text='')
##lspace.grid(column=0,row=5,padx=2,pady=5,sticky='W')
ltol=Label(f5b1, bg='white', fg='black', text='Tol    400', width=8, relief=RIDGE)
ltol.grid(column=0,row=1,padx=2,sticky='EW')
##sbsync=Spinbox(f5b,from_=-20,to=10,bg='white',width=4,textvariable=nsync)
##sbsync.grid(column=0,row=4)
##nsync.set(1)
##sbtol=Spinbox(f5b,values=(10,20,50,100,200,500,1000),bg='white', \
##    width=4,textvariable=jtol)
##sbtol.grid(column=0,row=5)
##jtol.set(200)
Widget.bind(ltol,'<Button-1>',inctol)
Widget.bind(ltol,'<Button-3>',dectol)
Widget.bind(ldsec,'<Button-1>',incdsec)
Widget.bind(ldsec,'<Button-3>',decdsec)

#------------------------------------------------------ Tx parameters
f5b2=Frame(f5b,bd=2,relief=GROOVE)
f5b2.grid(column=0,row=1,padx=2,sticky='EW')
txfirst=Checkbutton(f5b2,text='Tx First',justify=RIGHT,variable=TxFirst)
txfirst.grid(column=0,row=0,sticky='W',padx=2)
shmsg=Checkbutton(f5b2,text='Tx ST',justify=RIGHT,variable=ShOK,
            command=restart2)
shmsg.grid(column=1,row=0,sticky='W',padx=2)

report=Entry(f5b2, width=4)
report.insert(0,'26')
report.grid(column=1,row=1,sticky='W',padx=7)
report.bind('<Double-Button-1>',GenStdMsgs)
labreport=Label(f5b2,text='Rpt:',width=4,underline=0)
labreport.grid(column=0,row=1,sticky='E',padx=0)

btxdf=Button(f5b2,text='  TxDF = 0  ',command=toggletxdf,padx=1,pady=1)
btxdf.grid(column=1,row=0,sticky='EW',padx=2)

f5b3=Frame(f5b,bd=2,relief=GROOVE)
f5b3.grid(column=0,row=2,padx=2,sticky='EW')
genmsg=Button(f5b3,text=' Gen Msgs ',underline=1,command=GenStdMsgs,
            padx=1,pady=2)
genmsg.grid(column=0,row=0,sticky='W',padx=2)
auto=Button(f5b3,text='Auto is Off',underline=0,command=toggleauto,
            padx=2,pady=2)
auto.focus_set()
auto.grid(column=1,row=0,sticky='EW',padx=2)

f5b.pack(side=LEFT,expand=0,fill=BOTH)

#------------------------------------------------------ Tx params and msgs
f5c=Frame(iframe5,bd=2,relief=GROOVE)
##f5c2=Frame(f5c,bd=0)
##f5c2.grid(column=0,row=1,sticky='W',padx=8)

ntx=IntVar()
tx1=Entry(f5c,width=32)
rb1=Radiobutton(f5c,value=1,variable=ntx)
b1=Button(f5c, text='Tx1',underline=2,command=btx1,padx=1,pady=1)
tx1.grid(column=1,row=0)
rb1.grid(column=2,row=0)
b1.grid(column=3,row=0)

tx2=Entry(f5c,width=32)
rb2=Radiobutton(f5c,value=2,variable=ntx)
b2=Button(f5c, text='Tx2',underline=2,command=btx2,padx=1,pady=1)
tx2.grid(column=1,row=1)
rb2.grid(column=2,row=1)
b2.grid(column=3,row=1)

tx3=Entry(f5c,width=32)
rb3=Radiobutton(f5c,value=3,variable=ntx)
b3=Button(f5c, text='Tx3',underline=2,command=btx3,padx=1,pady=1)
tx3.grid(column=1,row=2)
rb3.grid(column=2,row=2)
b3.grid(column=3,row=2)

tx4=Entry(f5c,width=32)
rb4=Radiobutton(f5c,value=4,variable=ntx)
b4=Button(f5c, text='Tx4',underline=2,command=btx4,padx=1,pady=1)
tx4.grid(column=1,row=3)
rb4.grid(column=2,row=3)
b4.grid(column=3,row=3)

tx5=Entry(f5c,width=32)
rb5=Radiobutton(f5c,value=5,variable=ntx)
b5=Button(f5c, text='Tx5',underline=2,command=btx5,padx=1,pady=1)
tx5.grid(column=1,row=4)
rb5.grid(column=2,row=4)
b5.grid(column=3,row=4)

tx6=Entry(f5c,width=32)
rb6=Radiobutton(f5c,value=6,variable=ntx)
b6=Button(f5c, text='Tx6',underline=2,command=btx6,padx=1,pady=1)
tx6.grid(column=1,row=5)
rb6.grid(column=2,row=5)
b6.grid(column=3,row=5)

f5c.pack(side=LEFT,fill=BOTH)
iframe5.pack(expand=1, fill=X, padx=4)

#------------------------------------------------------------ Status Bar
iframe6 = Frame(frame, bd=1, relief=SUNKEN)
msg1=Message(iframe6, text='                    ', width=300,relief=SUNKEN)
msg1.pack(side=LEFT, fill=X, padx=1)
msg2=Message(iframe6, text='Message #2', width=300,relief=SUNKEN)
msg2.pack(side=LEFT, fill=X, padx=1)
msg3=Message(iframe6,width=300,relief=SUNKEN)
msg3.pack(side=LEFT, fill=X, padx=1)
msg4=Message(iframe6, text='Message #4', width=300,relief=SUNKEN)
msg4.pack(side=LEFT, fill=X, padx=1)
msg5=Message(iframe6, text='Message #5', width=300,relief=SUNKEN)
msg5.pack(side=LEFT, fill=X, padx=1)
##msg6=Message(iframe6, text='', width=300,relief=SUNKEN)
##msg6.pack(side=LEFT, fill=X, padx=1)
Widget.bind(msg5,'<Button-1>',inctrperiod)
Widget.bind(msg5,'<Button-3>',dectrperiod)
msg7=Message(iframe6, text='                        ', width=300,relief=SUNKEN)
msg7.pack(side=RIGHT, fill=X, padx=1)
iframe6.pack(expand=1, fill=X, padx=4)
frame.pack()

ldate.after(100,update)

lauto=0
isync=1
ntx.set(1)
ndepth.set(0)
from WsjtMod import options
options.defaults()
ModeFSK441()
lookup()
balloon.unbind(ToRadio)
g.astro_geom0="+25+25"
Audio.gcom1.mute=0
Audio.gcom2.nforce=1
Audio.gcom2.mode4=1

#---------------------------------------------------------- Process INI file
try:
    f=open(appdir+'/WSJT.INI',mode='r')
    params=f.readlines()
except:
    params=""
    if g.Win32:
        options.PttPort.set("0")
    else:
        options.PttPort.set("/dev/ttyS0")
    Audio.gcom2.nport=0

try:
    for i in range(len(params)):
        key,value=params[i].split()
        if   key == 'WSJTGeometry': root.geometry(value)
        elif key == 'Mode':
            mode.set(value)
            if value=='FSK441':
                ModeFSK441()
            elif value=='JT65A':
                ModeJT65A()
            elif value=='JT65B':
                ModeJT65B()
            elif value=='JT65C':
                ModeJT65C()
            elif value=='CW':
                ModeCW()
            elif value=='ISCAT-A':
                ModeISCAT_A()
            elif value=='ISCAT-B':
                ModeISCAT_B()
            elif value=='Diana':
                ModeDiana()
            elif value=='JTMS':
                ModeJTMS()
            elif value[:3]=='JT4':
                ModeJT4()
            elif value=='Echo':
                ModeEcho()
            elif value=='Measure':
                ModeMeasure()
        elif key == 'MyCall': options.MyCall.set(value)
        elif key == 'MyGrid': options.MyGrid.set(value)
        elif key == 'HisCall':
            hiscall=value
            if hiscall=="______": hiscall=""
            ToRadio.delete(0,END)
            ToRadio.insert(0,hiscall)
        elif key == 'HisGrid':
            hisgrid=value
            if hisgrid == "XX00xx":
                lookup()
            HisGrid.delete(0,END)
            HisGrid.insert(0,hisgrid)
#        elif key == 'RxDelay': options.RxDelay.set(value)
#        elif key == 'TxDelay': options.TxDelay.set(value)
        elif key == 'IDinterval': options.IDinterval.set(value)
        elif key == 'PttPort':
            try:
                options.PttPort.set(value)
                try:
                    Audio.gcom2.nport=int(options.PttPort.get())
                except:
                    Audio.gcom2.nport=0
            except:
                if g.Win32:
                    options.PttPort.set("0")
                else:
                    options.PttPort.set("/dev/ttyS0")
                Audio.gcom2.nport=0
            Audio.gcom2.pttport=(options.PttPort.get()+(' '*80))[:80]
        elif key == 'Mileskm': options.mileskm.set(value)
        elif key == 'nDTR': options.ndtr.set(value)
        elif key == 'MsgStyle': options.ireport.set(value)
        elif key == 'Region': options.iregion.set(value)
        elif key == 'AudioIn':
            try:
                g.ndevin.set(value)
            except:
                g.ndevin.set(0)
            g.DevinName.set(value)
            options.DevinName.set(value)
            Audio.gcom1.devin_name=(options.DevinName.get()+(' '*12))[:12]
        elif key == 'AudioOut':
            try:
                g.ndevout.set(value)
            except:
                g.ndevout.set(0)
            g.DevoutName.set(value)
            options.DevoutName.set(value)
            Audio.gcom1.devout_name=(options.DevoutName.get()+(' '*12))[:12]
        elif key == 'SamFacIn': options.samfacin.set(value)
        elif key == 'SamFacOut': options.samfacout.set(value)
        elif key == 'Template1': options.Template1.set(value.replace("_"," "))
        elif key == 'Template2': options.Template2.set(value.replace("_"," "))
        elif key == 'Template3': options.Template3.set(value.replace("_"," "))
        elif key == 'Template4': options.Template4.set(value.replace("_"," "))
        elif key == 'Template5': options.Template5.set(value.replace("_"," "))
        elif key == 'Template6':
            options.Template6.set(value.replace("_"," "))
            if options.Template6.get()==" ": options.Template6.set("")
        elif key == 'AddPrefix': options.addpfx.set(value.replace("_"," ").lstrip())
        elif key == 'AuxRA': options.auxra.set(value)
        elif key == 'AuxDEC': options.auxdec.set(value)
        elif key == 'AzElDir':
	    options.azeldir.set(value.replace("#"," "))
            try:
		os.stat(options.azeldir.get())
	    except:
		options.azeldir.set(os.getcwd())
        elif key == 'Ntc': options.ntc.set(value)
        elif key == 'fRIT': options.fRIT.set(value)
        elif key == 'Dither': options.dither.set(value)
##        elif key == 'Necho': options.necho.set(value)
##        elif key == 'Dlatency': options.dlatency.set(value)
        elif key == 'MyName': options.myname.set(value)
        elif key == 'HighPri': options.HighPri.set(value)
        elif key == 'TxFirst': TxFirst.set(value)
        elif key == 'KB8RQ': kb8rq.set(value)
        elif key == 'K2TXB': k2txb.set(value)
        elif key == 'SetSeq': setseq.set(value)
        elif key == 'Report':
            report.delete(0,END)
            report.insert(0,value)
        elif key == 'ShOK': ShOK.set(value)
        elif key == 'Nsave': nsave.set(value)
        elif key == 'Band': nfreq.set(value)
        elif key == 'S441': isync441=int(value)
        elif key == 'Siscat': isync_iscat=int(value)
        elif key == 'Sync': isync65=int(value)
        elif key == 'Zap': nzap.set(value)
        elif key == 'NAFC': nafc.set(value)
        elif key == 'nshrx': nshrx.set(value)
        elif key == 'NoShJT65all': noshjt65all.set(value)
        elif key == 'NoShJT65': noshjt65.set(value)
        elif key == 'QDecode': qdecode.set(value)
        elif key == 'NEME': neme.set(value)
        elif key == 'NDepth': ndepth.set(value)
        elif key == 'Debug': ndebug.set(value)
        elif key == 'LowBeacon': nlowbeacon.set(value)
        elif key == 'Monitor': nmonitor.set(value)
        elif key == 'HisCall':
            Audio.gcom2.hiscall=(value+' '*12)[:12]
            ToRadio.delete(0,99)
            ToRadio.insert(0,value)
            lookup()                       #Maybe should save HisGrid, instead?
        elif key == 'MRUDir': mrudir=value.replace("#"," ")
        elif key == 'AstroGeometry': g.astro_geom0 =value
        elif key == 'CWTRPeriod':
            ncwtrperiod=int(value)
            if mode.get()[:2]=="CW": Audio.gcom1.trperiod=ncwtrperiod
        else: pass
except:
    print 'Error reading WSJT.INI, continuing with defaults.'
    print key,value

g.mode=mode.get()
if mode.get()=='FSK441' or mode.get()=='JTMS': isync=isync441
elif mode.get()[:5]=="ISCAT":
    isync=isync_iscat
    if mode.get()[6:7]=='A': Audio.gcom2.mode4=1
    if mode.get()[6:7]=='B': Audio.gcom2.mode4=2
elif mode.get()[:4]=='JT65': isync=isync65
elif mode.get()[:3]=='JT4':
    if mode.get()[3:4]=='A': Audio.gcom2.mode4=1
    if mode.get()[3:4]=='B': Audio.gcom2.mode4=2
    if mode.get()[3:4]=='C': Audio.gcom2.mode4=4
    if mode.get()[3:4]=='D': Audio.gcom2.mode4=9
    if mode.get()[3:4]=='E': Audio.gcom2.mode4=18
    if mode.get()[3:4]=='F': Audio.gcom2.mode4=36
    if mode.get()[3:4]=='G': Audio.gcom2.mode4=72

lsync.configure(text=slabel+str(isync))
Audio.gcom2.azeldir=(options.azeldir.get()+' '*80)[:80]
Audio.gcom2.ndepth=ndepth.get()
Audio.gcom2.nhighpri=options.HighPri.get()
Audio.gcom4.addpfx=(options.addpfx.get().lstrip()+(' '*8))[:8]

if nmonitor.get():
    monitor()
else:
    stopmon()
if g.Win32: root.iconbitmap("wsjt.ico")
root.title('  WSJT 9.1     by K1JT')
from WsjtMod import astro
from WsjtMod import specjt

# SpecJT has a "mainloop", so does not return until it is terminated.
#root.mainloop()   #Superseded by mainloop in SpecJT

# Clean up and save user options before terminating
f=open(appdir+'/WSJT.INI',mode='a')
root_geom=root_geom[root_geom.index("+"):]
f.write("WSJTGeometry " + root_geom + "\n")
f.write("Mode " + g.mode + "\n")
f.write("MyCall " + options.MyCall.get() + "\n")
f.write("MyGrid " + options.MyGrid.get() + "\n")
t=g.ftnstr(Audio.gcom2.hiscall)
if t[:1]==" ": t="______"
f.write("HisCall " + t + "\n")
t=g.ftnstr(Audio.gcom2.hisgrid)
if t=="      ": t="XX00xx"
f.write("HisGrid " + t + "\n")
#f.write("RxDelay " + str(options.RxDelay.get()) + "\n")
#f.write("TxDelay " + str(options.TxDelay.get()) + "\n")
f.write("IDinterval " + str(options.IDinterval.get()) + "\n")
f.write("PttPort " + str(options.PttPort.get()) + "\n")
f.write("Mileskm " + str(options.mileskm.get()) + "\n")
f.write("nDTR " + str(options.ndtr.get()) + "\n")
f.write("MsgStyle " + str(options.ireport.get()) + "\n")
f.write("Region " + str(options.iregion.get()) + "\n")
f.write("AudioIn " + options.DevinName.get() + "\n")
f.write("AudioOut " + options.DevoutName.get() + "\n")
f.write("SamFacIn " + str(options.samfacin.get()) + "\n")
f.write("SamFacOut " + str(options.samfacout.get()) + "\n")
if options.Template6.get()=="": options.Template6.set("_")
f.write("Template1 " + options.Template1.get().replace(" ","_") + "\n")
f.write("Template2 " + options.Template2.get().replace(" ","_") + "\n")
f.write("Template3 " + options.Template3.get().replace(" ","_") + "\n")
f.write("Template4 " + options.Template4.get().replace(" ","_") + "\n")
f.write("Template5 " + options.Template5.get().replace(" ","_") + "\n")
f.write("Template6 " + options.Template6.get().replace(" ","_") + "\n")
if options.addpfx.get().lstrip()=="": options.addpfx.set("_")
f.write("AddPrefix " + options.addpfx.get().lstrip() + "\n")
if options.auxra.get()=="": options.auxra.set("0")
if options.auxdec.get()=="": options.auxdec.set("0")
f.write("AuxRA " + options.auxra.get() + "\n")
f.write("AuxDEC " + options.auxdec.get() + "\n")
f.write("AzElDir " + str(options.azeldir.get()).replace(" ","#") + "\n")
f.write("Ntc " + str(options.ntc.get()) + "\n")
f.write("fRIT " + str(options.fRIT.get()) + "\n")
f.write("Dither " + str(options.dither.get()) + "\n")
##f.write("Necho " + str(options.necho.get()) + "\n")
##f.write("Dlatency " + str(options.dlatency.get()) + "\n")
f.write("HighPri " + str(options.HighPri.get()) + "\n")
f.write("TxFirst " + str(TxFirst.get()) + "\n")
f.write("KB8RQ " + str(kb8rq.get()) + "\n")
f.write("K2TXB " + str(k2txb.get()) + "\n")
f.write("SetSeq " + str(setseq.get()) + "\n")
f.write("Report " + g.report + "\n")
f.write("ShOK " + str(ShOK.get()) + "\n")
f.write("Nsave " + str(nsave.get()) + "\n")
f.write("Band " + str(nfreq.get()) + "\n")
f.write("S441 " + str(isync441) + "\n")
f.write("Siscat " + str(isync_iscat) + "\n")
f.write("Sync " + str(isync65) + "\n")
f.write("Zap " + str(nzap.get()) + "\n")
f.write("NAFC " + str(nafc.get()) + "\n")
f.write("nshrx " + str(nshrx.get()) + "\n")
f.write("NoShJT65all " + str(noshjt65all.get()) + "\n")
f.write("NoShJT65 " + str(noshjt65.get()) + "\n")
f.write("QDecode " + str(qdecode.get()) + "\n")
f.write("NEME " + str(neme.get()) + "\n")
f.write("NDepth " + str(ndepth.get()) + "\n")
f.write("Debug " + str(ndebug.get()) + "\n")
f.write("LowBeacon " + str(nlowbeacon.get()) + "\n")
f.write("Monitor " + str(nmonitor.get()) + "\n")
#f.write("TRPeriod " + str(Audio.gcom1.trperiod) + "\n")
mrudir2=mrudir.replace(" ","#")
f.write("MRUDir " + mrudir2 + "\n")
if g.astro_geom[:7]=="200x200":
    g.astro_geom="316x416" + g.astro_geom[7:]
f.write("AstroGeometry " + g.astro_geom + "\n")
f.write("CWTRPeriod " + str(ncwtrperiod) + "\n")
f.close()

Audio.ftn_quit()
Audio.gcom1.ngo=0                         #Terminate audio streams
Audio.gcom2.lauto=0
Audio.gcom1.txok=0
time.sleep(1)
