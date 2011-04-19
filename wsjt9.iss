[Setup]
AppName=WSJT
AppVerName=WSJT Version 9.1 r2396
AppCopyright=Copyright (C) 2001-2011 by Joe Taylor, K1JT
DefaultDirName={pf}\WSJT9
DefaultGroupName=WSJT9

[Files]
Source: "c:\Users\joe\wsjt\trunk\WSJT9.EXE";         DestDir: "{app}"
Source: "c:\Users\joe\wsjt\trunk\UpdateHistory.txt"; DestDir: "{app}"
Source: "c:\Users\joe\wsjt\trunk\CALL3.TXT";         DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\trunk\wsjt.ico";          DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\trunk\TSKY.DAT";          DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\trunk\libsamplerate.dll"; DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\trunk\KVASD_g95.EXE";     DestDir: "{app}";
Source: "c:\Users\joe\wsjt\trunk\kvasd.dat";         DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\trunk\wsjtrc.win";        DestDir: "{app}";
Source: "c:\Users\joe\wsjt\trunk\WSJT_User_600.pdf"; DestDir: "{app}";
Source: "c:\Users\joe\wsjt\trunk\WSJT_Quick_Reference.pdf"; DestDir: "{app}";
Source: "c:\Users\joe\wsjt\trunk\rxwav\samples\W8WN_010809_110400.WAV";  DestDir: "{app}\RxWav\Samples\"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\trunk\dmet_10_-1_3.dat";  DestDir: "{app}"; Flags: onlyifdoesntexist

[Icons]
Name: "{group}\WSJT9";        Filename: "{app}\WSJT9.EXE"; WorkingDir: {app}; IconFileName: "{app}\wsjt.ico"
Name: "{userdesktop}\WSJT9";  Filename: "{app}\WSJT9.EXE"; WorkingDir: {app}; IconFileName: "{app}\wsjt.ico"


