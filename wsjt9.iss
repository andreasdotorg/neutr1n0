[Setup]
AppName=WSJT
AppVerName=WSJT Version 9.01 r2226
AppCopyright=Copyright (C) 2001-2010 by Joe Taylor, K1JT
DefaultDirName={pf}\WSJT9
DefaultGroupName=WSJT9

[Files]
Source: "c:\Users\joe\wsjt\wsjt7a\WSJT9.EXE";         DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjt7a\UpdateHistory.txt"; DestDir: "{app}"
Source: "c:\Users\joe\wsjt\wsjt7a\CALL3.TXT";         DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjt7a\wsjt.ico";          DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjt7a\TSKY.DAT";          DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjt7a\libsamplerate.dll"; DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjt7a\KVASD_g95.EXE";     DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjt7a\kvasd.dat";         DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjt7a\wsjtrc.win";        DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjt7a\WSJT_User_600.pdf"; DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjt7a\WSJT_Quick_Reference.pdf"; DestDir: "{app}";
Source: "c:\Users\joe\wsjt\wsjt7a\rxwav\samples\W8WN_010809_110400.WAV";  DestDir: "{app}\RxWav\Samples\"; Flags: onlyifdoesntexist
Source: "c:\Users\joe\wsjt\wsjt7a\dmet_10_-1_3.dat";  DestDir: "{app}"; Flags: onlyifdoesntexist

[Icons]
Name: "{group}\WSJT9";        Filename: "{app}\WSJT9.EXE"; WorkingDir: {app}; IconFileName: "{app}\wsjt.ico"
Name: "{userdesktop}\WSJT9";  Filename: "{app}\WSJT9.EXE"; WorkingDir: {app}; IconFileName: "{app}\wsjt.ico"


