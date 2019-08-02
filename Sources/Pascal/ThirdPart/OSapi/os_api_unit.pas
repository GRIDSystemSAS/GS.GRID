{ Windows API Funktionen

  10/2012 xe2 kompatibel
  02/2016 XE10 x64 Test
  xx/xxxx FPC Ubuntu

  --------------------------------------------------------------------
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.

  THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY

  Author: Peter Lorenz
  Is that code useful for you? Donate!
  Paypal webmaster@peter-ebe.de
  --------------------------------------------------------------------


}

{$WARN SYMBOL_PLATFORM OFF}
{$I share_settings.inc}
unit os_api_unit;

interface

uses
{$IFNDEF FPC}System.UITypes, {$ENDIF}
{$IFNDEF UNIX}Windows, ShellAPI, shlObj, {$ENDIF}
{$IFDEF FPC}LCLIntf, LCLType, LMessages, ctypes, fileutil, {$ENDIF}
  SysUtils, Classes;

{ File Attribute siehe
  http://msdn.microsoft.com/en-us/library/windows/desktop/gg258117%28v=vs.85%29.aspx
}
{$IFDEF UNIX}

const
  INFINITE = 4294967295;
  MOVEFILE_REPLACE_EXISTING = 1;
{$ENDIF}
{$IFDEF UNIX}

const
  Delimiter = '/';
  FileFilterAll = '*';
{$ELSE}

const
  Delimiter = '\';
  FileFilterAll = '*.*';
{$ENDIF}

const
  FILE_ATTRIBUTE_INVALID = {$IFDEF FPC}-1{$ELSE}faInvalid{$ENDIF};
  // Ungültig, vorab prüfen

  FILE_ATTRIBUTE_VOLUMEID = $0008;
  // deprecated: faVolumeID wurde nur aus Gründen der Kompatibilität mit DOS/9x verwendet.

  FILE_ATTRIBUTE_ARCHIVE = faArchive;
  // $0020;	  // A file or directory that is an archive file or directory. Applications typically use this attribute to mark files for backup or removal.
  FILE_ATTRIBUTE_COMPRESSED = faCompressed;
  // $0800;	  // A file or directory that is compressed. For a file, all of the data in the file is compressed. For a directory, compression is the default for newly created files and subdirectories.
  FILE_ATTRIBUTE_DEVICE = $0040; // This value is reserved for system use.
  FILE_ATTRIBUTE_DIRECTORY = faDirectory;
  // $0010; 	// The handle that identifies a directory.
  FILE_ATTRIBUTE_ENCRYPTED = faEncrypted;
  // $4000;   // A file or directory that is encrypted. For a file, all data streams in the file are encrypted. For a directory, encryption is the default for newly created files and subdirectories.
  FILE_ATTRIBUTE_HIDDEN = faHidden;
  // $0002;	  // The file or directory is hidden. It is not included in an ordinary directory listing.
  FILE_ATTRIBUTE_NORMAL = faNormal;
  // $0080;	  // A file that does not have other attributes set. This attribute is valid only when used alone.
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $2000;
  // The file or directory is not to be indexed by the content indexing service.
  FILE_ATTRIBUTE_OFFLINE = $1000;
  // The data of a file is not available immediately. This attribute indicates that the file data is physically moved to offline storage. This attribute is used by Remote Storage, which is the hierarchical storage management software. Applications should not arbitrarily change this attribute.
  FILE_ATTRIBUTE_READONLY = faReadOnly;
  // $0001;	  // A file that is read-only. Applications can read the file, but cannot write to it or delete it. This attribute is not honored on directories. For more information, see "You cannot view or change the Read-only or the System attributes of folders in Windows Server 2003, in Windows XP, or in Windows Vista".
  FILE_ATTRIBUTE_REPARSE_POINT = faSymLink;
  // $0400;   // A file or directory that has an associated reparse point, or a file that is a symbolic link.
  FILE_ATTRIBUTE_SPARSE_FILE = $0200; // A file that is a sparse file.
  FILE_ATTRIBUTE_SYSTEM = faSysFile;
  // $0004; 	// A file or directory that the operating system uses a part of, or uses exclusively.
  FILE_ATTRIBUTE_TEMPORARY = faTemporary;
  // $0100;	  // A file that is being used for temporary storage. File systems avoid writing data back to mass storage if sufficient cache memory is available, because typically, an application deletes a temporary file after the handle is closed. In that scenario, the system can entirely avoid writing the data. Otherwise, the data is written after the handle is closed.
  FILE_ATTRIBUTE_VIRTUAL = faVirtual;
  // $10000;  // This value is reserved for system use.

  { prozesstimeouts, diese können verwendet werden müssen aber nicht da übergabeparameter }
const
  timeout_decode = 60 * 1000;
  // wahnsinnig hoch weil auch große dateien auf langsamen rechnern verarbeitet werden können sollen
  timeout_crosscoding = 15 * 60 * 1000;
  // wahnsinnig hoch weil auch große dateien auf langsamen rechnern verarbeitet werden können sollen

  { funktionen und prozerduren }
function ShowProperties(hWndOwner: HWND; const FileName: string;
  Registerkarte: PChar): Boolean;

//VGS : Console backend server useage only.
//function OpenWithDialog(const AFileName: String): Boolean;
//procedure CopyFilesToClipboard(FileList: string);
//function Benutzername: string;

function GetOSVersion(var majorVer, minorVer: Integer;
  var VersionString: string): Boolean;
function GetCPUCount: Integer;
function GetUnix: Boolean;
function GetWineAvail: Boolean;

function Shellexecute_safe(Operation, FileName, Parameters, Directory: String;
  ShowCmd: Integer): Integer;
function GetConsoleOutput(Command: string; Output, Errors: TStream;
  Timeout: Cardinal = INFINITE): Boolean;
function ExecProcess(Command: string; var Error: String;
  Timeout: Cardinal = INFINITE): Boolean;

procedure OpenDirectory(Directory: string);

function GetAppVersionStr(bFullBuild: Boolean): string;

function ShowDriveType(Drive: char): string;
function DiskInDrive(Drive: char): Boolean;

procedure CopyDirContent(fromdir, todir: string);

function FileSizeLarge(FileName: string): int64;

function MoveFile_safe(fromfile, tofile: String; failifexists: Boolean)
  : Boolean;
function CopyFile_safe(fromfile, tofile: String; failifexists: Boolean)
  : Boolean;

{$IFDEF UNIX}
function GetLastError: Integer;
{$ENDIF}

resourcestring
  rsNotImplemented =
    'Die angefordere Funktion ist derzeit nicht implementiert.';

implementation

{$IFDEF UNIX}

uses UNIX, fileinfo, process, pipes;

// https://www.gnu.org/software/libc/manual/html_node/Processor-Resources.html
const
  _SC_NPROCESSORS_CONF = 83;
  // const _SC_NPROCESSORS_ONLN = 84;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';

type
  TVersion = class(TPersistent)
  private
    FBuild: Integer;
    FMajor: Integer;
    FMinor: Integer;
    FVersion: Integer;
  published
    property Version: Integer read FVersion write FVersion;
    property Major: Integer read FMajor write FMajor;
    property Minor: Integer read FMinor write FMinor;
    property Build: Integer read FBuild write FBuild;
  end;
{$ENDIF}

function ShowProperties(hWndOwner: HWND; const FileName: string;
  Registerkarte: PChar): Boolean;
{$IFDEF UNIX}
begin
  Assert(false, rsNotImplemented);
end;
{$ELSE}

var
  Info: {$IFDEF FPC}TShellExecuteInfoW{$ELSE}TShellExecuteInfo{$ENDIF};
begin
  with Info do
  begin
    cbSize := SizeOf(Info);
    fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_INVOKEIDLIST or
      SEE_MASK_FLAG_NO_UI;
    wnd := hWndOwner;
    lpVerb := 'properties';
    lpFile := PChar(FileName);
    lpParameters := Registerkarte;
    lpDirectory := nil;
    nShow := 0;
    hInstApp := 0;
    lpIDList := nil;
  end;
  Result := {$IFDEF FPC}ShellExecuteExW{$ELSE}ShellExecuteEx{$ENDIF}(@Info);
end;
{$ENDIF}

//VGS : Console backend server useage only.
//
//function OpenWithDialog(const AFileName: String): Boolean;
//{$IFDEF UNIX}
//begin
//  Assert(false, rsNotImplemented);
//end;
//{$ELSE}
//
//var
//  res: Integer;
//begin
//  Result := false;
//  res := ShellExecute({$IFDEF FPC}INVALID_HANDLE_VALUE{$ELSE}Application.
//    Handle{$ENDIF}, 'open', PChar('rundll32.exe'),
//    PChar('shell32.dll,OpenAs_RunDLL ' + AFileName), nil, SW_SHOWNORMAL);
//  if res > 32 then
//    Result := true;
//end;
//{$ENDIF}
//
//procedure CopyFilesToClipboard(FileList: string);
//{$IFDEF FPC}
//begin
//  Assert(false, rsNotImplemented);
//end;
//{$ELSE}
//
//var
//  DropFiles: PDropFiles;
//  hGlobal: THandle;
//  iLen: Integer;
//begin
//  iLen := Length(FileList);
//  hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
//    SizeOf(TDropFiles) + ((iLen + 2) * SizeOf(char)));
//  if (hGlobal = 0) then
//    raise Exception.Create('Could not allocate memory.');
//  try
//    DropFiles := GlobalLock(hGlobal);
//    if (DropFiles = nil) then
//      raise Exception.Create('Could not access allocated memory.');
//    try
//      DropFiles^.pFiles := SizeOf(TDropFiles);
//      DropFiles^.fWide := (SizeOf(char) = SizeOf(WideChar));
//
//      if FileList <> '' then
//        Move(FileList[1], (PByte(DropFiles) + SizeOf(TDropFiles))^,
//          iLen * SizeOf(char));
//    finally
//      GlobalUnlock(hGlobal);
//    end;
//    Clipboard.SetAsHandle(CF_HDROP, hGlobal);
//  except
//    GlobalFree(hGlobal);
//  end;
//end;
//{$ENDIF}
//
//function Benutzername: string;
//{$IFDEF UNIX}
//begin
//  Result := GetEnvironmentVariable('USER');
//end;
//{$ELSE}
//var
//  Buffer: Array [0 .. MAX_PATH + 1] of char;
//  Size: DWORD;
//begin
//  Size := 1024;
//{$IFDEF FPC}GetUserNameW{$ELSE}GetUserName{$ENDIF}(Buffer, Size);
//  Result := StrPas(Buffer);
//end;
//{$ENDIF}

function GetUnix: Boolean;
{$IFDEF UNIX}
begin
  Result := true;
end;
{$ELSE}

begin
  Result := false;
end;
{$ENDIF}

function GetWineAvail: Boolean;
{$IFDEF UNIX}
begin
  Result := false;
end;
{$ELSE}

var
  H: NativeUInt;
begin
  Result := false;
  H := 0;
  try
    H := LoadLibrary('ntdll.dll');
    if H > HINSTANCE_ERROR then
      Result := Assigned(GetProcAddress(H, 'wine_get_version'));
  finally
    if H > HINSTANCE_ERROR then
      FreeLibrary(H);
  end;
end;
{$ENDIF}

function GetOSVersion(var majorVer, minorVer: Integer;
  var VersionString: string): Boolean;
{$IFDEF UNIX}
begin
  majorVer := 0;
  minorVer := 0;
  VersionString := 'UNIX';
{$IFDEF LINUX}
  VersionString := 'LINUX';
{$ENDIF}
{$IFDEF DARWIN}
  VersionString := 'DARWIN';
{$ENDIF}
  Result := true;
end;
{$ELSE}

var
  osVerInfo: TOSVersionInfo;
  OSName: string;
begin
  Result := false;
  majorVer := 0;
  minorVer := 0;
  VersionString := '';

  // Windowsversion abfragen
  // https://msdn.microsoft.com/en-us/library/ms724832%28VS.85%29.aspx
  try
    osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    if GetVersionEx(osVerInfo) then
    begin
      majorVer := osVerInfo.dwMajorVersion;
      minorVer := osVerInfo.dwMinorVersion;
      VersionString := 'PlatformID ' + inttostr(osVerInfo.dwPlatformId) +
        ' Version ' + inttostr(majorVer) + '.' + inttostr(minorVer);
      case osVerInfo.dwPlatformId of
        // Windows NT/2000
        VER_PLATFORM_WIN32_NT:
          begin
            VersionString := 'VER_PLATFORM_WIN32_NT ' + inttostr(majorVer) + '.'
              + inttostr(minorVer);
            OSName := '';
            if majorVer <= 4 then
              OSName := 'WinNT'
            else if (majorVer = 5) and (minorVer = 0) then
              OSName := 'Win2000'
            else if (majorVer = 5) and (minorVer = 1) then
              OSName := 'WinXP'
            else if (majorVer = 6) and (minorVer = 0) then
              OSName := 'WinVista'
            else if (majorVer = 6) and (minorVer = 1) then
              OSName := 'Win7'
            else if (majorVer = 6) and (minorVer = 2) then
              OSName := 'Win8'
            else if (majorVer = 6) and (minorVer = 3) then
              OSName := 'Win8.1'
            else if (majorVer = 10) and (minorVer = 0) then
              OSName := 'Win10';

            VersionString := VersionString + ' [' + OSName + ']';
          end;
        // Windows 9x/ME
        VER_PLATFORM_WIN32_WINDOWS:
          begin
            VersionString := 'VER_PLATFORM_WIN32_WINDOWS ' + inttostr(majorVer)
              + '.' + inttostr(minorVer);
            OSName := '';
            if (majorVer = 4) and (minorVer = 0) then
              OSName := 'Win95'
            else if (majorVer = 4) and (minorVer = 10) then
            begin
              if osVerInfo.szCSDVersion[1] = 'A' then
                OSName := 'Win98SE'
              else
                OSName := 'Win98';
            end
            else if (majorVer = 4) and (minorVer = 90) then
              OSName := 'WinME';

            VersionString := VersionString + ' [' + OSName + ']';
          end;
      end;
    end
    else
    begin
      VersionString := 'Windows Version nicht ermittelbar';
    end;

    Result := true;
  except
    on e: Exception do
    begin
      //
    end;
  end;

  // Wine erkennen abfragen
  try
    if GetWineAvail then
      VersionString := VersionString + ' [WINE]';
  except
    on e: Exception do
    begin
      //
    end;
  end;
end;
{$ENDIF}

function GetCPUCount: Integer;
{$IFDEF UNIX}
begin
  Result := sysconf(_SC_NPROCESSORS_CONF);
end;
{$ELSE}

var
  lpsi: _system_info;
begin
  Result := 0;
  getsysteminfo(lpsi);
  Result := lpsi.dwNumberOfProcessors;
end;
{$ENDIF}

function GetAppVersionStr(bFullBuild: Boolean): string;
{$IFDEF UNIX}
var
  sVersion: String;
  aVersionInfo: TVersionInfo;
  VersionInfo: TVersion;
begin
  Result := '';

  aVersionInfo := TVersionInfo.Create;
  VersionInfo := TVersion.Create;
  try
    aVersionInfo.Load(HINSTANCE);
    VersionInfo.Version := aVersionInfo.FixedInfo.FileVersion[0];
    VersionInfo.Major := aVersionInfo.FixedInfo.FileVersion[1];
    VersionInfo.Minor := aVersionInfo.FixedInfo.FileVersion[2];
    VersionInfo.Build := aVersionInfo.FixedInfo.FileVersion[3];
  finally
    if Assigned(aVersionInfo) then
      aVersionInfo.Free;
  end;

  if bFullBuild then
  begin
    Result := Format('%d.%0.2d.%0.2d.%0.2d',
      [VersionInfo.Version, VersionInfo.Major, VersionInfo.Minor,
      VersionInfo.Build]);
  end
  else
  begin
    sVersion := Format('%d.%0.2d', [VersionInfo.Version, VersionInfo.Major]);
    if VersionInfo.Minor > 0 then
      sVersion := sVersion + chr(ord('a') - 1 + VersionInfo.Minor);
    Result := sVersion;
  end;
end;
{$ELSE}

var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
  sVersion: string;
begin
  Result := '';

//VGS : Console backend server usage only : Excpetion enabled.
//  try
    Exe := ParamStr(0);
    Size := {$IFDEF FPC}GetFileVersionInfoSizeW{$ELSE}GetFileVersionInfoSize{$ENDIF}(PChar(Exe), Handle);
    if Size = 0 then
      RaiseLastOSError;
    SetLength(Buffer, Size);
    if not {$IFDEF FPC}GetFileVersionInfoW{$ELSE}GetFileVersionInfo{$ENDIF}(PChar(Exe), Handle, Size, Buffer) then
      RaiseLastOSError;
    if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
      RaiseLastOSError;
    if bFullBuild then
    begin
      Result := Format('%d.%0.2d.%0.2d.%0.2d',
        [LongRec(FixedPtr.dwFileVersionMS).Hi, // major
        LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
        LongRec(FixedPtr.dwFileVersionLS).Hi, // release
        LongRec(FixedPtr.dwFileVersionLS).Lo]); // build
    end
    else
    begin
      sVersion := Format('%d.%0.2d', [LongRec(FixedPtr.dwFileVersionMS).Hi,
        // major
        LongRec(FixedPtr.dwFileVersionMS).Lo]); // minor
      if LongRec(FixedPtr.dwFileVersionLS).Hi > 0 then
        sVersion := sVersion +
          chr(ord('a') - 1 + LongRec(FixedPtr.dwFileVersionLS).Hi);
      Result := sVersion;
    end;
//  except
//    on e: Exception do
//    begin
//      messagedlg('GetAppVersionStr() : ' + e.Message, mtwarning, [mbok], 0);
//      Result := '';
//    end;
//  end;

end;
{$ENDIF}

function GetConsoleOutput(Command: string; Output, Errors: TStream;
  Timeout: Cardinal = INFINITE): Boolean;

  procedure ErrorToStream(Msg: string; stream: TStream);
  var
    ErrMsg: TStringList;
  begin
    try
      ErrMsg := TStringList.Create;
      ErrMsg.Add(Msg);
      ErrMsg.SaveToStream(Errors);
    finally
      FreeAndNil(ErrMsg);
    end;
  end;

{$IFDEF UNIX}
  procedure ReadPipeToStream(PipeStream: TInputPipeStream; stream: TStream);
  begin
    if PipeStream.NumBytesAvailable > 0 then
    begin
      stream.CopyFrom(PipeStream, PipeStream.NumBytesAvailable);
    end;
  end;

const
  itv = 10;
var
  p: TProcess;
  bTimeout: Boolean;
  iTimeout: Integer;
begin
  Result := false;
  Assert(Assigned(Output));
  Assert(Assigned(Errors));

  p := nil;
  try
    p := TProcess.Create(nil);
    p.CommandLine := Command;
    // p.PipeBufferSize:= Cardinal.MaxValue;  // wird in process/pips.inc für Linux nicht verwendet
    // Achtung Betriebssystemabhängig (Bei Ubuntu offenbar 64k - es gibt verschiedene möglichkeiten sich den Anzeigen zu lasen)
    // Der kleine Buffer macht die Kommunikation natürlich sehr träge
    p.Options := p.Options + [TProcessOption.poUsePipes];

    try
      p.Execute;
    except
      on e: Exception do
      begin
        Result := false;
        ErrorToStream(SysErrorMessage(GetLastError), Errors);
        exit;
      end;
    end;

    p.CloseInput;

    iTimeout := 0;
    bTimeout := false;
    while (p.Running) and (not bTimeout) do
    begin
      sleep(itv);
      inc(iTimeout);
      if (iTimeout >= Timeout div itv) then
        bTimeout := true;

      ReadPipeToStream(p.Output, Output);
      ReadPipeToStream(p.Stderr, Errors);
    end;

    Output.Position := 0;
    Errors.Position := 0;

    if bTimeout then
    begin
      p.Terminate(255);
      Result := false;
      ErrorToStream(Format('processtimeout after %d ms', [Timeout]), Errors);
    end
    else
    begin
      if p.ExitCode = 0 then
      begin
        Result := true;
      end
      else
      begin
        Result := false;
        ErrorToStream(Format('process exit code %d', [p.ExitCode]), Errors);
      end;
    end;

  finally
    FreeAndNil(p);
  end;

end;

{$ELSE}
  procedure ReadPipeToStream(PipHandle: THandle; stream: TStream);
  const
    pipeDefaultSize = 1024 * 1024 * 10;
    // relativ groß da pcm daten recht groß sind, sonst performed das nicht
  var
    pipeSize: DWORD;
    NumberOfBytesRead: DWORD;
    TransferBufferSize: DWORD;
    TransferBuffer: TBytes;
  begin
    pipeSize := GetFileSize(PipHandle, nil);
    if pipeSize > 0 then
    begin
      if pipeSize > pipeDefaultSize then
        TransferBufferSize := pipeDefaultSize
      else
        TransferBufferSize := pipeSize;
      SetLength(TransferBuffer, TransferBufferSize);
      // FillChar(TransferBuffer[0], TransferBufferSize, 0);
      while ReadFile(PipHandle, TransferBuffer[0], Length(TransferBuffer),
        NumberOfBytesRead, nil) do
      begin
        stream.Write(TransferBuffer, NumberOfBytesRead);
      end;
      stream.Position := 0;
    end;
  end;

var
  CreationFlags: DWORD;
  PipeErrorsRead: THandle;
  PipeErrorsWrite: THandle;
  PipeOutputRead: THandle;
  PipeOutputWrite: THandle;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  StartupInfo: {$IFDEF FPC}Windows.TStartupInfoW{$ELSE}TStartupInfo{$ENDIF};

  bKill: Boolean;
  ExitCode: DWORD;
begin
  Result := false;
  Assert(Assigned(Output));
  Assert(Assigned(Errors));

  // Initialisierung ProcessInfo
  FillChar(ProcessInfo, SizeOf(TProcessInformation), 0);

  // Initialisierung SecurityAttr
  FillChar(SecurityAttr, SizeOf(TSecurityAttributes), 0);
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := true;
  SecurityAttr.lpSecurityDescriptor := nil;

  // Pipes erzeugen (Größenangabe ist nur ein Vorschlag für das Betriebssystem, muss aber groß genug sein. 0=defaul=scheint 4k zu sein
  CreatePipe(PipeOutputRead, PipeOutputWrite, @SecurityAttr, MAXDWORD);
  CreatePipe(PipeErrorsRead, PipeErrorsWrite, @SecurityAttr, MAXDWORD);

  // Initialisierung StartupInfo
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := PipeOutputWrite;
  StartupInfo.hStdError := PipeErrorsWrite;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  CreationFlags := CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS;

  UniqueString(Command);
  if {$IFDEF FPC}CreateProcessW{$ELSE}CreateProcess{$ENDIF}(nil,
    // LPCTSTR lpApplicationName,
    PChar(Command), // LPTSTR lpCommandLine,
    @SecurityAttr, // LPSECURITY_ATTRIBUTES lpProcessAttributes,
    @SecurityAttr, // LPSECURITY_ATTRIBUTES lpThreadAttributes,
    true, // BOOL bInheritHandles,
    CreationFlags, // DWORD dwCreationFlags,
    nil, // LPVOID lpEnvironment,
    nil, // LPCTSTR lpCurrentDirectory,
    StartupInfo, // LPSTARTUPINFO lpStartupInfo,
    ProcessInfo) // LPPROCESS_INFORMATION lpProcessInformation
  then
  begin
    try
      // Write-Pipes schließen, die benötige ich nicht (Achtung, muss vor dem Lesen passieren)
      CloseHandle(PipeOutputWrite);
      CloseHandle(PipeErrorsWrite);

      // auf den Prozess warten
      bKill := false;
      if WaitForSingleObject(ProcessInfo.hProcess, Timeout) = WAIT_TIMEOUT then
      begin
        // Prozess killen
        bKill := true;
      end;

      // Pipes auslesen
      ReadPipeToStream(PipeErrorsRead, Errors);
      ReadPipeToStream(PipeOutputRead, Output);

      if bKill then
      begin
        Result := false;
        TerminateProcess(ProcessInfo.hProcess, 0);
        ErrorToStream(Format('processtimeout after %d ms', [Timeout]), Errors);
      end
      else
      begin
        if not GetExitCodeProcess(ProcessInfo.hProcess, ExitCode) then
          ExitCode := 255;
        if ExitCode = 0 then
          Result := true
        else
        begin
          ErrorToStream(Format('process exit code %d', [ExitCode]), Errors);
          Result := false;
        end;
      end;

    finally
      // Read-Pipes schließen
      CloseHandle(PipeErrorsRead);
      CloseHandle(PipeOutputRead);

      // Prozesshandle schließen
      CloseHandle(ProcessInfo.hProcess);

      // Streams auf Anfang
      Errors.Position := 0;
      Output.Position := 0;
    end;
  end
  else
  begin
    ErrorToStream(SysErrorMessage(GetLastError), Errors);
    Result := false;
    CloseHandle(PipeOutputRead);
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsRead);
    CloseHandle(PipeErrorsWrite);
  end;
end;
{$ENDIF}

function ExecProcess(Command: string; var Error: String;
  Timeout: Cardinal = INFINITE): Boolean;
{$IFDEF UNIX}
const
  itv = 10;
var
  p: TProcess;
  bTimeout: Boolean;
  iTimeout: Integer;
begin
  Result := false;
  Error := '';

  p := nil;
  try
    p := TProcess.Create(nil);
    p.CommandLine := Command;

    try
      p.Execute;
    except
      on e: Exception do
      begin
        Result := false;
        Error := SysErrorMessage(GetLastError);
        exit;
      end;
    end;

    iTimeout := 0;
    bTimeout := false;
    while (p.Running) and (not bTimeout) do
    begin
      sleep(itv);
      inc(iTimeout);
      if (iTimeout >= Timeout div itv) then
        bTimeout := true;
    end;
    if bTimeout then
    begin
      p.Terminate(255);
      Result := false;
      Error := Format('processtimeout after %d ms', [Timeout]);
    end
    else
    begin
      if p.ExitCode = 0 then
      begin
        Result := true;
      end
      else
      begin
        Result := false;
        Error := Format('process exit code %d', [p.ExitCode]);
      end;
    end;

  finally
    FreeAndNil(p);
  end;

end;
{$ELSE}

var
  CreationFlags: DWORD;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  StartupInfo: {$IFDEF FPC}Windows.TStartupInfoW{$ELSE}TStartupInfo{$ENDIF};
  ExitCode: DWORD;
begin
  Result := false;
  Error := '';

  // Initialisierung ProcessInfo
  FillChar(ProcessInfo, SizeOf(TProcessInformation), 0);

  // Initialisierung SecurityAttr
  FillChar(SecurityAttr, SizeOf(TSecurityAttributes), 0);
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := true;
  SecurityAttr.lpSecurityDescriptor := nil;

  // Initialisierung StartupInfo
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := 0;
  StartupInfo.hStdError := 0;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  CreationFlags := CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS;

  UniqueString(Command);
  if {$IFDEF FPC}CreateProcessW{$ELSE}CreateProcess{$ENDIF}(nil,
    // LPCTSTR lpApplicationName,
    PChar(Command), // LPTSTR lpCommandLine,
    @SecurityAttr, // LPSECURITY_ATTRIBUTES lpProcessAttributes,
    @SecurityAttr, // LPSECURITY_ATTRIBUTES lpThreadAttributes,
    true, // BOOL bInheritHandles,
    CreationFlags, // DWORD dwCreationFlags,
    nil, // LPVOID lpEnvironment,
    nil, // LPCTSTR lpCurrentDirectory,
    StartupInfo, // LPSTARTUPINFO lpStartupInfo,
    ProcessInfo) // LPPROCESS_INFORMATION lpProcessInformation
  then
  begin
    try
      if WaitForSingleObject(ProcessInfo.hProcess, Timeout) = WAIT_TIMEOUT then
      begin
        // Prozess killen
        TerminateProcess(ProcessInfo.hProcess, 0);
        Error := Format('processtimeout after %d ms', [Timeout]);
        Result := false;
      end
      else
      begin
        if not GetExitCodeProcess(ProcessInfo.hProcess, ExitCode) then
          ExitCode := 255;
        if ExitCode = 0 then
          Result := true
        else
        begin
          Error := Format('process exit code %d', [ExitCode]);
          Result := false;
        end;
      end;
    finally
      // Prozesshandle schließen
      CloseHandle(ProcessInfo.hProcess);
    end;
  end
  else
  begin
    Error := SysErrorMessage(GetLastError);
    Result := false;
  end;
end;
{$ENDIF}

function Shellexecute_safe(Operation, FileName, Parameters, Directory: String;
  ShowCmd: Integer): Integer;
{$IFDEF UNIX}
  function isurl(s: string): Boolean;
  const
    curl1 = 'http://';
    curl2 = 'https://';
    curl3 = 'ftp://';
    curl4 = 'ftps://';
    curl5 = 'mailto:';
  begin
    Result := false;
    s := lowercase(s);

    if Copy(s, 1, Length(curl1)) = curl1 then
      Result := true;
    if Copy(s, 1, Length(curl2)) = curl2 then
      Result := true;
    if Copy(s, 1, Length(curl3)) = curl3 then
      Result := true;
    if Copy(s, 1, Length(curl4)) = curl4 then
      Result := true;
    if Copy(s, 1, Length(curl5)) = curl5 then
      Result := true;
  end;

var
  p: TProcess;
begin
  // je nach typ müssen verschiedene Funktionen aufgerufen werden
  if isurl(FileName) then
  begin
    OpenURL(FileName);
  end
  else
  begin
    if Length(ExtractFileExt(FileName)) > 0 then
    begin
      OpenDocument(FileName);
    end
    else
    begin
      p := nil;
      try
        p := TProcess.Create(nil);
        p.CommandLine := FileName + ' ' + Parameters;
        p.Execute;
      finally
        FreeAndNil(p);
      end;
    end;
  end;
end;
{$ELSE}
  function includequote(s: string): string;
  begin
    Result := s;
    if Length(s) > 0 then
    begin
      if (s[1] <> '"') and (s[Length(s)] <> '"') then
      begin
        s := '"' + s + '"';
        Result := s;
      end;
    end;
  end;

var
  tmphwnd: THandle;
  pparameters, pdirectory: PChar;
begin
  Result := MaxInt;
  if Length(Operation) = 0 then
    exit;
  if Length(FileName) = 0 then
    exit;

  FileName := includequote(FileName);
  // bei den Parametern muss es richtig übergeben werden, das geht nicht nicht da parameteraufbau unbekannt
  Directory := includequote(Directory);

  pparameters := nil;
  if Length(Parameters) > 0 then
    pparameters := PChar(Parameters);
  pdirectory := nil;
  if Length(Directory) > 0 then
    pdirectory := PChar(Directory);

  tmphwnd := GetDesktopWindow; { handle des desktop }

  Result := ShellExecute(tmphwnd, PChar(Operation), PChar(FileName),
    PChar(Parameters), PChar(Directory), ShowCmd);
end;
{$ENDIF}

procedure OpenDirectory(Directory: string);
{$IFDEF UNIX}
begin
  OpenDocument(Directory);
end;
{$ELSE}

begin
  { Explorer + Verzeichnis }
  Shellexecute_safe('Open', 'Explorer', Directory, '', SW_SHOW);
end;
{$ENDIF}

function ShowDriveType(Drive: char): string;
{$IFDEF UNIX}
begin
  Result := '';
  Assert(false, rsNotImplemented);
end;
{$ELSE}

var
  i: word;
  drivepath: string;
begin
  drivepath := UpCase(Drive) + ':\';
  i := {$IFDEF FPC}GetDriveTypeW{$ELSE}GetDriveType{$ENDIF}(PChar(drivepath));
  case i of
    0:
      Result := 'unbekannt';
    1:
      Result := 'nicht ermittelbar';
    DRIVE_REMOVABLE:
      Result := 'Wechseldatenträger';
    DRIVE_FIXED:
      Result := 'Festplatte';
    DRIVE_REMOTE:
      Result := 'Netzwerk';
    DRIVE_CDROM:
      Result := 'CD-ROM';
    DRIVE_RAMDISK:
      Result := 'RAM-Drive';
  else
    Result := 'Fehler beim Abfragen des Laufwerkstypes';
  end;
end;
{$ENDIF}

function DiskInDrive(Drive: char): Boolean;
{$IFDEF UNIX}
begin
  Result := true;
end;
{$ELSE}

var
  ErrorMode: word;
begin
  DiskInDrive := false;
  try
    Drive := UpCase(Drive);
    if not CharinSet(Drive, ['A' .. 'Z']) then
      raise EConvertError.Create('Kein Laufwerksbuchstabe');
    ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
    try
      Result := DiskSize(ord(Drive) - $40) <> -1;
    finally
      SetErrorMode(ErrorMode);
    end;
  except
    on e: Exception do
    begin
      { nix }
    end;
  end;
end;
{$ENDIF}

procedure CopyDirContent(fromdir, todir: string);
{$IFDEF UNIX}
begin
  Assert(false, rsNotImplemented);
end;
{$ELSE}

var
  sr: TSearchRec;
  res: Integer;
begin
  fromdir := IncludeTrailingPathDelimiter(fromdir);
  todir := IncludeTrailingPathDelimiter(todir);
  if not directoryexists(fromdir) then
    exit;
  if not directoryexists(todir) then
    exit;

  res := FindFirst(fromdir + FileFilterAll, faAnyFile, sr);
  while res = 0 do
  begin
    try
{$IFDEF FPC}CopyFileW{$ELSE}CopyFile{$ENDIF}(PChar(fromdir + sr.Name), PChar(todir + sr.Name), true)
    except
      on e: Exception do
      begin
        //
      end;
    end;

    res := FindNext(sr)
  end;

end;
{$ENDIF}

function FileSizeLarge(FileName: string): int64;
{$IFDEF UNIX}
var
  F: TFileStream;
begin
  F := nil;
  try
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    Result := F.Size;
  finally
    FreeAndNil(F);
  end;
end;
{$ELSE}

var
  AHandle: THandle;
  L, H: Cardinal;
begin
  Result := 0;
  AHandle := INVALID_HANDLE_VALUE;
  try
    AHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
    L := GetFileSize(AHandle, @H);
    // Rückgabe ist Lowwert, zweiter Parameter ist Highwert für Dateien > 4GB
    if Result <> INVALID_HANDLE_VALUE then
    begin
      // h shl 32 or L geht leider nicht
      Int64Rec(Result).Lo := L;
      Int64Rec(Result).Hi := H;
    end;
  finally
    FileClose(AHandle);
  end;
end;
{$ENDIF}

function MoveFile_safe(fromfile, tofile: String; failifexists: Boolean)
  : Boolean;
begin
{$IFDEF UNIX}
  Result := false;
  if fileexists(tofile) then
  begin
    if failifexists then
      exit;
    if not deletefile(tofile) then
      exit;
  end;
  Result := RenameFile(fromfile, tofile);
{$ELSE}
  if failifexists then
    Result := {$IFDEF FPC}MoveFileW{$ELSE}MoveFile{$ENDIF}(PChar(fromfile),
      PChar(tofile))
  else
    Result := {$IFDEF FPC}MoveFileExW{$ELSE}MoveFileEx{$ENDIF}(PChar(fromfile),
      PChar(tofile), MOVEFILE_REPLACE_EXISTING);
{$ENDIF}
end;

function CopyFile_safe(fromfile, tofile: String; failifexists: Boolean)
  : Boolean;
begin
{$IFDEF UNIX}
  if failifexists then
    Result := CopyFile(fromfile, tofile, [], false)
  else
    Result := CopyFile(fromfile, tofile, [cffOverwriteFile], false);
{$ELSE}
  Result := {$IFDEF FPC}CopyFileW{$ELSE}CopyFile{$ENDIF}(PChar(fromfile),
    PChar(tofile), failifexists);
{$ENDIF}
end;

{$IFDEF UNIX}

function GetLastError: Integer;
begin
  Result := GetLastOSError;
end;
{$ENDIF}

end.
