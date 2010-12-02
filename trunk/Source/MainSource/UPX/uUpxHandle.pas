unit uUpxHandle;

interface

uses
  Classes, SysUtils, Windows, Forms, Math, uUpxResAPI;

type

  EUpxHandle = Exception;

  TOnUpxProgress = procedure(aProgress: Integer; aFileSize: int64) of object;

  TUPXHandle = class
  private
    FCompressLevel: Integer;
    FFileName: string;
    FIsStripRelocation: Boolean;
    FIsCompressExport: Boolean;
    FIsBackUpFile: Boolean;
    FCompressIconLevel: Integer;
    FIsForce: Boolean;
    FIsCompressRC: Boolean;
    FIsTestAfterCompression: Boolean;
    FCustomParam: string;
    FStdOut: THandle;
    FCompressPriority: Cardinal;
    FIsDebugMode: Boolean;
    FOnUpxProgress: TOnUpxProgress;
    FUpxVersion: TUPXVersions;

    procedure SetCompressIconLevel(const Value: Integer);
    procedure SetCompressLevel(const Value: Integer);
    procedure SetFileName(const Value: string);
    procedure SetIsBackUpFile(const Value: Boolean);
    procedure SetIsCompressExport(const Value: Boolean);
    procedure SetIsCompressRC(const Value: Boolean);
    procedure SetIsForce(const Value: Boolean);
    procedure SetIsStripRelocation(const Value: Boolean);
    procedure SetIsTestAfterCompression(const Value: Boolean);
    procedure SetCustomParam(const Value: string);
    procedure SetCompressPriority(const Value: Cardinal);
    procedure SetIsDebugMode(const Value: Boolean);
    procedure SetOnUpxProgress(const Value: TOnUpxProgress);
    procedure SetUpxVersion(const Value: TUPXVersions);
  protected
    procedure RaiseError(aError: string);
    function GetUpxName: string;
    procedure ExtractUPX;
    procedure DelUpx;

    function GetCompressParam: string; virtual;
    function GetDeCompressParam: string;
    procedure AllocateConsole;
    procedure HideConsole;
    procedure GetProgress(ProcInfo: TProcessInformation);

    function DoWork(IsCompress: Boolean): Integer;
    procedure DoScramblerFile;
  public
    function CompressFile: Integer;
    function DeCompressFile: Integer;

    procedure ScramblerFile;
    function IsFilePacked(): boolean;

    function GetErrorText: string;

    property FileName: string read FFileName write SetFileName;

    property CompressLevel: Integer read FCompressLevel write SetCompressLevel;
    property CompressIconLevel: Integer read FCompressIconLevel write
      SetCompressIconLevel;
    property CustomParam: string read FCustomParam write SetCustomParam;
    property CompressPriority: Cardinal read FCompressPriority write
      SetCompressPriority;

    property IsBackUpFile: Boolean read FIsBackUpFile write SetIsBackUpFile;
    property IsTestAfterCompression: Boolean read FIsTestAfterCompression write
      SetIsTestAfterCompression;
    property IsForce: Boolean read FIsForce write SetIsForce;
    property IsCompressRC: Boolean read FIsCompressRC write SetIsCompressRC;
    property IsStripRelocation: Boolean read FIsStripRelocation write
      SetIsStripRelocation;
    property IsCompressExport: Boolean read FIsCompressExport write
      SetIsCompressExport;
    property IsDebugMode: Boolean read FIsDebugMode write SetIsDebugMode;
    property UpxVersion: TUPXVersions read FUpxVersion write SetUpxVersion;

    property OnUpxProgress: TOnUpxProgress read FOnUpxProgress write
      SetOnUpxProgress;
  end;

implementation

uses Shared, Translator;


{ TUPXHandle }

function LastPos(const Substr: char; const S: string): Integer;
begin
  for Result := Length(S) downto 1 do
  begin
    if S[Result] = Substr then
    begin
      Break;
    end;
  end;
end;

procedure Split(const Delimiter: char; const Input: string;
  const Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  Strings.Delimiter := Delimiter;
  Strings.DelimitedText := Input;
end;



procedure TUPXHandle.GetProgress(ProcInfo: TProcessInformation);
var
  EC: Cardinal;
  BracketOffsetStart: Integer;
  BracketOffsetEnd: Integer;
  upxProgressBarStart: Integer;
  upxProgressBarSize: Integer;
  upxProgressBar: string;
  // This is filled by the LastPos function located in Globals.
  upxCurProgressBarPos: Integer;
  // This will hold the total amount of * to be counted when in MultiProgress.
  upxTotalProgress: Integer;
  upxCurrentProgress: Integer;

  ProgressValue: Integer;
  CompressSize: string;
  CursorPos: TCoord;
  CharsRead: DWord;
  Line: array [0 .. 500] of char;
  i: integer;
begin

  GetExitCodeProcess(ProcInfo.hProcess, EC);

  CursorPos.X := 0;
  CursorPos.Y := 0;
  BracketOffsetStart := 0;
  BracketOffsetEnd := 0;
  upxProgressBarStart := 0;
  upxProgressBarSize := 0;
  upxTotalProgress := 0;
  // Let's find where the progress starts
  while True do
  begin
    ReadConsoleOutputCharacter(FStdOut, Line, 80, CursorPos, CharsRead);
    BracketOffsetStart := Pos('[', Line) - 1;

    if BracketOffsetStart > -1 then
    begin
      BracketOffsetEnd := Pos(']', Line) - 1;
      upxProgressBarStart := BracketOffsetStart + 1;
      upxProgressBarSize := BracketOffsetEnd - upxProgressBarStart;
      Break;
    end
    else
    begin
      Inc(CursorPos.Y);
    end;
    if CursorPos.Y > 20 then
    begin // If we get here - something's wrong
      CursorPos.Y := 0;
      GetExitCodeProcess(ProcInfo.hProcess, EC);
      if EC <= 2 then
      begin
        Exit;
      end;
    end;
  end;

  // Here we go and check the progress of the compression
  while EC = STILL_ACTIVE do
  begin
    ReadConsoleOutputCharacter(FStdOut, Line, 80, CursorPos, CharsRead);

    if Line[BracketOffsetStart] = '[' then
    begin
      CompressSize := '';
      upxProgressBar := Copy(Line, upxProgressBarStart + 1, upxProgressBarSize);
      upxCurProgressBarPos := LastPos('*', upxProgressBar);

      if upxTotalProgress = 0 then
      begin
        upxTotalProgress := upxProgressBarSize;
      end;
      upxCurrentProgress := upxCurProgressBarPos;

      try
        ProgressValue := floor((upxCurrentProgress / upxTotalProgress) * 100);
        CompressSize := '';
        for i := BracketOffsetEnd + 1 to 500 - 1 do
        begin
          if Line[i] = '%' then
            Break;
          CompressSize := CompressSize + Line[i];
        end;

        CompressSize := Trim(CompressSize);
        if Assigned(FOnUpxProgress) then
          FOnUpxProgress(ProgressValue, Round(StrToFloat(CompressSize)));

        // Here we do some sleeping so we won't hogg the system.
        sleep(10);
        Application.ProcessMessages;
        if ProgressValue >= 100 then
        begin
          Exit;
        end;
      except
        on e: Exception do
        begin
          Break;
        end;
      end;

    end;
    GetExitCodeProcess(ProcInfo.hProcess, EC);
  end;


end;

procedure TUPXHandle.AllocateConsole;
var
  CursorPos: TCoord;
  ConsoleTitle: array [1 .. MAX_PATH] of char;
begin
  AllocConsole;
  if not IsDebugMode then
  begin
    GetConsoleTitle(@ConsoleTitle, MAX_PATH);
    ShowWindow(FindWindow(nil, @ConsoleTitle), 0);
    Application.BringToFront;
  end;
  FStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  CursorPos.X := 500;
  CursorPos.Y := 50;
  SetConsoleScreenBufferSize(FStdOut, CursorPos);
  CursorPos.X := 0;
  CursorPos.Y := 0;
  SetConsoleCursorPosition(FStdOut, CursorPos);
end;

function TUPXHandle.CompressFile: Integer;
begin
  Result := DoWork(True);
end;

function TUPXHandle.DeCompressFile: Integer;
begin
  Result := DoWork(False);
end;

procedure TUPXHandle.DelUpx;
begin
  Deletefile(PChar(ExtractFilePath(ParamStr(0)) + GetUpxName));
end;

procedure TUPXHandle.DoScramblerFile;
var
  fsSource: TFileStream;
  GlobalChain: array [1 .. $2BE0] of Byte;
  FileSize: Int64;

  function ReplaceUpxStr(aOld, aNew: AnsiString; aNewLen: Integer): Boolean;
  var
    PosString: integer;
  begin
    PosString := AnsiStrInByteArrayPos(aOld, @GlobalChain[1], $2BE0);
    Result := PosString <> 0;
    if Result then
    begin
      fsSource.Position := PosString - 1;
      fsSource.Write(aNew[1], aNewLen);
    end;
  end;
begin
  fsSource := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    if fsSource.Size < $2BE0 then
      FileSize := fsSource.Size
    else
      FileSize := $2BE0;

    fsSource.Position := 0;
    fsSource.ReadBuffer(GlobalChain, FileSize);

    // Scramble UPX0 -> code | UPX0 located in upx v0.6 until present
    ReplaceUpxStr('UPX0', 'CODE', 4);
    // Scramble UPX1 -> text | UPX1 located in upx v0.6 until present
    ReplaceUpxStr('UPX1', 'DATA', 4);
    // Scramble UPX2 -> data | UPX2 located in upx v0.6 until v1.0x
    ReplaceUpxStr('UPX2', 'BSS'#$00, 4);
    // Scramble UPX3 -> data | UPX3 located in upx v0.7x i think.
    ReplaceUpxStr('UPX3', 'IDATA', 4);
    // Scramble OLD '$Id: UPXScrambler.pas,v 1.14 2007/01/23 21:43:50 dextra Exp $Id: UPX' located in upx v0.06 until v1.07x
    if ReplaceUpxStr('$Id: UPX', #0#0#0#0#0#0#0#0#0#0#0#0#0, 13) then
    begin
      ReplaceUpxStr('UPX!', #0#0#0#0#0#0, 6);
    end
    else
    begin
      ReplaceUpxStr('UPX!', #0#0#0#0#0#0#0#0#0#0#0, 11);
    end;
    // Scramble anything that is left of something called UPX within the header
    ReplaceUpxStr('UPX', #0#0#0, 3);
  finally
    FreeAndNil(fsSource);
  end;
end;

function TUPXHandle.DoWork(IsCompress: Boolean): Integer;
var
  StartInfo: Tstartupinfo;
  ProcInfo: TProcessInformation;
  lpExitCode: Cardinal;
  aParams: string;
  aWorkDir: string;
begin
  aWorkDir := ExtractFilePath(ParamStr(0));

  AllocateConsole;
  FillChar(StartInfo, SizeOf(StartInfo), 0);
  with StartInfo do
  begin
    cb := SizeOf(StartInfo);
    dwFlags := startf_UseShowWindow;
    wShowWindow := 2;
    lpTitle := PChar('UPX Shell - ' + FileName);
  end;

  if UpxVersion <> UPXC then
    ExtractUPX;

  aParams := GetCompressParam;

  Createprocess(nil, PChar(aParams), nil, nil, True,
    Create_default_error_mode + CompressPriority, nil, PChar(aWorkDir),
    StartInfo, ProcInfo);

  // Hide console window if it still shows
  HideConsole;
  if IsCompress then
    GetProgress(ProcInfo);

  Waitforsingleobject(ProcInfo.hProcess, infinite);
  GetExitCodeProcess(ProcInfo.hProcess, lpExitCode);

  if UpxVersion <> UPXC then
    DelUpx;

  Result := lpExitCode;

end;

procedure TUPXHandle.ExtractUPX;
begin
  ExtractUPXApp(resUPXVersions[FUpxVersion],
    ExtractFilePath(ParamStr(0)) + GetUpxName);
end;

function TUPXHandle.GetCompressParam: string;
begin

  Result := ExtractFilePath(ParamStr(0)) + GetUpxName + ' "' + FFileName + '"';

  if CompressLevel < 10 then
    Result := Result + ' -' + IntToStr(CompressLevel)
  else
    Result := Result + ' --best';

  if IsForce then
    Result := Result + ' --force';

  if IsCompressRC then
    Result := Result + ' --compress-resources=1'
  else
    Result := Result + ' --compress-resources=0';

  if IsStripRelocation then
    Result := Result + ' --strip-relocs=1'
  else
    Result := Result + ' --strip-relocs=0';

  if IsBackUpFile then
    Result := Result + ' -k';

  Result := Result + ' --compress-icons=' + IntToStr(CompressIconLevel);

  if IsCompressExport then
    Result := Result + ' --compress-exports=1'
  else
    Result := Result + ' --compress-exports=0';

  Result := Result + ' ' + CustomParam;

end;

function TUPXHandle.GetDeCompressParam: string;
begin
  Result := ExtractFilePath(ParamStr(0))
    + GetUpxName + ' "' + FFileName + '"' + ' -d';
end;

function TUPXHandle.GetErrorText: string;
  function ReadErrorLine(I: integer): string;
  var
    TextLen: DWord;
    CursorPos: TCoord;
    CharsRead: DWord;
  begin
    TextLen := 500;
    SetLength(Result, 500);
    CursorPos.X := 0;
    CursorPos.Y := I;
    CharsRead := 0;
    ReadConsoleOutputCharacter(FStdOut, @Result[1], TextLen, CursorPos,
      CharsRead);
  end;
begin
  Result := Trim(ReadErrorLine(6));
end;

function TUPXHandle.GetUpxName: string;
begin
  if UpxVersion = UPXC then
    Result := 'upx.exe'
  else
    Result := 'upxe.exe';
end;

procedure TUPXHandle.HideConsole;
var
  ConsoleTitle: array [1 .. MAX_PATH] of char;
begin
  // only hide when we're not in debug mode
  if not IsDebugMode then
  begin
    GetConsoleTitle(@ConsoleTitle, MAX_PATH);
    ShowWindow(FindWindow(nil, @ConsoleTitle), 0);
    Application.BringToFront;
  end;
end;

function TUPXHandle.IsFilePacked(): boolean;
var
  f: TFileStream;
  aBuff: array [1 .. $3F0] of Byte;
begin
  Result := False;
  f := TFileStream.Create(FileName, fmOpenRead);
  try
    f.Position := 0;
    try
      f.ReadBuffer(aBuff, $3EF);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar(TranslateMsg(
              'Could not access file. It may be allready open!')),
          PChar(TranslateMsg('Error')), MB_ICONERROR or MB_OK);
        Exit;
      end;
    end;
    Result := (AnsiStrInByteArrayPos('UPX', @aBuff[1], $3EF) <> 0);
  finally
    FreeAndNil(f);
  end;
end;

procedure TUPXHandle.RaiseError(aError: string);
begin
  raise EUpxHandle.Create(aError);
end;

procedure TUPXHandle.ScramblerFile;
begin
  DoScramblerFile;
end;

procedure TUPXHandle.SetCompressIconLevel(const Value: Integer);
begin
  FCompressIconLevel := Value;
end;

procedure TUPXHandle.SetCompressLevel(const Value: Integer);
begin
  FCompressLevel := Value;
end;

procedure TUPXHandle.SetCompressPriority(const Value: Cardinal);
begin
  FCompressPriority := Value;
end;

procedure TUPXHandle.SetCustomParam(const Value: string);
begin
  FCustomParam := Value;
end;

procedure TUPXHandle.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TUPXHandle.SetIsBackUpFile(const Value: Boolean);
begin
  FIsBackUpFile := Value;
end;

procedure TUPXHandle.SetIsCompressExport(const Value: Boolean);
begin
  FIsCompressExport := Value;
end;

procedure TUPXHandle.SetIsCompressRC(const Value: Boolean);
begin
  FIsCompressRC := Value;
end;

procedure TUPXHandle.SetIsDebugMode(const Value: Boolean);
begin
  FIsDebugMode := Value;
end;

procedure TUPXHandle.SetIsForce(const Value: Boolean);
begin
  FIsForce := Value;
end;

procedure TUPXHandle.SetIsStripRelocation(const Value: Boolean);
begin
  FIsStripRelocation := Value;
end;

procedure TUPXHandle.SetIsTestAfterCompression(const Value: Boolean);
begin
  FIsTestAfterCompression := Value;
end;


procedure TUPXHandle.SetOnUpxProgress(const Value: TOnUpxProgress);
begin
  FOnUpxProgress := Value;
end;


procedure TUPXHandle.SetUpxVersion(const Value: TUPXVersions);
begin
  FUpxVersion := Value;
end;

end.
