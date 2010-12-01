unit uUpxHandle;

interface

uses
  Classes, SysUtils, Windows, Forms, Math;

type

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
    FFileSize: int64;
    FStdOut: THandle;
    FUpxResName: string;
    FCompressPriority: Cardinal;
    FIsDebugMode: Boolean;
    FOnUpxProgress: TOnUpxProgress;

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
    procedure SetUpxResName(const Value: string);
    procedure SetCompressPriority(const Value: Cardinal);
    procedure SetIsDebugMode(const Value: Boolean);
    procedure SetOnUpxProgress(const Value: TOnUpxProgress);
  protected
    function GetUpxName: string; virtual;

    function GetCompressParam: string;
    function GetDeCompressParam: string;

    function GetFileSize(aFile: string): int64;
    procedure AllocateConsole;
    procedure HideConsole;
    procedure GetProgress(ProcInfo: TProcessInformation;
      const IsMultiProgress: Boolean);

    function DoWork(IsCompress: Boolean): Integer;
  public
    function CompressFile: Integer;
    function DeCompressFile: Integer;

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

    property UpxResName: string read FUpxResName write SetUpxResName;

    property FileSize: int64 read FFileSize;

    property OnUpxProgress: TOnUpxProgress read FOnUpxProgress write
      SetOnUpxProgress;
  end;

implementation

uses uUpxResAPI, Translator;

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

function TUPXHandle.GetFileSize(aFile: string): int64;
var
  sr: TSearchRec;
begin
  if FindFirst(aFile, faAnyFile, sr) = 0 then
  begin
    Result := sr.Size;
  end
  else
  begin
    Result := -1;
  end;
  SysUtils.FindClose(sr);
end;

procedure TUPXHandle.GetProgress(ProcInfo: TProcessInformation;
  const IsMultiProgress: Boolean);
var
  EC: Cardinal;
  BracketOffsetStart: Integer;
  BracketOffsetEnd: Integer;
  upxProgressBarStart: Integer;
  upxProgressBarSize: Integer;
  upxProgressBar: string;
  upxCurProgressBarPos: Integer; // This is filled by the LastPos function located in Globals.
  upxTotalProgress: Integer; // This will hold the total amount of * to be counted when in MultiProgress.
  upxCurrentProgress: Integer;
  OldAppTitle: string;

  ProgressValue: Integer;
  CompressSize: string;
  MultiRepeat: TStringList;
  CursorPos: TCoord;
  CharsRead: DWord;
  Line: array [0 .. 500] of char;
begin
  GetExitCodeProcess(ProcInfo.hProcess, EC);
  OldAppTitle := Application.Title;
  CursorPos.X := 0;
  CursorPos.Y := 0;
  BracketOffsetStart := 0;
  upxProgressBarStart := 0;
  upxProgressBarSize := 0;
  upxTotalProgress := 0;
  MultiRepeat := TStringList.Create;

  while True do // Let's find where the progress starts
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
  while EC > 2 do // ec >= STILL_ACTIVE
  begin
    ReadConsoleOutputCharacter(FStdOut, Line, 80, CursorPos, CharsRead);
    if Line[BracketOffsetStart] = '[' then
    begin
      CompressSize := '';
      upxProgressBar := Copy(Line, upxProgressBarStart + 1, upxProgressBarSize);
      upxCurProgressBarPos := LastPos('*', upxProgressBar);

      if IsMultiProgress then
      begin
        Split('/', Trim(Copy(Line, BracketOffsetStart - 7, 6)), MultiRepeat);
        if upxTotalProgress = 0 then
        begin
          upxTotalProgress := (StrToInt(MultiRepeat[1]) * upxProgressBarSize);
        end;
        upxCurrentProgress := (((StrToInt(MultiRepeat[0]) - 1)
              * upxProgressBarSize) + upxCurProgressBarPos);
      end
      // ELSE NO MULTIPROGRESS
      else
      begin
        if upxTotalProgress = 0 then
        begin
          upxTotalProgress := upxProgressBarSize;
        end;
        upxCurrentProgress := upxCurProgressBarPos;
      end;

      // Calculate the current progress to show to the user.
      ProgressValue := floor((upxCurrentProgress / upxTotalProgress) * 100);

      // The percentage of the compression on the file.
      CompressSize := Line[69] + Line[70] + Line[71] + Line[72];

      Application.Title := OldAppTitle + ' - ' + IntToStr(ProgressValue) + '%';
      if Assigned(FOnUpxProgress) then
        FOnUpxProgress(Round(StrToFloat(CompressSize)), ProgressValue);

      // Here we do some sleeping so we won't hogg the system.
      sleep(50);
      Application.ProcessMessages;
      if ProgressValue >= 100 then
      begin
        Exit;
      end;
    end;
    GetExitCodeProcess(ProcInfo.hProcess, EC);
  end;
  Application.Title := OldAppTitle;
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

function TUPXHandle.DoWork(IsCompress: Boolean): Integer;
var
  StartInfo: Tstartupinfo;
  ProcInfo: TProcessInformation;
  lpExitCode: Cardinal;
  IsMultiProgress: Boolean;
  aParams: string;
  aWorkDir: string;
begin
  aWorkDir := ExtractFilePath(ParamStr(0));
  FFileSize := GetFileSize(FileName);

  AllocateConsole;
  FillChar(StartInfo, SizeOf(StartInfo), 0);
  with StartInfo do
  begin
    cb := SizeOf(StartInfo);
    dwFlags := startf_UseShowWindow;
    wShowWindow := 2;
    lpTitle := PChar('UPX Shell - ' + FileName);
  end;
  ExtractUPXApp(FUpxResName, aWorkDir + GetUpxName);

  aParams := GetCompressParam;

  // Now start upx.exe with specified parameters
  Createprocess(nil, PChar(aParams), nil, nil, True,
    Create_default_error_mode + CompressPriority, nil, PChar(aWorkDir),
    StartInfo, ProcInfo);

  HideConsole; // Hide console window if it still shows
  if IsCompress then
    GetProgress(ProcInfo, IsMultiProgress);

  Waitforsingleobject(ProcInfo.hProcess, infinite);
  GetExitCodeProcess(ProcInfo.hProcess, lpExitCode);

  Result := lpExitCode;

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

  Result := Result + ' --compress-icons=2' + IntToStr(CompressIconLevel);

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

function TUPXHandle.GetUpxName: string;
begin
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

procedure TUPXHandle.SetUpxResName(const Value: string);
begin
  FUpxResName := Value;
end;

end.
