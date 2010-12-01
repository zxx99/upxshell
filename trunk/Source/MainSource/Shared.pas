unit Shared;

interface

uses
  Windows, Forms, Classes, Graphics, Dialogs, Globals;

procedure Split(const Delimiter: char; const Input: string;
  const Strings: TStrings);
function GetPriority: cardinal;

function IsAppPacked(aApp: string): boolean;

function GetCompressParams: string;

procedure ExtractUPX(const Action: TExtractDelete);
procedure ScrambleUPX;

procedure DrawGradient(const DrawCanvas: TCanvas; const ColorCycles, Height,
  Width: integer; const StartColor, EndColor: TColor);
function ProcessSize(const Size: integer): string;
function GetFileSize(const FileName: string): integer;

procedure CalcFileSize;


procedure WriteLog(Const InStr: String);

function IsNumeric(const InStr: string): boolean;
function PropertyExists(Component: TComponent; const PropName: string): boolean;
function StringEncode(const InStr: string): string;
function StringDecode(const InStr: string): string;

//获取字符串在byte数组中的位置，大小从1开始，aBytes[0]匹配返回值为1 !!!!!
function AnsiStrInByteArrayPos(aStr: AnsiString; aBytes: PByte; aByteLen: Integer): integer;

function GetUPXBuild(const FilePath: string): string;


implementation

uses
  SysUtils, TypInfo,
  UPXScrambler, Translator,
  MainFrm, SetupFrm, uUpxResAPI;

procedure Split(const Delimiter: char; const Input: string;
  const Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  Strings.Delimiter := Delimiter;
  Strings.DelimitedText := Input;
end;


// This one is used for getting the priority of the compression thread
{ **
  * There are more Priorities when os is equal or higher then Windows 2000.
  * Idle						=		$00000040		=		IDLE_PRIORITY_CLASS
  * (Below Normal	=		$00004000		=		BELOW_NORMAL_PRIORITY_CLASS) 2k/xp/vista only
  * Normal					=		$00000020		=		NORMAL_PRIORITY_CLASS
  * (Above Normal	=		$00008000		=		ABOVE_NORMAL_PRIORITY_CLASS) 2k/xp/vista only
  * High						=		$00000080		=		HOGH_PRIORITY_CLASS
  * Realtime				=		$00000100		=		REALTIME_PRIORITY_CLASS
  ** }
function GetPriority: cardinal;
var
  Priority: integer;
begin
  Result := NORMAL_PRIORITY_CLASS;
  Priority := SetupForm.cmbPriority.ItemIndex;
  case Priority of
    0:
      begin
        Result := IDLE_PRIORITY_CLASS;
      end;
    1:
      begin
        Result := NORMAL_PRIORITY_CLASS;
      end;
    2:
      begin
        Result := HIGH_PRIORITY_CLASS;
      end;
    3:
      begin
        Result := REALTIME_PRIORITY_CLASS;
      end;
  end;
end;

// Analyzes the file to check if it's already compressed
function IsAppPacked(aApp: string): boolean;
var
  f: TFileStream;
  aBuff: array [1 .. $3F0] of Byte;
begin
  Result := False;

  try
    f := TFileStream.Create(aApp, fmOpenRead);
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

    // offset $34b... test the typical string in .EXE & .DLL 'This file is packed with the UPX'
    // offset $1F9... test string in .SCR 'This file is packed with the UPX'
    // offset $55... string 'UPX' in .COM
    // new offset in .EXE in UPX 1.08
    // another offset in .EXE in UPX 1.20
  finally
    FreeAndNil(f);
  end;
end;

// Gets compression parameters to be passed to upx
function GetCompressParams: string;
var
  upxExeFile: string;
begin
  with MainForm do
  begin
    if (curUPXVersion = UPXC) then
    begin
      upxExeFile := 'UPX.EXE';
    end
    else
    begin
      upxExeFile := 'UPXE.EXE';
    end;

    Result := WorkDir + upxExeFile + ' "' + GlobFileName + '"';
    if not chkDecomp.Checked then
    begin
      if trbCompressLvl.Position < 10 then
        Result := Result + ' -' + IntToStr(trbCompressLvl.Position)
      else
        Result := Result + ' --best';


      if SetupForm.chkForce.Checked then
        Result := Result + ' --force';


      if SetupForm.chkResources.Checked then
        Result := Result + ' --compress-resources=1'
      else
        Result := Result + ' --compress-resources=0';


      if SetupForm.chkRelocs.Checked then
        Result := Result + ' --strip-relocs=1'
      else
        Result := Result + ' --strip-relocs=0';

      if chkBackup.Checked then
        Result := Result + ' -k';


      case SetupForm.cmbIcons.ItemIndex of
        0:
          begin
            Result := Result + ' --compress-icons=2';
          end;
        1:
          begin
            Result := Result + ' --compress-icons=1';
          end;
        2:
          begin
            Result := Result + ' --compress-icons=0';
          end;
      end;

      if SetupForm.chkExports.Checked then
        Result := Result + ' --compress-exports=1'
      else
        Result := Result + ' --compress-exports=0';

      Result := Result + ' ' + SetupForm.txtCommands.Text;

    end
    else
    begin
      Result := Result + ' -d';
    end;
    MainForm.rchChangeLog.Text := Result;
    // MessageDlg('End Result: '+ Result, mtWarning, [mbOK], 0);
  end;
end;

procedure ExtractUPX(const Action: TExtractDelete);
begin
  SetCurrentDir(WorkDir);
  if not(curUPXVersion = UPXC) then
  begin
    if Action = edExtract then
    begin
      ExtractUPXApp(resUPXVersions[curUPXVersion], WorkDir + 'UPXE.EXE');
    end
    else
    begin
      DeleteFile(WorkDir + 'UPXE.EXE');
    end;
  end;
end;

{ *****************************************
  * This Function Scrambles the UPXed file *
  ***************************************** }
procedure ScrambleUPX;
var
  Scrambled: boolean;
begin
  if GlobFileName <> '' then
  begin
    if IsAppPacked(GlobFileName) then
    begin
      Scrambled := fScrambleUPX(GlobFileName);
      if Scrambled then
      begin
        MainForm.chkDecomp.Checked := False;
        MainForm.stbMain.Panels[1].Text := MainForm.stbMain.Panels[1]
          .Text + TranslateMsg(' & scrambled');
      end
      else
      begin
        MainForm.stbMain.Panels[1].Text := MainForm.stbMain.Panels[1]
          .Text + TranslateMsg(' & scrambled') + ' ' + TranslateMsg('Failed');
      end;
    end
    else
    begin
      if Application.MessageBox(PChar(TranslateMsg(
            'This file doesn''t seem to be packed. Run the Scrambler?')),
        PChar(TranslateMsg('Confirmation')), MB_YESNO + MB_ICONEXCLAMATION)
        = idYes then
      begin
        Scrambled := fScrambleUPX(GlobFileName);
        if Scrambled then
        begin
          MainForm.chkDecomp.Checked := False;
          MainForm.stbMain.Panels[1].Text := MainForm.stbMain.Panels[1]
            .Text + TranslateMsg(' & scrambled');
        end
        else
        begin
          MainForm.stbMain.Panels[1].Text := MainForm.stbMain.Panels[1]
            .Text + TranslateMsg(' & scrambled') + ' ' + TranslateMsg
            ('Failed');
        end;
      end;
    end;
  end;
end;

// These are the proceudres to draw that gradient near UPX logo
type
  TCustomColorArray = array [0 .. 255] of TColor;

function CalculateColorTable(StartColor, EndColor: TColor;
  ColorCycles: integer): TCustomColorArray;
var
  BeginRGB: array [0 .. 2] of byte;
  DiffRGB: array [0 .. 2] of integer;
  R, G, B, I: byte;
begin
  BeginRGB[0] := GetRValue(ColorToRGB(StartColor));
  BeginRGB[1] := GetGValue(ColorToRGB(StartColor));
  BeginRGB[2] := GetBValue(ColorToRGB(StartColor));
  DiffRGB[0] := GetRValue(ColorToRGB(EndColor)) - BeginRGB[0];
  DiffRGB[1] := GetGValue(ColorToRGB(EndColor)) - BeginRGB[1];
  DiffRGB[2] := GetBValue(ColorToRGB(EndColor)) - BeginRGB[2];
  for I := 0 to 255 do
  begin
    R := BeginRGB[0] + MulDiv(I, DiffRGB[0], ColorCycles - 1);
    G := BeginRGB[1] + MulDiv(I, DiffRGB[1], ColorCycles - 1);
    B := BeginRGB[2] + MulDiv(I, DiffRGB[2], ColorCycles - 1);
    Result[I] := RGB(R, G, B);
  end;
end;

{ ** ** }
procedure DrawGradient(const DrawCanvas: TCanvas; const ColorCycles, Height,
  Width: integer; const StartColor, EndColor: TColor);
var
  Rec: TRect;
  I: integer;
  Temp: TBitmap;
  ColorArr: TCustomColorArray;
begin
  try
    ColorArr := CalculateColorTable(StartColor, EndColor, ColorCycles);
    Temp := TBitmap.Create;
    Temp.Width := Width;
    Temp.Height := Height;
    Rec.Top := 0;
    Rec.Bottom := Height;
    with Temp do
    begin
      for I := 0 to ColorCycles do
      begin
        Rec.Left := MulDiv(I, Width, ColorCycles);
        Rec.Right := MulDiv(I + 1, Width, ColorCycles);
        Canvas.Brush.Color := ColorArr[I];
        Canvas.FillRect(Rec);
      end;
    end;
    DrawCanvas.Draw(0, 0, Temp);
  finally
    FreeAndNil(Temp);
  end;
end;

{ ** ** }
function ProcessSize(const Size: integer): string;
begin
  Result := IntToStr(Size);
  case length(Result) of
    1 .. 3:
      begin
        Result := IntToStr(Size) + ' B';
      end;
    4 .. 6:
      begin
        Result := IntToStr(Size shr 10) + ' KB';
      end;
    7 .. 9:
      begin
        Result := IntToStr(Size shr 20) + ' MB';
      end;
    10 .. 12:
      begin
        Result := IntToStr(Size shr 30) + ' GB';
      end;
  end;
end;

{ ** ** }
function GetFileSize(const FileName: string): integer;
var
  sr: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, sr) = 0 then
  begin
    Result := sr.Size;
  end
  else
  begin
    Result := -1;
  end;
  FindClose(sr);
end;



{ ** Calculates file size ** }
procedure CalcFileSize;
begin
  GlobFileSize := GetFileSize(GlobFileName);
  MainForm.lblFSize.Caption := ProcessSize(GlobFileSize);
end;


procedure WriteLog(const InStr: string);
const
  CRLF = #13#10;
  TimeFormat = 'dd/mm/yy||hh:nn:ss' + #09;
var
  fs: TFileStream;
  filemode: word;
  date: string;
begin
  if Globals.Config.DebugMode then
  begin
    if FileExists('log.txt') then
    begin
      filemode := fmOpenReadWrite;
    end
    else
    begin
      filemode := fmCreate;
    end;
    fs := TFileStream.Create('log.txt', filemode);
    try
      fs.Seek(0, soFromEnd);
      date := FormatDateTime(TimeFormat, now);
      fs.Write((@date[1])^, length(date));
      fs.Write((@InStr[1])^, length(InStr));
      fs.Write(CRLF, length(CRLF));
    finally
      FreeAndNil(fs);
    end;
  end;
end;


function IsNumeric(const InStr: string): boolean;
var
  I: integer;
begin
  Result := True;
  for I := 1 to length(InStr) do
  begin
    if not CharInSet(InStr[I], ['1' .. '9', '0']) then
    begin
      Result := False;
      break;
    end;
  end;
end;

function PropertyExists(Component: TComponent; const PropName: string): boolean;
var
  PropInfo: PPropInfo;
  TK: TTypeKind;
begin
  Result := False;
  PropInfo := GetPropInfo(Component.ClassInfo, PropName);
  if PropInfo <> nil then
  begin
    TK := PropInfo^.PropType^.Kind;
    if (TK = tkString) or (TK = tkLString) or (TK = tkWString) then
    begin
      Result := True;
    end;
  end;
end;

// converts a string, potentially containing newline
// characters into a flatened string
function StringEncode(const InStr: string): string;
var
  I: integer;
begin
  Result := InStr;
  for I := 1 to length(Result) do
  begin
    if (Result[I] = #13) and (Result[I + 1] = #10) then
    begin
      Result[I] := '\';
      Result[I + 1] := 'n';
    end;
  end;
end;

// converts a flat string into its original form
function StringDecode(const InStr: string): string;
var
  I: integer;
begin
  Result := InStr;
  for I := 1 to length(Result) do
  begin
    if (Result[I] = '\') and (Result[I + 1] = 'n') then
    begin
      Result[I] := #13;
      Result[I + 1] := #10;
    end;
  end;
end;

function AnsiStrInByteArrayPos(aStr: AnsiString; aBytes: PByte;
  aByteLen: Integer): integer;
var
  i: Integer;
  aStrLen: Integer;
  j: Integer;
  bIn: Boolean;
begin
  Result := 0;

  aStrLen := Length(aStr);
  if aStrLen = 0 then Exit;

  for i := 0 to aByteLen - aStrLen do
  begin
    bIn := True;
    for j := 1 to aStrLen do
    begin
      if Pbyte(integer(aBytes) + i + j - 1)^ <> Byte(AnsiChar(aStr[j])) then
      begin
        bIn := False;
        Break;
      end;
    end;
    if bIn then
    begin
      Result := i + 1;
      Exit;
    end;
  end;


end;



{ *****************************************
  * This Function extracts the UPX Version *
  ***************************************** }
function GetUPXBuild(const FilePath: string): string;
const
  offsets: array [1 .. 7] of int64 = ($1F0, $3DB, $39D, $320, $281, $259, $261);
var
  fStream: TFileStream;
  aBuff: array [1 .. 4] of AnsiChar; // This will contain something like '1.20'  begin
  I: integer;
  upxVersion: single;
  aStr: string;
begin
  Result := '';

  if FileExists(FilePath) then
  begin
    try
      fStream := TFileStream.Create(FilePath, fmOpenRead);
      for I := Low(offsets) to High(offsets) do
      begin
        fStream.Position := 1;
        fStream.Seek(offsets[I], soFromBeginning);
        fStream.ReadBuffer(aBuff, $4);
        if CharInSet(AnsiChar(aBuff[1]), ['0'..'9', '.']) then
        begin
          aStr := string(AnsiString(aBuff));
          if (TryStrToFloat(aStr, upxVersion)) then
          begin
            Result := aStr;
            Break;
          end;
        end;
      end;
    finally
      FreeAndNil(fStream);
    end;
  end;

end;

end.
