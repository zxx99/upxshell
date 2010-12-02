unit Shared;

interface

uses
  Windows, Forms, Classes, Graphics, Dialogs, Globals;

function GetPriority: cardinal;

function IsAppPacked(aApp: string): boolean;

function GetCompressParams: string;

procedure ExtractUPX(const Action: TExtractDelete);

function ProcessSize(const Size: integer): string;
function GetFileSize(const FileName: string): integer;

function StringEncode(const InStr: string): string;
function StringDecode(const InStr: string): string;

// 获取字符串在byte数组中的位置，大小从1开始，aBytes[0]匹配返回值为1 !!!!!
function AnsiStrInByteArrayPos(aStr: AnsiString; aBytes: PByte;
  aByteLen: integer): integer;

function GetUPXBuild(const FilePath: string): string;

implementation

uses
  SysUtils, TypInfo, Translator,
  MainFrm, SetupFrm, uUpxResAPI;

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

// These are the proceudres to draw that gradient near UPX logo
type
  TCustomColorArray = array [0 .. 255] of TColor;

function CalculateColorTable(StartColor, EndColor: TColor;
  ColorCycles: integer): TCustomColorArray;
var
  BeginRGB: array [0 .. 2] of Byte;
  DiffRGB: array [0 .. 2] of integer;
  R, G, B, I: Byte;
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
  aByteLen: integer): integer;
var
  I: integer;
  aStrLen: integer;
  j: integer;
  bIn: boolean;
begin
  Result := 0;

  aStrLen := length(aStr);
  if aStrLen = 0 then
    Exit;

  for I := 0 to aByteLen - aStrLen do
  begin
    bIn := True;
    for j := 1 to aStrLen do
    begin
      if PByte(integer(aBytes) + I + j - 1)^ <> Byte(AnsiChar(aStr[j])) then
      begin
        bIn := False;
        break;
      end;
    end;
    if bIn then
    begin
      Result := I + 1;
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
        if CharInSet(AnsiChar(aBuff[1]), ['0' .. '9', '.']) then
        begin
          aStr := string(AnsiString(aBuff));
          if (TryStrToFloat(aStr, upxVersion)) then
          begin
            Result := aStr;
            break;
          end;
        end;
      end;
    finally
      FreeAndNil(fStream);
    end;
  end;

end;

end.
