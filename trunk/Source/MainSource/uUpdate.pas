unit uUpdate;

interface

uses
  Classes, WinInet, SysUtils, Windows;

type
{
UPDATEFILE=UPXSHELL
version="1.00"
date="2010-12-14"
url="http://upxshell.googlecode.com/files/UPXShell%20V1.00%20Build10.12.04.15.exe"
ChangeLog="
 V1.00 Build10.12.04
  ===========================
  * Refator source.
  + add New Upx.
"
}

  TUpdate = class
  private
    FChangeLog: TStrings;
    FNewVersion: string;
    FCheckUpdateURL: string;
    FUpdateFileURL: string;
    FCurrVersion: string;
    FUpdateDate: string;
    procedure SetChangeLog(const Value: TStrings);
    procedure SetCheckUpdateURL(const Value: string);
    procedure SetCurrVersion(const Value: string);
    procedure SetNewVersion(const Value: string);
    procedure SetUpdateDate(const Value: string);
    procedure SetUpdateFileURL(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function GetUpdateInfo(): Boolean;

    property CheckUpdateURL: string read FCheckUpdateURL write SetCheckUpdateURL;
    property NewVersion: string read FNewVersion write SetNewVersion;
    property UpdateDate: string read FUpdateDate write SetUpdateDate;
    property UpdateFileURL: string read FUpdateFileURL write SetUpdateFileURL;
    property ChangeLog: TStrings read FChangeLog write SetChangeLog;

  end;

implementation

{ TUpdate }

constructor TUpdate.Create;
begin
  FChangeLog := TStringList.Create;
end;

destructor TUpdate.Destroy;
begin
  FChangeLog.Free;
  inherited;
end;

function TUpdate.GetUpdateInfo: Boolean;
// Inline function to get the update file
  function GetInetFile(const fileURL: string;
    strStream: TStringStream): boolean;
  const
    BufferSize = 1024;
  var
    hSession: HInternet;
    hURL: HInternet;
    Buffer: array [1 .. BufferSize] of byte;
    BufferLen: Cardinal;
    sAppName: string;
  begin
    sAppName := ExtractFileName(ParamStr(0));
    hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG,
      nil, nil, 0);
    try
      hURL := InternetOpenURL(hSession, PChar(fileURL), nil, 0, 0, 0);
      try
        repeat
        begin
          InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
          strStream.WriteBuffer(Buffer, BufferLen);
        end;
        until BufferLen = 0;
        Result := True;
      finally
        InternetCloseHandle(hURL)
      end;
    finally
      InternetCloseHandle(hSession)
    end;
  end;

// Main procedure
var
  sInetStream: TStringStream;
  sInetStrings: TStrings;
begin
  Result := False;

  sInetStream := TStringStream.Create('');
  sInetStrings := TStringList.Create;
  FChangeLog.Clear;

  try
    if GetInetFile(CheckUpdateURL, sInetStream) then
    begin
      sInetStrings.Clear;
      sInetStrings.Delimiter := '=';
      sInetStrings.QuoteChar := '"';
      sInetStrings.DelimitedText := sInetStream.DataString;

      if (sInetStrings[sInetStrings.IndexOf('UPDATEFILE') + 1] = 'UPXSHELL')
        then
      begin
        NewVersion := sInetStrings[sInetStrings.IndexOf('version') + 1];
        UpdateDate := sInetStrings[sInetStrings.IndexOf('date')+ 1];
        UpdateFileURL := sInetStrings[sInetStrings.IndexOf('url') + 1];
        FChangeLog.Add(sInetStrings[sInetStrings.IndexOf('changelog') + 1]);

        Result := True;
      end;
    end;
  finally
    FreeAndNil(sInetStream);
    FreeAndNil(sInetStrings);
  end;
end;

procedure TUpdate.SetChangeLog(const Value: TStrings);
begin
  FChangeLog := Value;
end;

procedure TUpdate.SetCheckUpdateURL(const Value: string);
begin
  FCheckUpdateURL := Value;
end;

procedure TUpdate.SetCurrVersion(const Value: string);
begin
  FCurrVersion := Value;
end;

procedure TUpdate.SetNewVersion(const Value: string);
begin
  FNewVersion := Value;
end;

procedure TUpdate.SetUpdateDate(const Value: string);
begin
  FUpdateDate := Value;
end;

procedure TUpdate.SetUpdateFileURL(const Value: string);
begin
  FUpdateFileURL := Value;
end;

end.
