unit uUpxHandle;

interface

uses
  Classes, SysUtils;

type

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

  protected
    function GetUpxName: string; virtual;

    function GetCompressParam: string;
    function GetDeCompressParam: string;
  public
    procedure CompressFile;
    procedure DeCompressFile;

    property FileName: string read FFileName write SetFileName;

    property CompressLevel: Integer read FCompressLevel write SetCompressLevel;
    property CompressIconLevel: Integer read FCompressIconLevel write
      SetCompressIconLevel;
    property CustomParam: string read FCustomParam write SetCustomParam;

    property IsBackUpFile: Boolean read FIsBackUpFile write SetIsBackUpFile;
    property IsTestAfterCompression: Boolean read FIsTestAfterCompression write
      SetIsTestAfterCompression;
    property IsForce: Boolean read FIsForce write SetIsForce;
    property IsCompressRC: Boolean read FIsCompressRC write SetIsCompressRC;
    property IsStripRelocation: Boolean read FIsStripRelocation write
      SetIsStripRelocation;
    property IsCompressExport: Boolean read FIsCompressExport write
      SetIsCompressExport;
  end;

implementation

{ TUPXHandle }

procedure TUPXHandle.CompressFile;
begin

end;

procedure TUPXHandle.DeCompressFile;
begin

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
  Result := ExtractFilePath(ParamStr(0)) + GetUpxName + ' "' + FFileName + '"'
    + ' -d';
end;

function TUPXHandle.GetUpxName: string;
begin

  Result := 'upxe.exe';
end;

procedure TUPXHandle.SetCompressIconLevel(const Value: Integer);
begin
  FCompressIconLevel := Value;
end;

procedure TUPXHandle.SetCompressLevel(const Value: Integer);
begin
  FCompressLevel := Value;
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

end.
