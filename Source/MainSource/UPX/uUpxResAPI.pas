unit uUpxResAPI;

interface

uses
  Classes, SysUtils, Windows;


  //UPX Versions for the combobox etc..
type
  //UPXC means Custom.
  TUPXVersions = (
    UPX120, UPX124, UPX125,
    UPX190, UPX192, UPX193, UPX194, UPX195,
    UPX202, UPX203,
    UPX290,
    UPX300, UPX301, UPX302, UPX303, UPX304, UPX305, UPX306, UPX307,
    UPXC);

const
  resUPXVersions: array[TUPXVersions] of string =
    (
    'UPX120', 'UPX124', 'UPX125',
    'UPX190', 'UPX192', 'UPX193', 'UPX194', 'UPX195',
    'UPX202', 'UPX203',
    'UPX290',
    'upx300','upx301','upx302','upx303','upx304','upx305','upx306','upx307',
    'UPXC');
  aUPXVersions: array[TUPXVersions] of string =
    (
    'UPX1.20', 'UPX1.24', 'UPX1.25',
    'UPX1.90', 'UPX1.92', 'UPX1.93', 'UPX1.94', 'UPX1.95',
    'UPX2.02', 'UPX2.03',
    'UPX2.90',
    'UPX3.00','UPX3.01','UPX3.02','UPX3.03','UPX3.04','UPX3.05','UPX3.06','UPX3.07',
    'Custom');



procedure ExtractUPXApp(aResName, aUpxName: string);


implementation

var
  g_UpxResDllHandle: THandle = 0;

procedure ExtractUPXApp(aResName, aUpxName: string);
var
  Res: TResourceStream;
begin
  if g_UpxResDllHandle = 0 then
    g_UpxResDllHandle := LoadLibrary('upxres.dll');

  if g_UpxResDllHandle <> 0 then
  begin
    try
      Res := TResourceStream.Create(g_UpxResDllHandle, aResName, RT_RCDATA);
      Res.SavetoFile(aUpxName);
    finally
      FreeAndNil(Res);
    end;
  end
  else
  begin
    raise Exception.Create('Lose Upxres.dll');
  end;

end;

end.
