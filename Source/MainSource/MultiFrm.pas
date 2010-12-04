unit MultiFrm;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, uUpxHandle,
  Dialogs, StdCtrls, ExtCtrls, Gauges, ComCtrls;

type
  TFilesRec = record
    Skip:           boolean;
    FullName:       string;
    FileName:       string;
    FilePath:       string;
    FileSize:       integer;
    CompressedSize: integer;
    CompressionResult: boolean;
  end;

type
  TMultiForm = class(TForm)
    grpSearch:      TGroupBox;
    btnBrowse:      TButton;
    chkRecurse:     TCheckBox;
    lblDir:         TLabel;
    cmbType:        TComboBox;
    lvFiles:        TListView;
    pnlBottom:      TPanel;
    lblCurrent:     TLabel;
    lblOverall:     TLabel;
    lblTotalCap:    TLabel;
    lblSelectedCap: TLabel;
    lblTimeCap:     TLabel;
    lblTotal:       TLabel;
    lblSelected:    TLabel;
    lblTime:        TLabel;
    btnPack:        TButton;
    btnExit:        TButton;
    btnScan:        TButton;
    pnlCurrent:     TPanel;
    pgbCurrent:     TGauge;
    sttRatio:       TStaticText;
    pnlOverall:     TPanel;
    pgbOverall:     TGauge;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnPackClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvFilesKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure lvFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private
    { Private declarations }
    Active:        boolean;
    FDirName:      string;
    FFileName:     string[5];
    //    FFileList: array [1..2] of TStringList;
    FFiles:        array of TFilesRec;
    procedure FindFiles(APath: string);
    function GetDirectoryName(const Dir: string): string;
    procedure FindFilesNR(APath: string);
    procedure PackFiles;
    procedure OnUpxProgress(aProgress: integer; aFileSize: int64);
  public
    { Public declarations }
  end;

var
  MultiForm: TMultiForm;

implementation

uses
  FileCtrl, TypInfo, SysUtils, Math, Translator, Globals,
  MainFrm, SetupFrm, uUpxResAPI;

var
  hStdOut: THandle;

{$R *.dfm}

{** This function formats the directory name so that it is a valid
  directory containing the back-slash (\) as the last character. **}
function TMultiForm.GetDirectoryName(const Dir: string): string;
begin
  if Dir[Length(Dir)] <> '\' then
  begin
    Result := Dir + '\';
  end
  else
  begin
    Result := Dir;
  end;
end;

{** This is a procedure which is called recursively so that it finds the
  file with a specified mask through the current directory and its
  sub-directories. **}
procedure TMultiForm.FindFiles(APath: string);
var
  FSearchRec, DSearchRec: TSearchRec;
  FindResult: integer;

  function IsDirNotation(const ADirName: string): boolean;
  begin
    Result := (ADirName = '.') or (ADirName = '..');
  end;

begin
  APath      := GetDirectoryName(APath); // Obtain a valid directory name
  { Find the first occurrence of the specified file name }
  FindResult := FindFirst(APath + FFileName, faAnyFile {+faHidden+
                          faSysFile+faReadOnly}, FSearchRec);
  try
    { Continue to search for the files according to the specified
      mask. If found, add the files and their paths to the listbox.}
    while FindResult = 0 do
    begin
      SetLength(FFiles, length(FFiles) + 1);
      with FFiles[high(FFiles)] do
      begin
        Skip           := False;
        FullName       := APath + FSearchRec.Name;
        FileName       := ExtractFileName(APath + FSearchRec.Name);
        FilePath       := APath;
        FileSize       := GetFileSize(FullName);
        CompressedSize := -1;
        CompressionResult := False;
      end;
      FindResult := FindNext(FSearchRec);
    end;
    { Now search the sub-directories of this current directory. Do this
      by using FindFirst to loop through each subdirectory, then call
      FindFiles (this function) again. This recursive process will
      continue until all sub-directories have been searched. }
    FindResult := FindFirst(APath + '*.*', faDirectory, DSearchRec);
    while FindResult = 0 do
    begin
      if ((DSearchRec.Attr and faDirectory) = faDirectory) and not
        IsDirNotation(DSearchRec.Name) then
      begin
        FindFiles(APath + DSearchRec.Name);
      end; // Recursion here
      FindResult := FindNext(DSearchRec);
    end;
  finally
    FindClose(FSearchRec);
  end;
end;

{** This one is used to search files without directory
  recursion **}
procedure TMultiForm.FindFilesNR(APath: string);
var
  SearchRec:  TSearchRec;
  FindResult: integer;
begin
  APath      := GetDirectoryName(APath);
  FindResult := FindFirst(APath + FFileName, faAnyFile {+faHidden+
                          faSysFile+faReadOnly}, SearchRec);
  while FindResult = 0 do
  begin
    SetLength(FFiles, length(FFiles) + 1);
    with FFiles[high(FFiles)] do
    begin
      Skip           := False;
      FullName       := APath + SearchRec.Name;
      FileName       := ExtractFileName(APath + SearchRec.Name);
      FilePath       := APath;
      FileSize       := GetFileSize(FullName);
      CompressedSize := -1;
      CompressionResult := False;
    end;
    FindResult := FindNext(SearchRec);
  end;
end;

{** **}
procedure TMultiForm.FormCreate(Sender: TObject);
begin
  cmbType.ItemIndex := 1;
  Active        := False;
end;


{** **}
procedure TMultiForm.btnBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory(TranslateMsg('Select directory to compress:'), '', Dir) then
  begin
    FDirName       := Dir;
    lblDir.Caption := FDirName;
  end;
end;

{** **}
procedure EnableButtons(Enable: boolean);
begin
  with MultiForm do
  begin
    Active          := Enable;
    btnBrowse.Enabled := Enable;
    cmbType.Enabled := Enable;
    chkRecurse.Enabled := Enable;
    btnScan.Enabled := Enable;
    btnPack.Enabled := Enable;
    btnExit.Enabled := Enable;
  end;
end;

{** **}
procedure FillView;
var
  lItem: TListItem;
  I:     integer;
begin
  with MultiForm do
  begin
    for I := low(FFiles) to high(FFiles) do
    begin
      lItem         := MultiForm.lvFiles.Items.Add;
      lItem.Caption := FFiles[I].FileName;
      lItem.SubItems.Add(FFiles[I].FilePath);
      lItem.SubItems.Add(ProcessSize(FFiles[I].FileSize));
      lItem.SubItems.Add('---');
      lItem.SubItems.Add('---');
    end;
  end;
end;

{** **}
procedure TMultiForm.btnScanClick(Sender: TObject);
begin
  if ((lblDir.Caption = TranslateMsg('N/A')) or (lblDir.Caption = '')) then
  begin
    ShowMessage(TranslateMsg('No directory selected'));
    Exit;
  end
  else
  begin
    Active := True;
    EnableButtons(False);
    lvFiles.Clear;
    SetLength(FFiles, 0);
    Screen.Cursor := crHourGlass;
    FFileName     := cmbType.Text;
    if chkRecurse.Checked then
    begin
      FindFiles(lblDir.Caption);
    end
    else
    begin
      FindFilesNR(lblDir.Caption);
    end;
    FillView;
    EnableButtons(True);
    Screen.Cursor := crDefault;
    Active        := False;
  end;
end;



{** **}
procedure UnSetReadOnly(const FileName: string);
var
  Attrib: cardinal;
begin
  Attrib := GetFileAttributes(PChar(FileName));
  if (Attrib and FILE_ATTRIBUTE_READONLY) > 0 then
  begin
    SetFileAttributes(PChar(FileName), Attrib - FILE_ATTRIBUTE_READONLY);
  end;
end;


{** **}
procedure TMultiForm.PackFiles;

  procedure SetPackedItem(Index: integer; Success: boolean = True);
  var
    lItem: TListItem;
  begin
    if Success then
    begin
      FFiles[Index].CompressionResult := True;
      FFiles[Index].CompressedSize := GetFileSize(FFiles[Index].FullName);
      lItem := lvFiles.FindCaption(0, FFiles[Index].FileName,
        False, True, True);
      if lItem <> nil then
      begin
        lItem.SubItems[2] := ProcessSize(FFiles[Index].CompressedSize);
        lItem.SubItems[3] := TranslateMsg('OK');
      end;
    end
    else
    begin
      FFiles[Index].CompressionResult := False;
      lItem := lvFiles.FindCaption(0, FFiles[Index].FileName,
        False, True, True);
      if lItem <> nil then
      begin
        lItem.SubItems[3] := TranslateMsg('Failed');
      end;
    end;
  end;


  procedure LoadUpxHandleParam(aUpxHandle: TUPXHandle);
  begin
    aUpxHandle.CompressLevel := MainForm.trbCompressLvl.Position;

    case SetupForm.cmbIcons.ItemIndex of
      0:
        aUpxHandle.CompressIconLevel := 2;
      1:
        aUpxHandle.CompressIconLevel := 1;
      2:
        aUpxHandle.CompressIconLevel := 0;
    end;

    aUpxHandle.CustomParam := SetupForm.txtCommands.Text;

    aUpxHandle.CompressPriority := NORMAL_PRIORITY_CLASS;
    case SetupForm.cmbPriority.ItemIndex of
      0:
        aUpxHandle.CompressPriority := IDLE_PRIORITY_CLASS;
      1:
        aUpxHandle.CompressPriority := NORMAL_PRIORITY_CLASS;
      2:
        aUpxHandle.CompressPriority := HIGH_PRIORITY_CLASS;
      3:
        aUpxHandle.CompressPriority := REALTIME_PRIORITY_CLASS;
    end;

    aUpxHandle.IsBackUpFile := MainForm.chkBackup.Checked;
    aUpxHandle.IsForce := SetupForm.chkForce.Checked;
    aUpxHandle.IsCompressRC := SetupForm.chkResources.Checked;
    aUpxHandle.IsStripRelocation := SetupForm.chkRelocs.Checked;
    aUpxHandle.IsCompressExport := SetupForm.chkExports.Checked;
    aUpxHandle.IsDebugMode := Config.DebugMode;

    aUpxHandle.UpxVersion := TUPXVersions(MainForm.cmbUPX.ItemIndex);

    aUpxHandle.OnUpxProgress := OnUpxProgress;
  end;

var
  I:         integer;
  C:         cardinal;
  CursorPos: TCoord;
  StartTime: int64;
  aUpxHandle: TUPXHandle;
begin
  CursorPos.X := 0;
  CursorPos.Y := 0;
  C           := 0;
  QueryTime(False, StartTime);

  aUpxHandle := TUPXHandle.Create();
  try
    LoadUpxHandleParam(aUpxHandle);

    for I := low(FFiles) to high(FFiles) do
    begin
      if not FFiles[I].Skip then
      begin
        aUpxHandle.FileName := FFiles[I].FullName;

        if aUpxHandle.CompressFile = 0 then
        begin
          SetPackedItem(I);
        end
        else
        begin
          SetPackedItem(I, False);
        end;
        MultiForm.pgbOverall.Progress := round((I + 1) / length(FFiles) * 100);
        MultiForm.sttRatio.Width      := 0;
        SetConsoleCursorPosition(hStdOut, CursorPos);
        FillConsoleOutputCharacter(hStdOut, #0, 1500, CursorPos, C);
      end;
    end;
  finally
    aUpxHandle.Free;
  end;





  MultiForm.pgbCurrent.Progress := 100;
  MultiForm.pgbOverall.Progress := 100;
  MultiForm.lblTimeCap.Visible  := True;
  MultiForm.lblTime.Visible     := True;
  MultiForm.lblTime.Caption     := QueryTime(True, StartTime);
end;

{** **}
procedure TMultiForm.btnPackClick(Sender: TObject);
begin
  if ((lblDir.Caption = TranslateMsg('N/A')) or (lblDir.Caption = '')) or
    (length(FFiles) < 1) then
  begin
    ShowMessage(TranslateMsg('No directory selected'));
    Exit;
  end;
  Active := True;
  EnableButtons(False);
  PackFiles;
  EnableButtons(True);
  Active := False;
end;

{** **}
procedure TMultiForm.FormActivate(Sender: TObject);
begin
  TranslateForm(MultiForm);
  if GlobFileName <> '' then
  begin
    lblDir.Caption := ExtractFileDir(GlobFileName);
  end;
  cmbType.ItemIndex          := 0;
  lvFiles.Columns[0].Caption := TranslateMsg('File Name');
  lvFiles.Columns[1].Caption := TranslateMsg('Folder');
  lvFiles.Columns[2].Caption := TranslateMsg('Size');
  lvFiles.Columns[3].Caption := TranslateMsg('Packed');
  lvFiles.Columns[4].Caption := TranslateMsg('Result');
end;

{** **}
procedure TMultiForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

end;

{** **}
procedure TMultiForm.lvFilesKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (not Active) and (lvFiles.Selected <> nil) then
  begin
    if lvFiles.Selected.SubItems[3] = TranslateMsg('Skip') then
    begin
      lvFiles.Selected.SubItems[3]        := '---';
      FFiles[lvFiles.Selected.Index].Skip := False;
    end
    else
    begin
      lvFiles.Selected.SubItems[3]        := TranslateMsg('Skip');
      FFiles[lvFiles.Selected.Index].Skip := True;
    end;
  end;
end;

{** **}
procedure TMultiForm.lvFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbRight) and (not Active) and (lvFiles.Selected <> nil) then
  begin
    if lvFiles.Selected.SubItems[3] = TranslateMsg('Skip') then
    begin
      lvFiles.Selected.SubItems[3]        := '---';
      FFiles[lvFiles.Selected.Index].Skip := False;
    end
    else
    begin
      lvFiles.Selected.SubItems[3]        := TranslateMsg('Skip');
      FFiles[lvFiles.Selected.Index].Skip := True;
    end;
  end;
end;

procedure TMultiForm.OnUpxProgress(aProgress: integer; aFileSize: int64);
begin
  pgbCurrent.Progress := aProgress;
end;

end.
