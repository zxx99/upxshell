unit MainFrm;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, uUpxResAPI,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Gauges, Menus, uUpxHandle;

type
  TMainForm = class(TForm)
    btnAdvanced: TButton;
    btnGo: TButton;
    btnHelp: TButton;
    btnMultiPck: TButton;
    btnOpen: TButton;
    btnRun: TButton;
    bvlCompressor: TBevel;
    bvlOpenLeft: TBevel;
    bvlRatio: TBevel;
    chkAutoCompress: TCheckBox;
    chkBackup: TCheckBox;
    chkExitDone: TCheckBox;
    chkTest: TCheckBox;
    ClearHistory: TMenuItem;
    cmbLanguage: TComboBox;
    dlgOpen: TOpenDialog;
    imgGradient: TImage;
    imgHistory: TImage;
    imgLogoGrad1: TImage;
    imgMail: TImage;
    imgUPX: TImage;
    imgWWW: TImage;
    lblBetter: TLabel;
    lblBuild: TLabel;
    lblBuildCap: TLabel;
    lblCompression: TLabel;
    lblCompressLevel: TLabel;
    lblCompressor: TLabel;
    lblCSize: TLabel;
    lblCSizeCap: TLabel;
    lblEMail: TLabel;
    lblFaster: TLabel;
    lblFName: TLabel;
    lblFNameCap: TLabel;
    lblFSize: TLabel;
    lblFSizeCap: TLabel;
    lblHistory: TLabel;
    lblWebSite: TLabel;
    lblLanguage: TLabel;
    lblProgress: TLabel;
    lblProgressSize: TLabel;
    lblRatio: TLabel;
    lblRatioCap: TLabel;
    lblRelease: TLabel;
    lblReleaseCap: TLabel;
    lblUPX: TLabel;
    lblWWW: TLabel;
    mnuHistory: TPopupMenu;
    N1: TMenuItem;
    pgcMain: TPageControl;
    pnlAbout: TPanel;
    pnlAll: TPanel;
    pnlCompress: TPanel;
    pnlFileInfo: TPanel;
    pnlHelp: TPanel;
    pnlOpen: TPanel;
    pnlOpenLeft: TPanel;
    pnlOptions: TPanel;
    pnlProgress: TPanel;
    pnlProgressSize: TPanel;
    pnlTop: TPanel;
    prbCompress: TGauge;
    prbSize: TGauge;
    stbMain: TStatusBar;
    sttDecomp: TStaticText;
    tbsAbout: TTabSheet;
    tbsCompress: TTabSheet;
    tbsHelp: TTabSheet;
    tbsOpen: TTabSheet;
    tbsOptions: TTabSheet;
    trbCompressLvl: TTrackBar;
    pnlAction: TPanel;
    tbsUpdate: TTabSheet;
    pnlUpdate: TPanel;
    lblOnlineVersionCap: TLabel;
    lblDownloadCap: TLabel;
    lblDownload: TLabel;
    lblOnlineVersion: TLabel;
    btnChkUpdate: TButton;
    lblReleaseDateCap: TLabel;
    lblReleaseDate: TLabel;
    rchChangeLog: TRichEdit;
    btnLocalizerMode: TButton;
    pnlLocalization: TPanel;
    btnSaveAs: TButton;
    btnEditMessages: TButton;
    dlgSaveLanguageFile: TSaveDialog;
    cmbUPX: TComboBox;
    chkDecomp: TCheckBox;
    lblmail: TLabel;
    lblIns2: TLabel;
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnEditMessagesClick(Sender: TObject);
    procedure btnLocalizerModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearHistoryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOpenClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure tbsOpenShow(Sender: TObject);
    procedure tbsCompressShow(Sender: TObject);
    procedure tbsOptionsShow(Sender: TObject);
    procedure tbsAboutShow(Sender: TObject);
    procedure tbsHelpShow(Sender: TObject);
    procedure trbCompressLvlChange(Sender: TObject);
    procedure btnAdvancedClick(Sender: TObject);
    procedure cmbLanguageChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure stbMainMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure btnHelpClick(Sender: TObject);
    procedure btnMultiPckClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure imgHistoryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormShow(Sender: TObject);
    procedure HyperClick(Sender: TObject);
    procedure btnChkUpdateClick(Sender: TObject);
    procedure lblDownloadClick(Sender: TObject);
    procedure cmbUPXChange(Sender: TObject);
  private
    curUPXVersion: TUPXVersions;
    bStdUPXVersion: byte; // Contains the default UPXVersion selected, see TUPXVersions.
    ProductVersion, FileVersion: string;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure ShowWorkInfo(const aInfo: string);
    procedure CalcFileSize;

    procedure HistoryPopUp(Sender: TObject);
    procedure WMDropfiles(var Msg: Tmessage); message WM_DROPFILES;
    procedure ParseCommandLine;
    procedure OnUpxProgress(aProgress: integer; aFileSize: int64);
    procedure LoadFile(const FileName: string);
    procedure ReSetProgress;
    procedure FillUPXVersionsBox;

    function GetRatio: integer;
    procedure Work;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses SysUtils, ShlObj, Wininet, ShellAPI, Globals, Translator,
  MultiFrm, SetupFrm, LocalizerFrm, uUpdate;
{$R *.dfm}

{ ** This procedure loads application settings from the registry ** }
procedure TMainForm.LoadSettings;
begin
  bStdUPXVersion := 0;

  chkAutoCompress.Checked := ReadKey('AutoCompress', ktBoolean).Bool;
  chkExitDone.Checked := ReadKey('ExitDone', ktBoolean).Bool;
  chkBackup.Checked := ReadKey('CreateBackup', ktBoolean).Bool;
  chkTest.Checked := ReadKey('TestFile', ktBoolean).Bool;
  trbCompressLvl.Position := ReadKey('CompressionLevel', ktInteger).Int;

  WorkDir := ReadKey('InstallPath', ktString).Str;
  if WorkDir = '' then
  begin
    WorkDir := ExtractFilePath(ParamStr(0));
  end;

  LangFile := ReadKey('LanguageFile', ktString).Str;
  if LangFile = '' then
  begin
    LangFile := 'English';
  end;

  bStdUPXVersion := ReadKey('StdUPXVersion', ktInteger).Int;
  curUPXVersion := TUPXVersions(bStdUPXVersion);
end;

procedure TMainForm.FillUPXVersionsBox;
var
  I: TUPXVersions;
  ItemText: String;
begin
  cmbUPX.Clear;
  for I := Low(aUPXVersions) to High(aUPXVersions) do
  begin
    ItemText := aUPXVersions[I];
    if (I = UPXC) then
    begin
      if (FileExists(WorkDir + 'upx.exe')) then
      begin
        ItemText := TranslateMsg('Custom upx.exe');
      end
      else
      begin
        Continue;
      end;
    end;
    cmbUPX.Items.Add(ItemText);
  end;
  if (bStdUPXVersion <= cmbUPX.Items.Count - 1) then
  begin
    cmbUPX.ItemIndex := bStdUPXVersion;
  end
  else
  begin
    cmbUPX.ItemIndex := 1;
  end;
end;

{ ** This procedure saves the app settings to registry ** }
procedure TMainForm.SaveSettings;
var
  RegValue: TRegValue;
begin
  { ** The following code saves everything that should be saved of the MainForm ** }
  RegValue.Bool := chkAutoCompress.Checked;
  StoreKey('AutoCompress', RegValue, ktBoolean);

  RegValue.Bool := chkExitDone.Checked;
  StoreKey('ExitDone', RegValue, ktBoolean);

  RegValue.Bool := chkBackup.Checked;
  StoreKey('CreateBackup', RegValue, ktBoolean);

  RegValue.Bool := chkTest.Checked;
  StoreKey('TestFile', RegValue, ktBoolean);

  RegValue.Int := trbCompressLvl.Position;
  StoreKey('CompressionLevel', RegValue, ktInteger);

  RegValue.Str := cmbLanguage.Text;
  StoreKey('LanguageFile', RegValue, ktString);

  RegValue.Int := cmbUPX.ItemIndex;
  StoreKey('StdUPXVersion', RegValue, ktInteger);
end;

procedure TMainForm.ShowWorkInfo(const aInfo: string);
begin
  stbMain.Panels[1].Text := aInfo;
end;

{ ** This one loads a list of previously opened files and adds'em to History menu ** }
procedure LoadHistory;
var
  Strings: TStrings;
  I: integer;
  MenuItem: TMenuItem; // To add to the History menu
begin
  Strings := TStringList.Create;
  try
    Strings.CommaText := ReadKey('History', ktString).Str;
    // Load the file history
    for I := Strings.Count - 1 downto 0 do
    begin
      MenuItem := TMenuItem.Create(MainForm);
      MenuItem.Caption := Strings.Strings[I];
      MenuItem.OnClick := MainForm.HistoryPopUp;
      MainForm.mnuHistory.Items.Add(MenuItem);
    end;
  finally
    FreeAndNil(Strings);
  end;
end;

{ ** Adds an item to the History menu and stores it to registry ** }
procedure WriteHistory(const FileName: string);
var
  Strings: TStrings;
  Value: TRegValue;

  { This nested procedure adds a new menu item to the History menu }
  procedure AddNewMenuItem;
  var
    MenuItem: TMenuItem; // To add to the History menu
  begin
    MenuItem := TMenuItem.Create(MainForm);
    MenuItem.Caption := FileName;
    MenuItem.OnClick := MainForm.HistoryPopUp;
    MainForm.mnuHistory.Items.Add(MenuItem);
  end;

begin
  Strings := TStringList.Create;
  try
    Strings.CommaText := ReadKey('History', ktString).Str;
    // Load the file history
    if Strings.IndexOf(FileName) = -1 then // If item isn't already in the list
    begin
      Strings.Add(FileName);
      Value.Str := Strings.CommaText;
      StoreKey('History', Value, ktString);
      AddNewMenuItem;
    end;
  finally
    FreeAndNil(Strings);
  end;
end;

procedure TMainForm.LoadFile(const FileName: string);
// This function unsets the ReadOnly attribute of the file
  function SetFileAttrib: boolean;
  var
    Attrib: cardinal;
  begin
    Result := True;
    Attrib := GetFileAttributes(PChar(FileName));
    if (Attrib and FILE_ATTRIBUTE_READONLY) > 0 then
    begin
      if Application.MessageBox(PChar(TranslateMsg
            ('The file attribute is set to ReadOnly.' + #10#13 +
              'To proceed it must be unset. Continue?')),
        PChar(TranslateMsg('Confirmation')), MB_YESNO + MB_ICONQUESTION)
        = idYes then
      begin
        SetFileAttributes(PChar(FileName), Attrib - FILE_ATTRIBUTE_READONLY);
        Result := True;
      end
      else
      begin
        Result := False;
      end;
    end;
  end;

// This one resets all visual controls to default state
  procedure ResetVisuals;
  begin
    lblCSizeCap.Visible := False;
    lblCSize.Visible := False;
    bvlRatio.Visible := False;
    lblRatioCap.Visible := False;
    lblRatio.Visible := False;
    btnRun.Visible := False;
    pgcMain.ActivePageIndex := 1;
    prbCompress.Progress := 0;
    prbSize.Progress := 0;
    sttDecomp.Width := 0;
  end;

// Extracts file name out of a path
  procedure ExtractName(aVer: string);
  var
    Temp: string;
  begin
    Temp := SysUtils.ExtractFileName(FileName);
    lblFName.Caption := Temp;
    stbMain.Panels[0].Text := Temp;
    if aVer = '' then
      stbMain.Panels[0].Text := Temp
    else
      stbMain.Panels[0].Text := Temp +
        (' (maybe Compress with Upx V' + aVer + ')');
  end;

  procedure FindUpxItem(aVer: string);
  var
    aTmpStr: string;
    I: integer;
  begin
    aTmpStr := StringReplace(aVer, ',', '', [rfReplaceAll, rfIgnoreCase]);
    for I := 0 to cmbUPX.Items.Count - 1 do
    begin
      if Pos(aTmpStr, MainForm.cmbUPX.Items[I]) <> 0 then
      begin
        cmbUPX.ItemIndex := I;
        Break;
      end;
    end;
  end;

var
  aVer: string;
  aUpxHandle: TUPXHandle;
begin
  if (FileName <> '') and (FileExists(FileName)) and (SetFileAttrib) then
  begin
    GlobFileName := FileName; // Assign a global filename variable
    ResetVisuals; // Resets visual controls
    aUpxHandle := TUPXHandle.Create;
    try
      aUpxHandle.FileName := FileName;

      if aUpxHandle.IsFilePacked then
      begin
        // This one checks if the file is compressed and sets RadioButton
        chkDecomp.Checked := True;
        aVer := aUpxHandle.GetUPXBuild();
        FindUpxItem(aVer);
      end
      else
      begin
        chkDecomp.Checked := False;
        aVer := '';
        cmbUPX.ItemIndex := bStdUPXVersion;
      end;
      ExtractName(aVer);
      CalcFileSize;
      WriteHistory(FileName);
      if chkAutoCompress.Checked then
      begin
        Work;
      end;
    finally
      aUpxHandle.Free;
    end;


  end;
end;

procedure TrackBest;
begin
  if MainForm.trbCompressLvl.Position < 10 then
  begin
    MainForm.lblCompression.Caption := IntToStr
      (MainForm.trbCompressLvl.Position);
  end
  else
  begin
    MainForm.lblCompression.Caption := TranslateMsg('Best');
  end;
end;

{ ** Loads visual settings, gets upx version... ** }
procedure LoadVisualSettings;
var
  UPXOutStr: string;
  aUpxHandle: TUPXHandle;
begin
  // Checks if there is newer upx installed and sets it
  with MainForm do
  begin
    aUpxHandle := TUPXHandle.Create;
    try
      aUpxHandle.Filename := WorkDir + 'upx.exe';
      UPXOutStr := aUpxHandle.GetUPXBuild();
      if (UPXOutStr <> '') then
      begin
        lblIns2.Caption := 'UPX Ver && ' + UPXOutStr;
      end
      else
      begin
        lblIns2.Caption := '';
      end;

      // Checks UPX Shell release and buil numbers
      lblRelease.Caption := ProductVersion;
      lblBuild.Caption := FileVersion;
      Caption := 'UPX Shell V' + ProductVersion + ' Build' + FileVersion;
    finally
      aUpxHandle.Free;
    end;

  end;
end;

{ ** Scans workdir for .lng files and adds them to cmbLanguage ** }
procedure EnumerateLanguages;
// Kill the final .lng extension to show in the cmbLanguage
  function KillExt(const FullPath: string): string;
  begin
    Result := ExtractFileName(FullPath);
    Result := copy(Result, 1, Pos('.', Result) - 1);
  end;

var
  SRec: TSearchRec;
  LangF: string;
begin
  SetCurrentDir(WorkDir + LanguageSubdir);
  if FindFirst('*.lng', faAnyFile, SRec) = 0 then
  begin
    MainForm.cmbLanguage.Items.Add(KillExt(SRec.Name));
    while FindNext(SRec) = 0 do
    begin
      MainForm.cmbLanguage.Items.Add(KillExt(SRec.Name));
    end;
  end;
  FindClose(SRec);
  with MainForm.cmbLanguage do
  begin
    LangF := ReadKey('LanguageFile', ktString).Str;
    if LangF = '' then
    begin
      ItemIndex := Items.IndexOf('English');
      LangFile := 'English';
    end
    else
    begin
      ItemIndex := Items.IndexOf(LangF);
      LangFile := LangF;
    end;
  end;
  SetCurrentDir(WorkDir);
end;

procedure TMainForm.ParseCommandLine;
var
  I: integer;
begin
  for I := 1 to ParamCount() do
  begin
    // Check if the setup calls to (Un)Register the extensions.
    if ParamStr(I) = '--SETUP-REGEXT' then
    begin
      LoadLanguage(self);
      IntergrateContext([doSetup, extRegister]);
      Application.Terminate;
    end
    else if ParamStr(I) = '--SETUP-UNREGEXT' then
    begin
      LoadLanguage(self);
      IntergrateContext([doSetup, extUnRegister]);
      Application.Terminate;
    end;
    // Check to start in the debug mode.
    if ParamStr(I) = '--debug' then
    begin
      Globals.Config.DebugMode := True;
    end;
  end;
end;

procedure TMainForm.ReSetProgress;
begin
  prbSize.Progress := 0;
  prbCompress.Progress := 0;
  sttDecomp.Width := 0;
end;

{ ** This procedure handles the History Menu ** }
procedure TMainForm.HistoryPopUp(Sender: TObject);
begin
  LoadFile((Sender as TMenuItem).Caption);
end;

{ ** ** }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Set the Global DecimalSeparator so it will be used everywhere since we need it to be a '.'
  GetAppVersion(Application.ExeName, ProductVersion, FileVersion);

  pgcMain.ActivePageIndex := 0;

  DecimalSeparator := '.';
  DragAcceptFiles(MainForm.Handle, True);

  with Globals.Config do
  begin
    DebugMode := False;
    LocalizerMode := False;
  end;
  LoadSettings;
  LoadHistory;
  Application.HintHidePause := 10000;
  Application.HelpFile := WorkDir + 'UPXShell.chm';
  EnumerateLanguages;
  ParseCommandLine;

end;

{ ** Clears the History menu ** }
procedure TMainForm.CalcFileSize;
begin
  GlobFileSize := GetFileSize(GlobFileName);
  lblFSize.Caption := ProcessSize(GlobFileSize);
end;

procedure TMainForm.ClearHistoryClick(Sender: TObject);
var
  Value: TRegValue;
  I: integer;
begin
  Value.Str := '';
  StoreKey('History', Value, ktString);
  for I := mnuHistory.Items.Count - 1 downto 2 do
  begin
    mnuHistory.Items.Delete(I);
  end;
end;

{ ** ** }
procedure TMainForm.imgHistoryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  aPt: TPoint;
begin
  aPt.X := X;
  aPt.Y := Y;
  aPt := imgHistory.ClientToScreen(aPt);
  mnuHistory.Popup(aPt.X + 3, aPt.Y + 2);

end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
  RegValue: TRegValue;
begin
  dlgOpen.FilterIndex := Extension;
  with ReadKey('LastFolder', ktString) do
  begin
    if Str <> '' then
    begin
      dlgOpen.InitialDir := Str;
    end;
  end;
  if dlgOpen.Execute then
  begin
    Extension := dlgOpen.FilterIndex;
    RegValue.Str := ExtractFileDir(dlgOpen.FileName);
    StoreKey('LastFolder', RegValue, ktString);
    if dlgOpen.FileName <> '' then
    begin
      LoadFile(dlgOpen.FileName);
    end;
  end;
end;

procedure TMainForm.btnGoClick(Sender: TObject);
var
  CompDecomp: string;
begin
  if GlobFileName = '' then
  begin
    if chkDecomp.Checked then
    begin
      CompDecomp := TranslateMsg('decompress');
    end
    else
    begin
      CompDecomp := TranslateMsg('compress');
    end;
    Application.MessageBox(PChar(TranslateMsg('There is nothing to ')
          + CompDecomp), PChar(TranslateMsg('Error')), MB_OK + MB_ICONERROR);
  end
  else
  begin
    Work;
  end;
end;

procedure TMainForm.WMDropfiles(var Msg: Tmessage);
var
  hdrop: integer; // THandle
  Buffer: string;
  BufLength: integer;
begin
  hdrop := Msg.WParam;
  BufLength := DragQueryFile(hdrop, 0, nil, 300) + 1;
  setlength(Buffer, BufLength);
  DragQueryFile(hdrop, 0, PChar(Buffer), BufLength);
  DragFinish(hdrop);
  LoadFile(trim(Buffer));
end;

procedure TMainForm.Work;

  procedure SetCompressionVisuals(ControlEnabled: boolean);
  begin
    btnOpen.Enabled := ControlEnabled;
    btnGo.Enabled := ControlEnabled;
    imgHistory.Enabled := ControlEnabled;
    chkDecomp.Enabled := ControlEnabled;
    cmbUPX.Enabled := ControlEnabled;
    btnRun.Enabled := ControlEnabled;
    chkBackup.Enabled := ControlEnabled;
    chkAutoCompress.Enabled := ControlEnabled;
    chkExitDone.Enabled := ControlEnabled;
    chkTest.Enabled := ControlEnabled;
    btnAdvanced.Enabled := ControlEnabled;
    btnMultiPck.Enabled := ControlEnabled;
    trbCompressLvl.Enabled := ControlEnabled;

    if not ControlEnabled then
    begin
      sttDecomp.Width := 0;
    end;

  end;

  procedure TouchFile(const FileName: string);
  begin
    SHChangeNotify(SHCNE_UPDATEITEM, SHCNF_PATH, PChar(FileName), nil);
    SHChangeNotify(SHCNE_ATTRIBUTES, SHCNF_PATH,
      PChar(ExtractFileDir(FileName)), nil);
  end;

  procedure LoadUpxHandleParam(aUpxHandle: TUPXHandle);
  begin
    aUpxHandle.FileName := GlobFileName;

    aUpxHandle.CompressLevel := trbCompressLvl.Position;

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

    aUpxHandle.IsBackUpFile := chkBackup.Checked;
    aUpxHandle.IsForce := SetupForm.chkForce.Checked;
    aUpxHandle.IsCompressRC := SetupForm.chkResources.Checked;
    aUpxHandle.IsStripRelocation := SetupForm.chkRelocs.Checked;
    aUpxHandle.IsCompressExport := SetupForm.chkExports.Checked;
    aUpxHandle.IsDebugMode := Config.DebugMode;

    aUpxHandle.UpxVersion := TUPXVersions(cmbUPX.ItemIndex);

    aUpxHandle.OnUpxProgress := OnUpxProgress;

  end;


var
  StartTime: int64;
  OldCursor: TCursor;
  IsCompress: boolean;
  aUpxHandle: TUPXHandle;
  aRet: integer;
begin
  ReSetProgress;
  OldCursor := Screen.Cursor;
  try
    QueryTime(False, StartTime);
    DragAcceptFiles(Handle, False);
    SetCompressionVisuals(False);
    IsCompress := not chkDecomp.Checked;

    aUpxHandle := TUPXHandle.Create;
    try
      LoadUpxHandleParam(aUpxHandle);

      if IsCompress then
        aRet := aUpxHandle.CompressFile
      else
        aRet := aUpxHandle.DeCompressFile;

      case aRet of
        0: // Successfull
          begin
            if IsCompress then
            begin
              ShowWorkInfo(TranslateMsg('File successfully compressed'));
              GetRatio;
            end
            else
            begin
              ShowWorkInfo(TranslateMsg('File successfully decompressed'));

              sttDecomp.Width := Round((prbSize.Width / GetRatio) * 100) - 3;
            end;

            SetCompressionVisuals(True);
            if IsCompress and (SetupForm.chkScramble.Checked) then
            begin
              try
                Screen.Cursor := crHourGlass;
                aUpxHandle.ScramblerFile;
                ShowWorkInfo(MainForm.stbMain.Panels[1].Text + TranslateMsg
                    (' & scrambled'));
              finally
                Screen.Cursor := OldCursor;
              end;
            end;

            TouchFile(GlobFileName);

            if IsCompress and (chkTest.Checked) then
            begin
              case aUpxHandle.TestCompressedFile of
                0:
                  begin
                    ShowWorkInfo(stbMain.Panels[1].Text +
                      TranslateMsg(' & tested'));
                  end;
                1:
                  begin
                    ShowWorkInfo(stbMain.Panels[1].Text +
                      TranslateMsg(' & tested w/warnings'));
                  end;
                2:
                  begin
                    ShowWorkInfo(stbMain.Panels[1].Text +
                      TranslateMsg(' & test failed'));
                  end
                else
                begin
                  ShowWorkInfo(stbMain.Panels[1].Text +
                    TranslateMsg(' & tested w/warnings'));
                end;
              end;
            end;

            if (chkExitDone.Checked) then
            begin
              Close;
            end;

          end;
      else
        begin
          // 1: Errors encountered
          // 2: Warnings encountered
          if aRet = 1 then
          begin
            if IsCompress then
            begin
              ShowWorkInfo(TranslateMsg('Errors occured. File not compressed'));
            end
            else
            begin
              ShowWorkInfo(TranslateMsg('Errors occured. File not decompressed')
                );
            end;
          end
          else
          begin
            if IsCompress then
            begin
              ShowWorkInfo(TranslateMsg('File compressed with warnings'));
            end
            else
            begin
              ShowWorkInfo(TranslateMsg('File decompressed with warnings'));
            end;
          end;
          Application.MessageBox(PChar(TranslateMsg
                ('UPX returned following error:\n')
                + aUpxHandle.GetErrorText), PChar(TranslateMsg('Error')),
            MB_OK + MB_ICONERROR);
        end;

      end;

      ShowWorkInfo(stbMain.Panels[1].Text + TranslateMsg(' (in ') + QueryTime
          (True, StartTime) + TranslateMsg(' seconds)'));

    finally
      aUpxHandle.Free;
    end;

  finally
    DragAcceptFiles(Handle, True);
  end;
end;

{ ** ** }
procedure TMainForm.tbsOpenShow(Sender: TObject);
begin
  if btnOpen.Enabled then
  begin
    btnOpen.SetFocus;
  end;
end;

{ ** ** }
procedure TMainForm.tbsCompressShow(Sender: TObject);
begin
  if btnGo.Enabled then
  begin
    btnGo.SetFocus;
  end;
end;

{ ** ** }
procedure TMainForm.tbsOptionsShow(Sender: TObject);
begin
  if chkBackup.Enabled then
  begin
    chkBackup.SetFocus;
  end;
end;

{ ** ** }
procedure TMainForm.tbsAboutShow(Sender: TObject);
begin
  if pnlAbout.Enabled then
  begin
    pnlAbout.SetFocus;
  end;
end;

{ ** ** }
procedure TMainForm.tbsHelpShow(Sender: TObject);
begin
  if btnHelp.Enabled then
  begin
    btnHelp.SetFocus;
  end;
end;

{ ** ** }
procedure TMainForm.trbCompressLvlChange(Sender: TObject);
begin
  TrackBest;
end;

{ ** ** }
procedure TMainForm.btnAdvancedClick(Sender: TObject);
begin
  SetupForm.ShowModal;
end;

{ ** ** }
procedure TMainForm.cmbLanguageChange(Sender: TObject);
var
  RegValue: TRegValue;
begin
  RegValue.Str := trim(TranslateMsg('Compress with UPX'));
  StoreKey('OldContext', RegValue, ktString);
  LangFile := cmbLanguage.Text;
  LoadLanguage(self);
  TrackBest;
  if SetupForm.chkIntegrate.Checked then
  begin
    IntergrateContext([extRegister]);
  end;
  FillUPXVersionsBox;
end;

{ ** ** }
procedure TMainForm.FormActivate(Sender: TObject);
begin
  LoadLanguage(self);
  FillUPXVersionsBox;
  // Checks the position of CompressionLevel TrackBar
  TrackBest;
  LoadVisualSettings;
end;

{ ** ** }
procedure TMainForm.stbMainMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if (stbMain.Panels[0].Text <> '') and (stbMain.Panels[1].Text = '') then
  begin
    stbMain.Hint := stbMain.Panels[0].Text;
  end
  else
  begin
    if (stbMain.Panels[0].Text <> '') and (stbMain.Panels[1].Text <> '') then
    begin
      stbMain.Hint := stbMain.Panels[0].Text + #13#10 + stbMain.Panels[1].Text;
    end;
  end;
end;

{ ** ** }
procedure TMainForm.btnHelpClick(Sender: TObject);
begin
  ShellExecute(self.Handle, 'open', PChar(Application.HelpFile), nil, nil,
    SW_SHOWNORMAL);
end;

procedure TMainForm.btnMultiPckClick(Sender: TObject);
begin
  MultiForm := TMultiForm.Create(self);
  try
    MainForm.Hide;
    MultiForm.ShowModal;
    MainForm.Show;
  finally
    MultiForm.Release
  end;
end;

{ ** ** }
procedure TMainForm.btnRunClick(Sender: TObject);
begin
  ShellExecute(self.Handle, 'open', PChar(GlobFileName), nil, nil,
    SW_SHOWNORMAL);
end;

{ ** ** }
procedure TMainForm.HyperClick(Sender: TObject);
var
  S: string;
begin
  case (Sender as TLabel).Tag of
    1:
      begin
        S := 'http://code.google.com/p/upxshell';
      end;
    2:
      begin
        S := 'http://upx.sf.net';
      end;
    3:
      begin
        S := 'mailto:sandy<sandyisone@gmail.com>?Subject=UPX_Shell_' +
          ProductVersion;
      end;
  end;
  ShellExecute(0, 'open', PChar(S), nil, nil, SW_SHOWNORMAL);

end;

{ ** ** }
procedure TMainForm.FormShow(Sender: TObject);
begin
  if lowercase(ParamStr(1)) <> '' then
  begin
    LoadFile(ParamStr(1));
  end;
  OnShow := nil;
end;

function TMainForm.GetRatio: integer;
var
  Finalsz: integer;
  Size: integer;
begin
  prbCompress.Progress := 100;
  Finalsz := GetFileSize(GlobFileName);
  Size := Round((Finalsz / GlobFileSize) * 100);

  lblCSizeCap.Visible := True;
  lblCSize.Visible := True;
  lblCSize.Caption := ProcessSize(Finalsz);
  prbSize.Progress := Size;
  bvlRatio.Visible := True;
  lblRatioCap.Visible := True;
  lblRatio.Visible := True;
  lblRatio.Caption := IntToStr(Size) + ' %';
  if lblFSize.Width > lblCSize.Width then
  begin
    bvlRatio.Width := lblFSize.Width + 10;
    bvlRatio.Left := lblFSize.Left - 5;
  end
  else
  begin
    bvlRatio.Width := lblCSize.Width + 10;
    bvlRatio.Left := lblCSize.Left - 5;
  end;

  Result := Size;
end;


procedure TMainForm.btnChkUpdateClick(Sender: TObject);
var
  OldCursor: TCursor;
  aUpdate: TUpdate;
begin
  rchChangeLog.Lines.Clear;
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    aUpdate := TUpdate.Create;
    try
      aUpdate.CheckUpdateURL := 'http://upxshell.googlecode.com/files/update.txt';

      if aUpdate.GetUpdateInfo then
      begin
        try
          lblOnlineVersion.Caption := aUpdate.NewVersion;
          lblReleaseDate.Caption := aUpdate.UpdateDate;
          lblDownload.Caption := aUpdate.UpdateFileURL;
          lblDownload.Font.Color := clBlue;
          lblDownload.Enabled := True;
          if StrToCurr(aUpdate.NewVersion) > StrToCurr(ProductVersion) then
          begin
            rchChangeLog.Lines.Text := aUpdate.ChangeLog.Text;
          end
          else
          begin
            rchChangeLog.Lines.Add('There is no new update avalable.');
          end;
          lblDownload.Hint := lblDownload.Caption;
        except
          on e: Exception do
          begin
            rchChangeLog.Lines.Add('Error retereving updates!');
          end;
        end;

      end
      else
      begin
        rchChangeLog.Lines.Add('Error retereving updates!');
      end;
    finally
      aUpdate.Free;
    end;

  finally
    Screen.Cursor := OldCursor;
  end;
end;

{ ** ** }
procedure TMainForm.lblDownloadClick(Sender: TObject);
begin
  if lblDownload.Enabled then
  begin
    ShellExecute(0, 'open', PChar(lblDownload.Caption), nil, nil, SW_SHOW);
  end;
end;

procedure TMainForm.OnUpxProgress(aProgress: integer; aFileSize: int64);
begin
  prbSize.Progress := aFileSize;
  prbCompress.Progress := aProgress;
end;

procedure TMainForm.btnLocalizerModeClick(Sender: TObject);
begin
  // Toggle localization mode
  // In this mode every object capable of MouseUp event
  // detection get's a popup menu, which allows one to set
  // the object's caption and hint
  Globals.Config.LocalizerMode := not Globals.Config.LocalizerMode;
  pnlLocalization.Visible := Globals.Config.LocalizerMode;

  LocalizerMode(self, Globals.Config.LocalizerMode);
  LocalizerMode(SetupForm, Globals.Config.LocalizerMode);

  // we need to do some special processing in order to show the otherwise
  // inaccessible MultiForm
  if Globals.Config.LocalizerMode then
  begin
    MultiForm := TMultiForm.Create(Application);
    MultiForm.Show();
    MultiForm.Left := self.Left + self.Width + 20;
    LocalizerMode(MultiForm, Globals.Config.LocalizerMode);
  end
  else
  begin
    MultiForm.Release();
    SetupForm.Hide();
  end;
end;

procedure TMainForm.btnSaveAsClick(Sender: TObject);
begin
  dlgSaveLanguageFile.InitialDir := WorkDir + LanguageSubdir;
  if dlgSaveLanguageFile.Execute() then
  begin
    DumpLanguage(self, dlgSaveLanguageFile.FileName, fmCreate);
    DumpLanguage(SetupForm, dlgSaveLanguageFile.FileName, fmOpenReadWrite);
    DumpLanguage(MultiForm, dlgSaveLanguageFile.FileName, fmOpenReadWrite,
      True);
  end;
end;

procedure TMainForm.btnEditMessagesClick(Sender: TObject);
var
  Localizer: TLocalizerForm;
begin
  Localizer := TLocalizerForm.Create(nil);
  try
    Localizer.FMode := lfmMessages;
    Localizer.ShowModal();
  finally
    Localizer.Release();
  end;
end;

procedure TMainForm.cmbUPXChange(Sender: TObject);
begin
  if (TUPXVersions(cmbUPX.ItemIndex) = UPXC) then
  begin
    if FileExists(WorkDir + 'upx.exe') then
    begin
      // TODO Create a function which will only check for the external upx version.
      // TODO Also make it so that it will set the corrosponding curUPXVersion if it knows which version it is. This so it can have some of the extra features.
      LoadVisualSettings;
      bStdUPXVersion := cmbUPX.ItemIndex;
      curUPXVersion := TUPXVersions(bStdUPXVersion);
    end
    else
    begin
      curUPXVersion := TUPXVersions(bStdUPXVersion);
      cmbUPX.ItemIndex := bStdUPXVersion;
    end;
  end
  else
  begin
    bStdUPXVersion := cmbUPX.ItemIndex;
    curUPXVersion := TUPXVersions(bStdUPXVersion);
  end;
end;

end.
