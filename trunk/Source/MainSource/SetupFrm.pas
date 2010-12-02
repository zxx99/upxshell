
unit SetupFrm;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TSetupForm = class(TForm)
    pnlBottom: TPanel;
    lblCommands: TLabel;
    chkCommands: TCheckBox;
    txtCommands: TEdit;
    btnOk: TButton;
    pnlTop: TPanel;
    chkScramble: TCheckBox;
    btnScramble: TButton;
    chkIntegrate: TCheckBox;
    pnlMiddle: TPanel;
    lblAdvacedOpts: TLabel;
    lblPriority: TLabel;
    lblIcons: TLabel;
    chkForce: TCheckBox;
    chkResources: TCheckBox;
    chkRelocs: TCheckBox;
    cmbPriority: TComboBox;
    cmbIcons: TComboBox;
    chkExports: TCheckBox;
    btnCommands: TButton;
    procedure FormCreate(Sender: TObject);
    procedure txtCommandsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chkIntegrateClick(Sender: TObject);
    procedure btnScrambleClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnCommandsClick(Sender: TObject);

  private
    { Private declarations }
    procedure LoadAdvSettings;
  public
    { Public declarations }
  end;

var
  SetupForm: TSetupForm;

implementation

uses
  SysUtils,
  Shared, Translator, Globals,
  MainFrm, CommandsFrm, uUpxHandle;
{$R *.dfm}

{ ** This procedure loads advanced application settings from the registry ** }
procedure TSetupForm.LoadAdvSettings;
begin
  // Now all the app settings will get loaded..
  chkScramble.Checked := ReadKey('Scramble', ktBoolean).Bool;
  chkIntegrate.Checked := ReadKey('ShellIntegrate', ktBoolean).Bool;
  chkResources.Checked := ReadKey('CompressResources', ktBoolean).Bool;
  chkExports.Checked := ReadKey('Compress exports', ktBoolean).Bool;
  chkRelocs.Checked := ReadKey('StripRelocs', ktBoolean).Bool;
  chkForce.Checked := ReadKey('ForceCompression', ktBoolean).Bool;
  txtCommands.Text := ReadKey('Commands', ktString).Str;
  chkCommands.Checked := ReadKey('SaveCommands', ktBoolean).Bool;

  if (ReadKey('Priority', ktInteger).Int <> -1) then
  begin
    cmbPriority.ItemIndex := ReadKey('Priority', ktInteger).Int;
  end
  else
  begin
    cmbPriority.ItemIndex := 1;
  end;

  if (ReadKey('Icons', ktInteger).Int <> -1) then
  begin
    cmbIcons.ItemIndex := ReadKey('Icons', ktInteger).Int;
  end
  else
  begin
    cmbIcons.ItemIndex := 2;
  end;

end;

{ ** ** }
procedure TSetupForm.FormCreate(Sender: TObject);
var
  Save: longint;
begin
  // Removes the header from the form
  if BorderStyle = bsNone then
  begin
    Exit;
  end;
  Save := GetWindowLong(Handle, GWL_STYLE);
  if (Save and WS_CAPTION) = WS_CAPTION then
  begin
    case BorderStyle of
      bsSingle, bsSizeable:
        begin
          SetWindowLong(Handle, GWL_STYLE,
            Save and (not WS_CAPTION) or WS_BORDER);
        end;
      bsDialog:
        begin
          SetWindowLong(Handle, GWL_STYLE,
            Save and (not WS_CAPTION) or DS_MODALFRAME or WS_DLGFRAME);
        end;
    end;
    Height := Height - GetSystemMetrics(SM_CYCAPTION);
    Refresh;
  end;
  // Loads settings
  LoadAdvSettings;
end;

{ ** ** }
procedure TSetupForm.txtCommandsChange(Sender: TObject);
begin
  if txtCommands.Text <> '' then
  begin
    chkCommands.Enabled := True;
  end
  else
  begin
    chkCommands.Enabled := False;
  end;
end;

{ ** ** }
procedure TSetupForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  RegValue: TRegValue;
begin
  // Now all the app settings will get loaded..
  RegValue.Bool := chkScramble.Checked;
  StoreKey('Scramble', RegValue, ktBoolean);

  RegValue.Bool := chkIntegrate.Checked;
  StoreKey('ShellIntegrate', RegValue, ktBoolean);

  RegValue.Bool := chkResources.Checked;
  StoreKey('CompressResources', RegValue, ktBoolean);

  RegValue.Bool := chkExports.Checked;
  StoreKey('Compress exports', RegValue, ktBoolean);

  RegValue.Bool := chkRelocs.Checked;
  StoreKey('StripRelocs', RegValue, ktBoolean);

  RegValue.Bool := chkForce.Checked;
  StoreKey('ForceCompression', RegValue, ktBoolean);

  RegValue.Int := cmbPriority.ItemIndex;
  StoreKey('Priority', RegValue, ktInteger);

  RegValue.Int := cmbIcons.ItemIndex;
  StoreKey('Icons', RegValue, ktInteger);


  RegValue.Bool := chkCommands.Checked;
  StoreKey('SaveCommands', RegValue, ktBoolean);

  if chkCommands.Checked then
  begin
    RegValue.Str := txtCommands.Text;
  end
  else
  begin
    RegValue.Str := '';
  end;
  StoreKey('Commands', RegValue, ktString);
end;

{ ** ** }
procedure TSetupForm.chkIntegrateClick(Sender: TObject);
begin
  if chkIntegrate.Checked then
  begin
    IntergrateContext([extRegister]);
  end
  else
  begin
    IntergrateContext([extUnRegister]);
  end;
end;

{ ** ** }
procedure TSetupForm.btnScrambleClick(Sender: TObject);
var
  aUpxHandle: TUPXHandle;
begin
  aUpxHandle := TUPXHandle.Create;
  try
    aUpxHandle.FileName := GlobFileName;
    if not aUpxHandle.IsFilePacked then
    begin
      if Application.MessageBox(PChar(TranslateMsg(
            'This file doesn''t seem to be packed. Run the Scrambler?')),
        PChar(TranslateMsg('Confirmation')), MB_YESNO + MB_ICONEXCLAMATION)
        <> idYes then
      begin
        Exit;
      end;
    end;
    aUpxHandle.ScramblerFile;

    Application.MessageBox(PChar(TranslateMsg(' & scrambled')),
      PChar(TranslateMsg('OK')));

  finally
    aUpxHandle.Free;
  end;
end;

{ ** ** }
procedure TSetupForm.FormActivate(Sender: TObject);
begin
  LoadLanguage(SetupForm);
  LoadAdvSettings;
end;


{ ** ** }
procedure TSetupForm.btnCommandsClick(Sender: TObject);
begin
  CommandsForm := TCommandsForm.Create(self);
  try
    CommandsForm.ShowModal;
  finally
    CommandsForm.Release
  end;
end;



end.
