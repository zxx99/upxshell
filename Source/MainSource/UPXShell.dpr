{*
    UPX Shell
    Copyright ?2000-2006, ION Tek

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*}

program UPXShell;
{
         UPDATED BY sandy 2010-2011
}

uses
  Forms,
  uUpxHandle in 'UPX\uUpxHandle.pas',
  uUpxResAPI in 'UPX\uUpxResAPI.pas',
  CommandsFrm in 'CommandsFrm.pas' {CommandsForm},
  Globals in 'Globals.pas',
  LocalizerFrm in 'LocalizerFrm.pas' {LocalizerForm},
  MainFrm in 'MainFrm.pas' {MainForm},
  MultiFrm in 'MultiFrm.pas' {MultiForm},
  SetupFrm in 'SetupFrm.pas' {SetupForm},
  Translator in 'Translator.pas',
  uUpdate in 'uUpdate.pas';

{$R Resources\resources.res}
{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'UPX Shell';
  Application.HelpFile := '';
  Application.ShowMainForm := False;
	Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSetupForm, SetupForm);
  Application.CreateForm(TCommandsForm, CommandsForm);
  Application.CreateForm(TLocalizerForm, LocalizerForm);
  Application.CreateForm(TMultiForm, MultiForm);
  Application.ShowMainForm := True;
	Application.Run;
end.

