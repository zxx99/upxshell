; 该脚本使用 HM VNISEdit 脚本编辑器向导产生

; 安装程序初始定义常量
!define PRODUCT_NAME "UPXShell"
!define PRODUCT_VERSION "1.01"
!define PRODUCT_BUILDVERSION "10.12.10.12"
!define PRODUCT_PUBLISHER "sandysoft"
!define PRODUCT_WEB_SITE "http://code.google.com/p/upxshell/"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\UpxShell.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"
!define PRODUCT_BRANDINGTEXT "sandy"

SetCompressor lzma

; ------ MUI 现代界面定义 (1.67 版本以上兼容) ------
!include "MUI.nsh"
!include "Sections.nsh"

; MUI 预定义常量
!define MUI_ABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\orange-install.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"
!define MUI_WELCOMEFINISHPAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Wizard\orange.bmp"

; 欢迎页面
!insertmacro MUI_PAGE_WELCOME
; 许可协议页面
;!define MUI_LICENSEPAGE_RADIOBUTTONS
;!insertmacro MUI_PAGE_LICENSE "${PATH_COMMONFILE}\License.txt"
; 组件选择页面
;!insertmacro MUI_PAGE_COMPONENTS
; 安装目录选择页面
!insertmacro MUI_PAGE_DIRECTORY
; 安装过程页面
!insertmacro MUI_PAGE_INSTFILES
; 安装完成页面
!define MUI_FINISHPAGE_RUN "$INSTDIR\UpxShell.exe"
!insertmacro MUI_PAGE_FINISH

; 安装卸载过程页面
!insertmacro MUI_UNPAGE_INSTFILES

; 安装界面包含的语言设置
!insertmacro MUI_LANGUAGE "SimpChinese"

; 安装预释放文件
!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS
; ------ MUI 现代界面定义结束 ------

Name "${PRODUCT_NAME} V${PRODUCT_VERSION}"
OutFile "${PRODUCT_NAME} V${PRODUCT_VERSION} Build${PRODUCT_BUILDVERSION}.exe"
InstallDir "$PROGRAMFILES\${PRODUCT_NAME}"
InstallDirRegKey HKLM "${PRODUCT_UNINST_KEY}" "UninstallString"
ShowInstDetails show
ShowUnInstDetails show
BrandingText "${PRODUCT_BRANDINGTEXT}"


var TmpInstallDir

Function .onInit
  ReadRegStr $TmpInstallDir HKLM "${PRODUCT_DIR_REGKEY}" "PATH"
  IfErrors +2 0
  StrCpy $INSTDIR $TmpInstallDir
FunctionEnd


Section "主程序" SEC01
  ;永远选择
  SectionIn RO

  SetOutPath "$INSTDIR"
  SetOverwrite ON

  File "..\bin\Whatsnew.txt"
  File "..\bin\UPXShell.exe"
  File "..\bin\UPXRes.dll"
  
  SetOutPath "$INSTDIR\Language"
	File "..\bin\Language\*.lng"

  CreateDirectory "$SMPROGRAMS\UPXShell"
  CreateShortCut "$SMPROGRAMS\UPXShell\UpxShell.lnk" "$INSTDIR\UPXShell.exe"
  CreateShortCut "$DESKTOP\UpxShell.lnk" "$INSTDIR\UPXShell.exe"

SectionEnd


Section -AdditionalIcons
  SetOutPath $INSTDIR
  WriteIniStr "$INSTDIR\web.url" "InternetShortcut" "URL" "${PRODUCT_WEB_SITE}"
  CreateShortCut "$SMPROGRAMS\UPXShell\web.lnk" "$INSTDIR\web.url"
  CreateShortCut "$SMPROGRAMS\UPXShell\Uninstall.lnk" "$INSTDIR\uninst.exe"
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\UpxShell.exe"
  ;覆盖安装时自动寻找目录
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "Path" "$INSTDIR"

  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\UpxShell.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
SectionEnd

/******************************
 *  以下是安装程序的卸载部分  *
 ******************************/

Section Uninstall
  Delete "$INSTDIR\Whatsnew.txt"
  Delete "$INSTDIR\UPXShell.exe"
  Delete "$INSTDIR\UPXRes.dll"
  Delete "$INSTDIR\web.url"
  Delete "$INSTDIR\uninst.exe"

  Delete "$DESKTOP\UpxShell.lnk"

  Delete "$SMPROGRAMS\UPXShell\UpxShell.lnk"
  Delete "$SMPROGRAMS\UPXShell\web.lnk"
  Delete "$SMPROGRAMS\UPXShell\Uninstall.lnk"
  RMDir "$SMPROGRAMS\UPXShell"
  Delete "$INSTDIR\Language\*.lng"
  RMDir "$INSTDIR\Language"

  RMDir "$INSTDIR"

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd

#-- 根据 NSIS 脚本编辑规则，所有 Function 区段必须放置在 Section 区段之后编写，以避免安装程序出现未可预知的问题。--#

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "你确实要完全移除 $(^Name) ，及其所有的组件？" IDYES +2
  Abort
FunctionEnd

Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) 已成功地从你的计算机移除。"
FunctionEnd
