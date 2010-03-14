/* Unicode-encapsulation of Win32 library functions.
   Copyright (C) 2000, 2001, 2002, 2004, 2010 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Authorship:

   Current primary author: Ben Wing <ben@xemacs.org>

   Created summer 2000 by Ben Wing.  Completed August 2001.  Completely
   written by Ben Wing.
   */

#define NEED_MSWINDOWS_COMMCTRL
#define NEED_MSWINDOWS_SHLOBJ

#include <config.h>
#include "lisp.h"

#include "console-msw.h"

int no_mswin_unicode_lib_calls;


/************************************************************************/
/*                              auto-generation                         */
/************************************************************************/

/* we use a simple script to control the auto-generation.

\(The following is copied from lib-src/make-mswin-unicode.pl.)

file specifies a file to start reading from.
yes indicates a function to be automatically Unicode-encapsulated.
   (All parameters either need no special processing or are LPTSTR or
   LPCTSTR.)
override indidates a function where the prototype can be overridden
   due to errors in Cygwin or Visual Studio.
soon indicates a function that should be automatically Unicode-encapsulated,
   but we're not ready to process it yet.
no indicates a function we don't support (it will be #defined to cause
   a compile error, with the text after the function included in the
   erroneous definition to indicate why we don't support it).
review indicates a function that we still need to review to determine whether
   or how to support it.  This has the same effect as `no', with a comment
   indicating that the function needs review.
skip indicates a function we support manually; only a comment about this
   will be generated.
split indicates a function with a split structure (different versions
   for Unicode and ANSI), but where the only difference is in pointer
   types, and the actual size does not differ.  The structure name
   should follow the function name, and it will be automatically
   Unicode-encapsulated with appropriate casts.
begin-bracket indicates a #if statement to be inserted here.
end-bracket indicates the corresponding #endif statement.
blank lines and lines beginning with // are ignored.

The generated files go into intl-auto-encap-win32.[ch].

To regenerate, go to the nt/ subdirectory and type

nmake -f xemacs.mak unicode-encapsulate

This does the following:

	cd $(SRC)
	perl ../lib-src/make-mswin-unicode.pl --c-output intl-auto-encap-win32.c --h-output intl-auto-encap-win32.h intl-encap-win32.c

*/

/*

terminology used below:

"split-simple" means a structure where the A and W versions are the same
size, and the only differences are string pointer arguments. (This does NOT
include structures with a pointer to a split-sized structure within them.)
This can also refer to a function pointer whose only split arguments are
string pointers or split-simple structures.

"split-sized" means a structure where the A and W versions are different
sizes (typically because of an inline string argument), or where there's a
pointer to another split-sized structure.

"split-complex" 

begin-unicode-encapsulation-script

// dir c:\Program Files\Microsoft Visual Studio\VC98\Include\

file ACLAPI.h

yes GetNamedSecurityInfo
review BuildExplicitAccessWithName
review BuildSecurityDescriptor
review BuildTrusteeWithName
review BuildTrusteeWithObjectsAndName
review BuildTrusteeWithObjectsAndSid
review BuildTrusteeWithSid
review GetAuditedPermissionsFromAcl
review GetEffectiveRightsFromAcl
review GetExplicitEntriesFromAcl
review GetTrusteeForm
review GetTrusteeName
review GetTrusteeType
review LookupSecurityDescriptorParts
review SetEntriesInAcl
review SetNamedSecurityInfo
review BuildImpersonateExplicitAccessWithName
review BuildImpersonateTrustee
review GetMultipleTrustee
review GetMultipleTrusteeOperation

file WINBASE.H

yes GetBinaryType
yes GetShortPathName
no GetLongPathName Win98/2K+ only
skip GetEnvironmentStrings misnamed ANSI version of the function
yes FreeEnvironmentStrings
yes FormatMessage
yes CreateMailslot
begin-bracket !defined (CYGWIN_HEADERS)
no EncryptFile Win2K+ only
no DecryptFile Win2K+ only
end-bracket
no OpenRaw error "The procedure entry point OpenRawW could not be located in the dynamic link library ADVAPI32.dll."
no QueryRecoveryAgents split-sized LPRECOVERY_AGENT_INFORMATION
yes lstrcmp
yes lstrcmpi
yes lstrcpyn
yes lstrcpy
yes lstrcat
yes lstrlen
yes CreateMutex
yes OpenMutex
yes CreateEvent
yes OpenEvent
yes CreateSemaphore
yes OpenSemaphore
yes CreateWaitableTimer
yes OpenWaitableTimer
yes CreateFileMapping
yes OpenFileMapping
yes GetLogicalDriveStrings
yes LoadLibrary
yes LoadLibraryEx
yes GetModuleFileName
yes GetModuleHandle
split CreateProcess LPSTARTUPINFO
yes FatalAppExit
split GetStartupInfo LPSTARTUPINFO
yes GetCommandLine
yes GetEnvironmentVariable
yes SetEnvironmentVariable
yes ExpandEnvironmentStrings
yes OutputDebugString
yes FindResource
yes FindResourceEx
skip EnumResourceTypes different prototypes in VC6 and VC7
skip EnumResourceNames different prototypes in VC6 and VC7
skip EnumResourceLanguages different prototypes in VC6 and VC7
yes BeginUpdateResource
yes UpdateResource
yes EndUpdateResource
yes GlobalAddAtom
yes GlobalFindAtom
yes GlobalGetAtomName
yes AddAtom
yes FindAtom
yes GetAtomName
yes GetProfileInt
yes GetProfileString
yes WriteProfileString
yes GetProfileSection
yes WriteProfileSection
yes GetPrivateProfileInt
yes GetPrivateProfileString
yes WritePrivateProfileString
yes GetPrivateProfileSection
yes WritePrivateProfileSection
yes GetPrivateProfileSectionNames
yes GetPrivateProfileStruct
yes WritePrivateProfileStruct
yes GetDriveType
yes GetSystemDirectory
yes GetTempPath
yes GetTempFileName
yes GetWindowsDirectory
yes SetCurrentDirectory
yes GetCurrentDirectory
yes GetDiskFreeSpace
yes GetDiskFreeSpaceEx
yes CreateDirectory
yes CreateDirectoryEx
yes RemoveDirectory
yes GetFullPathName
yes DefineDosDevice
yes QueryDosDevice
yes CreateFile
yes SetFileAttributes
yes GetFileAttributes
yes GetFileAttributesEx
yes GetCompressedFileSize
yes DeleteFile
no FindFirstFileEx split-sized LPWIN32_FIND_DATA; not used, NT 4.0+ only
skip FindFirstFile split-sized LPWIN32_FIND_DATA
skip FindNextFile split-sized LPWIN32_FIND_DATA
yes SearchPath
yes CopyFile
yes CopyFileEx NT 4.0+ only
yes MoveFile
yes MoveFileEx
no MoveFileWithProgress NT 5.0+ only
no CreateHardLink NT 5.0+ only
yes CreateNamedPipe
yes GetNamedPipeHandleState
yes CallNamedPipe
yes WaitNamedPipe
yes SetVolumeLabel
yes GetVolumeInformation
yes ClearEventLog
yes BackupEventLog
yes OpenEventLog
yes RegisterEventSource
yes OpenBackupEventLog
yes ReadEventLog
yes ReportEvent
yes AccessCheckAndAuditAlarm
no AccessCheckByTypeAndAuditAlarm NT 5.0+ only
no AccessCheckByTypeResultListAndAuditAlarm NT 5.0+ only
yes ObjectOpenAuditAlarm
yes ObjectPrivilegeAuditAlarm
yes ObjectCloseAuditAlarm
yes ObjectDeleteAuditAlarm
yes PrivilegedServiceAuditAlarm
yes SetFileSecurity
yes GetFileSecurity
yes FindFirstChangeNotification
no ReadDirectoryChanges Unicode-only
yes IsBadStringPtr
yes LookupAccountSid
yes LookupAccountName
yes LookupPrivilegeValue
yes LookupPrivilegeName
yes LookupPrivilegeDisplayName
yes BuildCommDCB
yes BuildCommDCBAndTimeouts
yes CommConfigDialog
yes GetDefaultCommConfig
yes SetDefaultCommConfig
yes GetComputerName
yes SetComputerName
yes GetUserName
yes LogonUser
split CreateProcessAsUser LPSTARTUPINFO
no GetCurrentHwProfile split-sized LPHW_PROFILE_INFO; NT 4.0+ only
no GetVersionEx split-sized LPOSVERSIONINFO
no CreateJobObject NT 5.0+ only
no OpenJobObject NT 5.0+ only
review CheckNameLegalDOS8Dot3
review CreateActCtx
review CreateProcessWithLogon
review DeleteVolumeMountPoint
review DnsHostnameToComputerName
review FileEncryptionStatus
review FindActCtxSectionString
review FindFirstVolume
review FindFirstVolumeMountPoint
review FindNextVolume
review FindNextVolumeMountPoint
review GetFirmwareEnvironmentVariable
review GetComputerNameEx
review GetDllDirectory
review GetModuleHandleEx
review GetSystemWindowsDirectory
review GetSystemWow64Directory
review GetVolumeNameForVolumeMountPoint
review GetVolumePathName
review GetVolumePathNamesForVolumeName
review QueryActCtx
review ReplaceFile
review SetComputerNameEx
review SetDllDirectory
review SetFileShortName
review SetFirmwareEnvironmentVariable
review SetVolumeMountPoint
review VerifyVersionInfo

file WINUSER.H

skip MAKEINTRESOURCE macro
yes wvsprintf
no wsprintf varargs
yes LoadKeyboardLayout
yes GetKeyboardLayoutName
no CreateDesktop split-sized LPDEVMODE
yes OpenDesktop
split EnumDesktops DESKTOPENUMPROC // callback fun differs only in string pointer type
override HWINSTA CreateWindowStationW(LPWSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES); error arg 1, VS6 prototype, missing const
yes OpenWindowStation
split EnumWindowStations WINSTAENUMPROC // callback fun differs only in string pointer type
yes GetUserObjectInformation
yes SetUserObjectInformation
yes RegisterWindowMessage
yes GetMessage
yes DispatchMessage
yes PeekMessage
skip SendMessage split messages and structures
no SendMessageTimeout VS6 has erroneous seventh parameter DWORD_PTR instead of PDWORD_PTR
yes SendNotifyMessage
yes SendMessageCallback
no BroadcastSystemMessage win95 version not split; NT 4.0+ only
no RegisterDeviceNotification NT 5.0+ only
yes PostMessage
yes PostThreadMessage
no PostAppMessage macro
skip DefWindowProc return value is conditionalized on _MAC, messes up parser
no CallWindowProc two versions, STRICT and non-STRICT
skip RegisterClass need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASS
skip UnregisterClass need to intercept for reasons related to RegisterClass
split GetClassInfo LPWNDCLASS
skip RegisterClassEx need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASSEX; NT 4.0+ only
split GetClassInfoEx LPWNDCLASSEX NT 4.0+ only
yes CreateWindowEx
skip CreateWindow macro
yes CreateDialogParam
split CreateDialogIndirectParam LPCDLGTEMPLATE error in Cygwin prototype (no split) but fixable with typedef
no CreateDialog macro
no CreateDialogIndirect macro w/split LPCDLGTEMPLATE
yes DialogBoxParam
split DialogBoxIndirectParam LPCDLGTEMPLATE error in Cygwin prototype (no split) but fixable with typedef
no DialogBox macro
no DialogBoxIndirect macro w/split LPCDLGTEMPLATE
yes SetDlgItemText
yes GetDlgItemText
yes SendDlgItemMessage
no DefDlgProc return value is conditionalized on _MAC, messes up parser
begin-bracket !defined (CYGWIN_HEADERS)
yes CallMsgFilter
end-bracket
yes RegisterClipboardFormat
yes GetClipboardFormatName
yes CharToOem
yes OemToChar
yes CharToOemBuff
yes OemToCharBuff
yes CharUpper
yes CharUpperBuff
yes CharLower
yes CharLowerBuff
yes CharNext
yes CharPrev
no IsCharAlpha split CHAR
no IsCharAlphaNumeric split CHAR
no IsCharUpper split CHAR
no IsCharLower split CHAR
yes GetKeyNameText
skip VkKeyScan split CHAR
no VkKeyScanEx split CHAR; NT 4.0+ only
yes MapVirtualKey
yes MapVirtualKeyEx NT 4.0+ only
yes LoadAccelerators
yes CreateAcceleratorTable
yes CopyAcceleratorTable
yes TranslateAccelerator
yes LoadMenu
split LoadMenuIndirect MENUTEMPLATE
yes ChangeMenu
yes GetMenuString
yes InsertMenu
yes AppendMenu
yes ModifyMenu
split InsertMenuItem LPCMENUITEMINFO NT 4.0+ only
split GetMenuItemInfo LPMENUITEMINFO NT 4.0+ only
split SetMenuItemInfo LPCMENUITEMINFO NT 4.0+ only
yes DrawText
yes DrawTextEx NT 4.0+ only
yes GrayString
yes DrawState NT 4.0+ only
yes TabbedTextOut
yes GetTabbedTextExtent
yes SetProp
yes GetProp
yes RemoveProp
split EnumPropsEx PROPENUMPROCEX // callback fun differs only in string pointer type
split EnumProps PROPENUMPROC // callback fun differs only in string pointer type
yes SetWindowText
yes GetWindowText
yes GetWindowTextLength
yes MessageBox
yes MessageBoxEx
// split MessageBoxIndirect LPMSGBOXPARAMS NT 4.0+ only
no MessageBoxIndirect Cygwin has split MSGBOXPARAMS* instead of LPMSGBOXPARAMS
yes GetWindowLong
yes SetWindowLong
yes GetClassLong
yes SetClassLong
yes FindWindow
yes FindWindowEx NT 4.0+ only
yes GetClassName
no SetWindowsHook obsolete; two versions, STRICT and non-STRICT
yes SetWindowsHookEx
yes LoadBitmap
yes LoadCursor
yes LoadCursorFromFile
yes LoadIcon
yes LoadImage NT 4.0+ only
yes LoadString
yes IsDialogMessage
yes DlgDirList
yes DlgDirSelectEx
yes DlgDirListComboBox
yes DlgDirSelectComboBoxEx
yes DefFrameProc
no DefMDIChildProc return value is conditionalized on _MAC, messes up parser
override HWND CreateMDIWindowW(LPWSTR,LPWSTR,DWORD,int,int,int,int,HWND,HINSTANCE,LPARAM); error arg 1, VS6 prototype, missing const
yes WinHelp
no ChangeDisplaySettings split-sized LPDEVMODE
no ChangeDisplaySettingsEx split-sized LPDEVMODE; NT 5.0/Win98+ only
no EnumDisplaySettings split-sized LPDEVMODE
no EnumDisplayDevices split-sized PDISPLAY_DEVICE; NT 5.0+ only, no Win98
yes SystemParametersInfo probs w/ICONMETRICS, NONCLIENTMETRICS
no GetMonitorInfo NT 5.0/Win98+ only
no GetWindowModuleFileName NT 5.0+ only
no RealGetWindowClass NT 5.0+ only
no GetAltTabInfo NT 5.0+ only
review BroadcastSystemMessageEx
review EnumDisplaySettingsEx
review GetClassLongPtr
review GetRawInputDeviceInfo
review GetWindowLongPtr
review SetClassLongPtr
review SetWindowLongPtr

file WINGDI.H

begin-bracket defined (HAVE_MS_WINDOWS)
// split-sized LOGCOLORSPACE
// split-sized TEXTMETRIC
// split-sized NEWTEXTMETRIC
// split-sized NEWTEXTMETRICEX
// split-sized LOGFONT
// split-sized ENUMLOGFONT
// split-sized ENUMLOGFONTEX
// split-sized EXTLOGFONT, used in EMREXTCREATEFONTINDIRECTW (Unicode-only) and (???) in DEVINFO (DDK structure)
// split-sized DEVMODE
// split-sized DISPLAY_DEVICE, used in EnumDisplayDevices
// split-sized OUTLINETEXTMETRIC
// split-simple POLYTEXT
// split-simple GCP_RESULTS
// split-sized function pointer OLDFONTENUMPROC, same as FONTENUMPROC
// split-sized function pointer FONTENUMPROC
yes AddFontResource
yes CopyMetaFile
skip CreateDC split-sized DEVMODE
skip CreateFontIndirect split-sized LOGFONT
yes CreateFont
skip CreateIC split-sized DEVMODE
yes CreateMetaFile
yes CreateScalableFontResource
skip DeviceCapabilities split-sized DEVMODE
skip EnumFontFamiliesEx split-complex FONTENUMPROC; NT 4.0+ only
no EnumFontFamilies split-complex FONTENUMPROC
no EnumFonts split-complex FONTENUMPROC
yes GetCharWidth
yes GetCharWidth32
yes GetCharWidthFloat
yes GetCharABCWidths
yes GetCharABCWidthsFloat
yes GetGlyphOutline
yes GetMetaFile
no GetOutlineTextMetrics split-sized LPOUTLINETEXTMETRIC
yes GetTextExtentPoint
yes GetTextExtentPoint32
yes GetTextExtentExPoint
split GetCharacterPlacement LPGCP_RESULTS NT 4.0+ only
no GetGlyphIndices NT 5.0+ only
no AddFontResourceEx NT 5.0+ only
no RemoveFontResourceEx NT 5.0+ only
// split-sized AXISINFO, used in AXESLIST; NT 5.0+ only
// split-sized AXESLIST, used in ENUMLOGFONTEXDV; NT 5.0+ only
// split-sized ENUMLOGFONTEXDV; NT 5.0+ only
no CreateFontIndirectEx split-sized ENUMLOGFONTEXDV; NT 5.0+ only
// split-sized ENUMTEXTMETRIC, returned in EnumFontFamExProc, on NT 5.0+; NT 5.0+ only
skip ResetDC split-sized DEVMODE
yes RemoveFontResource
yes CopyEnhMetaFile
yes CreateEnhMetaFile
yes GetEnhMetaFile
yes GetEnhMetaFileDescription
skip GetTextMetrics split-sized LPTEXTMETRIC
// split-simple DOCINFO
split StartDoc DOCINFO
skip GetObject split-sized LOGFONT
yes TextOut
yes ExtTextOut
split PolyTextOut POLYTEXT
yes GetTextFace
yes GetKerningPairs
// split-simple function pointer ICMENUMPROC
no GetLogColorSpace split-sized LPLOGCOLORSPACE; NT 4.0+ only
no CreateColorSpace split-sized LPLOGCOLORSPACE; NT 4.0+ only
yes GetICMProfile NT 4.0+ only, former error in Cygwin prototype but no more (Cygwin 1.7, 1-30-10)
yes SetICMProfile NT 4.0+ only
split EnumICMProfiles ICMENUMPROC NT 4.0+ only
skip UpdateICMRegKey NT 4.0+ only, error in Cygwin prototype
// non-split EMREXTTEXTOUT (A and W versions identical)
// non-split EMRPOLYTEXTOUT (A and W versions identical)
// Unicode-only EMREXTCREATEFONTINDIRECTW
no wglUseFontBitmaps causes link error
no wglUseFontOutlines causes link error
end-bracket

file WINSPOOL.H

begin-bracket defined (HAVE_MS_WINDOWS)
yes EnumPrinters #### problems with DEVMODE pointer in PRINTER_INFO_2
skip OpenPrinter split-sized DEVMODE pointer in split PRINTER_DEFAULTS
no ResetPrinter split-sized DEVMODE pointer in split PRINTER_DEFAULTS
no SetJob split-sized DEVMODE pointer in split JOB_INFO_2
no GetJob split-sized DEVMODE pointer in split JOB_INFO_2
no EnumJobs split-sized DEVMODE pointer in split JOB_INFO_2
no AddPrinter split-sized DEVMODE pointer in split PRINTER_INFO_2
no SetPrinter split-sized DEVMODE pointer in split PRINTER_INFO_2
no GetPrinter split-sized DEVMODE pointer in split PRINTER_INFO_2
// other than DocumentProperties below, we don't use any of the others,
// and they all pretty much have complicated interfaces with lots of
// split structures, etc.
no AddPrinterDriver not used, complicated interface with split structures
no AddPrinterDriverEx not used, complicated interface with split structures
no EnumPrinterDrivers not used, complicated interface with split structures
no GetPrinterDriver not used, complicated interface with split structures
no GetPrinterDriverDirectory not used, complicated interface with split structures
no DeletePrinterDriver not used, complicated interface with split structures
no DeletePrinterDriverEx not used, complicated interface with split structures
no AddPerMachineConnection not used, complicated interface with split structures
no DeletePerMachineConnection not used, complicated interface with split structures
no EnumPerMachineConnections not used, complicated interface with split structures
no AddPrintProcessor not used, complicated interface with split structures
no EnumPrintProcessors not used, complicated interface with split structures
no GetPrintProcessorDirectory not used, complicated interface with split structures
no EnumPrintProcessorDatatypes not used, complicated interface with split structures
no DeletePrintProcessor not used, complicated interface with split structures
no StartDocPrinter not used, complicated interface with split structures
no AddJob not used, complicated interface with split structures
skip DocumentProperties split-sized DEVMODE, error in Cygwin prototype
no AdvancedDocumentProperties not used, complicated interface with split structures
no GetPrinterData not used, complicated interface with split structures
no GetPrinterDataEx not used, complicated interface with split structures
no EnumPrinterData not used, complicated interface with split structures
no EnumPrinterDataEx not used, complicated interface with split structures
no EnumPrinterKey not used, complicated interface with split structures
no SetPrinterData not used, complicated interface with split structures
no SetPrinterDataEx not used, complicated interface with split structures
no DeletePrinterData not used, complicated interface with split structures
no DeletePrinterDataEx not used, complicated interface with split structures
no DeletePrinterKey not used, complicated interface with split structures
no PrinterMessageBox not used, complicated interface with split structures
no AddForm not used, complicated interface with split structures
no DeleteForm not used, complicated interface with split structures
no GetForm not used, complicated interface with split structures
no SetForm not used, complicated interface with split structures
no EnumForms not used, complicated interface with split structures
no EnumMonitors not used, complicated interface with split structures
no AddMonitor not used, complicated interface with split structures
no DeleteMonitor not used, complicated interface with split structures
no EnumPorts not used, complicated interface with split structures
no AddPort not used, complicated interface with split structures
no ConfigurePort not used, complicated interface with split structures
no DeletePort not used, complicated interface with split structures
no XcvData not used, complicated interface with split structures
no SetPort not used, complicated interface with split structures
no AddPrinterConnection not used, complicated interface with split structures
no DeletePrinterConnection not used, complicated interface with split structures
no AddPrintProvidor not used, complicated interface with split structures
no DeletePrintProvidor not used, complicated interface with split structures
no SetPrinterHTMLView not used, complicated interface with split structures
no GetPrinterHTMLView not used, complicated interface with split structures
review GetDefaultPrinter
end-bracket

file SHELLAPI.H

yes DragQueryFile
yes ShellExecute
yes FindExecutable
no CommandLineToArgv Unicode-only
yes ShellAbout
override HICON ExtractAssociatedIconW(HINSTANCE, LPWSTR, LPWORD); error arg2, Cygwin prototype, extra const
yes ExtractIcon
// split-simple DRAGINFO, used ??? (docs say "Not currently supported")
begin-bracket !defined (CYGWIN_HEADERS)
yes DoEnvironmentSubst NT 4.0+ only
end-bracket
no FindEnvironmentString causes link error; NT 4.0+ only
yes ExtractIconEx NT 4.0+ only, former error in Cygwin prototype but no more (Cygwin 1.7, 1-30-10)
// split-simple SHFILEOPSTRUCT, used in SHFileOperation
// split-simple SHNAMEMAPPING, used in SHFileOperation
split SHFileOperation LPSHFILEOPSTRUCT NT 4.0+ only
// split-simple SHELLEXECUTEINFO, used in ShellExecuteEx
split ShellExecuteEx LPSHELLEXECUTEINFO NT 4.0+ only
no WinExecError causes link error; NT 4.0+ only
begin-bracket !defined (CYGWIN_HEADERS)
yes SHQueryRecycleBin NT 4.0+ only
yes SHEmptyRecycleBin NT 4.0+ only
end-bracket
// split-sized NOTIFYICONDATA, used in Shell_NotifyIcon
no Shell_NotifyIcon split-sized NOTIFYICONDATA, NT 4.0+ only
// split-sized SHFILEINFO, used in SHGetFileInfo
skip SHGetFileInfo split-sized SHFILEINFO, NT 4.0+ only
no SHGetDiskFreeSpace causes link error; NT 4.0+ only
begin-bracket !defined (CYGWIN_HEADERS)
yes SHGetNewLinkInfo NT 4.0+ only
yes SHInvokePrinterCommand NT 4.0+ only
end-bracket

end-unicode-encapsulation-script

file COMMCTRL.H

yes ImageList_LoadImage
WC_HEADER
HDITEM
LPHDITEM
HDM_INSERTITEM
HDM_GETITEM
HDM_SETITEM
HDN_ITEMCHANGING
HDN_ITEMCHANGED
HDN_ITEMCLICK
HDN_ITEMDBLCLICK
HDN_DIVIDERDBLCLICK
HDN_BEGINTRACK
HDN_ENDTRACK
HDN_TRACK
HDN_GETDISPINFO
NMHEADER
LPNMHEADER
NMHDDISPINFO
LPNMHDDISPINFO
TOOLBARCLASSNAME
TBSAVEPARAMS
LPTBSAVEPARAMS
TB_GETBUTTONTEXT
TB_SAVERESTORE
TB_ADDSTRING
TBBUTTONINFO
LPTBBUTTONINFO
TB_GETBUTTONINFO
TB_SETBUTTONINFO
TB_INSERTBUTTON
TB_ADDBUTTONS
TBN_GETINFOTIP
NMTBGETINFOTIP
LPNMTBGETINFOTIP
TBN_GETDISPINFO
LPNMTBDISPINFO
TBN_GETBUTTONINFO
NMTOOLBAR
LPNMTOOLBAR
REBARCLASSNAME
REBARBANDINFO
LPREBARBANDINFO
LPCREBARBANDINFO
RB_INSERTBAND
RB_SETBANDINFO
RB_GETBANDINFO
TOOLTIPS_CLASS
TTTOOLINFO
PTOOLINFO
LPTTTOOLINFO
TTM_ADDTOOL
TTM_DELTOOL
TTM_NEWTOOLRECT
TTM_GETTOOLINFO
TTM_SETTOOLINFO
TTM_HITTEST
TTM_GETTEXT
TTM_UPDATETIPTEXT
TTM_ENUMTOOLS
TTM_GETCURRENTTOOL
TTHITTESTINFO
LPTTHITTESTINFO
TTN_GETDISPINFO
NMTTDISPINFO
LPNMTTDISPINFO
CreateStatusWindow
DrawStatusText
STATUSCLASSNAME
SB_GETTEXT
SB_SETTEXT
SB_GETTEXTLENGTH
SB_SETTIPTEXT
SB_GETTIPTEXT
TRACKBAR_CLASS
UPDOWN_CLASS
PROGRESS_CLASS
HOTKEY_CLASS
WC_LISTVIEW
LVITEM
LPLVITEM
LPSTR_TEXTCALLBACK
LVM_GETITEM
LVM_SETITEM
LVM_INSERTITEM
LVFINDINFO
LVM_FINDITEM
LVM_GETSTRINGWIDTH
LVM_EDITLABEL
LVCOLUMN
LPLVCOLUMN
LVM_GETCOLUMN
LVM_SETCOLUMN
LVM_GETITEMTEXT
LVM_SETITEMTEXT
LVM_GETISEARCHSTRING
LVBKIMAGE
LPLVBKIMAGE
LVM_SETBKIMAGE
LVM_GETBKIMAGE
LVN_ODFINDITEM
LVN_BEGINLABELEDIT
LVN_ENDLABELEDIT
LVN_GETDISPINFO
LVN_SETDISPINFO
NMLVDISPINFO
LVN_GETINFOTIP
NMLVGETINFOTIP
LPNMLVGETINFOTIP
WC_TREEVIEW
TVITEM
LPTVITEM
TVINSERTSTRUCT
LPTVINSERTSTRUCT
TVM_INSERTITEM
TVM_GETITEM
TVM_SETITEM
TVM_EDITLABEL
TVM_GETISEARCHSTRING
NMTREEVIEW
LPNMTREEVIEW
NMTVDISPINFO
LPNMTVDISPINFO
TVN_SELCHANGING
TVN_SELCHANGED
TVN_GETDISPINFO
TVN_SETDISPINFO
TVN_ITEMEXPANDING
TVN_ITEMEXPANDED
TVN_BEGINDRAG
TVN_BEGINRDRAG
TVN_DELETEITEM
TVN_BEGINLABELEDIT
TVN_ENDLABELEDIT
TVN_GETINFOTIP
NMTVGETINFOTIP
LPNMTVGETINFOTIP
WC_COMBOBOXEX
COMBOBOXEXITEM
PCOMBOBOXEXITEM
PCCOMBOBOXEXITEM
CBEM_INSERTITEM
CBEM_SETITEM
CBEM_GETITEM
NMCOMBOBOXEX
PNMCOMBOBOXEX
CBEN_GETDISPINFO
CBEN_DRAGBEGIN
CBEN_ENDEDIT
NMCBEDRAGBEGIN
LPNMCBEDRAGBEGIN
PNMCBEDRAGBEGIN
NMCBEENDEDIT
LPNMCBEENDEDIT
PNMCBEENDEDIT
WC_TABCONTROL
TCITEMHEADER
LPTCITEMHEADER
TCITEM
LPTCITEM
TCM_GETITEM
TCM_SETITEM
TCM_INSERTITEM
ANIMATE_CLASS
ACM_OPEN
MONTHCAL_CLASS
DATETIMEPICK_CLASS
DTM_SETFORMAT
DTN_USERSTRING
NMDATETIMESTRING
LPNMDATETIMESTRING
DTN_WMKEYDOWN
NMDATETIMEWMKEYDOWN
LPNMDATETIMEWMKEYDOWN
DTN_FORMAT
NMDATETIMEFORMAT
LPNMDATETIMEFORMAT
DTN_FORMATQUERY
NMDATETIMEFORMATQUERY
LPNMDATETIMEFORMATQUERY
WC_IPADDRESS
WC_PAGESCROLLER
WC_NATIVEFONTCTL

begin-unicode-encapsulation-script

file COMMDLG.H

begin-bracket defined (HAVE_MS_WINDOWS)
split GetOpenFileName LPOPENFILENAME
split GetSaveFileName LPOPENFILENAME
yes GetFileTitle
no CommDlg_OpenSave_GetSpec macro
no CommDlg_OpenSave_GetFilePath macro
no CommDlg_OpenSave_GetFolderPath macro
split ChooseColor LPCHOOSECOLOR
split FindText LPFINDREPLACE
split ReplaceText LPFINDREPLACE
no AfxReplaceText mac only
no ChooseFont split-sized LPLOGFONT in LPCHOOSEFONT
// LBSELCHSTRING
// SHAREVISTRING
// FILEOKSTRING
// COLOROKSTRING
// SETRGBSTRING
// HELPMSGSTRING
// FINDMSGSTRING
skip PrintDlg LPPRINTDLG with split-sized DEVMODE handle
skip PageSetupDlg LPPAGESETUPDLG with split-sized DEVMODE handle
review PrintDlgEx
end-bracket

file DDE.H

// nothing

file DDEML.H

yes DdeInitialize
yes DdeCreateStringHandle former error in Cygwin prototype, but no more (Cygwin 1.7, 1-30-10)
yes DdeQueryString
// #### split-sized (or split-simple??? not completely obvious) structure MONHSZSTRUCT, used when DDE event MF_HSZ_INFO is sent as part of the XTYP_MONITOR transaction sent to a DDE callback; not yet handled

file IMM.H

begin-bracket defined (HAVE_MS_WINDOWS)
yes ImmInstallIME
yes ImmGetDescription
yes ImmGetIMEFileName
yes ImmGetCompositionString
skip ImmSetCompositionString different prototypes in VC6 and VC7
yes ImmGetCandidateListCount
yes ImmGetCandidateList
yes ImmGetGuideLine
skip ImmGetCompositionFont split-sized LOGFONT
skip ImmSetCompositionFont split-sized LOGFONT
yes ImmConfigureIME // split-simple REGISTERWORD
yes ImmEscape // strings of various sorts
yes ImmGetConversionList
yes ImmIsUIMessage
yes ImmRegisterWord
yes ImmUnregisterWord
no ImmGetRegisterWordStyle split-sized STYLEBUF
split ImmEnumRegisterWord REGISTERWORDENUMPROC
no ImmGetImeMenuItems split-sized IMEMENUITEMINFO
end-bracket

file MMSYSTEM.H

yes sndPlaySound
yes PlaySound
no waveOutGetDevCaps split-sized LPWAVEOUTCAPS
yes waveOutGetErrorText
no waveInGetDevCaps split-sized LPWAVEINCAPS
yes waveInGetErrorText
no midiOutGetDevCaps split-sized LPMIDIOUTCAPS
yes midiOutGetErrorText
no midiInGetDevCaps split-sized LPMIDIOUTCAPS
yes midiInGetErrorText
no auxGetDevCaps split-sized LPAUXCAPS
no mixerGetDevCaps split-sized LPMIXERCAPS
no mixerGetLineInfo split-sized LPMIXERLINE
no mixerGetLineControls split-sized LPMIXERCONTROL
no mixerGetControlDetails split-sized LPMIXERCONTROL in LPMIXERLINECONTROLS in LPMIXERCONTROLDETAILS
no joyGetDevCaps split-sized LPJOYCAPS
yes mmioStringToFOURCC
yes mmioInstallIOProc
yes mmioOpen
yes mmioRename
yes mciSendCommand
yes mciSendString
yes mciGetDeviceID
begin-bracket !defined (MINGW)
no mciGetDeviceIDFromElementID missing from Win98se version of ADVAPI32.dll
end-bracket
yes mciGetErrorString

file WINNETWK.H

begin-bracket defined (HAVE_MS_WINDOWS)
yes WNetAddConnection
split WNetAddConnection2 LPNETRESOURCE
split WNetAddConnection3 LPNETRESOURCE
yes WNetCancelConnection
yes WNetCancelConnection2
yes WNetGetConnection
split WNetUseConnection LPNETRESOURCE
split WNetConnectionDialog1 LPCONNECTDLGSTRUCT contains split-simple LPNETRESOURCE
split WNetDisconnectDialog1 LPDISCDLGSTRUCT
split WNetOpenEnum LPNETRESOURCE
yes WNetEnumResource
yes WNetGetUniversalName
yes WNetGetUser
yes WNetGetProviderName
yes WNetGetNetworkInformation
// split-simple function pointer PFNGETPROFILEPATH
// split-simple function pointer PFNRECONCILEPROFILE
// split-simple function pointer PFNPROCESSPOLICIES
yes WNetGetLastError
split MultinetGetConnectionPerformance LPNETRESOURCE
review WNetSetConnection
review WNetGetResourceInformation
review WNetGetResourceParent
end-bracket

// file IME.H -- doesn't exist under Cygwin

// no SendIMEMessageEx obsolete, no docs available

file OBJBASE.H

// nothing

file SHLOBJ.H

// #### split code for IContextMenu not yet written
// split flag constant GCS_VERB of IContextMenu::GetCommandString
// split flag constant GCS_HELPTEXT of IContextMenu::GetCommandString
// split flag constant GCS_VALIDATE of IContextMenu::GetCommandString
// split string constant CMDSTR_NEWFOLDER of CMINVOKECOMMANDINFO.lpVerb or CMINVOKECOMMANDINFOEX.lpVerbW of IContextMenu::InvokeCommand
// split string constant CMDSTR_VIEWLIST of same
// split string constant CMDSTR_VIEWDETAILS of same
// #### split code for IExtractIcon, IShellLink, IShellExecuteHook, INewShortcutHook, ICopyHook, IFileViewer not yet written
// split interface IExtractIcon
// split interface IShellLink
// split interface IShellExecuteHook
// split interface INewShortcutHook
// split interface ICopyHook
// split interface IFileViewer
yes SHGetPathFromIDList
skip SHGetSpecialFolderPath error in Cygwin prototype, missing from Cygwin libraries
// split-simple structure BROWSEINFO used in SHBrowseForFolder
skip SHBrowseForFolder need to intercept callback for SendMessage
// split message BFFM_SETSTATUSTEXT handled in qxeSendMessage
// split message BFFM_SETSELECTION handled in qxeSendMessage
// split message BFFM_VALIDATEFAILED handled in qxeSHBrowseForFolder intercept proc
// #### code to handle split clipboard formats not yet written.  this will
// #### be tricky -- all functions that use such clipboard formats need to
// #### be split, and the data itself munged.  this may be too much effort,
// #### and we may just need to require that the app itself does the
// #### splitting.
// split clipboard format CFSTR_FILEDESCRIPTOR
// split clipboard format CFSTR_FILENAME
// split clipboard format CFSTR_FILENAMEMAP
// split-sized structure FILEDESCRIPTOR
// split-sized structure FILEGROUPDESCRIPTOR
// split flag SHCNF_PATH; we intercept SHChangeNotify
// split flag SHCNF_PRINTER; we intercept SHChangeNotify
// split flag SHARD_PATH; we intercept SHAddToRecentDocs
skip SHGetDataFromIDList split-sized WIN32_FIND_DATA or split-simple NETRESOURCE, missing from Cygwin libraries
review SHGetFolderPath
review SHGetIconOverlayIndex
review SHCreateDirectoryEx
review SHGetFolderPathAndSubDir

file WINNLS.H

no LOCALE_ENUMPROC not used, not examined yet
no CODEPAGE_ENUMPROC not used, not examined yet
no DATEFMT_ENUMPROC not used, not examined yet
no DATEFMT_ENUMPROCEX not used, not examined yet
no TIMEFMT_ENUMPROC not used, not examined yet
no CALINFO_ENUMPROC not used, not examined yet
no CALINFO_ENUMPROCEX not used, not examined yet
no GetCPInfoEx not used, not examined yet
no CompareString not used, not examined yet
no LCMapString not used, not examined yet
yes GetLocaleInfo
yes SetLocaleInfo
no GetTimeFormat not used, not examined yet
no GetDateFormat not used, not examined yet
no GetNumberFormat not used, not examined yet
no GetCurrencyFormat not used, not examined yet
no EnumCalendarInfo not used, not examined yet
no EnumCalendarInfoEx not used, not examined yet
no EnumTimeFormats not used, not examined yet
no EnumDateFormats not used, not examined yet
no EnumDateFormatsEx not used, not examined yet
no GetStringTypeEx not used, not examined yet
no GetStringType no such fun; A and W versions have different nos. of args
no FoldString not used, not examined yet
no EnumSystemLocales not used, not examined yet
no EnumSystemCodePages not used, not examined yet
review GetCalendarInfo
review GetGeoInfo
review SetCalendarInfo
review EnumSystemLanguageGroups
review EnumLanguageGroupLocales
review EnumUILanguages

end-unicode-encapsulation-script

file WINVER.H

VerFindFile
VerInstallFile
GetFileVersionInfoSize
GetFileVersionInfo
VerLanguageName
VerQueryValue

begin-unicode-encapsulation-script

file WINCON.H

yes PeekConsoleInput
yes ReadConsoleInput
yes WriteConsoleInput
yes ReadConsoleOutput
yes WriteConsoleOutput
yes ReadConsoleOutputCharacter
yes WriteConsoleOutputCharacter
no FillConsoleOutputCharacter split CHAR
yes ScrollConsoleScreenBuffer
yes GetConsoleTitle
yes SetConsoleTitle
yes ReadConsole
yes WriteConsole

file WINREG.H

yes RegConnectRegistry former error in Cygwin prototype, but no more (Cygwin 1.7, 1-30-10)
yes RegCreateKey
yes RegCreateKeyEx
yes RegDeleteKey
yes RegDeleteValue
yes RegEnumKey
yes RegEnumKeyEx
yes RegEnumValue
yes RegLoadKey
yes RegOpenKey
yes RegOpenKeyEx
yes RegQueryInfoKey
yes RegQueryValue
split RegQueryMultipleValues PVALENT
yes RegQueryValueEx
yes RegReplaceKey
yes RegRestoreKey
yes RegSaveKey
yes RegSetValue
yes RegSetValueEx
yes RegUnLoadKey
yes InitiateSystemShutdown
override BOOL AbortSystemShutdownW(LPWSTR); error arg 1, Cygwin prototype, extra const
review RegDeleteKeyEx

file EXCPT.H

// nothing

file STDARG.H

// nothing

file CDERR.H

// nothing

file WINPERF.H

// nothing

file RPC.H

// nothing

file NB30.H

// nothing

end-unicode-encapsulation-script

file WINSOCK2.H

SO_PROTOCOL_INFO
SERVICE_TYPE_VALUE_SAPID
SERVICE_TYPE_VALUE_TCPPORT
SERVICE_TYPE_VALUE_UDPPORT
SERVICE_TYPE_VALUE_OBJECTID
WSADuplicateSocket
LPFN_WSADUPLICATESOCKET
WSAEnumProtocols
LPFN_WSAENUMPROTOCOLS
WSASocket
LPFN_WSASOCKET
WSAAddressToString
LPFN_WSAADDRESSTOSTRING
WSAStringToAddress
LPFN_WSASTRINGTOADDRESS
WSALookupServiceBegin
LPFN_WSALOOKUPSERVICEBEGIN
WSALookupServiceNext
LPFN_WSALOOKUPSERVICENEXT
WSAInstallServiceClass
LPFN_WSAINSTALLSERVICECLASS
WSAGetServiceClassInfo
LPFN_WSAGETSERVICECLASSINFO
WSAEnumNameSpaceProviders
LPFN_WSAENUMNAMESPACEPROVIDERS
WSAGetServiceClassNameByClassId
LPFN_WSAGETSERVICECLASSNAMEBYCLASSID
WSASetService
LPFN_WSASETSERVICE

file WINCRYPT.H

MS_DEF_PROV_
MS_ENHANCED_PROV_
MS_DEF_RSA_SIG_PROV_
MS_DEF_RSA_SCHANNEL_PROV_
MS_ENHANCED_RSA_SCHANNEL_PROV_
MS_DEF_DSS_PROV_
MS_DEF_DSS_DH_PROV_
CryptAcquireContext
CryptSignHash
CryptVerifySignature
CryptSetProvider
CryptSetProviderEx
CryptGetDefaultProvider
CryptEnumProviderTypes
CryptEnumProviders
CERT_STORE_PROV_FILENAME_
CERT_STORE_PROV_SYSTEM_
sz_CERT_STORE_PROV_FILENAME_
sz_CERT_STORE_PROV_SYSTEM_
CERT_STORE_SAVE_TO_FILENAME_
CERT_FIND_SUBJECT_STR_
CERT_FIND_ISSUER_STR_
CertRDNValueToStr
CertNameToStr
CertStrToName
CertOpenSystemStore
CertAddEncodedCertificateToSystemStore

*/

/* the functions below are examples of hand-written Unicode-splitting
   code.  note that it needs to be written very carefully and with
   intimate knowledge of the structures involved, and can sometimes be
   very hairy (EnumFontFamiliesEx is the most extreme example).  it can
   be argued with some justification that this behind-the-scenes magic
   is confusing and potentially dangerous, and shouldn't be done.  but
   making the calling code deal with the results in extremely hard-to-
   read code and is very error-prone. */


/************************************************************************/
/*        would be encapsulatable but for parsing problems              */
/************************************************************************/

/* NOTE: return value is conditionalized on _MAC, messes up parser */
LRESULT
qxeDefWindowProc (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return DefWindowProcW (hWnd, Msg, wParam, lParam);
  else
    return DefWindowProcA (hWnd, Msg, wParam, lParam);
}


/* NOTE: two versions, STRICT and non-STRICT */
LRESULT
qxeCallWindowProc (WNDPROC lpPrevWndFunc, HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return CallWindowProcW (lpPrevWndFunc, hWnd, Msg, wParam, lParam);
  else
    return CallWindowProcA (lpPrevWndFunc, hWnd, Msg, wParam, lParam);
}

/* NOTE: return value is conditionalized on _MAC, messes up parser */
LRESULT
qxeDefDlgProc (HWND hDlg, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return DefDlgProcW (hDlg, Msg, wParam, lParam);
  else
    return DefDlgProcA (hDlg, Msg, wParam, lParam);
}

/* NOTE: return value is conditionalized on _MAC, messes up parser */
LRESULT
qxeDefMDIChildProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return DefMDIChildProcW (hWnd, uMsg, wParam, lParam);
  else
    return DefMDIChildProcA (hWnd, uMsg, wParam, lParam);
}

/* This one has two entry points called GetEnvironmentStringsW and
   GetEnvironmentStrings. (misnamed A version) */
Extbyte *
qxeGetEnvironmentStrings (void)
{
  if (XEUNICODE_P)
    return (Extbyte *) GetEnvironmentStringsW ();
  else
    return (Extbyte *) GetEnvironmentStrings ();
}


/************************************************************************/
/*           would be encapsulatable but for Cygwin problems            */
/************************************************************************/

#ifdef HAVE_MS_WINDOWS

/* NOTE: NT 4.0+ only */
BOOL
qxeUpdateICMRegKey (DWORD arg1, Extbyte * arg2, Extbyte * arg3, UINT arg4)
{
#ifdef CYGWIN_HEADERS
  /* Cygwin mistakenly declares the second argument as DWORD. */
  if (XEUNICODE_P)
    return UpdateICMRegKeyW (arg1, (DWORD) arg2, (LPWSTR) arg3, arg4);
  else
    return UpdateICMRegKeyA (arg1, (DWORD) arg2, (LPSTR) arg3, arg4);
#else
  if (XEUNICODE_P)
    return UpdateICMRegKeyW (arg1, (LPWSTR) arg2, (LPWSTR) arg3, arg4);
  else
    return UpdateICMRegKeyA (arg1, (LPSTR) arg2, (LPSTR) arg3, arg4);
#endif /* CYGWIN_HEADERS */
}

#endif /* HAVE_MS_WINDOWS */

#ifndef CYGWIN /* present in headers but missing in shell32.a */

BOOL
qxeSHGetSpecialFolderPath (HWND hwndOwner, Extbyte * lpszPath, int nFolder, BOOL fCreate)
{
#ifdef CYGWIN_HEADERS
  /* Cygwin mistakenly declares the second argument as LPSTR in both
     versions. */
  if (XEUNICODE_P)
    return SHGetSpecialFolderPathW (hwndOwner, (LPSTR) lpszPath, nFolder, fCreate);
  else
    return SHGetSpecialFolderPathA (hwndOwner, (LPSTR) lpszPath, nFolder, fCreate);
#else
  if (XEUNICODE_P)
    return SHGetSpecialFolderPathW (hwndOwner, (LPWSTR) lpszPath, nFolder, fCreate);
  else
    return SHGetSpecialFolderPathA (hwndOwner, (LPSTR) lpszPath, nFolder, fCreate);
#endif
}

#endif /* not CYGWIN */

/********************************************************************************/
/* would be encapsulatable but for header changes in different versions of VC++ */
/********************************************************************************/

#if MSC_VERSION >= 1300

BOOL
qxeEnumResourceTypes (HMODULE hModule, ENUMRESTYPEPROCW lpEnumFunc, LONG lParam)
{
  if (XEUNICODE_P)
    return EnumResourceTypesW (hModule, lpEnumFunc, lParam);
  else
    return EnumResourceTypesA (hModule, (ENUMRESTYPEPROCA) lpEnumFunc, lParam);
}

BOOL
qxeEnumResourceNames (HMODULE hModule, const Extbyte * lpType, ENUMRESNAMEPROCW lpEnumFunc, LONG lParam)
{
  if (XEUNICODE_P)
    return EnumResourceNamesW (hModule, (LPCWSTR) lpType, lpEnumFunc, lParam);
  else
    return EnumResourceNamesA (hModule, (LPCSTR) lpType, (ENUMRESNAMEPROCA) lpEnumFunc, lParam);
}

BOOL
qxeEnumResourceLanguages (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, ENUMRESLANGPROCW lpEnumFunc, LONG lParam)
{
  if (XEUNICODE_P)
    return EnumResourceLanguagesW (hModule, (LPCWSTR) lpType, (LPCWSTR) lpName, lpEnumFunc, lParam);
  else
    return EnumResourceLanguagesA (hModule, (LPCSTR) lpType, (LPCSTR) lpName, (ENUMRESLANGPROCA) lpEnumFunc, lParam);
}

#else

BOOL
qxeEnumResourceTypes (HMODULE hModule, ENUMRESTYPEPROC lpEnumFunc, LONG lParam)
{
  if (XEUNICODE_P)
    return EnumResourceTypesW (hModule, lpEnumFunc, lParam);
  else
    return EnumResourceTypesA (hModule, lpEnumFunc, lParam);
}

BOOL
qxeEnumResourceNames (HMODULE hModule, const Extbyte * lpType, ENUMRESNAMEPROC lpEnumFunc, LONG lParam)
{
  if (XEUNICODE_P)
    return EnumResourceNamesW (hModule, (LPCWSTR) lpType, lpEnumFunc, lParam);
  else
    return EnumResourceNamesA (hModule, (LPCSTR) lpType, lpEnumFunc, lParam);
}

BOOL
qxeEnumResourceLanguages (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, ENUMRESLANGPROC lpEnumFunc, LONG lParam)
{
  if (XEUNICODE_P)
    return EnumResourceLanguagesW (hModule, (LPCWSTR) lpType, (LPCWSTR) lpName, lpEnumFunc, lParam);
  else
    return EnumResourceLanguagesA (hModule, (LPCSTR) lpType, (LPCSTR) lpName, lpEnumFunc, lParam);
}

#endif /* MSC_VERSION >= 1300 */

/************************************************************************/
/*                                files                                 */
/************************************************************************/

static void
copy_win32_find_dataa_to_win32_find_dataw (const WIN32_FIND_DATAA *pa,
					   WIN32_FIND_DATAW *pw)
{
  /* the layout of WIN32_FIND_DATA is

     non-split fields;
     TCHAR cFileName[...];
     TCHAR cAlternateFileName[...];
     */

  xzero (*pw);
  memcpy (pw, pa, offsetof (WIN32_FIND_DATAA, cFileName));
  memcpy (pw->cFileName, pa->cFileName, sizeof (pa->cFileName));
  memcpy (pw->cAlternateFileName, pa->cAlternateFileName,
	  sizeof (pa->cAlternateFileName));
}

HANDLE
qxeFindFirstFile (const Extbyte *lpFileName,
		  WIN32_FIND_DATAW *lpFindFileData)
{
  if (XEUNICODE_P)
    return FindFirstFileW ((LPCWSTR) lpFileName, lpFindFileData);
  else
    {
      WIN32_FIND_DATAA ansidat;
      HANDLE retval;

      retval = FindFirstFileA ((LPCSTR) lpFileName, &ansidat);
      if (retval != INVALID_HANDLE_VALUE)
	copy_win32_find_dataa_to_win32_find_dataw (&ansidat, lpFindFileData);
      return retval;
    }
}

BOOL
qxeFindNextFile (HANDLE hFindFile, WIN32_FIND_DATAW *lpFindFileData)
{
  if (XEUNICODE_P)
    return FindNextFileW (hFindFile, lpFindFileData);
  else
    {
      WIN32_FIND_DATAA ansidat;
      BOOL retval;

      retval = FindNextFileA (hFindFile, &ansidat);
      if (retval)
	copy_win32_find_dataa_to_win32_find_dataw (&ansidat, lpFindFileData);
      return retval;
    }
}


/************************************************************************/
/*                                shell                                 */
/************************************************************************/

static void
copy_shfileinfoa_to_shfileinfow (const SHFILEINFOA *pa,
				 SHFILEINFOW *pw, UINT sz)
{
  /* the layout of SHFILEINFO is

     non-split fields;
     TCHAR szDisplayName[...];
     TCHAR szTypeName[...];
     */

  assert (sz >= sizeof (SHFILEINFOW));
  xzero (*pw);
  memcpy (pw, pa, offsetof (SHFILEINFOA, szDisplayName));
  memcpy (pw->szDisplayName, pa->szDisplayName, sizeof (pa->szDisplayName));
  memcpy (pw->szTypeName, pa->szTypeName, sizeof (pa->szTypeName));
}

DWORD
qxeSHGetFileInfo (const Extbyte *pszPath, DWORD dwFileAttributes,
		  SHFILEINFOW *psfi, UINT cbFileInfo, UINT uFlags)
{
  if (XEUNICODE_P)
    return SHGetFileInfoW ((LPCWSTR) pszPath, dwFileAttributes,
			   psfi, cbFileInfo, uFlags);
  else
    {
      SHFILEINFOA ansidat;
      BOOL retval;

      retval = SHGetFileInfoA ((LPCSTR) pszPath, dwFileAttributes,
			       (SHFILEINFOA FAR *) &ansidat,
			       cbFileInfo ? sizeof (ansidat) : 0, uFlags);
      if (retval && cbFileInfo)
	copy_shfileinfoa_to_shfileinfow (&ansidat, psfi, cbFileInfo);
      return retval;
    }
}

struct intercepted_SHBrowseForFolder
{
  BFFCALLBACK lpfn;
  LPARAM lParam;
  HWND hwnd;
  struct intercepted_SHBrowseForFolder *next;
};

static struct intercepted_SHBrowseForFolder *SHBrowseForFolder_list;

static int
CALLBACK intercepted_SHBrowseForFolder_proc (HWND hwnd, UINT msg,
					     LPARAM lParam, LPARAM lpData)
{
  struct intercepted_SHBrowseForFolder *s =
    (struct intercepted_SHBrowseForFolder *) lpData;

  if (s->hwnd == 0)
    s->hwnd = hwnd;
  if (s->lpfn)
    {
      /* see below */
      if (XEUNICODE_P && msg == BFFM_VALIDATEFAILEDW)
	msg = BFFM_VALIDATEFAILEDA;
      else if (!XEUNICODE_P && msg == BFFM_VALIDATEFAILEDA)
	msg = BFFM_VALIDATEFAILEDW;
      return (s->lpfn) (hwnd, msg, lParam, s->lParam);
    }
  else
    return 0;
}

#ifdef HAVE_MS_WINDOWS

static int
is_SHBrowseForFolder (HWND hwnd)
{
  struct intercepted_SHBrowseForFolder *s;

  for (s = SHBrowseForFolder_list; s; s = s->next)
    if (s->hwnd == hwnd)
      return 1;
  return 0;
}

#endif /* HAVE_MS_WINDOWS */

LPITEMIDLIST
qxeSHBrowseForFolder (LPBROWSEINFOW lpbi)
{
  struct intercepted_SHBrowseForFolder s;
  LPITEMIDLIST retval;

  /* There are two outgoing Unicode-split messages:

     BFFM_SETSELECTION
     BFFM_SETSTATUSTEXT

     and one incoming:

     BFFM_VALIDATEFAILED

     To handle this, we need to intercept the callback.  We handle the
     incoming message in the callback, and record the window; when
     qxeSendMessage() is called, we handle the outgoing messages.  None of
     the messages have split-sized structures so we don't need to do
     anything complicated there. */
  
  s.lParam = lpbi->lParam;
  s.lpfn = lpbi->lpfn;
  s.next = SHBrowseForFolder_list;
  s.hwnd = 0;
  SHBrowseForFolder_list = &s;
  
  lpbi->lpfn = intercepted_SHBrowseForFolder_proc;
  lpbi->lParam = (LPARAM) &s;

  if (XEUNICODE_P)
    retval = SHBrowseForFolderW (lpbi);
  else
    retval = SHBrowseForFolderA ((LPBROWSEINFOA) lpbi);
  SHBrowseForFolder_list = SHBrowseForFolder_list->next;
  return retval;
}

VOID
qxeSHAddToRecentDocs (UINT uFlags, LPCVOID pv)
{
  /* pv can be a string pointer; this is handled by Unicode-splitting the
     flag SHARD_PATH rather than the function itself.  Fix up the flag to
     be correct.  We write it symmetrically so it doesn't matter whether
     UNICODE is defined. */
  if (XEUNICODE_P)
    {
      if (uFlags & SHARD_PATHA)
	{
	  uFlags |= SHARD_PATHW;
	  uFlags &= ~SHARD_PATHA;
	}
    }
  else
    {
      if (uFlags & SHARD_PATHW)
	{
	  uFlags |= SHARD_PATHA;
	  uFlags &= ~SHARD_PATHW;
	}
    }
  SHAddToRecentDocs (uFlags, pv);
}

VOID
qxeSHChangeNotify (LONG wEventId, UINT uFlags, LPCVOID dwItem1,
		   LPCVOID dwItem2)
{
  /* works like SHAddToRecentDocs */
  if (XEUNICODE_P)
    {
      if (uFlags & SHCNF_PATHA)
	{
	  uFlags |= SHCNF_PATHW;
	  uFlags &= ~SHCNF_PATHA;
	}
      if (uFlags & SHCNF_PRINTERA)
	{
	  uFlags |= SHCNF_PRINTERW;
	  uFlags &= ~SHCNF_PRINTERA;
	}
    }
  else
    {
      if (uFlags & SHCNF_PATHW)
	{
	  uFlags |= SHCNF_PATHA;
	  uFlags &= ~SHCNF_PATHW;
	}
      if (uFlags & SHCNF_PRINTERW)
	{
	  uFlags |= SHCNF_PRINTERA;
	  uFlags &= ~SHCNF_PRINTERW;
	}
    }
  SHChangeNotify (wEventId, uFlags, dwItem1, dwItem2);
}

#ifndef CYGWIN /* present in headers but missing in shell32.a */

HRESULT
qxeSHGetDataFromIDList (IShellFolder *psf, LPCITEMIDLIST pidl, int nFormat,
			PVOID pv, int cb)
{
  if (XEUNICODE_P)
    return SHGetDataFromIDListW (psf, pidl, nFormat, pv, cb);
  else if (nFormat == SHGDFIL_FINDDATA)
    {
      WIN32_FIND_DATAA ansidat;
      BOOL retval;

      retval = SHGetDataFromIDListA (psf, pidl, nFormat, &ansidat, cb);
      if (retval == NOERROR)
	copy_win32_find_dataa_to_win32_find_dataw (&ansidat,
						   (WIN32_FIND_DATAW *) pv);
      return retval;
    }
  else
    /* nFormat == SHGDFIL_NETRESOURCE, and pv is split-simple NETRESOURCE
       structure, but we don't need to worry about that currently since we
       don't translate strings */
    return SHGetDataFromIDListA (psf, pidl, nFormat, pv, cb);
}

#endif /* not CYGWIN */



#ifdef HAVE_MS_WINDOWS

/************************************************************************/
/*                              devmode                                 */
/************************************************************************/

/* These functions return globally allocated blocks because some
   callers (e.g. qxePrintDlg) want this. */

static HGLOBAL
copy_devmodew_to_devmodea (const DEVMODEW *src, DEVMODEA *dst)
{
  /* the layout of DEVMODE is

     TCHAR dmDeviceName[...];
     non-split fields, including dmSize (size of structure; differs between
       Unicode and ANSI) and dmDriverExtra;
     TCHAR dmFormName[...];
     non-split fields;
     extra data, of size DEVMODE->dmDriverExtra
  */
  HGLOBAL hdst = NULL;

  if (!dst)
    {
      hdst = GlobalAlloc (GHND, src->dmSize + src->dmDriverExtra -
			  (sizeof (DEVMODEW) - sizeof (DEVMODEA)));
      dst = (DEVMODEA *) GlobalLock (hdst);
    }

  memcpy (dst->dmDeviceName, src->dmDeviceName, sizeof (dst->dmDeviceName));
  memcpy ((char *) dst + sizeof (dst->dmDeviceName),
	  (char *) src + sizeof (src->dmDeviceName),
	  offsetof (DEVMODEA, dmFormName) - sizeof (dst->dmDeviceName));
  dst->dmSize -= sizeof (DEVMODEW) - sizeof (DEVMODEA);
  memcpy (dst->dmFormName, src->dmFormName, sizeof (dst->dmFormName));
  memcpy ((char *) dst + offsetof (DEVMODEA, dmFormName) +
	  sizeof (dst->dmFormName),
	  (char *) src + offsetof (DEVMODEW, dmFormName) +
	  sizeof (src->dmFormName),
	  dst->dmSize + dst->dmDriverExtra -
	  (offsetof (DEVMODEA, dmFormName) + sizeof (dst->dmFormName)));

  if (hdst)
    GlobalUnlock (hdst);
  return hdst;
}

static HGLOBAL
copy_devmodea_to_devmodew (const DEVMODEA *src, DEVMODEW *dst)
{
  HGLOBAL hdst = NULL;

  if (!dst)
    {
      hdst = GlobalAlloc (GHND, src->dmSize + src->dmDriverExtra +
			  (sizeof (DEVMODEW) - sizeof (DEVMODEA)));
      dst = (DEVMODEW *) GlobalLock (hdst);
    }

  memcpy (dst->dmDeviceName, src->dmDeviceName, sizeof (src->dmDeviceName));
  memcpy ((char *) dst + sizeof (dst->dmDeviceName),
	  (char *) src + sizeof (src->dmDeviceName),
	  offsetof (DEVMODEA, dmFormName) - sizeof (src->dmDeviceName));
  dst->dmSize += sizeof (DEVMODEW) - sizeof (DEVMODEA);
  memcpy (dst->dmFormName, src->dmFormName, sizeof (src->dmFormName));
  memcpy ((char *) dst + offsetof (DEVMODEW, dmFormName) +
	  sizeof (dst->dmFormName),
	  (char *) src + offsetof (DEVMODEA, dmFormName) +
	  sizeof (src->dmFormName),
	  src->dmSize + src->dmDriverExtra -
	  (offsetof (DEVMODEA, dmFormName) + sizeof (src->dmFormName)));

  if (hdst)
    GlobalUnlock (hdst);
  return hdst;
}

HDC
qxeCreateDC (const Extbyte *lpszDriver, const Extbyte *lpszDevice,
	     const Extbyte *lpszOutput, CONST DEVMODEW *lpInitData)
{
  if (XEUNICODE_P)
    return CreateDCW ((LPCWSTR) lpszDriver, (LPCWSTR) lpszDevice,
		      (LPCWSTR) lpszOutput, lpInitData);
  else
    {
      HGLOBAL hInitData = NULL;
      DEVMODEA *lpInitDataa = NULL;
      HDC retval;

      if (lpInitData)
	{
	  hInitData = copy_devmodew_to_devmodea (lpInitData, NULL);
	  lpInitDataa = (DEVMODEA *) GlobalLock (hInitData);
	}
      retval = CreateDCA ((LPCSTR) lpszDriver, (LPCSTR) lpszDevice,
			  (LPCSTR) lpszOutput, lpInitDataa);

      if (hInitData)
	{
	  GlobalUnlock (hInitData);
	  GlobalFree (hInitData);
	}

      return retval;
    }
}

HDC
qxeResetDC (HDC hdc, CONST DEVMODEW *lpInitData)
{
  if (XEUNICODE_P)
    return ResetDCW (hdc, lpInitData);
  else
    {
      HGLOBAL hInitData = NULL;
      DEVMODEA *lpInitDataa = NULL;
      HDC retval;

      if (lpInitData)
	{
	  hInitData = copy_devmodew_to_devmodea (lpInitData, NULL);
	  lpInitDataa = (DEVMODEA *) GlobalLock (hInitData);
	}
      retval = ResetDCA (hdc, lpInitDataa);

      if (hInitData)
	{
	  GlobalUnlock (hInitData);
	  GlobalFree (hInitData);
	}

      return retval;
    }
}

DWORD
qxeOpenPrinter (Extbyte *pPrinterName, LPHANDLE phPrinter,
		LPPRINTER_DEFAULTSW pDefaultconst)
{
  assert (!pDefaultconst); /* we don't split it, so let's make sure we
			      don't try. */
  if (XEUNICODE_P)
    return OpenPrinterW ((LPWSTR) pPrinterName, phPrinter,
			 pDefaultconst);
  else
    return OpenPrinterA ((LPSTR) pPrinterName, phPrinter,
			 (LPPRINTER_DEFAULTSA) pDefaultconst);
}

LONG
qxeDocumentProperties (HWND hWnd, HANDLE hPrinter, Extbyte *pDeviceName,
		       DEVMODEW *pDevModeOutput, DEVMODEW *pDevModeInput,
		       DWORD fMode)
{
  if (XEUNICODE_P)
#if defined (CYGWIN_HEADERS) && W32API_INSTALLED_VER < W32API_VER(3,1)
    /* Cygwin used to mistakenly declare the fourth and fifth arguments as
       PDEVMODEA. */
    return DocumentPropertiesW (hWnd, hPrinter, (LPWSTR) pDeviceName,
				(DEVMODEA *) pDevModeOutput,
				(DEVMODEA *) pDevModeInput, fMode);
#else
    return DocumentPropertiesW (hWnd, hPrinter, (LPWSTR) pDeviceName,
				pDevModeOutput, pDevModeInput, fMode);
#endif /* CYGWIN_HEADERS */
  else
    {
      HGLOBAL hDevModeInput = NULL;
      DEVMODEA *pDevModeInputa = NULL;
      LONG retval;

      if (pDevModeInput)
	{
	  hDevModeInput = copy_devmodew_to_devmodea (pDevModeInput, NULL);
	  pDevModeInputa = (DEVMODEA *) GlobalLock (hDevModeInput);
	}

      /* Here we cheat a bit to avoid a problem: If the output
	 structure is given but not the input one, how do we know how
	 big to allocate our shadow output structure?  Since the
	 shadow structure is ANSI and the original Unicode, we know
	 the shadow structure is smaller than what's given, so we just
	 write into the given structure and then fix. */
      retval = DocumentPropertiesA (hWnd, hPrinter, (LPSTR) pDeviceName,
				    pDevModeOutput ?
				    (DEVMODEA *) pDevModeOutput : 0,
				    pDevModeInput ? pDevModeInputa : 0,
				    fMode);

      if (hDevModeInput)
	{
	  GlobalUnlock (hDevModeInput);
	  GlobalFree (hDevModeInput);
	}

      if (retval >= 0 && pDevModeOutput)
	{
	  /* copy the shadow structure out of the way and then put the
             right contents back. */
	  DEVMODEA *shadow = (DEVMODEA *) pDevModeOutput;
	  DEVMODEA *newshadow = alloca_array (DEVMODEA, shadow->dmSize +
					      shadow->dmDriverExtra);

	  memcpy (newshadow, shadow, shadow->dmSize + shadow->dmDriverExtra);
	  copy_devmodea_to_devmodew (newshadow, pDevModeOutput);
	}

      if (fMode == 0)
	retval += (sizeof (DEVMODEW) - sizeof (DEVMODEA));
      return retval;
    }
}

static BOOL
ansi_printer_dialog_1 (void *strucked, HGLOBAL *devmode_inout, int do_PrintDlg)
{
  HGLOBAL hdma = NULL;
  HGLOBAL hdmw = *devmode_inout;
  DEVMODEW *dmw = NULL;
  BOOL retval;

  if (hdmw != NULL)
    {
      /* copy to shadow in structure if needed */
      dmw = (DEVMODEW *) GlobalLock (hdmw);
      hdma = copy_devmodew_to_devmodea (dmw, NULL);
      *devmode_inout = hdma;
    }

  if (do_PrintDlg)
    retval = PrintDlgA ((PRINTDLGA *) strucked);
  else
    retval = PageSetupDlgA ((PAGESETUPDLGA *) strucked);

  if (retval)
    {
      /* copy the shadow output structure back to original, or
	 allocate new one. */
      if (*devmode_inout)
	{
	  DEVMODEA *newdma = (DEVMODEA *) GlobalLock (*devmode_inout);
	  if (dmw)
	    {
	      copy_devmodea_to_devmodew (newdma, dmw);
	      GlobalUnlock (hdmw);
	    }
	  else
	    hdmw = copy_devmodea_to_devmodew (newdma, NULL);
	  GlobalUnlock (*devmode_inout);
	  GlobalFree (*devmode_inout);
	  *devmode_inout = hdmw;
	}
      else if (hdma)
	/* #### can this happen? */
	GlobalFree (hdma);
    }

  return retval;
}

BOOL
qxePrintDlg (PRINTDLGW *lppd)
{
  if (XEUNICODE_P)
    return PrintDlgW (lppd);
  else
    return ansi_printer_dialog_1 (lppd, &lppd->hDevMode, 1);
}

BOOL
qxePageSetupDlg (PAGESETUPDLGW *lppd)
{
  if (XEUNICODE_P)
    return PageSetupDlgW (lppd);
  else
    return ansi_printer_dialog_1 (lppd, &lppd->hDevMode, 0);
}


/************************************************************************/
/*                                fonts                                 */
/************************************************************************/

static void
copy_logfonta_to_logfontw (const LOGFONTA *src, LOGFONTW *dst)
{
  /* the layout of LOGFONT is

     non-split fields;
     TCHAR lfFaceName[...];
  */
  memcpy (dst, src, sizeof (LOGFONTA));
}

static void
copy_logfontw_to_logfonta (const LOGFONTW *src, LOGFONTA *dst)
{
  memcpy (dst, src, sizeof (LOGFONTA));
}

#if 0 /* unused */

static void
copy_enumlogfonta_to_enumlogfontw (const ENUMLOGFONTA *src, ENUMLOGFONTW *dst)
{
  /* the layout of ENUMLOGFONT is

     LOGFONT elfLogFont; 
     TCHAR elfFullName[...]; 
     TCHAR elfStyle[...]; 
  */
  xzero (*dst);
  copy_logfonta_to_logfontw (&src->elfLogFont, &dst->elfLogFont);
  memcpy (dst->elfFullName, src->elfFullName, sizeof (src->elfFullName));
  memcpy (dst->elfStyle, src->elfStyle, sizeof (src->elfStyle));
}

#endif /* 0 */

static void
copy_enumlogfontexa_to_enumlogfontexw (const ENUMLOGFONTEXA *src,
				       ENUMLOGFONTEXW *dst)
{
  /* the layout of ENUMLOGFONT is

     LOGFONT elfLogFont; 
     TCHAR elfFullName[...]; 
     TCHAR elfStyle[...]; 
     TCHAR elfScript[...];
  */
  xzero (*dst);
  copy_logfonta_to_logfontw (&src->elfLogFont, &dst->elfLogFont);
  memcpy (dst->elfFullName, src->elfFullName, sizeof (src->elfFullName));
  memcpy (dst->elfStyle, src->elfStyle, sizeof (src->elfStyle));
  memcpy (dst->elfScript, src->elfScript, sizeof (src->elfScript));
}

static void
copy_newtextmetrica_to_newtextmetricw (const NEWTEXTMETRICA *src,
				       NEWTEXTMETRICW *dst)
{
  /* the layout of NEWTEXTMETRIC is

     non-split fields;
     WCHAR/BYTE      tmFirstChar;
     WCHAR/BYTE      tmLastChar;
     WCHAR/BYTE      tmDefaultChar;
     WCHAR/BYTE      tmBreakChar;
     BYTE            tmItalic;
     non-split fields;
  */
  xzero (*dst);
  memcpy ((char *) dst, (char *) src,
	  offsetof (NEWTEXTMETRICA, tmFirstChar));
  memcpy ((char *) dst + offsetof (NEWTEXTMETRICW, tmItalic),
	  (char *) src + offsetof (NEWTEXTMETRICA, tmItalic),
	  sizeof (NEWTEXTMETRICA) - offsetof (NEWTEXTMETRICA, tmItalic));
  dst->tmFirstChar = (WCHAR) src->tmFirstChar;
  dst->tmLastChar = (WCHAR) src->tmLastChar;
  dst->tmDefaultChar = (WCHAR) src->tmDefaultChar;
  dst->tmBreakChar = (WCHAR) src->tmBreakChar;
}

static void
copy_newtextmetricexa_to_newtextmetricexw (const NEWTEXTMETRICEXA *src,
					   NEWTEXTMETRICEXW *dst)
{
  /* the layout of NEWTEXTMETRICEX is

     NEWTEXTMETRICA/W  ntmTm;
     FONTSIGNATURE   ntmFontSig;
     */
  copy_newtextmetrica_to_newtextmetricw (&src->ntmTm, &dst->ntmTm);
  dst->ntmFontSig = src->ntmFontSig;
}

#if 0 /* unused */

static void
copy_textmetricw_to_textmetrica (const TEXTMETRICW *src,
				       TEXTMETRICA *dst)
{
  /* the layout of TEXTMETRIC is like NEWTEXTMETRIC; see above. */
  xzero (*dst);
  memcpy ((char *) dst, (char *) src,
	  offsetof (TEXTMETRICA, tmFirstChar));
  memcpy ((char *) dst + offsetof (TEXTMETRICA, tmItalic),
	  (char *) src + offsetof (TEXTMETRICW, tmItalic),
	  sizeof (TEXTMETRICA) - offsetof (TEXTMETRICA, tmItalic));
  dst->tmFirstChar = (BYTE) src->tmFirstChar;
  dst->tmLastChar = (BYTE) src->tmLastChar;
  dst->tmDefaultChar = (BYTE) src->tmDefaultChar;
  dst->tmBreakChar = (BYTE) src->tmBreakChar;
}

#endif /* 0 */

static void
copy_textmetrica_to_textmetricw (const TEXTMETRICA *src,
				       TEXTMETRICW *dst)
{
  /* the layout of TEXTMETRIC is like NEWTEXTMETRIC; see above. */
  xzero (*dst);
  memcpy ((char *) dst, (char *) src,
	  offsetof (TEXTMETRICA, tmFirstChar));
  memcpy ((char *) dst + offsetof (TEXTMETRICW, tmItalic),
	  (char *) src + offsetof (TEXTMETRICA, tmItalic),
	  sizeof (TEXTMETRICA) - offsetof (TEXTMETRICA, tmItalic));
  dst->tmFirstChar = (WCHAR) src->tmFirstChar;
  dst->tmLastChar = (WCHAR) src->tmLastChar;
  dst->tmDefaultChar = (WCHAR) src->tmDefaultChar;
  dst->tmBreakChar = (WCHAR) src->tmBreakChar;
}

typedef int (CALLBACK *qxeEnumFontFamExProcW) (ENUMLOGFONTEXW *lpelfe,
					       NEWTEXTMETRICEXW *lpntme, 
					       DWORD FontType,
					       LPARAM lParam);

struct qxeEnumFontFamExProcA_wrapper_t
{
  qxeEnumFontFamExProcW orig_proc;
  LPARAM orig_lparam;
};

static int CALLBACK
qxeEnumFontFamExProcA_wrapper (ENUMLOGFONTEXA *lpelfe,
			       NEWTEXTMETRICEXA *lpntme, 
			       DWORD fontType,
			       struct qxeEnumFontFamExProcA_wrapper_t
			       *closure)
{
  ENUMLOGFONTEXW lpelfew;
  NEWTEXTMETRICEXW lpntmew;

  /* #### if we're on Windows 2000 or above, lpelfe is actually an
     ENUMLOGFONTEXDV structure, and lpntme is an ENUMTEXTMETRIC structure
     when TRUETYPE_FONTTYPE.  both are split-sized and need their own copy
     functions.  need to handle. */
  copy_enumlogfontexa_to_enumlogfontexw (lpelfe, &lpelfew);
  if (fontType & TRUETYPE_FONTTYPE)
    copy_newtextmetricexa_to_newtextmetricexw (lpntme, &lpntmew);
  else
    {
      /* see docs of EnumFontFamExProc */
      xzero (lpntmew);
      copy_textmetrica_to_textmetricw ((TEXTMETRICA *) lpntme,
				       (TEXTMETRICW *) &lpntmew);
    }
  return (closure->orig_proc) (&lpelfew, &lpntmew, fontType,
                               closure->orig_lparam);
}

int
qxeEnumFontFamiliesEx (HDC hdc, LOGFONTW *lpLogfont,
		       FONTENUMPROCW lpEnumFontFamProc, LPARAM lParam,
		       DWORD dwFlags)
{
  if (XEUNICODE_P)
    return EnumFontFamiliesExW (hdc, lpLogfont, lpEnumFontFamProc, lParam,
				dwFlags);
  else
    {
      struct qxeEnumFontFamExProcA_wrapper_t closure;
      LOGFONTA lfa;

      closure.orig_proc = (qxeEnumFontFamExProcW) lpEnumFontFamProc;
      closure.orig_lparam = lParam;
      copy_logfontw_to_logfonta (lpLogfont, &lfa);
      return EnumFontFamiliesExA (hdc, &lfa,
				  (FONTENUMPROCA)
				  qxeEnumFontFamExProcA_wrapper,
				  (LPARAM) &closure, dwFlags);
    }
}

HFONT
qxeCreateFontIndirect (CONST LOGFONTW *lplf)
{
  if (XEUNICODE_P)
    return CreateFontIndirectW (lplf);
  else
    {
      LOGFONTA lfa;

      copy_logfontw_to_logfonta (lplf, &lfa);
      return CreateFontIndirectA (&lfa);
    }
}

BOOL
qxeImmSetCompositionFont (HIMC imc, LOGFONTW *lplf)
{
  if (XEUNICODE_P)
    return ImmSetCompositionFontW (imc, lplf);
  else
    {
      LOGFONTA lfa;

      copy_logfontw_to_logfonta (lplf, &lfa);
      return ImmSetCompositionFontA (imc, &lfa);
    }
}

BOOL
qxeImmGetCompositionFont (HIMC imc, LOGFONTW *lplf)
{
  if (XEUNICODE_P)
    return ImmGetCompositionFontW (imc, lplf);
  else
    {
      LOGFONTA lfa;
      BOOL retval = ImmGetCompositionFontA (imc, &lfa);

      if (retval)
	copy_logfonta_to_logfontw (&lfa, lplf);
      return retval;
    }
}

#if MSC_VERSION >= 1300

BOOL
qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPVOID lpComp, DWORD arg4, LPVOID lpRead, DWORD arg6)
{
  if (XEUNICODE_P)
    return ImmSetCompositionStringW (arg1, dwIndex, lpComp, arg4, lpRead, arg6);
  else
    return ImmSetCompositionStringA (arg1, dwIndex, lpComp, arg4, lpRead, arg6);
}

#else

BOOL
qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPCVOID lpComp, DWORD arg4, LPCVOID lpRead, DWORD arg6)
{
  if (XEUNICODE_P)
    return ImmSetCompositionStringW (arg1, dwIndex, lpComp, arg4, lpRead, arg6);
  else
    return ImmSetCompositionStringA (arg1, dwIndex, lpComp, arg4, lpRead, arg6);
}

#endif /* MSC_VERSION >= 1300 */

int
qxeGetObject (HGDIOBJ hgdiobj, int cbBuffer, LPVOID lpvObject)
{
  if (XEUNICODE_P)
    return GetObjectW (hgdiobj, cbBuffer, lpvObject);
  else
    {
      if (cbBuffer == sizeof (LOGFONTW))
	{
	  LOGFONTA lfa;
	  int retval = GetObjectA (hgdiobj, sizeof (LOGFONTA), &lfa);

	  if (!retval)
	    return retval;
	  copy_logfonta_to_logfontw (&lfa, (LOGFONTW *) lpvObject);
	  return retval;
	}
      else
	return GetObjectA (hgdiobj, cbBuffer, lpvObject);
    }
}

BOOL
qxeGetTextMetrics (HDC hdc, LPTEXTMETRICW lptm)
{
  if (XEUNICODE_P)
    return GetTextMetricsW (hdc, lptm);
  else
    {
      TEXTMETRICA tma;
      BOOL retval = GetTextMetricsA (hdc, &tma);

      if (retval)
	copy_textmetrica_to_textmetricw (&tma, lptm);
      return retval;
    }
}


/************************************************************************/
/*                                windows                               */
/************************************************************************/

typedef struct Intercepted_wnd_proc
{
  WNDPROC proc;
  Extbyte *name;
  int is_ansi;
} Intercepted_wnd_proc;

typedef struct
{
  Dynarr_declare (Intercepted_wnd_proc);
} Intercepted_wnd_proc_dynarr;

static Intercepted_wnd_proc_dynarr *intercepted_wnd_procs;

static Intercepted_wnd_proc *
find_window_class (const Extbyte *name, int is_ansi)
{
  int i;

  if (!intercepted_wnd_procs)
    intercepted_wnd_procs = Dynarr_new (Intercepted_wnd_proc);

  for (i = 0; i < Dynarr_length (intercepted_wnd_procs); i++)
    {
      Intercepted_wnd_proc *s = Dynarr_atp (intercepted_wnd_procs, i);
      
      if (s->is_ansi == is_ansi && (is_ansi ? !strcmp (s->name, name) :
				    !wcscmp ((wchar_t *) s->name,
					     (wchar_t *) name)))
	return s;
    }

  return 0;
}

/* ####

   check problem with cutting and pasting in my current mule -- if i cut,
   then go to another application, then switch back to this one and
   paste, it seems to get confused -- loses the size or something?

   other things: split flags on CreateProcess and DDE stuff should be
   handled by us.
   */  

static LRESULT WINAPI
intercepted_wnd_proc (HWND hwnd, UINT message_, WPARAM wParam, LPARAM lParam)
{
  Intercepted_wnd_proc *s;
  int is_ansi = XEUNICODE_P ? !IsWindowUnicode (hwnd) : 1;
  Extbyte *classname;
  int size = 100;

  /* Just in case XEUNICODE_P changes during the execution of the program
     (admittedly, unlikely), check whether the window is Unicode and keep
     track of this in the list of classes. */
  while (1)
    {
      classname = alloca_extbytes (size * XETCHAR_SIZE);
      if ((is_ansi ? GetClassNameA (hwnd, (LPSTR) classname, size) :
	   GetClassNameW (hwnd, (LPWSTR) classname, size)) < size - 1)
	break;
      size *= 2;
    }

  s = find_window_class (classname, is_ansi);

  assert (s);

  if (message_ == WM_NOTIFY)
    {
      LPNMHDR nmhdr = (LPNMHDR) lParam;
      int putback = nmhdr->code;
      int do_putback = 0;

#define FROB(msg)							     \
        case msg##W:							     \
        /* split structures are the same size, so no conversion necessary */ \
	  nmhdr->code = (UINT) msg##A;					     \
          do_putback = 1;						     \
          break;
      switch (nmhdr->code)
	{
	  /* NMHEADER */
	  FROB (HDN_ITEMCHANGING);
	  FROB (HDN_ITEMCHANGED);
	  FROB (HDN_ITEMCLICK);
	  FROB (HDN_ITEMDBLCLICK);
	  FROB (HDN_DIVIDERDBLCLICK);
	  FROB (HDN_BEGINTRACK);
	  FROB (HDN_ENDTRACK);
	  FROB (HDN_TRACK);
	  /* NMDISPINFO */
	  FROB (HDN_GETDISPINFO);
	  /* NMTBGETINFOTIP */
	  FROB (TBN_GETINFOTIP);
  	  /* NMTBDISPINFO */
	  FROB (TBN_GETDISPINFO);
	  /* NMTOOLBAR */
	  FROB (TBN_GETBUTTONINFO);

	  /* split-sized NMTTDISPINFO */
	  FROB (TTN_GETDISPINFO); /* handle the ...W case; then handle the
				     ...A case specially, since we need to
				     mess with the structure */
	case TTN_GETDISPINFOA: /* same as TTN_NEEDTEXTA */
	  {
	    NMTTDISPINFOW *nmw = alloca_new (NMTTDISPINFOW);
	    NMTTDISPINFOA *nma = (NMTTDISPINFOA *) lParam;
	    LRESULT retval;
	    /* the layout of NMTTDISPINFO is

	       non-split fields;
	       TCHAR szText[...];
	       non-split fields;
	       */

	    xzero (*nmw);
	    /* copy to ...W struct for Unicode code */
	    memcpy ((char *) nmw, (char *) nma,
		    offsetof (NMTTDISPINFOA, szText));
	    memcpy ((char *) nmw + offsetof (NMTTDISPINFOW, szText) +
		    sizeof (nmw->szText),
		    (char *) nma + offsetof (NMTTDISPINFOA, szText) +
		    sizeof (nma->szText),
		    sizeof (NMTTDISPINFOA) -
		    (offsetof (NMTTDISPINFOA, szText) + sizeof (nma->szText)));
	    memcpy (nmw->szText, nma->szText, sizeof (nma->szText));
	    retval = (s->proc) (hwnd, message_, wParam, lParam);
	    /* copy back to ...A struct */
	    xzero (*nma);
	    memcpy ((char *) nma, (char *) nmw,
		    offsetof (NMTTDISPINFOA, szText));
	    memcpy ((char *) nma + offsetof (NMTTDISPINFOA, szText) +
		    sizeof (nma->szText),
		    (char *) nmw + offsetof (NMTTDISPINFOW, szText) +
		    sizeof (nmw->szText),
		    sizeof (NMTTDISPINFOA) -
		    (offsetof (NMTTDISPINFOA, szText) + sizeof (nma->szText)));
	    memcpy (nma->szText, nmw->szText, sizeof (nma->szText));
	    return retval;
	  }
	  
	  /* NMLVFINDITEM */
	  FROB (LVN_ODFINDITEM);
	  /* NMLVDISPINFO */
	  FROB (LVN_BEGINLABELEDIT);
	  FROB (LVN_ENDLABELEDIT);
	  FROB (LVN_GETDISPINFO);
	  FROB (LVN_SETDISPINFO);
	  /* NMLVGETINFOTIP */
	  FROB (LVN_GETINFOTIP);
	  /* NMTREEVIEW */
	  FROB (TVN_SELCHANGING);
	  FROB (TVN_SELCHANGED);
	  FROB (TVN_ITEMEXPANDING);
	  FROB (TVN_ITEMEXPANDED);
	  FROB (TVN_BEGINDRAG);
	  FROB (TVN_BEGINRDRAG);
	  FROB (TVN_DELETEITEM);
	  /* NMTVDISPINFO */
	  FROB (TVN_GETDISPINFO);
	  FROB (TVN_SETDISPINFO);
	  FROB (TVN_BEGINLABELEDIT);
	  FROB (TVN_ENDLABELEDIT);
	  /* NMTVGETINFOTIP */
	  FROB (TVN_GETINFOTIP);
	  /* NMCOMBOBOXEX */
	  FROB (CBEN_GETDISPINFO);

	  /* split-sized NMCBEDRAGBEGIN */
	  FROB (CBEN_DRAGBEGIN); /* handle the ...W case; then handle the
				    ...A case specially, since we need to
				    mess with the structure */
	  {
	    NMCBEDRAGBEGINW *nmw = alloca_new (NMCBEDRAGBEGINW);
	    NMCBEDRAGBEGINA *nma = (NMCBEDRAGBEGINA *) lParam;
	    LRESULT retval;
	    /* the layout of NNMCBEDRAGBEGIN is

	       non-split fields;
	       TCHAR szText[...];
	       */

	    xzero (*nmw);
	    /* copy to ...W struct for Unicode code */
	    memcpy ((char *) nmw, (char *) nma,
		    sizeof (*nma));
	    retval = (s->proc) (hwnd, message_, wParam, lParam);
	    /* copy back to ...A struct */
	    xzero (*nma);
	    memcpy ((char *) nma, (char *) nmw,
		    sizeof (*nma));
	    return retval;
	  }

	  /* split-sized NMCBEENDEDIT */
	  FROB (CBEN_ENDEDIT); /* handle the ...W case; then handle the
				  ...A case specially, since we need to
				  mess with the structure */
	  {
	    NMCBEENDEDITW *nmw = alloca_new (NMCBEENDEDITW);
	    NMCBEENDEDITA *nma = (NMCBEENDEDITA *) lParam;
	    LRESULT retval;
	    /* the layout of NMCBEENDEDIT is

	       non-split fields;
	       TCHAR szText[...];
	       non-split fields;
	       */

	    xzero (*nmw);
	    /* copy to ...W struct for Unicode code */
	    memcpy ((char *) nmw, (char *) nma,
		    offsetof (NMCBEENDEDITA, szText));
	    memcpy ((char *) nmw + offsetof (NMCBEENDEDITW, szText) +
		    sizeof (nmw->szText),
		    (char *) nma + offsetof (NMCBEENDEDITA, szText) +
		    sizeof (nma->szText),
		    sizeof (NMCBEENDEDITA) -
		    (offsetof (NMCBEENDEDITA, szText) + sizeof (nma->szText)));
	    memcpy (nmw->szText, nma->szText, sizeof (nma->szText));
	    retval = (s->proc) (hwnd, message_, wParam, lParam);
	    /* copy back to ...A struct */
	    xzero (*nma);
	    memcpy ((char *) nma, (char *) nmw,
		    offsetof (NMCBEENDEDITA, szText));
	    memcpy ((char *) nma + offsetof (NMCBEENDEDITA, szText) +
		    sizeof (nma->szText),
		    (char *) nmw + offsetof (NMCBEENDEDITW, szText) +
		    sizeof (nmw->szText),
		    sizeof (NMCBEENDEDITA) -
		    (offsetof (NMCBEENDEDITA, szText) + sizeof (nma->szText)));
	    memcpy (nma->szText, nmw->szText, sizeof (nma->szText));
	    return retval;
	  }

	  /* NMDATETIMESTRING */
	  FROB (DTN_USERSTRING);
	  /* NMDATETIMEWMKEYDOWN */
	  FROB (DTN_WMKEYDOWN);

	  /* split-sized NMDATETIMEFORMAT */
	  FROB (DTN_FORMAT); /* handle the ...W case; then handle the
				...A case specially, since we need to
				mess with the structure */
	  {
	    NMDATETIMEFORMATW *nmw = alloca_new (NMDATETIMEFORMATW);
	    NMDATETIMEFORMATA *nma = (NMDATETIMEFORMATA *) lParam;
	    LRESULT retval;
	    /* the layout of NMDATETIMEFORMAT is

	       non-split fields;
	       TCHAR szText[...];
	       */

	    xzero (*nmw);
	    /* copy to ...W struct for Unicode code */
	    memcpy ((char *) nmw, (char *) nma,
		    sizeof (*nma));
	    retval = (s->proc) (hwnd, message_, wParam, lParam);
	    /* copy back to ...A struct */
	    xzero (*nma);
	    memcpy ((char *) nma, (char *) nmw,
		    sizeof (*nma));
	    return retval;
	  }

	  /* NMDATETIMEFORMATQUERY */
	  FROB (DTN_FORMATQUERY);
	default: break;
	}
#undef FROB
      if (do_putback)
	{
	  LRESULT retval = (s->proc) (hwnd, message_, wParam, lParam);
	  ((LPNMHDR) lParam)->code = putback;
	  return retval;
	}
    }
 
  return (s->proc) (hwnd, message_, wParam, lParam);
}

ATOM
qxeRegisterClass (CONST WNDCLASSW * lpWndClass)
{
  Intercepted_wnd_proc *s =
    find_window_class ((Extbyte *) lpWndClass->lpszClassName, !XEUNICODE_P);
  WNDCLASSW classnew;

  if (s)
    {
      s->proc = lpWndClass->lpfnWndProc;
      s->name = (Extbyte *) lpWndClass->lpszClassName;
      s->is_ansi = !XEUNICODE_P;
    }
  else
    {
      Intercepted_wnd_proc news;
      news.proc = lpWndClass->lpfnWndProc;
      news.name = (Extbyte *) lpWndClass->lpszClassName;
      news.is_ansi = !XEUNICODE_P;
      Dynarr_add (intercepted_wnd_procs, news);
    }
  classnew = *lpWndClass;
  classnew.lpfnWndProc = intercepted_wnd_proc;
  if (XEUNICODE_P)
    return RegisterClassW (&classnew);
  else
    /* The intermediate cast fools gcc into not outputting strict-aliasing
       complaints */
    return RegisterClassA ((CONST WNDCLASSA *) (void *) &classnew);
}

BOOL
qxeUnregisterClass (const Extbyte * lpClassName, HINSTANCE hInstance)
{
  Intercepted_wnd_proc *s =
    find_window_class (lpClassName, !XEUNICODE_P);

  if (s)
    Dynarr_delete_by_pointer (intercepted_wnd_procs, s);
  if (XEUNICODE_P)
    return UnregisterClassW ((LPCWSTR) lpClassName, hInstance);
  else
    return UnregisterClassA ((LPCSTR) lpClassName, hInstance);
}

/* NOTE: NT 4.0+ only */
ATOM
qxeRegisterClassEx (CONST WNDCLASSEXW *lpWndClass)
{
  Intercepted_wnd_proc *s =
    find_window_class ((Extbyte *) lpWndClass->lpszClassName, !XEUNICODE_P);
  WNDCLASSEXW classnew;

  if (s)
    {
      s->proc = lpWndClass->lpfnWndProc;
      s->name = (Extbyte *) lpWndClass->lpszClassName;
      s->is_ansi = !XEUNICODE_P;
    }
  else
    {
      Intercepted_wnd_proc news;
      news.proc = lpWndClass->lpfnWndProc;
      news.name = (Extbyte *) lpWndClass->lpszClassName;
      news.is_ansi = !XEUNICODE_P;
      Dynarr_add (intercepted_wnd_procs, news);
    }
  classnew = *lpWndClass;
  classnew.lpfnWndProc = intercepted_wnd_proc;
  if (XEUNICODE_P)
    return RegisterClassExW (&classnew);
  else
    /* The intermediate cast fools gcc into not outputting strict-aliasing
       complaints */
    return RegisterClassExA ((CONST WNDCLASSEXA *) (void *) &classnew);
}


/************************************************************************/
/*                              COMMCTRL.H				*/
/************************************************************************/

/* there are only four structures in commctrl.h that cannot be cast
   between Unicode/ANSI versions:

   NMTTDISPINFO aka TOOLTIPTEXT
   NMCBEDRAGBEGIN
   NMCBEENDEDIT
   NMDATETIMEFORMAT

   these are all notify structures, and we handle them above in
   intercepted_wnd_proc().

   in addition, this constant is weird, being a struct size of one of these:

   NMTTDISPINFO_V1_SIZE
*/

/*
split class names:

WC_HEADER
TOOLBARCLASSNAME
REBARCLASSNAME
TOOLTIPS_CLASS
STATUSCLASSNAME
TRACKBAR_CLASS
UPDOWN_CLASS
PROGRESS_CLASS
HOTKEY_CLASS
WC_LISTVIEW
WC_TREEVIEW
WC_COMBOBOXEX
WC_TABCONTROL
ANIMATE_CLASS
MONTHCAL_CLASS
DATETIMEPICK_CLASS
WC_IPADDRESS
WC_PAGESCROLLER
WC_NATIVEFONTCTL
*/

/*
SendMessage split messages:

HDM_INSERTITEM
HDM_GETITEM
HDM_SETITEM
TB_GETBUTTONTEXT
TB_SAVERESTORE
TB_ADDSTRING
TB_GETBUTTONINFO
TB_SETBUTTONINFO
TB_INSERTBUTTON
TB_ADDBUTTONS
RB_INSERTBAND
RB_SETBANDINFO
RB_GETBANDINFO
TTM_ADDTOOL
TTM_DELTOOL
TTM_NEWTOOLRECT
TTM_GETTOOLINFO
TTM_SETTOOLINFO
TTM_HITTEST
TTM_GETTEXT
TTM_UPDATETIPTEXT
TTM_ENUMTOOLS
TTM_GETCURRENTTOOL
SB_GETTEXT
SB_SETTEXT
SB_GETTEXTLENGTH
SB_SETTIPTEXT
SB_GETTIPTEXT
LVM_GETITEM
LVM_SETITEM
LVM_INSERTITEM
LVM_FINDITEM
LVM_GETSTRINGWIDTH
LVM_EDITLABEL
LVM_GETCOLUMN
LVM_SETCOLUMN
LVM_GETITEMTEXT
LVM_SETITEMTEXT
LVM_GETISEARCHSTRING
LVM_SETBKIMAGE
LVM_GETBKIMAGE
TVM_INSERTITEM
TVM_GETITEM
TVM_SETITEM
TVM_EDITLABEL
TVM_GETISEARCHSTRING
CBEM_INSERTITEM
CBEM_SETITEM
CBEM_GETITEM
TCM_GETITEM
TCM_SETITEM
TCM_INSERTITEM
ACM_OPEN
DTM_SETFORMAT
BFFM_SETSTATUSTEXT
BFFM_SETSELECTION
*/

/*
split notify messages:

HDN_ITEMCHANGING
HDN_ITEMCHANGED
HDN_ITEMCLICK
HDN_ITEMDBLCLICK
HDN_DIVIDERDBLCLICK
HDN_BEGINTRACK
HDN_ENDTRACK
HDN_TRACK
HDN_GETDISPINFO
TBN_GETINFOTIP
TBN_GETDISPINFO
TBN_GETBUTTONINFO
TTN_GETDISPINFO
TTN_NEEDTEXTW
LVN_ODFINDITEM
LVN_BEGINLABELEDIT
LVN_ENDLABELEDIT
LVN_GETDISPINFO
LVN_SETDISPINFO
LVN_GETINFOTIP
TVN_SELCHANGING
TVN_SELCHANGED
TVN_GETDISPINFO
TVN_SETDISPINFO
TVN_ITEMEXPANDING
TVN_ITEMEXPANDED
TVN_BEGINDRAG
TVN_BEGINRDRAG
TVN_DELETEITEM
TVN_BEGINLABELEDIT
TVN_ENDLABELEDIT
TVN_GETINFOTIP
CBEN_GETDISPINFO
CBEN_DRAGBEGIN
CBEN_ENDEDIT
DTN_USERSTRING
DTN_WMKEYDOWN
DTN_FORMAT
DTN_FORMATQUERY
BFFM_VALIDATEFAILED (send to SHBrowseForFolder procedure)
*/

/*
split structures:

TV_INSERTSTRUCT (simple-split, though -- just cast)
TC_ITEM (simple-split, though -- just cast)

####
*/

/*
split macros or macros needing splitting:

####
*/

LRESULT
qxeSendMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
#define FROB(msg)							   \
    case msg##A:							   \
      /* split structures are the same size, so no conversion necessary */ \
	Msg = msg##W;							   \
        break;

  if (XEUNICODE_P)
    {
      WCHAR classname[100];
      /* the name for SHBrowseForFolder windows is non-obvious so we have
         to intercept the callback */
      if (is_SHBrowseForFolder (hWnd))
	{
	  switch (Msg)
	    {
	      FROB (BFFM_SETSELECTION);
	      FROB (BFFM_SETSTATUSTEXT);
	    default: break;
	    }
	}
      else if (!GetClassNameW (hWnd, classname, 100))
	;
      /* luckily, subclassing leaves the name alone, so we can still
	 determine fairly easily the correct class to switch on */
      else if (!wcscmp (classname, WC_HEADERW))
	{
	  switch (Msg)
	    {
	      FROB (HDM_INSERTITEM);
	      FROB (HDM_GETITEM);
	      FROB (HDM_SETITEM);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, TOOLBARCLASSNAMEW))
	{
	  switch (Msg)
	    {
	      FROB (TB_GETBUTTONTEXT);
	      FROB (TB_SAVERESTORE);
	      FROB (TB_ADDSTRING);
	      FROB (TB_GETBUTTONINFO);
	      FROB (TB_SETBUTTONINFO);
	      FROB (TB_INSERTBUTTON);
	      FROB (TB_ADDBUTTONS);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, REBARCLASSNAMEW))
	{
	  switch (Msg)
	    {
	      FROB (RB_INSERTBAND);
	      FROB (RB_SETBANDINFO);
	      FROB (RB_GETBANDINFO);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, TOOLTIPS_CLASSW))
	{
	  switch (Msg)
	    {
	      FROB (TTM_ADDTOOL);
	      FROB (TTM_DELTOOL);
	      FROB (TTM_NEWTOOLRECT);
	      FROB (TTM_GETTOOLINFO);
	      FROB (TTM_SETTOOLINFO);
	      FROB (TTM_HITTEST);
	      FROB (TTM_GETTEXT);
	      FROB (TTM_UPDATETIPTEXT);
	      FROB (TTM_ENUMTOOLS);
	      FROB (TTM_GETCURRENTTOOL);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, STATUSCLASSNAMEW))
	{
	  switch (Msg)
	    {
	      FROB (SB_GETTEXT);
	      FROB (SB_SETTEXT);
	      FROB (SB_GETTEXTLENGTH);
	      FROB (SB_SETTIPTEXT);
	      FROB (SB_GETTIPTEXT);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, WC_LISTVIEWW))
	{
	  switch (Msg)
	    {
	      FROB (LVM_GETITEM);
	      FROB (LVM_SETITEM);
	      FROB (LVM_INSERTITEM);
	      FROB (LVM_FINDITEM);
	      FROB (LVM_GETSTRINGWIDTH);
	      FROB (LVM_EDITLABEL);
	      FROB (LVM_GETCOLUMN);
	      FROB (LVM_SETCOLUMN);
	      FROB (LVM_GETITEMTEXT);
	      FROB (LVM_SETITEMTEXT);
	      FROB (LVM_GETISEARCHSTRING);
	      FROB (LVM_SETBKIMAGE);
	      FROB (LVM_GETBKIMAGE);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, WC_TREEVIEWW))
	{
	  switch (Msg)
	    {
	      FROB (TVM_INSERTITEM); /* no need to split TV_INSERTSTRUCT */
	      FROB (TVM_GETITEM);
	      FROB (TVM_SETITEM);
	      FROB (TVM_EDITLABEL);
	      FROB (TVM_GETISEARCHSTRING);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, WC_COMBOBOXEXW))
	{
	  switch (Msg)
	    {
	      FROB (CBEM_INSERTITEM);
	      FROB (CBEM_SETITEM);
	      FROB (CBEM_GETITEM);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, WC_TABCONTROLW))
	{
	  switch (Msg)
	    {
	      FROB (TCM_GETITEM);
	      FROB (TCM_SETITEM);
	      FROB (TCM_INSERTITEM); /* no need to split TC_ITEM */
	    default: break;
	    }
	}
      else if (!wcscmp (classname, ANIMATE_CLASSW))
	{
	  switch (Msg)
	    {
	      FROB (ACM_OPEN);
	    default: break;
	    }
	}
      else if (!wcscmp (classname, DATETIMEPICK_CLASSW))
	{
	  switch (Msg)
	    {
	      FROB (DTM_SETFORMAT);
	    default: break;
	    }
	}
    }

  if (XEUNICODE_P)
    return SendMessageW (hWnd, Msg, wParam, lParam);
  else
    return SendMessageA (hWnd, Msg, wParam, lParam);

#undef FROB
}

#endif /* HAVE_MS_WINDOWS */


