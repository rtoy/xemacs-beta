/* Automatically-generated Unicode-encapsulation header file.
   Do not edit.  See `make-mswin-unicode.pl'.
*/


/* Processing file WINCON.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PeekConsoleInput
#define PeekConsoleInput error_use_qxePeekConsoleInput_or_PeekConsoleInputA_and_PeekConsoleInputW
#endif
BOOL qxePeekConsoleInput (HANDLE hConsoleInput, PINPUT_RECORD lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsRead);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsoleInput
#define ReadConsoleInput error_use_qxeReadConsoleInput_or_ReadConsoleInputA_and_ReadConsoleInputW
#endif
BOOL qxeReadConsoleInput (HANDLE hConsoleInput, PINPUT_RECORD lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsRead);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsoleInput
#define WriteConsoleInput error_use_qxeWriteConsoleInput_or_WriteConsoleInputA_and_WriteConsoleInputW
#endif
BOOL qxeWriteConsoleInput (HANDLE hConsoleInput, CONST INPUT_RECORD * lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsWritten);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsoleOutput
#define ReadConsoleOutput error_use_qxeReadConsoleOutput_or_ReadConsoleOutputA_and_ReadConsoleOutputW
#endif
BOOL qxeReadConsoleOutput (HANDLE hConsoleOutput, PCHAR_INFO lpBuffer, COORD dwBufferSize, COORD dwBufferCoord, PSMALL_RECT lpReadRegion);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsoleOutput
#define WriteConsoleOutput error_use_qxeWriteConsoleOutput_or_WriteConsoleOutputA_and_WriteConsoleOutputW
#endif
BOOL qxeWriteConsoleOutput (HANDLE hConsoleOutput, CONST CHAR_INFO * lpBuffer, COORD dwBufferSize, COORD dwBufferCoord, PSMALL_RECT lpWriteRegion);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsoleOutputCharacter
#define ReadConsoleOutputCharacter error_use_qxeReadConsoleOutputCharacter_or_ReadConsoleOutputCharacterA_and_ReadConsoleOutputCharacterW
#endif
BOOL qxeReadConsoleOutputCharacter (HANDLE hConsoleOutput, Extbyte * lpCharacter, DWORD nLength, COORD dwReadCoord, LPDWORD lpNumberOfCharsRead);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsoleOutputCharacter
#define WriteConsoleOutputCharacter error_use_qxeWriteConsoleOutputCharacter_or_WriteConsoleOutputCharacterA_and_WriteConsoleOutputCharacterW
#endif
BOOL qxeWriteConsoleOutputCharacter (HANDLE hConsoleOutput, const Extbyte * lpCharacter, DWORD nLength, COORD dwWriteCoord, LPDWORD lpNumberOfCharsWritten);

#undef FillConsoleOutputCharacter
#define FillConsoleOutputCharacter error_split_CHAR

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ScrollConsoleScreenBuffer
#define ScrollConsoleScreenBuffer error_use_qxeScrollConsoleScreenBuffer_or_ScrollConsoleScreenBufferA_and_ScrollConsoleScreenBufferW
#endif
BOOL qxeScrollConsoleScreenBuffer (HANDLE hConsoleOutput, CONST SMALL_RECT * lpScrollRectangle, CONST SMALL_RECT * lpClipRectangle, COORD dwDestinationOrigin, CONST CHAR_INFO * lpFill);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetConsoleTitle
#define GetConsoleTitle error_use_qxeGetConsoleTitle_or_GetConsoleTitleA_and_GetConsoleTitleW
#endif
DWORD qxeGetConsoleTitle (Extbyte * lpConsoleTitle, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetConsoleTitle
#define SetConsoleTitle error_use_qxeSetConsoleTitle_or_SetConsoleTitleA_and_SetConsoleTitleW
#endif
BOOL qxeSetConsoleTitle (const Extbyte * lpConsoleTitle);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsole
#define ReadConsole error_use_qxeReadConsole_or_ReadConsoleA_and_ReadConsoleW
#endif
BOOL qxeReadConsole (HANDLE hConsoleInput, LPVOID lpBuffer, DWORD nNumberOfCharsToRead, LPDWORD lpNumberOfCharsRead, LPVOID lpReserved);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsole
#define WriteConsole error_use_qxeWriteConsole_or_WriteConsoleA_and_WriteConsoleW
#endif
BOOL qxeWriteConsole (HANDLE hConsoleOutput, CONST VOID * lpBuffer, DWORD nNumberOfCharsToWrite, LPDWORD lpNumberOfCharsWritten, LPVOID lpReserved);


/* Processing file SHELLAPI.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DragQueryFile
#define DragQueryFile error_use_qxeDragQueryFile_or_DragQueryFileA_and_DragQueryFileW
#endif
UINT  qxeDragQueryFile (HDROP arg1, UINT arg2, Extbyte * arg3, UINT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ShellExecute
#define ShellExecute error_use_qxeShellExecute_or_ShellExecuteA_and_ShellExecuteW
#endif
HINSTANCE  qxeShellExecute (HWND hwnd, const Extbyte * lpOperation, const Extbyte * lpFile, const Extbyte * lpParameters, const Extbyte * lpDirectory, INT nShowCmd);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindExecutable
#define FindExecutable error_use_qxeFindExecutable_or_FindExecutableA_and_FindExecutableW
#endif
HINSTANCE  qxeFindExecutable (const Extbyte * lpFile, const Extbyte * lpDirectory, Extbyte * lpResult);

#undef CommandLineToArgv
#define CommandLineToArgv error_Unicode_only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ShellAbout
#define ShellAbout error_use_qxeShellAbout_or_ShellAboutA_and_ShellAboutW
#endif
INT        qxeShellAbout (HWND hWnd, const Extbyte * szApp, const Extbyte * szOtherStuff, HICON hIcon);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtractAssociatedIcon
#define ExtractAssociatedIcon error_use_qxeExtractAssociatedIcon_or_ExtractAssociatedIconA_and_ExtractAssociatedIconW
#endif
HICON      qxeExtractAssociatedIcon (HINSTANCE hInst, Extbyte * lpIconPath, LPWORD lpiIcon);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtractIcon
#define ExtractIcon error_use_qxeExtractIcon_or_ExtractIconA_and_ExtractIconW
#endif
HICON      qxeExtractIcon (HINSTANCE hInst, const Extbyte * lpszExeFileName, UINT nIconIndex);

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DoEnvironmentSubst
#define DoEnvironmentSubst error_use_qxeDoEnvironmentSubst_or_DoEnvironmentSubstA_and_DoEnvironmentSubstW
#endif
DWORD    qxeDoEnvironmentSubst (Extbyte * szString, UINT cbString);
#endif /* !defined (CYGWIN_HEADERS) */

#undef FindEnvironmentString
#define FindEnvironmentString error_causes_link_error__NT_4_0__only

/* Skipping ExtractIconEx because NT 4.0+ only, error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHFileOperation
#define SHFileOperation error_use_qxeSHFileOperation_or_SHFileOperationA_and_SHFileOperationW
#endif
int qxeSHFileOperation (LPSHFILEOPSTRUCTW lpFileOp);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ShellExecuteEx
#define ShellExecuteEx error_use_qxeShellExecuteEx_or_ShellExecuteExA_and_ShellExecuteExW
#endif
BOOL qxeShellExecuteEx (LPSHELLEXECUTEINFOW lpExecInfo);

#undef WinExecError
#define WinExecError error_causes_link_error__NT_4_0__only

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHQueryRecycleBin
#define SHQueryRecycleBin error_use_qxeSHQueryRecycleBin_or_SHQueryRecycleBinA_and_SHQueryRecycleBinW
#endif
HRESULT qxeSHQueryRecycleBin (const Extbyte * pszRootPath, LPSHQUERYRBINFO pSHQueryRBInfo);
#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHEmptyRecycleBin
#define SHEmptyRecycleBin error_use_qxeSHEmptyRecycleBin_or_SHEmptyRecycleBinA_and_SHEmptyRecycleBinW
#endif
HRESULT qxeSHEmptyRecycleBin (HWND hwnd, const Extbyte * pszRootPath, DWORD dwFlags);
#endif /* !defined (CYGWIN_HEADERS) */

#undef Shell_NotifyIcon
#define Shell_NotifyIcon error_split_sized_NOTIFYICONDATA__NT_4_0__only

/* Skipping SHGetFileInfo because split-sized SHFILEINFO, NT 4.0+ only */

#undef SHGetDiskFreeSpace
#define SHGetDiskFreeSpace error_causes_link_error__NT_4_0__only

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHGetNewLinkInfo
#define SHGetNewLinkInfo error_use_qxeSHGetNewLinkInfo_or_SHGetNewLinkInfoA_and_SHGetNewLinkInfoW
#endif
BOOL qxeSHGetNewLinkInfo (const Extbyte * pszLinkTo, const Extbyte * pszDir, Extbyte * pszName, BOOL * pfMustCopy, UINT uFlags);
#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHInvokePrinterCommand
#define SHInvokePrinterCommand error_use_qxeSHInvokePrinterCommand_or_SHInvokePrinterCommandA_and_SHInvokePrinterCommandW
#endif
BOOL qxeSHInvokePrinterCommand (HWND hwnd, UINT uAction, const Extbyte * lpBuf1, const Extbyte * lpBuf2, BOOL fModal);
#endif /* !defined (CYGWIN_HEADERS) */


/* Processing file WINSPOOL.H */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumPrinters
#define EnumPrinters error_use_qxeEnumPrinters_or_EnumPrintersA_and_EnumPrintersW
#endif
BOOL qxeEnumPrinters (DWORD Flags, Extbyte * Name, DWORD Level, LPBYTE pPrinterEnum, DWORD cbBuf, LPDWORD pcbNeeded, LPDWORD pcReturned);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
/* Skipping OpenPrinter because split-sized DEVMODE pointer in split PRINTER_DEFAULTS */
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ResetPrinter
#define ResetPrinter error_split_sized_DEVMODE_pointer_in_split_PRINTER_DEFAULTS
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetJob
#define SetJob error_split_sized_DEVMODE_pointer_in_split_JOB_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetJob
#define GetJob error_split_sized_DEVMODE_pointer_in_split_JOB_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumJobs
#define EnumJobs error_split_sized_DEVMODE_pointer_in_split_JOB_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinter
#define AddPrinter error_split_sized_DEVMODE_pointer_in_split_PRINTER_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinter
#define SetPrinter error_split_sized_DEVMODE_pointer_in_split_PRINTER_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinter
#define GetPrinter error_split_sized_DEVMODE_pointer_in_split_PRINTER_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinterDriver
#define AddPrinterDriver error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinterDriverEx
#define AddPrinterDriverEx error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterDrivers
#define EnumPrinterDrivers error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterDriver
#define GetPrinterDriver error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterDriverDirectory
#define GetPrinterDriverDirectory error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterDriver
#define DeletePrinterDriver error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterDriverEx
#define DeletePrinterDriverEx error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPerMachineConnection
#define AddPerMachineConnection error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePerMachineConnection
#define DeletePerMachineConnection error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPerMachineConnections
#define EnumPerMachineConnections error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrintProcessor
#define AddPrintProcessor error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrintProcessors
#define EnumPrintProcessors error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrintProcessorDirectory
#define GetPrintProcessorDirectory error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrintProcessorDatatypes
#define EnumPrintProcessorDatatypes error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrintProcessor
#define DeletePrintProcessor error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef StartDocPrinter
#define StartDocPrinter error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddJob
#define AddJob error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
/* Skipping DocumentProperties because split-sized DEVMODE, error in Cygwin prototype */
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AdvancedDocumentProperties
#define AdvancedDocumentProperties error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterData
#define GetPrinterData error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterDataEx
#define GetPrinterDataEx error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterData
#define EnumPrinterData error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterDataEx
#define EnumPrinterDataEx error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterKey
#define EnumPrinterKey error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinterData
#define SetPrinterData error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinterDataEx
#define SetPrinterDataEx error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterData
#define DeletePrinterData error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterDataEx
#define DeletePrinterDataEx error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterKey
#define DeletePrinterKey error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef PrinterMessageBox
#define PrinterMessageBox error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddForm
#define AddForm error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeleteForm
#define DeleteForm error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetForm
#define GetForm error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetForm
#define SetForm error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumForms
#define EnumForms error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumMonitors
#define EnumMonitors error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddMonitor
#define AddMonitor error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeleteMonitor
#define DeleteMonitor error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPorts
#define EnumPorts error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPort
#define AddPort error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ConfigurePort
#define ConfigurePort error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePort
#define DeletePort error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef XcvData
#define XcvData error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPort
#define SetPort error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinterConnection
#define AddPrinterConnection error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterConnection
#define DeletePrinterConnection error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrintProvidor
#define AddPrintProvidor error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrintProvidor
#define DeletePrintProvidor error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinterHTMLView
#define SetPrinterHTMLView error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterHTMLView
#define GetPrinterHTMLView error_not_used__complicated_interface_with_split_structures
#endif /* defined (HAVE_MS_WINDOWS) */


/* Processing file WINNETWK.H */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetAddConnection
#define WNetAddConnection error_use_qxeWNetAddConnection_or_WNetAddConnectionA_and_WNetAddConnectionW
#endif
DWORD  qxeWNetAddConnection (const Extbyte * lpRemoteName, const Extbyte * lpPassword, const Extbyte * lpLocalName);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetAddConnection2
#define WNetAddConnection2 error_use_qxeWNetAddConnection2_or_WNetAddConnection2A_and_WNetAddConnection2W
#endif
DWORD  qxeWNetAddConnection2 (LPNETRESOURCEW lpNetResource, const Extbyte * lpPassword, const Extbyte * lpUserName, DWORD dwFlags);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetAddConnection3
#define WNetAddConnection3 error_use_qxeWNetAddConnection3_or_WNetAddConnection3A_and_WNetAddConnection3W
#endif
DWORD  qxeWNetAddConnection3 (HWND hwndOwner, LPNETRESOURCEW lpNetResource, const Extbyte * lpPassword, const Extbyte * lpUserName, DWORD dwFlags);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetCancelConnection
#define WNetCancelConnection error_use_qxeWNetCancelConnection_or_WNetCancelConnectionA_and_WNetCancelConnectionW
#endif
DWORD  qxeWNetCancelConnection (const Extbyte * lpName, BOOL fForce);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetCancelConnection2
#define WNetCancelConnection2 error_use_qxeWNetCancelConnection2_or_WNetCancelConnection2A_and_WNetCancelConnection2W
#endif
DWORD  qxeWNetCancelConnection2 (const Extbyte * lpName, DWORD dwFlags, BOOL fForce);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetConnection
#define WNetGetConnection error_use_qxeWNetGetConnection_or_WNetGetConnectionA_and_WNetGetConnectionW
#endif
DWORD  qxeWNetGetConnection (const Extbyte * lpLocalName, Extbyte * lpRemoteName, LPDWORD lpnLength);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetUseConnection
#define WNetUseConnection error_use_qxeWNetUseConnection_or_WNetUseConnectionA_and_WNetUseConnectionW
#endif
DWORD  qxeWNetUseConnection (HWND hwndOwner, LPNETRESOURCEW lpNetResource, const Extbyte * lpUserID, const Extbyte * lpPassword, DWORD dwFlags, Extbyte * lpAccessName, LPDWORD lpBufferSize, LPDWORD lpResult);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetConnectionDialog1
#define WNetConnectionDialog1 error_use_qxeWNetConnectionDialog1_or_WNetConnectionDialog1A_and_WNetConnectionDialog1W
#endif
DWORD  qxeWNetConnectionDialog1 (LPCONNECTDLGSTRUCTW lpConnDlgStruct);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetDisconnectDialog1
#define WNetDisconnectDialog1 error_use_qxeWNetDisconnectDialog1_or_WNetDisconnectDialog1A_and_WNetDisconnectDialog1W
#endif
DWORD  qxeWNetDisconnectDialog1 (LPDISCDLGSTRUCTW lpConnDlgStruct);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetOpenEnum
#define WNetOpenEnum error_use_qxeWNetOpenEnum_or_WNetOpenEnumA_and_WNetOpenEnumW
#endif
DWORD  qxeWNetOpenEnum (DWORD dwScope, DWORD dwType, DWORD dwUsage, LPNETRESOURCEW lpNetResource, LPHANDLE lphEnum);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetEnumResource
#define WNetEnumResource error_use_qxeWNetEnumResource_or_WNetEnumResourceA_and_WNetEnumResourceW
#endif
DWORD  qxeWNetEnumResource (HANDLE hEnum, LPDWORD lpcCount, LPVOID lpBuffer, LPDWORD lpBufferSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetUniversalName
#define WNetGetUniversalName error_use_qxeWNetGetUniversalName_or_WNetGetUniversalNameA_and_WNetGetUniversalNameW
#endif
DWORD  qxeWNetGetUniversalName (const Extbyte * lpLocalPath, DWORD dwInfoLevel, LPVOID lpBuffer, LPDWORD lpBufferSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetUser
#define WNetGetUser error_use_qxeWNetGetUser_or_WNetGetUserA_and_WNetGetUserW
#endif
DWORD  qxeWNetGetUser (const Extbyte * lpName, Extbyte * lpUserName, LPDWORD lpnLength);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetProviderName
#define WNetGetProviderName error_use_qxeWNetGetProviderName_or_WNetGetProviderNameA_and_WNetGetProviderNameW
#endif
DWORD  qxeWNetGetProviderName (DWORD dwNetType, Extbyte * lpProviderName, LPDWORD lpBufferSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetNetworkInformation
#define WNetGetNetworkInformation error_use_qxeWNetGetNetworkInformation_or_WNetGetNetworkInformationA_and_WNetGetNetworkInformationW
#endif
DWORD  qxeWNetGetNetworkInformation (const Extbyte * lpProvider, LPNETINFOSTRUCT lpNetInfoStruct);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetLastError
#define WNetGetLastError error_use_qxeWNetGetLastError_or_WNetGetLastErrorA_and_WNetGetLastErrorW
#endif
DWORD  qxeWNetGetLastError (LPDWORD lpError, Extbyte * lpErrorBuf, DWORD nErrorBufSize, Extbyte * lpNameBuf, DWORD nNameBufSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MultinetGetConnectionPerformance
#define MultinetGetConnectionPerformance error_use_qxeMultinetGetConnectionPerformance_or_MultinetGetConnectionPerformanceA_and_MultinetGetConnectionPerformanceW
#endif
DWORD  qxeMultinetGetConnectionPerformance (LPNETRESOURCEW lpNetResource, LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct);
#endif /* defined (HAVE_MS_WINDOWS) */


/* Processing file WINUSER.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef wvsprintf
#define wvsprintf error_use_qxewvsprintf_or_wvsprintfA_and_wvsprintfW
#endif
int qxewvsprintf (Extbyte * arg1, const Extbyte * arg2, va_list arglist);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadKeyboardLayout
#define LoadKeyboardLayout error_use_qxeLoadKeyboardLayout_or_LoadKeyboardLayoutA_and_LoadKeyboardLayoutW
#endif
HKL qxeLoadKeyboardLayout (const Extbyte * pwszKLID, UINT Flags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetKeyboardLayoutName
#define GetKeyboardLayoutName error_use_qxeGetKeyboardLayoutName_or_GetKeyboardLayoutNameA_and_GetKeyboardLayoutNameW
#endif
BOOL qxeGetKeyboardLayoutName (Extbyte * pwszKLID);

#undef CreateDesktop
#define CreateDesktop error_split_sized_LPDEVMODE

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenDesktop
#define OpenDesktop error_use_qxeOpenDesktop_or_OpenDesktopA_and_OpenDesktopW
#endif
HDESK qxeOpenDesktop (Extbyte * lpszDesktop, DWORD dwFlags, BOOL fInherit, ACCESS_MASK dwDesiredAccess);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumDesktops
#define EnumDesktops error_use_qxeEnumDesktops_or_EnumDesktopsA_and_EnumDesktopsW
#endif
BOOL qxeEnumDesktops (HWINSTA hwinsta, DESKTOPENUMPROCW lpEnumFunc, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWindowStation
#define CreateWindowStation error_use_qxeCreateWindowStation_or_CreateWindowStationA_and_CreateWindowStationW
#endif
HWINSTA qxeCreateWindowStation (Extbyte * lpwinsta, DWORD dwReserved, ACCESS_MASK dwDesiredAccess, LPSECURITY_ATTRIBUTES lpsa);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenWindowStation
#define OpenWindowStation error_use_qxeOpenWindowStation_or_OpenWindowStationA_and_OpenWindowStationW
#endif
HWINSTA qxeOpenWindowStation (Extbyte * lpszWinSta, BOOL fInherit, ACCESS_MASK dwDesiredAccess);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumWindowStations
#define EnumWindowStations error_use_qxeEnumWindowStations_or_EnumWindowStationsA_and_EnumWindowStationsW
#endif
BOOL qxeEnumWindowStations (WINSTAENUMPROCW lpEnumFunc, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetUserObjectInformation
#define GetUserObjectInformation error_use_qxeGetUserObjectInformation_or_GetUserObjectInformationA_and_GetUserObjectInformationW
#endif
BOOL qxeGetUserObjectInformation (HANDLE hObj, int nIndex, PVOID pvInfo, DWORD nLength, LPDWORD lpnLengthNeeded);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetUserObjectInformation
#define SetUserObjectInformation error_use_qxeSetUserObjectInformation_or_SetUserObjectInformationA_and_SetUserObjectInformationW
#endif
BOOL qxeSetUserObjectInformation (HANDLE hObj, int nIndex, PVOID pvInfo, DWORD nLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterWindowMessage
#define RegisterWindowMessage error_use_qxeRegisterWindowMessage_or_RegisterWindowMessageA_and_RegisterWindowMessageW
#endif
UINT qxeRegisterWindowMessage (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMessage
#define GetMessage error_use_qxeGetMessage_or_GetMessageA_and_GetMessageW
#endif
BOOL qxeGetMessage (LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DispatchMessage
#define DispatchMessage error_use_qxeDispatchMessage_or_DispatchMessageA_and_DispatchMessageW
#endif
LONG qxeDispatchMessage (CONST MSG * lpMsg);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PeekMessage
#define PeekMessage error_use_qxePeekMessage_or_PeekMessageA_and_PeekMessageW
#endif
BOOL qxePeekMessage (LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax, UINT wRemoveMsg);

/* Skipping SendMessage because split messages and structures */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendMessageTimeout
#define SendMessageTimeout error_use_qxeSendMessageTimeout_or_SendMessageTimeoutA_and_SendMessageTimeoutW
#endif
LRESULT qxeSendMessageTimeout (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam, UINT fuFlags, UINT uTimeout, LPDWORD lpdwResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendNotifyMessage
#define SendNotifyMessage error_use_qxeSendNotifyMessage_or_SendNotifyMessageA_and_SendNotifyMessageW
#endif
BOOL qxeSendNotifyMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendMessageCallback
#define SendMessageCallback error_use_qxeSendMessageCallback_or_SendMessageCallbackA_and_SendMessageCallbackW
#endif
BOOL qxeSendMessageCallback (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam, SENDASYNCPROC lpResultCallBack, DWORD dwData);

#undef BroadcastSystemMessage
#define BroadcastSystemMessage error_win95_version_not_split__NT_4_0__only

#undef RegisterDeviceNotification
#define RegisterDeviceNotification error_NT_5_0__only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PostMessage
#define PostMessage error_use_qxePostMessage_or_PostMessageA_and_PostMessageW
#endif
BOOL qxePostMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PostThreadMessage
#define PostThreadMessage error_use_qxePostThreadMessage_or_PostThreadMessageA_and_PostThreadMessageW
#endif
BOOL qxePostThreadMessage (DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam);

/* Skipping DefWindowProc because return value is conditionalized on _MAC, messes up parser */

#undef CallWindowProc
#define CallWindowProc error_two_versions__STRICT_and_non_STRICT

#undef CallWindowProc
#define CallWindowProc error_two_versions__STRICT_and_non_STRICT

/* Skipping RegisterClass because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASS */

/* Skipping UnregisterClass because need to intercept for reasons related to RegisterClass */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassInfo
#define GetClassInfo error_use_qxeGetClassInfo_or_GetClassInfoA_and_GetClassInfoW
#endif
BOOL qxeGetClassInfo (HINSTANCE hInstance, const Extbyte * lpClassName, LPWNDCLASSW lpWndClass);

/* Skipping RegisterClassEx because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASSEX; NT 4.0+ only */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassInfoEx
#define GetClassInfoEx error_use_qxeGetClassInfoEx_or_GetClassInfoExA_and_GetClassInfoExW
#endif
BOOL qxeGetClassInfoEx (HINSTANCE arg1, const Extbyte * arg2, LPWNDCLASSEXW arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWindowEx
#define CreateWindowEx error_use_qxeCreateWindowEx_or_CreateWindowExA_and_CreateWindowExW
#endif
HWND qxeCreateWindowEx (DWORD dwExStyle, const Extbyte * lpClassName, const Extbyte * lpWindowName, DWORD dwStyle, int X, int Y, int nWidth, int nHeight, HWND hWndParent, HMENU hMenu, HINSTANCE hInstance, LPVOID lpParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDialogParam
#define CreateDialogParam error_use_qxeCreateDialogParam_or_CreateDialogParamA_and_CreateDialogParamW
#endif
HWND qxeCreateDialogParam (HINSTANCE hInstance, const Extbyte * lpTemplateName, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDialogIndirectParam
#define CreateDialogIndirectParam error_use_qxeCreateDialogIndirectParam_or_CreateDialogIndirectParamA_and_CreateDialogIndirectParamW
#endif
HWND qxeCreateDialogIndirectParam (HINSTANCE hInstance, LPCDLGTEMPLATEW lpTemplate, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DialogBoxParam
#define DialogBoxParam error_use_qxeDialogBoxParam_or_DialogBoxParamA_and_DialogBoxParamW
#endif
int qxeDialogBoxParam (HINSTANCE hInstance, const Extbyte * lpTemplateName, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DialogBoxIndirectParam
#define DialogBoxIndirectParam error_use_qxeDialogBoxIndirectParam_or_DialogBoxIndirectParamA_and_DialogBoxIndirectParamW
#endif
int qxeDialogBoxIndirectParam (HINSTANCE hInstance, LPCDLGTEMPLATEW hDialogTemplate, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetDlgItemText
#define SetDlgItemText error_use_qxeSetDlgItemText_or_SetDlgItemTextA_and_SetDlgItemTextW
#endif
BOOL qxeSetDlgItemText (HWND hDlg, int nIDDlgItem, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDlgItemText
#define GetDlgItemText error_use_qxeGetDlgItemText_or_GetDlgItemTextA_and_GetDlgItemTextW
#endif
UINT qxeGetDlgItemText (HWND hDlg, int nIDDlgItem, Extbyte * lpString, int nMaxCount);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendDlgItemMessage
#define SendDlgItemMessage error_use_qxeSendDlgItemMessage_or_SendDlgItemMessageA_and_SendDlgItemMessageW
#endif
LONG qxeSendDlgItemMessage (HWND hDlg, int nIDDlgItem, UINT Msg, WPARAM wParam, LPARAM lParam);

#undef DefDlgProc
#define DefDlgProc error_return_value_is_conditionalized_on__MAC__messes_up_parser

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CallMsgFilter
#define CallMsgFilter error_use_qxeCallMsgFilter_or_CallMsgFilterA_and_CallMsgFilterW
#endif
BOOL qxeCallMsgFilter (LPMSG lpMsg, int nCode);
#endif /* !defined (CYGWIN_HEADERS) */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterClipboardFormat
#define RegisterClipboardFormat error_use_qxeRegisterClipboardFormat_or_RegisterClipboardFormatA_and_RegisterClipboardFormatW
#endif
UINT qxeRegisterClipboardFormat (const Extbyte * lpszFormat);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClipboardFormatName
#define GetClipboardFormatName error_use_qxeGetClipboardFormatName_or_GetClipboardFormatNameA_and_GetClipboardFormatNameW
#endif
int qxeGetClipboardFormatName (UINT format, Extbyte * lpszFormatName, int cchMaxCount);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharToOem
#define CharToOem error_use_qxeCharToOem_or_CharToOemA_and_CharToOemW
#endif
BOOL qxeCharToOem (const Extbyte * lpszSrc, LPSTR lpszDst);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OemToChar
#define OemToChar error_use_qxeOemToChar_or_OemToCharA_and_OemToCharW
#endif
BOOL qxeOemToChar (LPCSTR lpszSrc, Extbyte * lpszDst);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharToOemBuff
#define CharToOemBuff error_use_qxeCharToOemBuff_or_CharToOemBuffA_and_CharToOemBuffW
#endif
BOOL qxeCharToOemBuff (const Extbyte * lpszSrc, LPSTR lpszDst, DWORD cchDstLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OemToCharBuff
#define OemToCharBuff error_use_qxeOemToCharBuff_or_OemToCharBuffA_and_OemToCharBuffW
#endif
BOOL qxeOemToCharBuff (LPCSTR lpszSrc, Extbyte * lpszDst, DWORD cchDstLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharUpper
#define CharUpper error_use_qxeCharUpper_or_CharUpperA_and_CharUpperW
#endif
Extbyte * qxeCharUpper (Extbyte * lpsz);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharUpperBuff
#define CharUpperBuff error_use_qxeCharUpperBuff_or_CharUpperBuffA_and_CharUpperBuffW
#endif
DWORD qxeCharUpperBuff (Extbyte * lpsz, DWORD cchLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharLower
#define CharLower error_use_qxeCharLower_or_CharLowerA_and_CharLowerW
#endif
Extbyte * qxeCharLower (Extbyte * lpsz);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharLowerBuff
#define CharLowerBuff error_use_qxeCharLowerBuff_or_CharLowerBuffA_and_CharLowerBuffW
#endif
DWORD qxeCharLowerBuff (Extbyte * lpsz, DWORD cchLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharNext
#define CharNext error_use_qxeCharNext_or_CharNextA_and_CharNextW
#endif
Extbyte * qxeCharNext (const Extbyte * lpsz);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharPrev
#define CharPrev error_use_qxeCharPrev_or_CharPrevA_and_CharPrevW
#endif
Extbyte * qxeCharPrev (const Extbyte * lpszStart, const Extbyte * lpszCurrent);

#undef IsCharAlpha
#define IsCharAlpha error_split_CHAR

#undef IsCharAlphaNumeric
#define IsCharAlphaNumeric error_split_CHAR

#undef IsCharUpper
#define IsCharUpper error_split_CHAR

#undef IsCharLower
#define IsCharLower error_split_CHAR

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetKeyNameText
#define GetKeyNameText error_use_qxeGetKeyNameText_or_GetKeyNameTextA_and_GetKeyNameTextW
#endif
int qxeGetKeyNameText (LONG lParam, Extbyte * lpString, int nSize);

/* Skipping VkKeyScan because split CHAR */

#undef VkKeyScanEx
#define VkKeyScanEx error_split_CHAR__NT_4_0__only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MapVirtualKey
#define MapVirtualKey error_use_qxeMapVirtualKey_or_MapVirtualKeyA_and_MapVirtualKeyW
#endif
UINT qxeMapVirtualKey (UINT uCode, UINT uMapType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MapVirtualKeyEx
#define MapVirtualKeyEx error_use_qxeMapVirtualKeyEx_or_MapVirtualKeyExA_and_MapVirtualKeyExW
#endif
UINT qxeMapVirtualKeyEx (UINT uCode, UINT uMapType, HKL dwhkl);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadAccelerators
#define LoadAccelerators error_use_qxeLoadAccelerators_or_LoadAcceleratorsA_and_LoadAcceleratorsW
#endif
HACCEL qxeLoadAccelerators (HINSTANCE hInstance, const Extbyte * lpTableName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateAcceleratorTable
#define CreateAcceleratorTable error_use_qxeCreateAcceleratorTable_or_CreateAcceleratorTableA_and_CreateAcceleratorTableW
#endif
HACCEL qxeCreateAcceleratorTable (LPACCEL arg1, int arg2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyAcceleratorTable
#define CopyAcceleratorTable error_use_qxeCopyAcceleratorTable_or_CopyAcceleratorTableA_and_CopyAcceleratorTableW
#endif
int qxeCopyAcceleratorTable (HACCEL hAccelSrc, LPACCEL lpAccelDst, int cAccelEntries);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef TranslateAccelerator
#define TranslateAccelerator error_use_qxeTranslateAccelerator_or_TranslateAcceleratorA_and_TranslateAcceleratorW
#endif
int qxeTranslateAccelerator (HWND hWnd, HACCEL hAccTable, LPMSG lpMsg);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadMenu
#define LoadMenu error_use_qxeLoadMenu_or_LoadMenuA_and_LoadMenuW
#endif
HMENU qxeLoadMenu (HINSTANCE hInstance, const Extbyte * lpMenuName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadMenuIndirect
#define LoadMenuIndirect error_use_qxeLoadMenuIndirect_or_LoadMenuIndirectA_and_LoadMenuIndirectW
#endif
HMENU qxeLoadMenuIndirect (CONST MENUTEMPLATEW * lpMenuTemplate);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ChangeMenu
#define ChangeMenu error_use_qxeChangeMenu_or_ChangeMenuA_and_ChangeMenuW
#endif
BOOL qxeChangeMenu (HMENU hMenu, UINT cmd, const Extbyte * lpszNewItem, UINT cmdInsert, UINT flags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMenuString
#define GetMenuString error_use_qxeGetMenuString_or_GetMenuStringA_and_GetMenuStringW
#endif
int qxeGetMenuString (HMENU hMenu, UINT uIDItem, Extbyte * lpString, int nMaxCount, UINT uFlag);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef InsertMenu
#define InsertMenu error_use_qxeInsertMenu_or_InsertMenuA_and_InsertMenuW
#endif
BOOL qxeInsertMenu (HMENU hMenu, UINT uPosition, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AppendMenu
#define AppendMenu error_use_qxeAppendMenu_or_AppendMenuA_and_AppendMenuW
#endif
BOOL qxeAppendMenu (HMENU hMenu, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ModifyMenu
#define ModifyMenu error_use_qxeModifyMenu_or_ModifyMenuA_and_ModifyMenuW
#endif
BOOL qxeModifyMenu (HMENU hMnu, UINT uPosition, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef InsertMenuItem
#define InsertMenuItem error_use_qxeInsertMenuItem_or_InsertMenuItemA_and_InsertMenuItemW
#endif
BOOL qxeInsertMenuItem (HMENU arg1, UINT arg2, BOOL arg3, LPCMENUITEMINFOW arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMenuItemInfo
#define GetMenuItemInfo error_use_qxeGetMenuItemInfo_or_GetMenuItemInfoA_and_GetMenuItemInfoW
#endif
BOOL qxeGetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPMENUITEMINFOW arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetMenuItemInfo
#define SetMenuItemInfo error_use_qxeSetMenuItemInfo_or_SetMenuItemInfoA_and_SetMenuItemInfoW
#endif
BOOL qxeSetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPCMENUITEMINFOW arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DrawText
#define DrawText error_use_qxeDrawText_or_DrawTextA_and_DrawTextW
#endif
int qxeDrawText (HDC hDC, const Extbyte * lpString, int nCount, LPRECT lpRect, UINT uFormat);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DrawTextEx
#define DrawTextEx error_use_qxeDrawTextEx_or_DrawTextExA_and_DrawTextExW
#endif
int qxeDrawTextEx (HDC arg1, Extbyte * arg2, int arg3, LPRECT arg4, UINT arg5, LPDRAWTEXTPARAMS arg6);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GrayString
#define GrayString error_use_qxeGrayString_or_GrayStringA_and_GrayStringW
#endif
BOOL qxeGrayString (HDC hDC, HBRUSH hBrush, GRAYSTRINGPROC lpOutputFunc, LPARAM lpData, int nCount, int X, int Y, int nWidth, int nHeight);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DrawState
#define DrawState error_use_qxeDrawState_or_DrawStateA_and_DrawStateW
#endif
BOOL qxeDrawState (HDC arg1, HBRUSH arg2, DRAWSTATEPROC arg3, LPARAM arg4, WPARAM arg5, int arg6, int arg7, int arg8, int arg9, UINT arg10);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef TabbedTextOut
#define TabbedTextOut error_use_qxeTabbedTextOut_or_TabbedTextOutA_and_TabbedTextOutW
#endif
LONG qxeTabbedTextOut (HDC hDC, int X, int Y, const Extbyte * lpString, int nCount, int nTabPositions, LPINT lpnTabStopPositions, int nTabOrigin);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTabbedTextExtent
#define GetTabbedTextExtent error_use_qxeGetTabbedTextExtent_or_GetTabbedTextExtentA_and_GetTabbedTextExtentW
#endif
DWORD qxeGetTabbedTextExtent (HDC hDC, const Extbyte * lpString, int nCount, int nTabPositions, LPINT lpnTabStopPositions);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetProp
#define SetProp error_use_qxeSetProp_or_SetPropA_and_SetPropW
#endif
BOOL qxeSetProp (HWND hWnd, const Extbyte * lpString, HANDLE hData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProp
#define GetProp error_use_qxeGetProp_or_GetPropA_and_GetPropW
#endif
HANDLE qxeGetProp (HWND hWnd, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RemoveProp
#define RemoveProp error_use_qxeRemoveProp_or_RemovePropA_and_RemovePropW
#endif
HANDLE qxeRemoveProp (HWND hWnd, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumPropsEx
#define EnumPropsEx error_use_qxeEnumPropsEx_or_EnumPropsExA_and_EnumPropsExW
#endif
int qxeEnumPropsEx (HWND hWnd, PROPENUMPROCEXW lpEnumFunc, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumProps
#define EnumProps error_use_qxeEnumProps_or_EnumPropsA_and_EnumPropsW
#endif
int qxeEnumProps (HWND hWnd, PROPENUMPROCW lpEnumFunc);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowText
#define SetWindowText error_use_qxeSetWindowText_or_SetWindowTextA_and_SetWindowTextW
#endif
BOOL qxeSetWindowText (HWND hWnd, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowText
#define GetWindowText error_use_qxeGetWindowText_or_GetWindowTextA_and_GetWindowTextW
#endif
int qxeGetWindowText (HWND hWnd, Extbyte * lpString, int nMaxCount);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowTextLength
#define GetWindowTextLength error_use_qxeGetWindowTextLength_or_GetWindowTextLengthA_and_GetWindowTextLengthW
#endif
int qxeGetWindowTextLength (HWND hWnd);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MessageBox
#define MessageBox error_use_qxeMessageBox_or_MessageBoxA_and_MessageBoxW
#endif
int qxeMessageBox (HWND hWnd, const Extbyte * lpText, const Extbyte * lpCaption, UINT uType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MessageBoxEx
#define MessageBoxEx error_use_qxeMessageBoxEx_or_MessageBoxExA_and_MessageBoxExW
#endif
int qxeMessageBoxEx (HWND hWnd, const Extbyte * lpText, const Extbyte * lpCaption, UINT uType, WORD wLanguageId);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MessageBoxIndirect
#define MessageBoxIndirect error_use_qxeMessageBoxIndirect_or_MessageBoxIndirectA_and_MessageBoxIndirectW
#endif
int qxeMessageBoxIndirect (LPMSGBOXPARAMSW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowLong
#define GetWindowLong error_use_qxeGetWindowLong_or_GetWindowLongA_and_GetWindowLongW
#endif
LONG qxeGetWindowLong (HWND hWnd, int nIndex);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowLong
#define SetWindowLong error_use_qxeSetWindowLong_or_SetWindowLongA_and_SetWindowLongW
#endif
LONG qxeSetWindowLong (HWND hWnd, int nIndex, LONG dwNewLong);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassLong
#define GetClassLong error_use_qxeGetClassLong_or_GetClassLongA_and_GetClassLongW
#endif
DWORD qxeGetClassLong (HWND hWnd, int nIndex);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetClassLong
#define SetClassLong error_use_qxeSetClassLong_or_SetClassLongA_and_SetClassLongW
#endif
DWORD qxeSetClassLong (HWND hWnd, int nIndex, LONG dwNewLong);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindWindow
#define FindWindow error_use_qxeFindWindow_or_FindWindowA_and_FindWindowW
#endif
HWND qxeFindWindow (const Extbyte * lpClassName, const Extbyte * lpWindowName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindWindowEx
#define FindWindowEx error_use_qxeFindWindowEx_or_FindWindowExA_and_FindWindowExW
#endif
HWND qxeFindWindowEx (HWND arg1, HWND arg2, const Extbyte * arg3, const Extbyte * arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassName
#define GetClassName error_use_qxeGetClassName_or_GetClassNameA_and_GetClassNameW
#endif
int qxeGetClassName (HWND hWnd, Extbyte * lpClassName, int nMaxCount);

#undef SetWindowsHook
#define SetWindowsHook error_obsolete__two_versions__STRICT_and_non_STRICT

#undef SetWindowsHook
#define SetWindowsHook error_obsolete__two_versions__STRICT_and_non_STRICT

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowsHookEx
#define SetWindowsHookEx error_use_qxeSetWindowsHookEx_or_SetWindowsHookExA_and_SetWindowsHookExW
#endif
HHOOK qxeSetWindowsHookEx (int idHook, HOOKPROC lpfn, HINSTANCE hmod, DWORD dwThreadId);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadBitmap
#define LoadBitmap error_use_qxeLoadBitmap_or_LoadBitmapA_and_LoadBitmapW
#endif
HBITMAP qxeLoadBitmap (HINSTANCE hInstance, const Extbyte * lpBitmapName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadCursor
#define LoadCursor error_use_qxeLoadCursor_or_LoadCursorA_and_LoadCursorW
#endif
HCURSOR qxeLoadCursor (HINSTANCE hInstance, const Extbyte * lpCursorName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadCursorFromFile
#define LoadCursorFromFile error_use_qxeLoadCursorFromFile_or_LoadCursorFromFileA_and_LoadCursorFromFileW
#endif
HCURSOR qxeLoadCursorFromFile (const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadIcon
#define LoadIcon error_use_qxeLoadIcon_or_LoadIconA_and_LoadIconW
#endif
HICON qxeLoadIcon (HINSTANCE hInstance, const Extbyte * lpIconName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadImage
#define LoadImage error_use_qxeLoadImage_or_LoadImageA_and_LoadImageW
#endif
HANDLE qxeLoadImage (HINSTANCE arg1, const Extbyte * arg2, UINT arg3, int arg4, int arg5, UINT arg6);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadString
#define LoadString error_use_qxeLoadString_or_LoadStringA_and_LoadStringW
#endif
int qxeLoadString (HINSTANCE hInstance, UINT uID, Extbyte * lpBuffer, int nBufferMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef IsDialogMessage
#define IsDialogMessage error_use_qxeIsDialogMessage_or_IsDialogMessageA_and_IsDialogMessageW
#endif
BOOL qxeIsDialogMessage (HWND hDlg, LPMSG lpMsg);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirList
#define DlgDirList error_use_qxeDlgDirList_or_DlgDirListA_and_DlgDirListW
#endif
int qxeDlgDirList (HWND hDlg, Extbyte * lpPathSpec, int nIDListBox, int nIDStaticPath, UINT uFileType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirSelectEx
#define DlgDirSelectEx error_use_qxeDlgDirSelectEx_or_DlgDirSelectExA_and_DlgDirSelectExW
#endif
BOOL qxeDlgDirSelectEx (HWND hDlg, Extbyte * lpString, int nCount, int nIDListBox);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirListComboBox
#define DlgDirListComboBox error_use_qxeDlgDirListComboBox_or_DlgDirListComboBoxA_and_DlgDirListComboBoxW
#endif
int qxeDlgDirListComboBox (HWND hDlg, Extbyte * lpPathSpec, int nIDComboBox, int nIDStaticPath, UINT uFiletype);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirSelectComboBoxEx
#define DlgDirSelectComboBoxEx error_use_qxeDlgDirSelectComboBoxEx_or_DlgDirSelectComboBoxExA_and_DlgDirSelectComboBoxExW
#endif
BOOL qxeDlgDirSelectComboBoxEx (HWND hDlg, Extbyte * lpString, int nCount, int nIDComboBox);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefFrameProc
#define DefFrameProc error_use_qxeDefFrameProc_or_DefFrameProcA_and_DefFrameProcW
#endif
LRESULT qxeDefFrameProc (HWND hWnd, HWND hWndMDIClient, UINT uMsg, WPARAM wParam, LPARAM lParam);

#undef DefMDIChildProc
#define DefMDIChildProc error_return_value_is_conditionalized_on__MAC__messes_up_parser

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMDIWindow
#define CreateMDIWindow error_use_qxeCreateMDIWindow_or_CreateMDIWindowA_and_CreateMDIWindowW
#endif
HWND qxeCreateMDIWindow (Extbyte * lpClassName, Extbyte * lpWindowName, DWORD dwStyle, int X, int Y, int nWidth, int nHeight, HWND hWndParent, HINSTANCE hInstance, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WinHelp
#define WinHelp error_use_qxeWinHelp_or_WinHelpA_and_WinHelpW
#endif
BOOL qxeWinHelp (HWND hWndMain, const Extbyte * lpszHelp, UINT uCommand, DWORD dwData);

#undef ChangeDisplaySettings
#define ChangeDisplaySettings error_split_sized_LPDEVMODE

#undef ChangeDisplaySettingsEx
#define ChangeDisplaySettingsEx error_split_sized_LPDEVMODE__NT_5_0_Win98__only

#undef EnumDisplaySettings
#define EnumDisplaySettings error_split_sized_LPDEVMODE

#undef EnumDisplayDevices
#define EnumDisplayDevices error_split_sized_PDISPLAY_DEVICE__NT_5_0__only__no_Win98

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SystemParametersInfo
#define SystemParametersInfo error_use_qxeSystemParametersInfo_or_SystemParametersInfoA_and_SystemParametersInfoW
#endif
BOOL qxeSystemParametersInfo (UINT uiAction, UINT uiParam, PVOID pvParam, UINT fWinIni);

#undef GetMonitorInfo
#define GetMonitorInfo error_NT_5_0_Win98__only

#undef GetWindowModuleFileName
#define GetWindowModuleFileName error_NT_5_0__only

#undef RealGetWindowClass
#define RealGetWindowClass error_NT_5_0__only

#undef GetAltTabInfo
#define GetAltTabInfo error_NT_5_0__only


/* Processing file DDEML.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DdeInitialize
#define DdeInitialize error_use_qxeDdeInitialize_or_DdeInitializeA_and_DdeInitializeW
#endif
UINT qxeDdeInitialize (LPDWORD pidInst, PFNCALLBACK pfnCallback, DWORD afCmd, DWORD ulRes);

/* Skipping DdeCreateStringHandle because error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DdeQueryString
#define DdeQueryString error_use_qxeDdeQueryString_or_DdeQueryStringA_and_DdeQueryStringW
#endif
DWORD qxeDdeQueryString (DWORD idInst, HSZ hsz, Extbyte * psz, DWORD cchMax, int iCodePage);


/* Processing file WINREG.H */

/* Skipping RegConnectRegistry because error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegCreateKey
#define RegCreateKey error_use_qxeRegCreateKey_or_RegCreateKeyA_and_RegCreateKeyW
#endif
LONG
 qxeRegCreateKey (HKEY hKey, const Extbyte * lpSubKey, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegCreateKeyEx
#define RegCreateKeyEx error_use_qxeRegCreateKeyEx_or_RegCreateKeyExA_and_RegCreateKeyExW
#endif
LONG
 qxeRegCreateKeyEx (HKEY hKey, const Extbyte * lpSubKey, DWORD Reserved, Extbyte * lpClass, DWORD dwOptions, REGSAM samDesired, LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegDeleteKey
#define RegDeleteKey error_use_qxeRegDeleteKey_or_RegDeleteKeyA_and_RegDeleteKeyW
#endif
LONG
 qxeRegDeleteKey (HKEY hKey, const Extbyte * lpSubKey);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegDeleteValue
#define RegDeleteValue error_use_qxeRegDeleteValue_or_RegDeleteValueA_and_RegDeleteValueW
#endif
LONG
 qxeRegDeleteValue (HKEY hKey, const Extbyte * lpValueName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegEnumKey
#define RegEnumKey error_use_qxeRegEnumKey_or_RegEnumKeyA_and_RegEnumKeyW
#endif
LONG
 qxeRegEnumKey (HKEY hKey, DWORD dwIndex, Extbyte * lpName, DWORD cbName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegEnumKeyEx
#define RegEnumKeyEx error_use_qxeRegEnumKeyEx_or_RegEnumKeyExA_and_RegEnumKeyExW
#endif
LONG
 qxeRegEnumKeyEx (HKEY hKey, DWORD dwIndex, Extbyte * lpName, LPDWORD lpcbName, LPDWORD lpReserved, Extbyte * lpClass, LPDWORD lpcbClass, PFILETIME lpftLastWriteTime);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegEnumValue
#define RegEnumValue error_use_qxeRegEnumValue_or_RegEnumValueA_and_RegEnumValueW
#endif
LONG
 qxeRegEnumValue (HKEY hKey, DWORD dwIndex, Extbyte * lpValueName, LPDWORD lpcbValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegLoadKey
#define RegLoadKey error_use_qxeRegLoadKey_or_RegLoadKeyA_and_RegLoadKeyW
#endif
LONG
 qxeRegLoadKey (HKEY hKey, const Extbyte * lpSubKey, const Extbyte * lpFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegOpenKey
#define RegOpenKey error_use_qxeRegOpenKey_or_RegOpenKeyA_and_RegOpenKeyW
#endif
LONG
 qxeRegOpenKey (HKEY hKey, const Extbyte * lpSubKey, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegOpenKeyEx
#define RegOpenKeyEx error_use_qxeRegOpenKeyEx_or_RegOpenKeyExA_and_RegOpenKeyExW
#endif
LONG
 qxeRegOpenKeyEx (HKEY hKey, const Extbyte * lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryInfoKey
#define RegQueryInfoKey error_use_qxeRegQueryInfoKey_or_RegQueryInfoKeyA_and_RegQueryInfoKeyW
#endif
LONG
 qxeRegQueryInfoKey (HKEY hKey, Extbyte * lpClass, LPDWORD lpcbClass, LPDWORD lpReserved, LPDWORD lpcSubKeys, LPDWORD lpcbMaxSubKeyLen, LPDWORD lpcbMaxClassLen, LPDWORD lpcValues, LPDWORD lpcbMaxValueNameLen, LPDWORD lpcbMaxValueLen, LPDWORD lpcbSecurityDescriptor, PFILETIME lpftLastWriteTime);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryValue
#define RegQueryValue error_use_qxeRegQueryValue_or_RegQueryValueA_and_RegQueryValueW
#endif
LONG
 qxeRegQueryValue (HKEY hKey, const Extbyte * lpSubKey, Extbyte * lpValue, PLONG lpcbValue);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryMultipleValues
#define RegQueryMultipleValues error_use_qxeRegQueryMultipleValues_or_RegQueryMultipleValuesA_and_RegQueryMultipleValuesW
#endif
LONG
 qxeRegQueryMultipleValues (HKEY hKey, PVALENTW val_list, DWORD num_vals, Extbyte * lpValueBuf, LPDWORD ldwTotsize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryValueEx
#define RegQueryValueEx error_use_qxeRegQueryValueEx_or_RegQueryValueExA_and_RegQueryValueExW
#endif
LONG
 qxeRegQueryValueEx (HKEY hKey, const Extbyte * lpValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegReplaceKey
#define RegReplaceKey error_use_qxeRegReplaceKey_or_RegReplaceKeyA_and_RegReplaceKeyW
#endif
LONG
 qxeRegReplaceKey (HKEY hKey, const Extbyte * lpSubKey, const Extbyte * lpNewFile, const Extbyte * lpOldFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegRestoreKey
#define RegRestoreKey error_use_qxeRegRestoreKey_or_RegRestoreKeyA_and_RegRestoreKeyW
#endif
LONG
 qxeRegRestoreKey (HKEY hKey, const Extbyte * lpFile, DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegSaveKey
#define RegSaveKey error_use_qxeRegSaveKey_or_RegSaveKeyA_and_RegSaveKeyW
#endif
LONG
 qxeRegSaveKey (HKEY hKey, const Extbyte * lpFile, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegSetValue
#define RegSetValue error_use_qxeRegSetValue_or_RegSetValueA_and_RegSetValueW
#endif
LONG
 qxeRegSetValue (HKEY hKey, const Extbyte * lpSubKey, DWORD dwType, const Extbyte * lpData, DWORD cbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegSetValueEx
#define RegSetValueEx error_use_qxeRegSetValueEx_or_RegSetValueExA_and_RegSetValueExW
#endif
LONG
 qxeRegSetValueEx (HKEY hKey, const Extbyte * lpValueName, DWORD Reserved, DWORD dwType, CONST BYTE* lpData, DWORD cbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegUnLoadKey
#define RegUnLoadKey error_use_qxeRegUnLoadKey_or_RegUnLoadKeyA_and_RegUnLoadKeyW
#endif
LONG
 qxeRegUnLoadKey (HKEY hKey, const Extbyte * lpSubKey);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef InitiateSystemShutdown
#define InitiateSystemShutdown error_use_qxeInitiateSystemShutdown_or_InitiateSystemShutdownA_and_InitiateSystemShutdownW
#endif
BOOL
 qxeInitiateSystemShutdown (Extbyte * lpMachineName, Extbyte * lpMessage, DWORD dwTimeout, BOOL bForceAppsClosed, BOOL bRebootAfterShutdown);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AbortSystemShutdown
#define AbortSystemShutdown error_use_qxeAbortSystemShutdown_or_AbortSystemShutdownA_and_AbortSystemShutdownW
#endif
BOOL
 qxeAbortSystemShutdown (Extbyte * lpMachineName);


/* Processing file WINNLS.H */

#undef GetCPInfoEx
#define GetCPInfoEx error_not_used__not_examined_yet

#undef CompareString
#define CompareString error_not_used__not_examined_yet

#undef LCMapString
#define LCMapString error_not_used__not_examined_yet

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetLocaleInfo
#define GetLocaleInfo error_use_qxeGetLocaleInfo_or_GetLocaleInfoA_and_GetLocaleInfoW
#endif
int qxeGetLocaleInfo (LCID Locale, LCTYPE LCType, Extbyte * lpLCData, int cchData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetLocaleInfo
#define SetLocaleInfo error_use_qxeSetLocaleInfo_or_SetLocaleInfoA_and_SetLocaleInfoW
#endif
BOOL qxeSetLocaleInfo (LCID Locale, LCTYPE LCType, const Extbyte * lpLCData);

#undef GetTimeFormat
#define GetTimeFormat error_not_used__not_examined_yet

#undef GetDateFormat
#define GetDateFormat error_not_used__not_examined_yet

#undef GetNumberFormat
#define GetNumberFormat error_not_used__not_examined_yet

#undef GetCurrencyFormat
#define GetCurrencyFormat error_not_used__not_examined_yet

#undef EnumCalendarInfo
#define EnumCalendarInfo error_not_used__not_examined_yet

#undef EnumCalendarInfoEx
#define EnumCalendarInfoEx error_not_used__not_examined_yet

#undef EnumTimeFormats
#define EnumTimeFormats error_not_used__not_examined_yet

#undef EnumDateFormats
#define EnumDateFormats error_not_used__not_examined_yet

#undef EnumDateFormatsEx
#define EnumDateFormatsEx error_not_used__not_examined_yet

#undef GetStringTypeEx
#define GetStringTypeEx error_not_used__not_examined_yet

#undef GetStringType
#define GetStringType error_no_such_fun__A_and_W_versions_have_different_nos__of_args

#undef FoldString
#define FoldString error_not_used__not_examined_yet

#undef EnumSystemLocales
#define EnumSystemLocales error_not_used__not_examined_yet

#undef EnumSystemCodePages
#define EnumSystemCodePages error_not_used__not_examined_yet


/* Processing file IME.H */

#undef SendIMEMessageEx
#define SendIMEMessageEx error_obsolete__no_docs_available


/* Processing file WINGDI.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AddFontResource
#define AddFontResource error_use_qxeAddFontResource_or_AddFontResourceA_and_AddFontResourceW
#endif
int qxeAddFontResource (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyMetaFile
#define CopyMetaFile error_use_qxeCopyMetaFile_or_CopyMetaFileA_and_CopyMetaFileW
#endif
HMETAFILE qxeCopyMetaFile (HMETAFILE arg1, const Extbyte * arg2);

/* Skipping CreateDC because split-sized DEVMODE */

/* Skipping CreateFontIndirect because split-sized LOGFONT */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFont
#define CreateFont error_use_qxeCreateFont_or_CreateFontA_and_CreateFontW
#endif
HFONT qxeCreateFont (int arg1, int arg2, int arg3, int arg4, int arg5, DWORD arg6, DWORD arg7, DWORD arg8, DWORD arg9, DWORD arg10, DWORD arg11, DWORD arg12, DWORD arg13, const Extbyte * arg14);

/* Skipping CreateIC because split-sized DEVMODE */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMetaFile
#define CreateMetaFile error_use_qxeCreateMetaFile_or_CreateMetaFileA_and_CreateMetaFileW
#endif
HDC qxeCreateMetaFile (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateScalableFontResource
#define CreateScalableFontResource error_use_qxeCreateScalableFontResource_or_CreateScalableFontResourceA_and_CreateScalableFontResourceW
#endif
BOOL qxeCreateScalableFontResource (DWORD arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4);

/* Skipping DeviceCapabilities because split-sized DEVMODE */

/* Skipping EnumFontFamiliesEx because split-complex FONTENUMPROC; NT 4.0+ only */

#undef EnumFontFamilies
#define EnumFontFamilies error_split_complex_FONTENUMPROC

#undef EnumFonts
#define EnumFonts error_split_complex_FONTENUMPROC

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharWidth
#define GetCharWidth error_use_qxeGetCharWidth_or_GetCharWidthA_and_GetCharWidthW
#endif
BOOL qxeGetCharWidth (HDC arg1, UINT arg2, UINT arg3, LPINT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharWidth32
#define GetCharWidth32 error_use_qxeGetCharWidth32_or_GetCharWidth32A_and_GetCharWidth32W
#endif
BOOL qxeGetCharWidth32 (HDC arg1, UINT arg2, UINT arg3, LPINT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharWidthFloat
#define GetCharWidthFloat error_use_qxeGetCharWidthFloat_or_GetCharWidthFloatA_and_GetCharWidthFloatW
#endif
BOOL   qxeGetCharWidthFloat (HDC arg1, UINT arg2, UINT arg3, PFLOAT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharABCWidths
#define GetCharABCWidths error_use_qxeGetCharABCWidths_or_GetCharABCWidthsA_and_GetCharABCWidthsW
#endif
BOOL   qxeGetCharABCWidths (HDC arg1, UINT arg2, UINT arg3, LPABC arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharABCWidthsFloat
#define GetCharABCWidthsFloat error_use_qxeGetCharABCWidthsFloat_or_GetCharABCWidthsFloatA_and_GetCharABCWidthsFloatW
#endif
BOOL   qxeGetCharABCWidthsFloat (HDC arg1, UINT arg2, UINT arg3, LPABCFLOAT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetGlyphOutline
#define GetGlyphOutline error_use_qxeGetGlyphOutline_or_GetGlyphOutlineA_and_GetGlyphOutlineW
#endif
DWORD qxeGetGlyphOutline (HDC arg1, UINT arg2, UINT arg3, LPGLYPHMETRICS arg4, DWORD arg5, LPVOID arg6, CONST MAT2 * arg7);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMetaFile
#define GetMetaFile error_use_qxeGetMetaFile_or_GetMetaFileA_and_GetMetaFileW
#endif
HMETAFILE qxeGetMetaFile (const Extbyte * arg1);

#undef GetOutlineTextMetrics
#define GetOutlineTextMetrics error_split_sized_LPOUTLINETEXTMETRIC

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextExtentPoint
#define GetTextExtentPoint error_use_qxeGetTextExtentPoint_or_GetTextExtentPointA_and_GetTextExtentPointW
#endif
BOOL   qxeGetTextExtentPoint (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextExtentPoint32
#define GetTextExtentPoint32 error_use_qxeGetTextExtentPoint32_or_GetTextExtentPoint32A_and_GetTextExtentPoint32W
#endif
BOOL   qxeGetTextExtentPoint32 (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextExtentExPoint
#define GetTextExtentExPoint error_use_qxeGetTextExtentExPoint_or_GetTextExtentExPointA_and_GetTextExtentExPointW
#endif
BOOL   qxeGetTextExtentExPoint (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPINT arg5, LPINT arg6, LPSIZE arg7);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharacterPlacement
#define GetCharacterPlacement error_use_qxeGetCharacterPlacement_or_GetCharacterPlacementA_and_GetCharacterPlacementW
#endif
DWORD qxeGetCharacterPlacement (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPGCP_RESULTSW arg5, DWORD arg6);

#undef GetGlyphIndices
#define GetGlyphIndices error_NT_5_0__only

#undef AddFontResourceEx
#define AddFontResourceEx error_NT_5_0__only

#undef RemoveFontResourceEx
#define RemoveFontResourceEx error_NT_5_0__only

#undef CreateFontIndirectEx
#define CreateFontIndirectEx error_split_sized_ENUMLOGFONTEXDV__NT_5_0__only

/* Skipping ResetDC because split-sized DEVMODE */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RemoveFontResource
#define RemoveFontResource error_use_qxeRemoveFontResource_or_RemoveFontResourceA_and_RemoveFontResourceW
#endif
BOOL qxeRemoveFontResource (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyEnhMetaFile
#define CopyEnhMetaFile error_use_qxeCopyEnhMetaFile_or_CopyEnhMetaFileA_and_CopyEnhMetaFileW
#endif
HENHMETAFILE qxeCopyEnhMetaFile (HENHMETAFILE arg1, const Extbyte * arg2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateEnhMetaFile
#define CreateEnhMetaFile error_use_qxeCreateEnhMetaFile_or_CreateEnhMetaFileA_and_CreateEnhMetaFileW
#endif
HDC qxeCreateEnhMetaFile (HDC arg1, const Extbyte * arg2, CONST RECT * arg3, const Extbyte * arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetEnhMetaFile
#define GetEnhMetaFile error_use_qxeGetEnhMetaFile_or_GetEnhMetaFileA_and_GetEnhMetaFileW
#endif
HENHMETAFILE qxeGetEnhMetaFile (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetEnhMetaFileDescription
#define GetEnhMetaFileDescription error_use_qxeGetEnhMetaFileDescription_or_GetEnhMetaFileDescriptionA_and_GetEnhMetaFileDescriptionW
#endif
UINT qxeGetEnhMetaFileDescription (HENHMETAFILE arg1, UINT arg2, Extbyte * arg3);

/* Skipping GetTextMetrics because split-sized LPTEXTMETRIC */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef StartDoc
#define StartDoc error_use_qxeStartDoc_or_StartDocA_and_StartDocW
#endif
int qxeStartDoc (HDC arg1, CONST DOCINFOW * arg2);

/* Skipping GetObject because split-sized LOGFONT */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef TextOut
#define TextOut error_use_qxeTextOut_or_TextOutA_and_TextOutW
#endif
BOOL qxeTextOut (HDC arg1, int arg2, int arg3, const Extbyte * arg4, int arg5);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtTextOut
#define ExtTextOut error_use_qxeExtTextOut_or_ExtTextOutA_and_ExtTextOutW
#endif
BOOL qxeExtTextOut (HDC arg1, int arg2, int arg3, UINT arg4, CONST RECT * arg5, const Extbyte * arg6, UINT arg7, CONST INT * arg8);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PolyTextOut
#define PolyTextOut error_use_qxePolyTextOut_or_PolyTextOutA_and_PolyTextOutW
#endif
BOOL qxePolyTextOut (HDC arg1, CONST POLYTEXTW * arg2, int arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextFace
#define GetTextFace error_use_qxeGetTextFace_or_GetTextFaceA_and_GetTextFaceW
#endif
int qxeGetTextFace (HDC arg1, int arg2, Extbyte * arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetKerningPairs
#define GetKerningPairs error_use_qxeGetKerningPairs_or_GetKerningPairsA_and_GetKerningPairsW
#endif
DWORD qxeGetKerningPairs (HDC arg1, DWORD arg2, LPKERNINGPAIR arg3);

#undef GetLogColorSpace
#define GetLogColorSpace error_split_sized_LPLOGCOLORSPACE__NT_4_0__only

#undef CreateColorSpace
#define CreateColorSpace error_split_sized_LPLOGCOLORSPACE__NT_4_0__only

/* Skipping GetICMProfile because NT 4.0+ only, error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetICMProfile
#define SetICMProfile error_use_qxeSetICMProfile_or_SetICMProfileA_and_SetICMProfileW
#endif
BOOL qxeSetICMProfile (HDC arg1, Extbyte * arg2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumICMProfiles
#define EnumICMProfiles error_use_qxeEnumICMProfiles_or_EnumICMProfilesA_and_EnumICMProfilesW
#endif
int qxeEnumICMProfiles (HDC arg1, ICMENUMPROCW arg2, LPARAM arg3);

/* Skipping UpdateICMRegKey because NT 4.0+ only, error in Cygwin prototype */

#undef wglUseFontBitmaps
#define wglUseFontBitmaps error_causes_link_error

#undef wglUseFontOutlines
#define wglUseFontOutlines error_causes_link_error


/* Processing file SHLOBJ.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHGetPathFromIDList
#define SHGetPathFromIDList error_use_qxeSHGetPathFromIDList_or_SHGetPathFromIDListA_and_SHGetPathFromIDListW
#endif
BOOL qxeSHGetPathFromIDList (LPCITEMIDLIST pidl, Extbyte * pszPath);

/* Skipping SHGetSpecialFolderPath because error in Cygwin prototype, missing from Cygwin libraries */

/* Skipping SHBrowseForFolder because need to intercept callback for SendMessage */

/* Skipping SHGetDataFromIDList because split-sized WIN32_FIND_DATA or split-simple NETRESOURCE, missing from Cygwin libraries */


/* Processing file COMMDLG.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetOpenFileName
#define GetOpenFileName error_use_qxeGetOpenFileName_or_GetOpenFileNameA_and_GetOpenFileNameW
#endif
BOOL   qxeGetOpenFileName (LPOPENFILENAMEW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetSaveFileName
#define GetSaveFileName error_use_qxeGetSaveFileName_or_GetSaveFileNameA_and_GetSaveFileNameW
#endif
BOOL   qxeGetSaveFileName (LPOPENFILENAMEW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileTitle
#define GetFileTitle error_use_qxeGetFileTitle_or_GetFileTitleA_and_GetFileTitleW
#endif
short  qxeGetFileTitle (const Extbyte * arg1, Extbyte * arg2, WORD arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ChooseColor
#define ChooseColor error_use_qxeChooseColor_or_ChooseColorA_and_ChooseColorW
#endif
BOOL   qxeChooseColor (LPCHOOSECOLORW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindText
#define FindText error_use_qxeFindText_or_FindTextA_and_FindTextW
#endif
HWND   qxeFindText (LPFINDREPLACEW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReplaceText
#define ReplaceText error_use_qxeReplaceText_or_ReplaceTextA_and_ReplaceTextW
#endif
HWND   qxeReplaceText (LPFINDREPLACEW arg1);

#undef AfxReplaceText
#define AfxReplaceText error_mac_only

#undef ChooseFont
#define ChooseFont error_split_sized_LPLOGFONT_in_LPCHOOSEFONT

/* Skipping PrintDlg because LPPRINTDLG with split-sized DEVMODE handle */

/* Skipping PageSetupDlg because LPPAGESETUPDLG with split-sized DEVMODE handle */


/* Processing file IMM.H */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmInstallIME
#define ImmInstallIME error_use_qxeImmInstallIME_or_ImmInstallIMEA_and_ImmInstallIMEW
#endif
HKL qxeImmInstallIME (const Extbyte * lpszIMEFileName, const Extbyte * lpszLayoutText);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetDescription
#define ImmGetDescription error_use_qxeImmGetDescription_or_ImmGetDescriptionA_and_ImmGetDescriptionW
#endif
UINT qxeImmGetDescription (HKL arg1, Extbyte * arg2, UINT uBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetIMEFileName
#define ImmGetIMEFileName error_use_qxeImmGetIMEFileName_or_ImmGetIMEFileNameA_and_ImmGetIMEFileNameW
#endif
UINT qxeImmGetIMEFileName (HKL arg1, Extbyte * arg2, UINT uBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCompositionString
#define ImmGetCompositionString error_use_qxeImmGetCompositionString_or_ImmGetCompositionStringA_and_ImmGetCompositionStringW
#endif
LONG qxeImmGetCompositionString (HIMC arg1, DWORD arg2, LPVOID arg3, DWORD arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmSetCompositionString
#define ImmSetCompositionString error_use_qxeImmSetCompositionString_or_ImmSetCompositionStringA_and_ImmSetCompositionStringW
#endif
BOOL qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPCVOID lpComp, DWORD arg4, LPCVOID lpRead, DWORD arg6);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCandidateListCount
#define ImmGetCandidateListCount error_use_qxeImmGetCandidateListCount_or_ImmGetCandidateListCountA_and_ImmGetCandidateListCountW
#endif
DWORD qxeImmGetCandidateListCount (HIMC arg1, LPDWORD lpdwListCount);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCandidateList
#define ImmGetCandidateList error_use_qxeImmGetCandidateList_or_ImmGetCandidateListA_and_ImmGetCandidateListW
#endif
DWORD qxeImmGetCandidateList (HIMC arg1, DWORD deIndex, LPCANDIDATELIST arg3, DWORD dwBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetGuideLine
#define ImmGetGuideLine error_use_qxeImmGetGuideLine_or_ImmGetGuideLineA_and_ImmGetGuideLineW
#endif
DWORD qxeImmGetGuideLine (HIMC arg1, DWORD dwIndex, Extbyte * arg3, DWORD dwBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
/* Skipping ImmGetCompositionFont because split-sized LOGFONT */
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
/* Skipping ImmSetCompositionFont because split-sized LOGFONT */
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmConfigureIME
#define ImmConfigureIME error_use_qxeImmConfigureIME_or_ImmConfigureIMEA_and_ImmConfigureIMEW
#endif
BOOL qxeImmConfigureIME (HKL arg1, HWND arg2, DWORD arg3, LPVOID arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmEscape
#define ImmEscape error_use_qxeImmEscape_or_ImmEscapeA_and_ImmEscapeW
#endif
LRESULT qxeImmEscape (HKL arg1, HIMC arg2, UINT arg3, LPVOID arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetConversionList
#define ImmGetConversionList error_use_qxeImmGetConversionList_or_ImmGetConversionListA_and_ImmGetConversionListW
#endif
DWORD qxeImmGetConversionList (HKL arg1, HIMC arg2, const Extbyte * arg3, LPCANDIDATELIST arg4, DWORD dwBufLen, UINT uFlag);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmIsUIMessage
#define ImmIsUIMessage error_use_qxeImmIsUIMessage_or_ImmIsUIMessageA_and_ImmIsUIMessageW
#endif
BOOL qxeImmIsUIMessage (HWND arg1, UINT arg2, WPARAM arg3, LPARAM arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmRegisterWord
#define ImmRegisterWord error_use_qxeImmRegisterWord_or_ImmRegisterWordA_and_ImmRegisterWordW
#endif
BOOL qxeImmRegisterWord (HKL arg1, const Extbyte * lpszReading, DWORD arg3, const Extbyte * lpszRegister);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmUnregisterWord
#define ImmUnregisterWord error_use_qxeImmUnregisterWord_or_ImmUnregisterWordA_and_ImmUnregisterWordW
#endif
BOOL qxeImmUnregisterWord (HKL arg1, const Extbyte * lpszReading, DWORD arg3, const Extbyte * lpszUnregister);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ImmGetRegisterWordStyle
#define ImmGetRegisterWordStyle error_split_sized_STYLEBUF
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmEnumRegisterWord
#define ImmEnumRegisterWord error_use_qxeImmEnumRegisterWord_or_ImmEnumRegisterWordA_and_ImmEnumRegisterWordW
#endif
UINT qxeImmEnumRegisterWord (HKL arg1, REGISTERWORDENUMPROCW arg2, const Extbyte * lpszReading, DWORD arg4, const Extbyte * lpszRegister, LPVOID arg6);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ImmGetImeMenuItems
#define ImmGetImeMenuItems error_split_sized_IMEMENUITEMINFO
#endif /* defined (HAVE_MS_WINDOWS) */


/* Processing file WINBASE.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetBinaryType
#define GetBinaryType error_use_qxeGetBinaryType_or_GetBinaryTypeA_and_GetBinaryTypeW
#endif
BOOL qxeGetBinaryType (const Extbyte * lpApplicationName, LPDWORD lpBinaryType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetShortPathName
#define GetShortPathName error_use_qxeGetShortPathName_or_GetShortPathNameA_and_GetShortPathNameW
#endif
DWORD qxeGetShortPathName (const Extbyte * lpszLongPath, Extbyte * lpszShortPath, DWORD cchBuffer);

#undef GetLongPathName
#define GetLongPathName error_Win98_2K__only

/* Skipping GetEnvironmentStrings because misnamed ANSI version of the function */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FreeEnvironmentStrings
#define FreeEnvironmentStrings error_use_qxeFreeEnvironmentStrings_or_FreeEnvironmentStringsA_and_FreeEnvironmentStringsW
#endif
BOOL qxeFreeEnvironmentStrings (Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FormatMessage
#define FormatMessage error_use_qxeFormatMessage_or_FormatMessageA_and_FormatMessageW
#endif
DWORD qxeFormatMessage (DWORD dwFlags, LPCVOID lpSource, DWORD dwMessageId, DWORD dwLanguageId, Extbyte * lpBuffer, DWORD nSize, va_list * Arguments);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMailslot
#define CreateMailslot error_use_qxeCreateMailslot_or_CreateMailslotA_and_CreateMailslotW
#endif
HANDLE qxeCreateMailslot (const Extbyte * lpName, DWORD nMaxMessageSize, DWORD lReadTimeout, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#if !defined (CYGWIN_HEADERS)
#undef EncryptFile
#define EncryptFile error_Win2K__only
#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)
#undef DecryptFile
#define DecryptFile error_Win2K__only
#endif /* !defined (CYGWIN_HEADERS) */

#undef OpenRaw
#define OpenRaw error_error__The_procedure_entry_point_OpenRawW_could_not_be_located_in_the_dynamic_link_library_ADVAPI32_dll__

#undef QueryRecoveryAgents
#define QueryRecoveryAgents error_split_sized_LPRECOVERY_AGENT_INFORMATION

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcmp
#define lstrcmp error_use_qxelstrcmp_or_lstrcmpA_and_lstrcmpW
#endif
int qxelstrcmp (const Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcmpi
#define lstrcmpi error_use_qxelstrcmpi_or_lstrcmpiA_and_lstrcmpiW
#endif
int qxelstrcmpi (const Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcpyn
#define lstrcpyn error_use_qxelstrcpyn_or_lstrcpynA_and_lstrcpynW
#endif
Extbyte * qxelstrcpyn (Extbyte * lpString1, const Extbyte * lpString2, int iMaxLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcpy
#define lstrcpy error_use_qxelstrcpy_or_lstrcpyA_and_lstrcpyW
#endif
Extbyte * qxelstrcpy (Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcat
#define lstrcat error_use_qxelstrcat_or_lstrcatA_and_lstrcatW
#endif
Extbyte * qxelstrcat (Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrlen
#define lstrlen error_use_qxelstrlen_or_lstrlenA_and_lstrlenW
#endif
int qxelstrlen (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMutex
#define CreateMutex error_use_qxeCreateMutex_or_CreateMutexA_and_CreateMutexW
#endif
HANDLE qxeCreateMutex (LPSECURITY_ATTRIBUTES lpMutexAttributes, BOOL bInitialOwner, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenMutex
#define OpenMutex error_use_qxeOpenMutex_or_OpenMutexA_and_OpenMutexW
#endif
HANDLE qxeOpenMutex (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateEvent
#define CreateEvent error_use_qxeCreateEvent_or_CreateEventA_and_CreateEventW
#endif
HANDLE qxeCreateEvent (LPSECURITY_ATTRIBUTES lpEventAttributes, BOOL bManualReset, BOOL bInitialState, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenEvent
#define OpenEvent error_use_qxeOpenEvent_or_OpenEventA_and_OpenEventW
#endif
HANDLE qxeOpenEvent (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateSemaphore
#define CreateSemaphore error_use_qxeCreateSemaphore_or_CreateSemaphoreA_and_CreateSemaphoreW
#endif
HANDLE qxeCreateSemaphore (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenSemaphore
#define OpenSemaphore error_use_qxeOpenSemaphore_or_OpenSemaphoreA_and_OpenSemaphoreW
#endif
HANDLE qxeOpenSemaphore (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWaitableTimer
#define CreateWaitableTimer error_use_qxeCreateWaitableTimer_or_CreateWaitableTimerA_and_CreateWaitableTimerW
#endif
HANDLE qxeCreateWaitableTimer (LPSECURITY_ATTRIBUTES lpTimerAttributes, BOOL bManualReset, const Extbyte * lpTimerName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenWaitableTimer
#define OpenWaitableTimer error_use_qxeOpenWaitableTimer_or_OpenWaitableTimerA_and_OpenWaitableTimerW
#endif
HANDLE qxeOpenWaitableTimer (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpTimerName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFileMapping
#define CreateFileMapping error_use_qxeCreateFileMapping_or_CreateFileMappingA_and_CreateFileMappingW
#endif
HANDLE qxeCreateFileMapping (HANDLE hFile, LPSECURITY_ATTRIBUTES lpFileMappingAttributes, DWORD flProtect, DWORD dwMaximumSizeHigh, DWORD dwMaximumSizeLow, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenFileMapping
#define OpenFileMapping error_use_qxeOpenFileMapping_or_OpenFileMappingA_and_OpenFileMappingW
#endif
HANDLE qxeOpenFileMapping (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetLogicalDriveStrings
#define GetLogicalDriveStrings error_use_qxeGetLogicalDriveStrings_or_GetLogicalDriveStringsA_and_GetLogicalDriveStringsW
#endif
DWORD qxeGetLogicalDriveStrings (DWORD nBufferLength, Extbyte * lpBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadLibrary
#define LoadLibrary error_use_qxeLoadLibrary_or_LoadLibraryA_and_LoadLibraryW
#endif
HMODULE qxeLoadLibrary (const Extbyte * lpLibFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadLibraryEx
#define LoadLibraryEx error_use_qxeLoadLibraryEx_or_LoadLibraryExA_and_LoadLibraryExW
#endif
HMODULE qxeLoadLibraryEx (const Extbyte * lpLibFileName, HANDLE hFile, DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetModuleFileName
#define GetModuleFileName error_use_qxeGetModuleFileName_or_GetModuleFileNameA_and_GetModuleFileNameW
#endif
DWORD qxeGetModuleFileName (HMODULE hModule, Extbyte * lpFilename, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetModuleHandle
#define GetModuleHandle error_use_qxeGetModuleHandle_or_GetModuleHandleA_and_GetModuleHandleW
#endif
HMODULE qxeGetModuleHandle (const Extbyte * lpModuleName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateProcess
#define CreateProcess error_use_qxeCreateProcess_or_CreateProcessA_and_CreateProcessW
#endif
BOOL qxeCreateProcess (const Extbyte * lpApplicationName, Extbyte * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, const Extbyte * lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FatalAppExit
#define FatalAppExit error_use_qxeFatalAppExit_or_FatalAppExitA_and_FatalAppExitW
#endif
VOID qxeFatalAppExit (UINT uAction, const Extbyte * lpMessageText);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetStartupInfo
#define GetStartupInfo error_use_qxeGetStartupInfo_or_GetStartupInfoA_and_GetStartupInfoW
#endif
VOID qxeGetStartupInfo (LPSTARTUPINFOW lpStartupInfo);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCommandLine
#define GetCommandLine error_use_qxeGetCommandLine_or_GetCommandLineA_and_GetCommandLineW
#endif
Extbyte * qxeGetCommandLine (void);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetEnvironmentVariable
#define GetEnvironmentVariable error_use_qxeGetEnvironmentVariable_or_GetEnvironmentVariableA_and_GetEnvironmentVariableW
#endif
DWORD qxeGetEnvironmentVariable (const Extbyte * lpName, Extbyte * lpBuffer, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetEnvironmentVariable
#define SetEnvironmentVariable error_use_qxeSetEnvironmentVariable_or_SetEnvironmentVariableA_and_SetEnvironmentVariableW
#endif
BOOL qxeSetEnvironmentVariable (const Extbyte * lpName, const Extbyte * lpValue);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExpandEnvironmentStrings
#define ExpandEnvironmentStrings error_use_qxeExpandEnvironmentStrings_or_ExpandEnvironmentStringsA_and_ExpandEnvironmentStringsW
#endif
DWORD qxeExpandEnvironmentStrings (const Extbyte * lpSrc, Extbyte * lpDst, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OutputDebugString
#define OutputDebugString error_use_qxeOutputDebugString_or_OutputDebugStringA_and_OutputDebugStringW
#endif
VOID qxeOutputDebugString (const Extbyte * lpOutputString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindResource
#define FindResource error_use_qxeFindResource_or_FindResourceA_and_FindResourceW
#endif
HRSRC qxeFindResource (HMODULE hModule, const Extbyte * lpName, const Extbyte * lpType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindResourceEx
#define FindResourceEx error_use_qxeFindResourceEx_or_FindResourceExA_and_FindResourceExW
#endif
HRSRC qxeFindResourceEx (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, WORD wLanguage);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceTypes
#define EnumResourceTypes error_use_qxeEnumResourceTypes_or_EnumResourceTypesA_and_EnumResourceTypesW
#endif
BOOL qxeEnumResourceTypes (HMODULE hModule, ENUMRESTYPEPROC lpEnumFunc, LONG lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceNames
#define EnumResourceNames error_use_qxeEnumResourceNames_or_EnumResourceNamesA_and_EnumResourceNamesW
#endif
BOOL qxeEnumResourceNames (HMODULE hModule, const Extbyte * lpType, ENUMRESNAMEPROC lpEnumFunc, LONG lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceLanguages
#define EnumResourceLanguages error_use_qxeEnumResourceLanguages_or_EnumResourceLanguagesA_and_EnumResourceLanguagesW
#endif
BOOL qxeEnumResourceLanguages (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, ENUMRESLANGPROC lpEnumFunc, LONG lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BeginUpdateResource
#define BeginUpdateResource error_use_qxeBeginUpdateResource_or_BeginUpdateResourceA_and_BeginUpdateResourceW
#endif
HANDLE qxeBeginUpdateResource (const Extbyte * pFileName, BOOL bDeleteExistingResources);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef UpdateResource
#define UpdateResource error_use_qxeUpdateResource_or_UpdateResourceA_and_UpdateResourceW
#endif
BOOL qxeUpdateResource (HANDLE hUpdate, const Extbyte * lpType, const Extbyte * lpName, WORD wLanguage, LPVOID lpData, DWORD cbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EndUpdateResource
#define EndUpdateResource error_use_qxeEndUpdateResource_or_EndUpdateResourceA_and_EndUpdateResourceW
#endif
BOOL qxeEndUpdateResource (HANDLE hUpdate, BOOL fDiscard);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GlobalAddAtom
#define GlobalAddAtom error_use_qxeGlobalAddAtom_or_GlobalAddAtomA_and_GlobalAddAtomW
#endif
ATOM qxeGlobalAddAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GlobalFindAtom
#define GlobalFindAtom error_use_qxeGlobalFindAtom_or_GlobalFindAtomA_and_GlobalFindAtomW
#endif
ATOM qxeGlobalFindAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GlobalGetAtomName
#define GlobalGetAtomName error_use_qxeGlobalGetAtomName_or_GlobalGetAtomNameA_and_GlobalGetAtomNameW
#endif
UINT qxeGlobalGetAtomName (ATOM nAtom, Extbyte * lpBuffer, int nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AddAtom
#define AddAtom error_use_qxeAddAtom_or_AddAtomA_and_AddAtomW
#endif
ATOM qxeAddAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindAtom
#define FindAtom error_use_qxeFindAtom_or_FindAtomA_and_FindAtomW
#endif
ATOM qxeFindAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetAtomName
#define GetAtomName error_use_qxeGetAtomName_or_GetAtomNameA_and_GetAtomNameW
#endif
UINT qxeGetAtomName (ATOM nAtom, Extbyte * lpBuffer, int nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProfileInt
#define GetProfileInt error_use_qxeGetProfileInt_or_GetProfileIntA_and_GetProfileIntW
#endif
UINT qxeGetProfileInt (const Extbyte * lpAppName, const Extbyte * lpKeyName, INT nDefault);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProfileString
#define GetProfileString error_use_qxeGetProfileString_or_GetProfileStringA_and_GetProfileStringW
#endif
DWORD qxeGetProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpDefault, Extbyte * lpReturnedString, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteProfileString
#define WriteProfileString error_use_qxeWriteProfileString_or_WriteProfileStringA_and_WriteProfileStringW
#endif
BOOL qxeWriteProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProfileSection
#define GetProfileSection error_use_qxeGetProfileSection_or_GetProfileSectionA_and_GetProfileSectionW
#endif
DWORD qxeGetProfileSection (const Extbyte * lpAppName, Extbyte * lpReturnedString, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteProfileSection
#define WriteProfileSection error_use_qxeWriteProfileSection_or_WriteProfileSectionA_and_WriteProfileSectionW
#endif
BOOL qxeWriteProfileSection (const Extbyte * lpAppName, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileInt
#define GetPrivateProfileInt error_use_qxeGetPrivateProfileInt_or_GetPrivateProfileIntA_and_GetPrivateProfileIntW
#endif
UINT qxeGetPrivateProfileInt (const Extbyte * lpAppName, const Extbyte * lpKeyName, INT nDefault, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileString
#define GetPrivateProfileString error_use_qxeGetPrivateProfileString_or_GetPrivateProfileStringA_and_GetPrivateProfileStringW
#endif
DWORD qxeGetPrivateProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpDefault, Extbyte * lpReturnedString, DWORD nSize, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WritePrivateProfileString
#define WritePrivateProfileString error_use_qxeWritePrivateProfileString_or_WritePrivateProfileStringA_and_WritePrivateProfileStringW
#endif
BOOL qxeWritePrivateProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpString, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileSection
#define GetPrivateProfileSection error_use_qxeGetPrivateProfileSection_or_GetPrivateProfileSectionA_and_GetPrivateProfileSectionW
#endif
DWORD qxeGetPrivateProfileSection (const Extbyte * lpAppName, Extbyte * lpReturnedString, DWORD nSize, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WritePrivateProfileSection
#define WritePrivateProfileSection error_use_qxeWritePrivateProfileSection_or_WritePrivateProfileSectionA_and_WritePrivateProfileSectionW
#endif
BOOL qxeWritePrivateProfileSection (const Extbyte * lpAppName, const Extbyte * lpString, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileSectionNames
#define GetPrivateProfileSectionNames error_use_qxeGetPrivateProfileSectionNames_or_GetPrivateProfileSectionNamesA_and_GetPrivateProfileSectionNamesW
#endif
DWORD qxeGetPrivateProfileSectionNames (Extbyte * lpszReturnBuffer, DWORD nSize, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileStruct
#define GetPrivateProfileStruct error_use_qxeGetPrivateProfileStruct_or_GetPrivateProfileStructA_and_GetPrivateProfileStructW
#endif
BOOL qxeGetPrivateProfileStruct (const Extbyte * lpszSection, const Extbyte * lpszKey, LPVOID lpStruct, UINT uSizeStruct, const Extbyte * szFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WritePrivateProfileStruct
#define WritePrivateProfileStruct error_use_qxeWritePrivateProfileStruct_or_WritePrivateProfileStructA_and_WritePrivateProfileStructW
#endif
BOOL qxeWritePrivateProfileStruct (const Extbyte * lpszSection, const Extbyte * lpszKey, LPVOID lpStruct, UINT uSizeStruct, const Extbyte * szFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDriveType
#define GetDriveType error_use_qxeGetDriveType_or_GetDriveTypeA_and_GetDriveTypeW
#endif
UINT qxeGetDriveType (const Extbyte * lpRootPathName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetSystemDirectory
#define GetSystemDirectory error_use_qxeGetSystemDirectory_or_GetSystemDirectoryA_and_GetSystemDirectoryW
#endif
UINT qxeGetSystemDirectory (Extbyte * lpBuffer, UINT uSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTempPath
#define GetTempPath error_use_qxeGetTempPath_or_GetTempPathA_and_GetTempPathW
#endif
DWORD qxeGetTempPath (DWORD nBufferLength, Extbyte * lpBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTempFileName
#define GetTempFileName error_use_qxeGetTempFileName_or_GetTempFileNameA_and_GetTempFileNameW
#endif
UINT qxeGetTempFileName (const Extbyte * lpPathName, const Extbyte * lpPrefixString, UINT uUnique, Extbyte * lpTempFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowsDirectory
#define GetWindowsDirectory error_use_qxeGetWindowsDirectory_or_GetWindowsDirectoryA_and_GetWindowsDirectoryW
#endif
UINT qxeGetWindowsDirectory (Extbyte * lpBuffer, UINT uSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetCurrentDirectory
#define SetCurrentDirectory error_use_qxeSetCurrentDirectory_or_SetCurrentDirectoryA_and_SetCurrentDirectoryW
#endif
BOOL qxeSetCurrentDirectory (const Extbyte * lpPathName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCurrentDirectory
#define GetCurrentDirectory error_use_qxeGetCurrentDirectory_or_GetCurrentDirectoryA_and_GetCurrentDirectoryW
#endif
DWORD qxeGetCurrentDirectory (DWORD nBufferLength, Extbyte * lpBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDiskFreeSpace
#define GetDiskFreeSpace error_use_qxeGetDiskFreeSpace_or_GetDiskFreeSpaceA_and_GetDiskFreeSpaceW
#endif
BOOL qxeGetDiskFreeSpace (const Extbyte * lpRootPathName, LPDWORD lpSectorsPerCluster, LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, LPDWORD lpTotalNumberOfClusters);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDiskFreeSpaceEx
#define GetDiskFreeSpaceEx error_use_qxeGetDiskFreeSpaceEx_or_GetDiskFreeSpaceExA_and_GetDiskFreeSpaceExW
#endif
BOOL qxeGetDiskFreeSpaceEx (const Extbyte * lpDirectoryName, PULARGE_INTEGER lpFreeBytesAvailableToCaller, PULARGE_INTEGER lpTotalNumberOfBytes, PULARGE_INTEGER lpTotalNumberOfFreeBytes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDirectory
#define CreateDirectory error_use_qxeCreateDirectory_or_CreateDirectoryA_and_CreateDirectoryW
#endif
BOOL qxeCreateDirectory (const Extbyte * lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDirectoryEx
#define CreateDirectoryEx error_use_qxeCreateDirectoryEx_or_CreateDirectoryExA_and_CreateDirectoryExW
#endif
BOOL qxeCreateDirectoryEx (const Extbyte * lpTemplateDirectory, const Extbyte * lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RemoveDirectory
#define RemoveDirectory error_use_qxeRemoveDirectory_or_RemoveDirectoryA_and_RemoveDirectoryW
#endif
BOOL qxeRemoveDirectory (const Extbyte * lpPathName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFullPathName
#define GetFullPathName error_use_qxeGetFullPathName_or_GetFullPathNameA_and_GetFullPathNameW
#endif
DWORD qxeGetFullPathName (const Extbyte * lpFileName, DWORD nBufferLength, Extbyte * lpBuffer, Extbyte * * lpFilePart);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefineDosDevice
#define DefineDosDevice error_use_qxeDefineDosDevice_or_DefineDosDeviceA_and_DefineDosDeviceW
#endif
BOOL qxeDefineDosDevice (DWORD dwFlags, const Extbyte * lpDeviceName, const Extbyte * lpTargetPath);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef QueryDosDevice
#define QueryDosDevice error_use_qxeQueryDosDevice_or_QueryDosDeviceA_and_QueryDosDeviceW
#endif
DWORD qxeQueryDosDevice (const Extbyte * lpDeviceName, Extbyte * lpTargetPath, DWORD ucchMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFile
#define CreateFile error_use_qxeCreateFile_or_CreateFileA_and_CreateFileW
#endif
HANDLE qxeCreateFile (const Extbyte * lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetFileAttributes
#define SetFileAttributes error_use_qxeSetFileAttributes_or_SetFileAttributesA_and_SetFileAttributesW
#endif
BOOL qxeSetFileAttributes (const Extbyte * lpFileName, DWORD dwFileAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileAttributes
#define GetFileAttributes error_use_qxeGetFileAttributes_or_GetFileAttributesA_and_GetFileAttributesW
#endif
DWORD qxeGetFileAttributes (const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileAttributesEx
#define GetFileAttributesEx error_use_qxeGetFileAttributesEx_or_GetFileAttributesExA_and_GetFileAttributesExW
#endif
BOOL qxeGetFileAttributesEx (const Extbyte * lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCompressedFileSize
#define GetCompressedFileSize error_use_qxeGetCompressedFileSize_or_GetCompressedFileSizeA_and_GetCompressedFileSizeW
#endif
DWORD qxeGetCompressedFileSize (const Extbyte * lpFileName, LPDWORD lpFileSizeHigh);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DeleteFile
#define DeleteFile error_use_qxeDeleteFile_or_DeleteFileA_and_DeleteFileW
#endif
BOOL qxeDeleteFile (const Extbyte * lpFileName);

#undef FindFirstFileEx
#define FindFirstFileEx error_split_sized_LPWIN32_FIND_DATA__not_used__NT_4_0__only

/* Skipping FindFirstFile because split-sized LPWIN32_FIND_DATA */

/* Skipping FindNextFile because split-sized LPWIN32_FIND_DATA */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SearchPath
#define SearchPath error_use_qxeSearchPath_or_SearchPathA_and_SearchPathW
#endif
DWORD qxeSearchPath (const Extbyte * lpPath, const Extbyte * lpFileName, const Extbyte * lpExtension, DWORD nBufferLength, Extbyte * lpBuffer, Extbyte * * lpFilePart);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyFile
#define CopyFile error_use_qxeCopyFile_or_CopyFileA_and_CopyFileW
#endif
BOOL qxeCopyFile (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, BOOL bFailIfExists);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyFileEx
#define CopyFileEx error_use_qxeCopyFileEx_or_CopyFileExA_and_CopyFileExW
#endif
BOOL qxeCopyFileEx (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MoveFile
#define MoveFile error_use_qxeMoveFile_or_MoveFileA_and_MoveFileW
#endif
BOOL qxeMoveFile (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MoveFileEx
#define MoveFileEx error_use_qxeMoveFileEx_or_MoveFileExA_and_MoveFileExW
#endif
BOOL qxeMoveFileEx (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, DWORD dwFlags);

#undef MoveFileWithProgress
#define MoveFileWithProgress error_NT_5_0__only

#undef CreateHardLink
#define CreateHardLink error_NT_5_0__only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateNamedPipe
#define CreateNamedPipe error_use_qxeCreateNamedPipe_or_CreateNamedPipeA_and_CreateNamedPipeW
#endif
HANDLE qxeCreateNamedPipe (const Extbyte * lpName, DWORD dwOpenMode, DWORD dwPipeMode, DWORD nMaxInstances, DWORD nOutBufferSize, DWORD nInBufferSize, DWORD nDefaultTimeOut, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetNamedPipeHandleState
#define GetNamedPipeHandleState error_use_qxeGetNamedPipeHandleState_or_GetNamedPipeHandleStateA_and_GetNamedPipeHandleStateW
#endif
BOOL qxeGetNamedPipeHandleState (HANDLE hNamedPipe, LPDWORD lpState, LPDWORD lpCurInstances, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout, Extbyte * lpUserName, DWORD nMaxUserNameSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CallNamedPipe
#define CallNamedPipe error_use_qxeCallNamedPipe_or_CallNamedPipeA_and_CallNamedPipeW
#endif
BOOL qxeCallNamedPipe (const Extbyte * lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WaitNamedPipe
#define WaitNamedPipe error_use_qxeWaitNamedPipe_or_WaitNamedPipeA_and_WaitNamedPipeW
#endif
BOOL qxeWaitNamedPipe (const Extbyte * lpNamedPipeName, DWORD nTimeOut);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetVolumeLabel
#define SetVolumeLabel error_use_qxeSetVolumeLabel_or_SetVolumeLabelA_and_SetVolumeLabelW
#endif
BOOL qxeSetVolumeLabel (const Extbyte * lpRootPathName, const Extbyte * lpVolumeName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetVolumeInformation
#define GetVolumeInformation error_use_qxeGetVolumeInformation_or_GetVolumeInformationA_and_GetVolumeInformationW
#endif
BOOL qxeGetVolumeInformation (const Extbyte * lpRootPathName, Extbyte * lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, Extbyte * lpFileSystemNameBuffer, DWORD nFileSystemNameSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ClearEventLog
#define ClearEventLog error_use_qxeClearEventLog_or_ClearEventLogA_and_ClearEventLogW
#endif
BOOL qxeClearEventLog (HANDLE hEventLog, const Extbyte * lpBackupFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BackupEventLog
#define BackupEventLog error_use_qxeBackupEventLog_or_BackupEventLogA_and_BackupEventLogW
#endif
BOOL qxeBackupEventLog (HANDLE hEventLog, const Extbyte * lpBackupFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenEventLog
#define OpenEventLog error_use_qxeOpenEventLog_or_OpenEventLogA_and_OpenEventLogW
#endif
HANDLE qxeOpenEventLog (const Extbyte * lpUNCServerName, const Extbyte * lpSourceName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterEventSource
#define RegisterEventSource error_use_qxeRegisterEventSource_or_RegisterEventSourceA_and_RegisterEventSourceW
#endif
HANDLE qxeRegisterEventSource (const Extbyte * lpUNCServerName, const Extbyte * lpSourceName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenBackupEventLog
#define OpenBackupEventLog error_use_qxeOpenBackupEventLog_or_OpenBackupEventLogA_and_OpenBackupEventLogW
#endif
HANDLE qxeOpenBackupEventLog (const Extbyte * lpUNCServerName, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadEventLog
#define ReadEventLog error_use_qxeReadEventLog_or_ReadEventLogA_and_ReadEventLogW
#endif
BOOL qxeReadEventLog (HANDLE hEventLog, DWORD dwReadFlags, DWORD dwRecordOffset, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, DWORD      * pnBytesRead, DWORD      * pnMinNumberOfBytesNeeded);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReportEvent
#define ReportEvent error_use_qxeReportEvent_or_ReportEventA_and_ReportEventW
#endif
BOOL qxeReportEvent (HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID, PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, const Extbyte *   * lpStrings, LPVOID lpRawData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AccessCheckAndAuditAlarm
#define AccessCheckAndAuditAlarm error_use_qxeAccessCheckAndAuditAlarm_or_AccessCheckAndAuditAlarmA_and_AccessCheckAndAuditAlarmW
#endif
BOOL qxeAccessCheckAndAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, Extbyte * ObjectTypeName, Extbyte * ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, DWORD DesiredAccess, PGENERIC_MAPPING GenericMapping, BOOL ObjectCreation, LPDWORD GrantedAccess, LPBOOL AccessStatus, LPBOOL pfGenerateOnClose);

#undef AccessCheckByTypeAndAuditAlarm
#define AccessCheckByTypeAndAuditAlarm error_NT_5_0__only

#undef AccessCheckByTypeResultListAndAuditAlarm
#define AccessCheckByTypeResultListAndAuditAlarm error_NT_5_0__only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectOpenAuditAlarm
#define ObjectOpenAuditAlarm error_use_qxeObjectOpenAuditAlarm_or_ObjectOpenAuditAlarmA_and_ObjectOpenAuditAlarmW
#endif
BOOL qxeObjectOpenAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, Extbyte * ObjectTypeName, Extbyte * ObjectName, PSECURITY_DESCRIPTOR pSecurityDescriptor, HANDLE ClientToken, DWORD DesiredAccess, DWORD GrantedAccess, PPRIVILEGE_SET Privileges, BOOL ObjectCreation, BOOL AccessGranted, LPBOOL GenerateOnClose);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectPrivilegeAuditAlarm
#define ObjectPrivilegeAuditAlarm error_use_qxeObjectPrivilegeAuditAlarm_or_ObjectPrivilegeAuditAlarmA_and_ObjectPrivilegeAuditAlarmW
#endif
BOOL qxeObjectPrivilegeAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, HANDLE ClientToken, DWORD DesiredAccess, PPRIVILEGE_SET Privileges, BOOL AccessGranted);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectCloseAuditAlarm
#define ObjectCloseAuditAlarm error_use_qxeObjectCloseAuditAlarm_or_ObjectCloseAuditAlarmA_and_ObjectCloseAuditAlarmW
#endif
BOOL qxeObjectCloseAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, BOOL GenerateOnClose);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectDeleteAuditAlarm
#define ObjectDeleteAuditAlarm error_use_qxeObjectDeleteAuditAlarm_or_ObjectDeleteAuditAlarmA_and_ObjectDeleteAuditAlarmW
#endif
BOOL qxeObjectDeleteAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, BOOL GenerateOnClose);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PrivilegedServiceAuditAlarm
#define PrivilegedServiceAuditAlarm error_use_qxePrivilegedServiceAuditAlarm_or_PrivilegedServiceAuditAlarmA_and_PrivilegedServiceAuditAlarmW
#endif
BOOL qxePrivilegedServiceAuditAlarm (const Extbyte * SubsystemName, const Extbyte * ServiceName, HANDLE ClientToken, PPRIVILEGE_SET Privileges, BOOL AccessGranted);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetFileSecurity
#define SetFileSecurity error_use_qxeSetFileSecurity_or_SetFileSecurityA_and_SetFileSecurityW
#endif
BOOL qxeSetFileSecurity (const Extbyte * lpFileName, SECURITY_INFORMATION SecurityInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileSecurity
#define GetFileSecurity error_use_qxeGetFileSecurity_or_GetFileSecurityA_and_GetFileSecurityW
#endif
BOOL qxeGetFileSecurity (const Extbyte * lpFileName, SECURITY_INFORMATION RequestedInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor, DWORD nLength, LPDWORD lpnLengthNeeded);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindFirstChangeNotification
#define FindFirstChangeNotification error_use_qxeFindFirstChangeNotification_or_FindFirstChangeNotificationA_and_FindFirstChangeNotificationW
#endif
HANDLE qxeFindFirstChangeNotification (const Extbyte * lpPathName, BOOL bWatchSubtree, DWORD dwNotifyFilter);

#undef ReadDirectoryChanges
#define ReadDirectoryChanges error_Unicode_only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef IsBadStringPtr
#define IsBadStringPtr error_use_qxeIsBadStringPtr_or_IsBadStringPtrA_and_IsBadStringPtrW
#endif
BOOL qxeIsBadStringPtr (const Extbyte * lpsz, UINT ucchMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupAccountSid
#define LookupAccountSid error_use_qxeLookupAccountSid_or_LookupAccountSidA_and_LookupAccountSidW
#endif
BOOL qxeLookupAccountSid (const Extbyte * lpSystemName, PSID Sid, Extbyte * Name, LPDWORD cbName, Extbyte * ReferencedDomainName, LPDWORD cbReferencedDomainName, PSID_NAME_USE peUse);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupAccountName
#define LookupAccountName error_use_qxeLookupAccountName_or_LookupAccountNameA_and_LookupAccountNameW
#endif
BOOL qxeLookupAccountName (const Extbyte * lpSystemName, const Extbyte * lpAccountName, PSID Sid, LPDWORD cbSid, Extbyte * ReferencedDomainName, LPDWORD cbReferencedDomainName, PSID_NAME_USE peUse);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupPrivilegeValue
#define LookupPrivilegeValue error_use_qxeLookupPrivilegeValue_or_LookupPrivilegeValueA_and_LookupPrivilegeValueW
#endif
BOOL qxeLookupPrivilegeValue (const Extbyte * lpSystemName, const Extbyte * lpName, PLUID lpLuid);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupPrivilegeName
#define LookupPrivilegeName error_use_qxeLookupPrivilegeName_or_LookupPrivilegeNameA_and_LookupPrivilegeNameW
#endif
BOOL qxeLookupPrivilegeName (const Extbyte * lpSystemName, PLUID lpLuid, Extbyte * lpName, LPDWORD cbName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupPrivilegeDisplayName
#define LookupPrivilegeDisplayName error_use_qxeLookupPrivilegeDisplayName_or_LookupPrivilegeDisplayNameA_and_LookupPrivilegeDisplayNameW
#endif
BOOL qxeLookupPrivilegeDisplayName (const Extbyte * lpSystemName, const Extbyte * lpName, Extbyte * lpDisplayName, LPDWORD cbDisplayName, LPDWORD lpLanguageId);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BuildCommDCB
#define BuildCommDCB error_use_qxeBuildCommDCB_or_BuildCommDCBA_and_BuildCommDCBW
#endif
BOOL qxeBuildCommDCB (const Extbyte * lpDef, LPDCB lpDCB);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BuildCommDCBAndTimeouts
#define BuildCommDCBAndTimeouts error_use_qxeBuildCommDCBAndTimeouts_or_BuildCommDCBAndTimeoutsA_and_BuildCommDCBAndTimeoutsW
#endif
BOOL qxeBuildCommDCBAndTimeouts (const Extbyte * lpDef, LPDCB lpDCB, LPCOMMTIMEOUTS lpCommTimeouts);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CommConfigDialog
#define CommConfigDialog error_use_qxeCommConfigDialog_or_CommConfigDialogA_and_CommConfigDialogW
#endif
BOOL qxeCommConfigDialog (const Extbyte * lpszName, HWND hWnd, LPCOMMCONFIG lpCC);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDefaultCommConfig
#define GetDefaultCommConfig error_use_qxeGetDefaultCommConfig_or_GetDefaultCommConfigA_and_GetDefaultCommConfigW
#endif
BOOL qxeGetDefaultCommConfig (const Extbyte * lpszName, LPCOMMCONFIG lpCC, LPDWORD lpdwSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetDefaultCommConfig
#define SetDefaultCommConfig error_use_qxeSetDefaultCommConfig_or_SetDefaultCommConfigA_and_SetDefaultCommConfigW
#endif
BOOL qxeSetDefaultCommConfig (const Extbyte * lpszName, LPCOMMCONFIG lpCC, DWORD dwSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetComputerName
#define GetComputerName error_use_qxeGetComputerName_or_GetComputerNameA_and_GetComputerNameW
#endif
BOOL qxeGetComputerName (Extbyte * lpBuffer, LPDWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetComputerName
#define SetComputerName error_use_qxeSetComputerName_or_SetComputerNameA_and_SetComputerNameW
#endif
BOOL qxeSetComputerName (const Extbyte * lpComputerName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetUserName
#define GetUserName error_use_qxeGetUserName_or_GetUserNameA_and_GetUserNameW
#endif
BOOL qxeGetUserName (Extbyte * lpBuffer, LPDWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LogonUser
#define LogonUser error_use_qxeLogonUser_or_LogonUserA_and_LogonUserW
#endif
BOOL qxeLogonUser (Extbyte * lpszUsername, Extbyte * lpszDomain, Extbyte * lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateProcessAsUser
#define CreateProcessAsUser error_use_qxeCreateProcessAsUser_or_CreateProcessAsUserA_and_CreateProcessAsUserW
#endif
BOOL qxeCreateProcessAsUser (HANDLE hToken, const Extbyte * lpApplicationName, Extbyte * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, const Extbyte * lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation);

#undef GetCurrentHwProfile
#define GetCurrentHwProfile error_split_sized_LPHW_PROFILE_INFO__NT_4_0__only

#undef GetVersionEx
#define GetVersionEx error_split_sized_LPOSVERSIONINFO

#undef CreateJobObject
#define CreateJobObject error_NT_5_0__only

#undef OpenJobObject
#define OpenJobObject error_NT_5_0__only


/* Processing file MMSYSTEM.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef sndPlaySound
#define sndPlaySound error_use_qxesndPlaySound_or_sndPlaySoundA_and_sndPlaySoundW
#endif
BOOL qxesndPlaySound (const Extbyte * pszSound, UINT fuSound);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PlaySound
#define PlaySound error_use_qxePlaySound_or_PlaySoundA_and_PlaySoundW
#endif
BOOL qxePlaySound (const Extbyte * pszSound, HMODULE hmod, DWORD fdwSound);

#undef waveOutGetDevCaps
#define waveOutGetDevCaps error_split_sized_LPWAVEOUTCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef waveOutGetErrorText
#define waveOutGetErrorText error_use_qxewaveOutGetErrorText_or_waveOutGetErrorTextA_and_waveOutGetErrorTextW
#endif
MMRESULT qxewaveOutGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef waveInGetDevCaps
#define waveInGetDevCaps error_split_sized_LPWAVEINCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef waveInGetErrorText
#define waveInGetErrorText error_use_qxewaveInGetErrorText_or_waveInGetErrorTextA_and_waveInGetErrorTextW
#endif
MMRESULT qxewaveInGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef midiOutGetDevCaps
#define midiOutGetDevCaps error_split_sized_LPMIDIOUTCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef midiOutGetErrorText
#define midiOutGetErrorText error_use_qxemidiOutGetErrorText_or_midiOutGetErrorTextA_and_midiOutGetErrorTextW
#endif
MMRESULT qxemidiOutGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef midiInGetDevCaps
#define midiInGetDevCaps error_split_sized_LPMIDIOUTCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef midiInGetErrorText
#define midiInGetErrorText error_use_qxemidiInGetErrorText_or_midiInGetErrorTextA_and_midiInGetErrorTextW
#endif
MMRESULT qxemidiInGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef auxGetDevCaps
#define auxGetDevCaps error_split_sized_LPAUXCAPS

#undef mixerGetDevCaps
#define mixerGetDevCaps error_split_sized_LPMIXERCAPS

#undef mixerGetLineInfo
#define mixerGetLineInfo error_split_sized_LPMIXERLINE

#undef mixerGetLineControls
#define mixerGetLineControls error_split_sized_LPMIXERCONTROL

#undef mixerGetControlDetails
#define mixerGetControlDetails error_split_sized_LPMIXERCONTROL_in_LPMIXERLINECONTROLS_in_LPMIXERCONTROLDETAILS

#undef joyGetDevCaps
#define joyGetDevCaps error_split_sized_LPJOYCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioStringToFOURCC
#define mmioStringToFOURCC error_use_qxemmioStringToFOURCC_or_mmioStringToFOURCCA_and_mmioStringToFOURCCW
#endif
FOURCC qxemmioStringToFOURCC (const Extbyte * sz, UINT uFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioInstallIOProc
#define mmioInstallIOProc error_use_qxemmioInstallIOProc_or_mmioInstallIOProcA_and_mmioInstallIOProcW
#endif
LPMMIOPROC qxemmioInstallIOProc (FOURCC fccIOProc, LPMMIOPROC pIOProc, DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioOpen
#define mmioOpen error_use_qxemmioOpen_or_mmioOpenA_and_mmioOpenW
#endif
HMMIO qxemmioOpen (Extbyte * pszFileName, LPMMIOINFO pmmioinfo, DWORD fdwOpen);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioRename
#define mmioRename error_use_qxemmioRename_or_mmioRenameA_and_mmioRenameW
#endif
MMRESULT qxemmioRename (const Extbyte * pszFileName, const Extbyte * pszNewFileName, LPCMMIOINFO pmmioinfo, DWORD fdwRename);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciSendCommand
#define mciSendCommand error_use_qxemciSendCommand_or_mciSendCommandA_and_mciSendCommandW
#endif
MCIERROR qxemciSendCommand (MCIDEVICEID mciId, UINT uMsg, DWORD dwParam1, DWORD dwParam2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciSendString
#define mciSendString error_use_qxemciSendString_or_mciSendStringA_and_mciSendStringW
#endif
MCIERROR qxemciSendString (const Extbyte * lpstrCommand, Extbyte * lpstrReturnString, UINT uReturnLength, HWND hwndCallback);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciGetDeviceID
#define mciGetDeviceID error_use_qxemciGetDeviceID_or_mciGetDeviceIDA_and_mciGetDeviceIDW
#endif
MCIDEVICEID qxemciGetDeviceID (const Extbyte * pszDevice);

#if !defined (MINGW)
#undef mciGetDeviceIDFromElementID
#define mciGetDeviceIDFromElementID error_missing_from_Win98se_version_of_ADVAPI32_dll
#endif /* !defined (MINGW) */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciGetErrorString
#define mciGetErrorString error_use_qxemciGetErrorString_or_mciGetErrorStringA_and_mciGetErrorStringW
#endif
BOOL qxemciGetErrorString (MCIERROR mcierr, Extbyte * pszText, UINT cchText);

