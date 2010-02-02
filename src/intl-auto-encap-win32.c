/* Automatically-generated Unicode-encapsulation file,
   using the command

   ../lib-src/make-mswin-unicode.pl --c-output intl-auto-encap-win32.c --h-output intl-auto-encap-win32.h intl-encap-win32.c

   Do not edit.  See `make-mswin-unicode.pl'.
*/

#include <config.h>
#include "lisp.h"

#include "syswindows.h"


/*----------------------------------------------------------------------*/
/*                       Processing file WINCON.H                       */
/*----------------------------------------------------------------------*/

/* Error if FillConsoleOutputCharacter used: split CHAR */

DWORD
qxeGetConsoleTitle (Extbyte * arg1, DWORD arg2)
{
  if (XEUNICODE_P)
    return GetConsoleTitleW ((LPWSTR) arg1, arg2);
  else
    return GetConsoleTitleA ((LPSTR) arg1, arg2);
}

BOOL
qxePeekConsoleInput (HANDLE arg1, PINPUT_RECORD arg2, DWORD arg3, PDWORD arg4)
{
  if (XEUNICODE_P)
    return PeekConsoleInputW (arg1, arg2, arg3, arg4);
  else
    return PeekConsoleInputA (arg1, arg2, arg3, arg4);
}

BOOL
qxeReadConsole (HANDLE arg1, PVOID arg2, DWORD arg3, PDWORD arg4, PVOID arg5)
{
  if (XEUNICODE_P)
    return ReadConsoleW (arg1, arg2, arg3, arg4, arg5);
  else
    return ReadConsoleA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeReadConsoleInput (HANDLE arg1, PINPUT_RECORD arg2, DWORD arg3, PDWORD arg4)
{
  if (XEUNICODE_P)
    return ReadConsoleInputW (arg1, arg2, arg3, arg4);
  else
    return ReadConsoleInputA (arg1, arg2, arg3, arg4);
}

BOOL
qxeReadConsoleOutputCharacter (HANDLE arg1, Extbyte * arg2, DWORD arg3, COORD arg4, PDWORD arg5)
{
  if (XEUNICODE_P)
    return ReadConsoleOutputCharacterW (arg1, (LPWSTR) arg2, arg3, arg4, arg5);
  else
    return ReadConsoleOutputCharacterA (arg1, (LPSTR) arg2, arg3, arg4, arg5);
}

BOOL
qxeReadConsoleOutput (HANDLE arg1, PCHAR_INFO arg2, COORD arg3, COORD arg4, PSMALL_RECT arg5)
{
  if (XEUNICODE_P)
    return ReadConsoleOutputW (arg1, arg2, arg3, arg4, arg5);
  else
    return ReadConsoleOutputA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeScrollConsoleScreenBuffer (HANDLE arg1, const SMALL_RECT* arg2, const SMALL_RECT* arg3, COORD arg4, const CHAR_INFO* arg5)
{
  if (XEUNICODE_P)
    return ScrollConsoleScreenBufferW (arg1, arg2, arg3, arg4, arg5);
  else
    return ScrollConsoleScreenBufferA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeSetConsoleTitle (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return SetConsoleTitleW ((LPCWSTR) arg1);
  else
    return SetConsoleTitleA ((LPCSTR) arg1);
}

BOOL
qxeWriteConsole (HANDLE arg1, PCVOID arg2, DWORD arg3, PDWORD arg4, PVOID arg5)
{
  if (XEUNICODE_P)
    return WriteConsoleW (arg1, arg2, arg3, arg4, arg5);
  else
    return WriteConsoleA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeWriteConsoleInput (HANDLE arg1, const INPUT_RECORD* arg2, DWORD arg3, PDWORD arg4)
{
  if (XEUNICODE_P)
    return WriteConsoleInputW (arg1, arg2, arg3, arg4);
  else
    return WriteConsoleInputA (arg1, arg2, arg3, arg4);
}

BOOL
qxeWriteConsoleOutput (HANDLE arg1, const CHAR_INFO* arg2, COORD arg3, COORD arg4, PSMALL_RECT arg5)
{
  if (XEUNICODE_P)
    return WriteConsoleOutputW (arg1, arg2, arg3, arg4, arg5);
  else
    return WriteConsoleOutputA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeWriteConsoleOutputCharacter (HANDLE arg1, const Extbyte * arg2, DWORD arg3, COORD arg4, PDWORD arg5)
{
  if (XEUNICODE_P)
    return WriteConsoleOutputCharacterW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return WriteConsoleOutputCharacterA (arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}


/*----------------------------------------------------------------------*/
/*                      Processing file SHELLAPI.H                      */
/*----------------------------------------------------------------------*/

/* Error if CommandLineToArgv used: Unicode-only */

UINT
qxeDragQueryFile (HDROP arg1, UINT arg2, Extbyte * arg3, UINT arg4)
{
  if (XEUNICODE_P)
    return DragQueryFileW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return DragQueryFileA (arg1, arg2, (LPSTR) arg3, arg4);
}

/* NOTE: error arg2, Cygwin prototype, extra const.
   NOTE: Prototype manually overridden.
         Header file claims:
           HICON WINAPI ExtractAssociatedIcon(HINSTANCE,LPCWSTR,PWORD)
         Overridden with:
           HICON ExtractAssociatedIcon(HINSTANCE, LPWSTR, LPWORD)
         Differences in return-type qualifiers, e.g. WINAPI, are not important.
 */
HICON
qxeExtractAssociatedIcon (HINSTANCE arg1, Extbyte * arg2, LPWORD arg3)
{
  if (XEUNICODE_P)
    return ExtractAssociatedIconW (arg1, (LPWSTR) arg2, arg3);
  else
    return ExtractAssociatedIconA (arg1, (LPSTR) arg2, arg3);
}

HICON
qxeExtractIcon (HINSTANCE arg1, const Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return ExtractIconW (arg1, (LPCWSTR) arg2, arg3);
  else
    return ExtractIconA (arg1, (LPCSTR) arg2, arg3);
}

/* NOTE: NT 4.0+ only, former error in Cygwin prototype but no more (Cygwin 1.7, 1-30-10) */
UINT
qxeExtractIconEx (const Extbyte * arg1, int arg2, HICON* arg3, HICON* arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return ExtractIconExW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5);
  else
    return ExtractIconExA ((LPCSTR) arg1, arg2, arg3, arg4, arg5);
}

HINSTANCE
qxeFindExecutable (const Extbyte * arg1, const Extbyte * arg2, Extbyte * arg3)
{
  if (XEUNICODE_P)
    return FindExecutableW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPWSTR) arg3);
  else
    return FindExecutableA ((LPCSTR) arg1, (LPCSTR) arg2, (LPSTR) arg3);
}

/* Error if Shell_NotifyIcon used: split-sized NOTIFYICONDATA, NT 4.0+ only */

int
qxeShellAbout (HWND arg1, const Extbyte * arg2, const Extbyte * arg3, HICON arg4)
{
  if (XEUNICODE_P)
    return ShellAboutW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4);
  else
    return ShellAboutA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4);
}

HINSTANCE
qxeShellExecute (HWND arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4, const Extbyte * arg5, INT arg6)
{
  if (XEUNICODE_P)
    return ShellExecuteW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPCWSTR) arg4, (LPCWSTR) arg5, arg6);
  else
    return ShellExecuteA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPCSTR) arg4, (LPCSTR) arg5, arg6);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeShellExecuteEx (LPSHELLEXECUTEINFOW arg1)
{
  if (XEUNICODE_P)
    return ShellExecuteExW (arg1);
  else
    return ShellExecuteExA ((LPSHELLEXECUTEINFOA) arg1);
}

/* NOTE: NT 4.0+ only */
int
qxeSHFileOperation (LPSHFILEOPSTRUCTW arg1)
{
  if (XEUNICODE_P)
    return SHFileOperationW (arg1);
  else
    return SHFileOperationA ((LPSHFILEOPSTRUCTA) arg1);
}

/* Skipping SHGetFileInfo because split-sized SHFILEINFO, NT 4.0+ only */

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
HRESULT
qxeSHQueryRecycleBin (const Extbyte * arg1, LPSHQUERYRBINFO arg2)
{
  if (XEUNICODE_P)
    return SHQueryRecycleBinW ((LPCWSTR) arg1, arg2);
  else
    return SHQueryRecycleBinA ((LPCSTR) arg1, arg2);
}

#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
HRESULT
qxeSHEmptyRecycleBin (HWND arg1, const Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return SHEmptyRecycleBinW (arg1, (LPCWSTR) arg2, arg3);
  else
    return SHEmptyRecycleBinA (arg1, (LPCSTR) arg2, arg3);
}

#endif /* !defined (CYGWIN_HEADERS) */


/*----------------------------------------------------------------------*/
/*                      Processing file WINSPOOL.H                      */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

/* Error if AddForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddJob used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddMonitor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinter used: split-sized DEVMODE pointer in split PRINTER_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinterConnection used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinterDriver used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrintProcessor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrintProvidor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AdvancedDocumentProperties used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ConfigurePort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeleteForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeleteMonitor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterConnection used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterDriver used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrintProcessor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrintProvidor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping DocumentProperties because split-sized DEVMODE, error in Cygwin prototype */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumForms used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumJobs used: split-sized DEVMODE pointer in split JOB_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumMonitors used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPorts used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrinterDrivers used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: #### problems with DEVMODE pointer in PRINTER_INFO_2 */
BOOL
qxeEnumPrinters (DWORD arg1, Extbyte * arg2, DWORD arg3, PBYTE arg4, DWORD arg5, PDWORD arg6, PDWORD arg7)
{
  if (XEUNICODE_P)
    return EnumPrintersW (arg1, (LPWSTR) arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return EnumPrintersA (arg1, (LPSTR) arg2, arg3, arg4, arg5, arg6, arg7);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrintProcessorDatatypes used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrintProcessors used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetDefaultPrinter used: Function needs review to determine how to handle it */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetJob used: split-sized DEVMODE pointer in split JOB_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinter used: split-sized DEVMODE pointer in split PRINTER_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterDriver used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterDriverDirectory used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrintProcessorDirectory used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping OpenPrinter because split-sized DEVMODE pointer in split PRINTER_DEFAULTS */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if PrinterMessageBox used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ResetPrinter used: split-sized DEVMODE pointer in split PRINTER_DEFAULTS */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetJob used: split-sized DEVMODE pointer in split JOB_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPrinter used: split-sized DEVMODE pointer in split PRINTER_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if StartDocPrinter used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                      Processing file WINNETWK.H                      */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetAddConnection (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return WNetAddConnectionW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3);
  else
    return WNetAddConnectionA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetAddConnection2 (LPNETRESOURCEW arg1, const Extbyte * arg2, const Extbyte * arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return WNetAddConnection2W (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4);
  else
    return WNetAddConnection2A ((LPNETRESOURCEA) arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetAddConnection3 (HWND arg1, LPNETRESOURCEW arg2, const Extbyte * arg3, const Extbyte * arg4, DWORD arg5)
{
  if (XEUNICODE_P)
    return WNetAddConnection3W (arg1, arg2, (LPCWSTR) arg3, (LPCWSTR) arg4, arg5);
  else
    return WNetAddConnection3A (arg1, (LPNETRESOURCEA) arg2, (LPCSTR) arg3, (LPCSTR) arg4, arg5);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetCancelConnection (const Extbyte * arg1, BOOL arg2)
{
  if (XEUNICODE_P)
    return WNetCancelConnectionW ((LPCWSTR) arg1, arg2);
  else
    return WNetCancelConnectionA ((LPCSTR) arg1, arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetCancelConnection2 (const Extbyte * arg1, DWORD arg2, BOOL arg3)
{
  if (XEUNICODE_P)
    return WNetCancelConnection2W ((LPCWSTR) arg1, arg2, arg3);
  else
    return WNetCancelConnection2A ((LPCSTR) arg1, arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetConnection (const Extbyte * arg1, Extbyte * arg2, PDWORD arg3)
{
  if (XEUNICODE_P)
    return WNetGetConnectionW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return WNetGetConnectionA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetUseConnection (HWND arg1, LPNETRESOURCEW arg2, const Extbyte * arg3, const Extbyte * arg4, DWORD arg5, Extbyte * arg6, PDWORD arg7, PDWORD arg8)
{
  if (XEUNICODE_P)
    return WNetUseConnectionW (arg1, arg2, (LPCWSTR) arg3, (LPCWSTR) arg4, arg5, (LPWSTR) arg6, arg7, arg8);
  else
    return WNetUseConnectionA (arg1, (LPNETRESOURCEA) arg2, (LPCSTR) arg3, (LPCSTR) arg4, arg5, (LPSTR) arg6, arg7, arg8);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if WNetSetConnection used: Function needs review to determine how to handle it */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: contains split-simple LPNETRESOURCE */
DWORD 
qxeWNetConnectionDialog1 (LPCONNECTDLGSTRUCTW arg1)
{
  if (XEUNICODE_P)
    return WNetConnectionDialog1W (arg1);
  else
    return WNetConnectionDialog1A ((LPCONNECTDLGSTRUCTA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetDisconnectDialog1 (LPDISCDLGSTRUCTW arg1)
{
  if (XEUNICODE_P)
    return WNetDisconnectDialog1W (arg1);
  else
    return WNetDisconnectDialog1A ((LPDISCDLGSTRUCTA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetOpenEnum (DWORD arg1, DWORD arg2, DWORD arg3, LPNETRESOURCEW arg4, LPHANDLE arg5)
{
  if (XEUNICODE_P)
    return WNetOpenEnumW (arg1, arg2, arg3, arg4, arg5);
  else
    return WNetOpenEnumA (arg1, arg2, arg3, (LPNETRESOURCEA) arg4, arg5);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetEnumResource (HANDLE arg1, PDWORD arg2, PVOID arg3, PDWORD arg4)
{
  if (XEUNICODE_P)
    return WNetEnumResourceW (arg1, arg2, arg3, arg4);
  else
    return WNetEnumResourceA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetUniversalName (const Extbyte * arg1, DWORD arg2, PVOID arg3, PDWORD arg4)
{
  if (XEUNICODE_P)
    return WNetGetUniversalNameW ((LPCWSTR) arg1, arg2, arg3, arg4);
  else
    return WNetGetUniversalNameA ((LPCSTR) arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetUser (const Extbyte * arg1, Extbyte * arg2, PDWORD arg3)
{
  if (XEUNICODE_P)
    return WNetGetUserW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return WNetGetUserA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetProviderName (DWORD arg1, Extbyte * arg2, PDWORD arg3)
{
  if (XEUNICODE_P)
    return WNetGetProviderNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return WNetGetProviderNameA (arg1, (LPSTR) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetNetworkInformation (const Extbyte * arg1, LPNETINFOSTRUCT arg2)
{
  if (XEUNICODE_P)
    return WNetGetNetworkInformationW ((LPCWSTR) arg1, arg2);
  else
    return WNetGetNetworkInformationA ((LPCSTR) arg1, arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if WNetGetResourceInformation used: Function needs review to determine how to handle it */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if WNetGetResourceParent used: Function needs review to determine how to handle it */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetLastError (PDWORD arg1, Extbyte * arg2, DWORD arg3, Extbyte * arg4, DWORD arg5)
{
  if (XEUNICODE_P)
    return WNetGetLastErrorW (arg1, (LPWSTR) arg2, arg3, (LPWSTR) arg4, arg5);
  else
    return WNetGetLastErrorA (arg1, (LPSTR) arg2, arg3, (LPSTR) arg4, arg5);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeMultinetGetConnectionPerformance (LPNETRESOURCEW arg1, LPNETCONNECTINFOSTRUCT arg2)
{
  if (XEUNICODE_P)
    return MultinetGetConnectionPerformanceW (arg1, arg2);
  else
    return MultinetGetConnectionPerformanceA ((LPNETRESOURCEA) arg1, arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                      Processing file WINUSER.H                       */
/*----------------------------------------------------------------------*/

BOOL
qxeAppendMenu (HMENU arg1, UINT arg2, UINT_PTR arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return AppendMenuW (arg1, arg2, arg3, (LPCWSTR) arg4);
  else
    return AppendMenuA (arg1, arg2, arg3, (LPCSTR) arg4);
}

/* Error if BroadcastSystemMessage used: win95 version not split; NT 4.0+ only */

/* Error if BroadcastSystemMessageEx used: Function needs review to determine how to handle it */

#if !defined (CYGWIN_HEADERS)

BOOL
qxeCallMsgFilter (LPMSG arg1, INT arg2)
{
  if (XEUNICODE_P)
    return CallMsgFilterW (arg1, arg2);
  else
    return CallMsgFilterA (arg1, arg2);
}

#endif /* !defined (CYGWIN_HEADERS) */

/* Error if CallWindowProc used: two versions, STRICT and non-STRICT */

/* Error if ChangeDisplaySettings used: split-sized LPDEVMODE */

/* Error if ChangeDisplaySettingsEx used: split-sized LPDEVMODE; NT 5.0/Win98+ only */

BOOL
qxeChangeMenu (HMENU arg1, UINT arg2, const Extbyte * arg3, UINT arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return ChangeMenuW (arg1, arg2, (LPCWSTR) arg3, arg4, arg5);
  else
    return ChangeMenuA (arg1, arg2, (LPCSTR) arg3, arg4, arg5);
}

Extbyte *
qxeCharLower (Extbyte * arg1)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharLowerW ((LPWSTR) arg1);
  else
    return (Extbyte *) CharLowerA ((LPSTR) arg1);
}

DWORD
qxeCharLowerBuff (Extbyte * arg1, DWORD arg2)
{
  if (XEUNICODE_P)
    return CharLowerBuffW ((LPWSTR) arg1, arg2);
  else
    return CharLowerBuffA ((LPSTR) arg1, arg2);
}

Extbyte *
qxeCharNext (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharNextW ((LPCWSTR) arg1);
  else
    return (Extbyte *) CharNextA ((LPCSTR) arg1);
}

Extbyte *
qxeCharPrev (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharPrevW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return (Extbyte *) CharPrevA ((LPCSTR) arg1, (LPCSTR) arg2);
}

BOOL
qxeCharToOem (const Extbyte * arg1, LPSTR arg2)
{
  if (XEUNICODE_P)
    return CharToOemW ((LPCWSTR) arg1, arg2);
  else
    return CharToOemA ((LPCSTR) arg1, arg2);
}

BOOL
qxeCharToOemBuff (const Extbyte * arg1, LPSTR arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return CharToOemBuffW ((LPCWSTR) arg1, arg2, arg3);
  else
    return CharToOemBuffA ((LPCSTR) arg1, arg2, arg3);
}

Extbyte *
qxeCharUpper (Extbyte * arg1)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharUpperW ((LPWSTR) arg1);
  else
    return (Extbyte *) CharUpperA ((LPSTR) arg1);
}

DWORD
qxeCharUpperBuff (Extbyte * arg1, DWORD arg2)
{
  if (XEUNICODE_P)
    return CharUpperBuffW ((LPWSTR) arg1, arg2);
  else
    return CharUpperBuffA ((LPSTR) arg1, arg2);
}

int
qxeCopyAcceleratorTable (HACCEL arg1, LPACCEL arg2, int arg3)
{
  if (XEUNICODE_P)
    return CopyAcceleratorTableW (arg1, arg2, arg3);
  else
    return CopyAcceleratorTableA (arg1, arg2, arg3);
}

HACCEL
qxeCreateAcceleratorTable (LPACCEL arg1, int arg2)
{
  if (XEUNICODE_P)
    return CreateAcceleratorTableW (arg1, arg2);
  else
    return CreateAcceleratorTableA (arg1, arg2);
}

/* Error if CreateDesktop used: split-sized LPDEVMODE */

/* NOTE: error in Cygwin prototype (no split) but fixable with typedef */
HWND
qxeCreateDialogIndirectParam (HINSTANCE arg1, LPCDLGTEMPLATE arg2, HWND arg3, DLGPROC arg4, LPARAM arg5)
{
  if (XEUNICODE_P)
    return CreateDialogIndirectParamW (arg1, arg2, arg3, arg4, arg5);
  else
    return CreateDialogIndirectParamA (arg1, arg2, arg3, arg4, arg5);
}

HWND
qxeCreateDialogParam (HINSTANCE arg1, const Extbyte * arg2, HWND arg3, DLGPROC arg4, LPARAM arg5)
{
  if (XEUNICODE_P)
    return CreateDialogParamW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return CreateDialogParamA (arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}

/* NOTE: error arg 1, VS6 prototype, missing const.
   NOTE: Prototype manually overridden.
         Header file claims:
           WINUSERAPI HWND WINAPI CreateMDIWindow(LPCWSTR,LPCWSTR,DWORD,int,int,int,int,HWND,HINSTANCE,LPARAM)
         Overridden with:
           HWND CreateMDIWindow(LPWSTR,LPCWSTR,DWORD,int,int,int,int,HWND,HINSTANCE,LPARAM)
         Differences in return-type qualifiers, e.g. WINAPI, are not important.
 */
HWND
qxeCreateMDIWindow (Extbyte * arg1, const Extbyte * arg2, DWORD arg3, int arg4, int arg5, int arg6, int arg7, HWND arg8, HINSTANCE arg9, LPARAM arg10)
{
  if (XEUNICODE_P)
    return CreateMDIWindowW ((LPWSTR) arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  else
    return CreateMDIWindowA ((LPSTR) arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

HWND
qxeCreateWindowEx (DWORD arg1, const Extbyte * arg2, const Extbyte * arg3, DWORD arg4, int arg5, int arg6, int arg7, int arg8, HWND arg9, HMENU arg10, HINSTANCE arg11, LPVOID arg12)
{
  if (XEUNICODE_P)
    return CreateWindowExW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  else
    return CreateWindowExA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

/* NOTE: error arg 1, VS6 prototype, missing const.
   NOTE: Prototype manually overridden.
         Header file claims:
           WINUSERAPI HWINSTA WINAPI CreateWindowStation(LPCWSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES)
         Overridden with:
           HWINSTA CreateWindowStation(LPWSTR,DWORD,DWORD,LPSECURITY_ATTRIBUTES)
         Differences in return-type qualifiers, e.g. WINAPI, are not important.
 */
HWINSTA
qxeCreateWindowStation (Extbyte * arg1, DWORD arg2, DWORD arg3, LPSECURITY_ATTRIBUTES arg4)
{
  if (XEUNICODE_P)
    return CreateWindowStationW ((LPWSTR) arg1, arg2, arg3, arg4);
  else
    return CreateWindowStationA ((LPSTR) arg1, arg2, arg3, arg4);
}

/* Error if DefDlgProc used: return value is conditionalized on _MAC, messes up parser */

LRESULT
qxeDefFrameProc (HWND arg1, HWND arg2, UINT arg3, WPARAM arg4, LPARAM arg5)
{
  if (XEUNICODE_P)
    return DefFrameProcW (arg1, arg2, arg3, arg4, arg5);
  else
    return DefFrameProcA (arg1, arg2, arg3, arg4, arg5);
}

/* Error if DefMDIChildProc used: return value is conditionalized on _MAC, messes up parser */

/* Skipping DefWindowProc because return value is conditionalized on _MAC, messes up parser */

/* NOTE: error in Cygwin prototype (no split) but fixable with typedef */
int
qxeDialogBoxIndirectParam (HINSTANCE arg1, LPCDLGTEMPLATE arg2, HWND arg3, DLGPROC arg4, LPARAM arg5)
{
  if (XEUNICODE_P)
    return DialogBoxIndirectParamW (arg1, arg2, arg3, arg4, arg5);
  else
    return DialogBoxIndirectParamA (arg1, arg2, arg3, arg4, arg5);
}

int
qxeDialogBoxParam (HINSTANCE arg1, const Extbyte * arg2, HWND arg3, DLGPROC arg4, LPARAM arg5)
{
  if (XEUNICODE_P)
    return DialogBoxParamW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return DialogBoxParamA (arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}

LONG
qxeDispatchMessage (const MSG* arg1)
{
  if (XEUNICODE_P)
    return DispatchMessageW (arg1);
  else
    return DispatchMessageA (arg1);
}

int
qxeDlgDirList (HWND arg1, Extbyte * arg2, int arg3, int arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return DlgDirListW (arg1, (LPWSTR) arg2, arg3, arg4, arg5);
  else
    return DlgDirListA (arg1, (LPSTR) arg2, arg3, arg4, arg5);
}

int
qxeDlgDirListComboBox (HWND arg1, Extbyte * arg2, int arg3, int arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return DlgDirListComboBoxW (arg1, (LPWSTR) arg2, arg3, arg4, arg5);
  else
    return DlgDirListComboBoxA (arg1, (LPSTR) arg2, arg3, arg4, arg5);
}

BOOL
qxeDlgDirSelectComboBoxEx (HWND arg1, Extbyte * arg2, int arg3, int arg4)
{
  if (XEUNICODE_P)
    return DlgDirSelectComboBoxExW (arg1, (LPWSTR) arg2, arg3, arg4);
  else
    return DlgDirSelectComboBoxExA (arg1, (LPSTR) arg2, arg3, arg4);
}

BOOL
qxeDlgDirSelectEx (HWND arg1, Extbyte * arg2, int arg3, int arg4)
{
  if (XEUNICODE_P)
    return DlgDirSelectExW (arg1, (LPWSTR) arg2, arg3, arg4);
  else
    return DlgDirSelectExA (arg1, (LPSTR) arg2, arg3, arg4);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeDrawState (HDC arg1, HBRUSH arg2, DRAWSTATEPROC arg3, LPARAM arg4, WPARAM arg5, int arg6, int arg7, int arg8, int arg9, UINT arg10)
{
  if (XEUNICODE_P)
    return DrawStateW (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  else
    return DrawStateA (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

int
qxeDrawText (HDC arg1, const Extbyte * arg2, int arg3, LPRECT arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return DrawTextW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return DrawTextA (arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}

/* NOTE: NT 4.0+ only */
int
qxeDrawTextEx (HDC arg1, Extbyte * arg2, int arg3, LPRECT arg4, UINT arg5, LPDRAWTEXTPARAMS arg6)
{
  if (XEUNICODE_P)
    return DrawTextExW (arg1, (LPWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return DrawTextExA (arg1, (LPSTR) arg2, arg3, arg4, arg5, arg6);
}

/* NOTE: // callback fun differs only in string pointer type */
BOOL
qxeEnumDesktops (HWINSTA arg1, DESKTOPENUMPROCW arg2, LPARAM arg3)
{
  if (XEUNICODE_P)
    return EnumDesktopsW (arg1, arg2, arg3);
  else
    return EnumDesktopsA (arg1, (DESKTOPENUMPROCA) arg2, arg3);
}

/* Error if EnumDisplaySettings used: split-sized LPDEVMODE */

/* Error if EnumDisplaySettingsEx used: Function needs review to determine how to handle it */

/* Error if EnumDisplayDevices used: split-sized PDISPLAY_DEVICE; NT 5.0+ only, no Win98 */

/* NOTE: // callback fun differs only in string pointer type */
int
qxeEnumProps (HWND arg1, PROPENUMPROCW arg2)
{
  if (XEUNICODE_P)
    return EnumPropsW (arg1, arg2);
  else
    return EnumPropsA (arg1, (PROPENUMPROCA) arg2);
}

/* NOTE: // callback fun differs only in string pointer type */
int
qxeEnumPropsEx (HWND arg1, PROPENUMPROCEXW arg2, LPARAM arg3)
{
  if (XEUNICODE_P)
    return EnumPropsExW (arg1, arg2, arg3);
  else
    return EnumPropsExA (arg1, (PROPENUMPROCEXA) arg2, arg3);
}

/* NOTE: // callback fun differs only in string pointer type */
BOOL
qxeEnumWindowStations (WINSTAENUMPROCW arg1, LPARAM arg2)
{
  if (XEUNICODE_P)
    return EnumWindowStationsW (arg1, arg2);
  else
    return EnumWindowStationsA ((WINSTAENUMPROCA) arg1, arg2);
}

/* NOTE: NT 4.0+ only */
HWND
qxeFindWindowEx (HWND arg1, HWND arg2, const Extbyte * arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return FindWindowExW (arg1, arg2, (LPCWSTR) arg3, (LPCWSTR) arg4);
  else
    return FindWindowExA (arg1, arg2, (LPCSTR) arg3, (LPCSTR) arg4);
}

HWND
qxeFindWindow (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return FindWindowW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return FindWindowA ((LPCSTR) arg1, (LPCSTR) arg2);
}

BOOL
qxeGetClassInfo (HINSTANCE arg1, const Extbyte * arg2, LPWNDCLASSW arg3)
{
  if (XEUNICODE_P)
    return GetClassInfoW (arg1, (LPCWSTR) arg2, arg3);
  else
    return GetClassInfoA (arg1, (LPCSTR) arg2, (LPWNDCLASSA) arg3);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeGetClassInfoEx (HINSTANCE arg1, const Extbyte * arg2, LPWNDCLASSEXW arg3)
{
  if (XEUNICODE_P)
    return GetClassInfoExW (arg1, (LPCWSTR) arg2, arg3);
  else
    return GetClassInfoExA (arg1, (LPCSTR) arg2, (LPWNDCLASSEXA) arg3);
}

DWORD
qxeGetClassLong (HWND arg1, int arg2)
{
  if (XEUNICODE_P)
    return GetClassLongW (arg1, arg2);
  else
    return GetClassLongA (arg1, arg2);
}

/* Error if GetClassLongPtr used: Function needs review to determine how to handle it */

int
qxeGetClassName (HWND arg1, Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return GetClassNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return GetClassNameA (arg1, (LPSTR) arg2, arg3);
}

int
qxeGetClipboardFormatName (UINT arg1, Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return GetClipboardFormatNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return GetClipboardFormatNameA (arg1, (LPSTR) arg2, arg3);
}

UINT
qxeGetDlgItemText (HWND arg1, int arg2, Extbyte * arg3, int arg4)
{
  if (XEUNICODE_P)
    return GetDlgItemTextW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return GetDlgItemTextA (arg1, arg2, (LPSTR) arg3, arg4);
}

BOOL
qxeGetKeyboardLayoutName (Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetKeyboardLayoutNameW ((LPWSTR) arg1);
  else
    return GetKeyboardLayoutNameA ((LPSTR) arg1);
}

int
qxeGetKeyNameText (LONG arg1, Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return GetKeyNameTextW (arg1, (LPWSTR) arg2, arg3);
  else
    return GetKeyNameTextA (arg1, (LPSTR) arg2, arg3);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeGetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPMENUITEMINFOW arg4)
{
  if (XEUNICODE_P)
    return GetMenuItemInfoW (arg1, arg2, arg3, arg4);
  else
    return GetMenuItemInfoA (arg1, arg2, arg3, (LPMENUITEMINFOA) arg4);
}

int
qxeGetMenuString (HMENU arg1, UINT arg2, Extbyte * arg3, int arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return GetMenuStringW (arg1, arg2, (LPWSTR) arg3, arg4, arg5);
  else
    return GetMenuStringA (arg1, arg2, (LPSTR) arg3, arg4, arg5);
}

BOOL
qxeGetMessage (LPMSG arg1, HWND arg2, UINT arg3, UINT arg4)
{
  if (XEUNICODE_P)
    return GetMessageW (arg1, arg2, arg3, arg4);
  else
    return GetMessageA (arg1, arg2, arg3, arg4);
}

HANDLE
qxeGetProp (HWND arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return GetPropW (arg1, (LPCWSTR) arg2);
  else
    return GetPropA (arg1, (LPCSTR) arg2);
}

/* Error if GetRawInputDeviceInfo used: Function needs review to determine how to handle it */

DWORD
qxeGetTabbedTextExtent (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPINT arg5)
{
  if (XEUNICODE_P)
    return GetTabbedTextExtentW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return GetTabbedTextExtentA (arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}

LONG
qxeGetWindowLong (HWND arg1, int arg2)
{
  if (XEUNICODE_P)
    return GetWindowLongW (arg1, arg2);
  else
    return GetWindowLongA (arg1, arg2);
}

/* Error if GetWindowLongPtr used: Function needs review to determine how to handle it */

BOOL
qxeGetUserObjectInformation (HANDLE arg1, int arg2, PVOID arg3, DWORD arg4, PDWORD arg5)
{
  if (XEUNICODE_P)
    return GetUserObjectInformationW (arg1, arg2, arg3, arg4, arg5);
  else
    return GetUserObjectInformationA (arg1, arg2, arg3, arg4, arg5);
}

int
qxeGetWindowTextLength (HWND arg1)
{
  if (XEUNICODE_P)
    return GetWindowTextLengthW (arg1);
  else
    return GetWindowTextLengthA (arg1);
}

int
qxeGetWindowText (HWND arg1, Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return GetWindowTextW (arg1, (LPWSTR) arg2, arg3);
  else
    return GetWindowTextA (arg1, (LPSTR) arg2, arg3);
}

/* Error if GetAltTabInfo used: NT 5.0+ only */

/* Error if GetMonitorInfo used: NT 5.0/Win98+ only */

/* Error if GetWindowModuleFileName used: NT 5.0+ only */

BOOL
qxeGrayString (HDC arg1, HBRUSH arg2, GRAYSTRINGPROC arg3, LPARAM arg4, int arg5, int arg6, int arg7, int arg8, int arg9)
{
  if (XEUNICODE_P)
    return GrayStringW (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  else
    return GrayStringA (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

BOOL
qxeInsertMenu (HMENU arg1, UINT arg2, UINT arg3, UINT arg4, const Extbyte * arg5)
{
  if (XEUNICODE_P)
    return InsertMenuW (arg1, arg2, arg3, arg4, (LPCWSTR) arg5);
  else
    return InsertMenuA (arg1, arg2, arg3, arg4, (LPCSTR) arg5);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeInsertMenuItem (HMENU arg1, UINT arg2, BOOL arg3, LPCMENUITEMINFOW arg4)
{
  if (XEUNICODE_P)
    return InsertMenuItemW (arg1, arg2, arg3, arg4);
  else
    return InsertMenuItemA (arg1, arg2, arg3, (LPCMENUITEMINFOA) arg4);
}

/* Error if IsCharAlphaNumeric used: split CHAR */

/* Error if IsCharAlpha used: split CHAR */

/* Error if IsCharLower used: split CHAR */

/* Error if IsCharUpper used: split CHAR */

BOOL
qxeIsDialogMessage (HWND arg1, LPMSG arg2)
{
  if (XEUNICODE_P)
    return IsDialogMessageW (arg1, arg2);
  else
    return IsDialogMessageA (arg1, arg2);
}

HACCEL
qxeLoadAccelerators (HINSTANCE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return LoadAcceleratorsW (arg1, (LPCWSTR) arg2);
  else
    return LoadAcceleratorsA (arg1, (LPCSTR) arg2);
}

HBITMAP
qxeLoadBitmap (HINSTANCE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return LoadBitmapW (arg1, (LPCWSTR) arg2);
  else
    return LoadBitmapA (arg1, (LPCSTR) arg2);
}

HCURSOR
qxeLoadCursorFromFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return LoadCursorFromFileW ((LPCWSTR) arg1);
  else
    return LoadCursorFromFileA ((LPCSTR) arg1);
}

HCURSOR
qxeLoadCursor (HINSTANCE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return LoadCursorW (arg1, (LPCWSTR) arg2);
  else
    return LoadCursorA (arg1, (LPCSTR) arg2);
}

HICON
qxeLoadIcon (HINSTANCE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return LoadIconW (arg1, (LPCWSTR) arg2);
  else
    return LoadIconA (arg1, (LPCSTR) arg2);
}

/* NOTE: NT 4.0+ only */
HANDLE
qxeLoadImage (HINSTANCE arg1, const Extbyte * arg2, UINT arg3, int arg4, int arg5, UINT arg6)
{
  if (XEUNICODE_P)
    return LoadImageW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return LoadImageA (arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6);
}

HKL
qxeLoadKeyboardLayout (const Extbyte * arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return LoadKeyboardLayoutW ((LPCWSTR) arg1, arg2);
  else
    return LoadKeyboardLayoutA ((LPCSTR) arg1, arg2);
}

HMENU
qxeLoadMenuIndirect (const MENUTEMPLATE* arg1)
{
  if (XEUNICODE_P)
    return LoadMenuIndirectW (arg1);
  else
    return LoadMenuIndirectA (arg1);
}

HMENU
qxeLoadMenu (HINSTANCE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return LoadMenuW (arg1, (LPCWSTR) arg2);
  else
    return LoadMenuA (arg1, (LPCSTR) arg2);
}

int
qxeLoadString (HINSTANCE arg1, UINT arg2, Extbyte * arg3, int arg4)
{
  if (XEUNICODE_P)
    return LoadStringW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return LoadStringA (arg1, arg2, (LPSTR) arg3, arg4);
}

/* NOTE: NT 4.0+ only */
UINT
qxeMapVirtualKeyEx (UINT arg1, UINT arg2, HKL arg3)
{
  if (XEUNICODE_P)
    return MapVirtualKeyExW (arg1, arg2, arg3);
  else
    return MapVirtualKeyExA (arg1, arg2, arg3);
}

UINT
qxeMapVirtualKey (UINT arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return MapVirtualKeyW (arg1, arg2);
  else
    return MapVirtualKeyA (arg1, arg2);
}

int
qxeMessageBox (HWND arg1, const Extbyte * arg2, const Extbyte * arg3, UINT arg4)
{
  if (XEUNICODE_P)
    return MessageBoxW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4);
  else
    return MessageBoxA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4);
}

int
qxeMessageBoxEx (HWND arg1, const Extbyte * arg2, const Extbyte * arg3, UINT arg4, WORD arg5)
{
  if (XEUNICODE_P)
    return MessageBoxExW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4, arg5);
  else
    return MessageBoxExA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4, arg5);
}

/* Error if MessageBoxIndirect used: Cygwin has split MSGBOXPARAMS* instead of LPMSGBOXPARAMS */

BOOL
qxeModifyMenu (HMENU arg1, UINT arg2, UINT arg3, UINT arg4, const Extbyte * arg5)
{
  if (XEUNICODE_P)
    return ModifyMenuW (arg1, arg2, arg3, arg4, (LPCWSTR) arg5);
  else
    return ModifyMenuA (arg1, arg2, arg3, arg4, (LPCSTR) arg5);
}

BOOL
qxeOemToCharBuff (LPCSTR arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return OemToCharBuffW (arg1, (LPWSTR) arg2, arg3);
  else
    return OemToCharBuffA (arg1, (LPSTR) arg2, arg3);
}

BOOL
qxeOemToChar (LPCSTR arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return OemToCharW (arg1, (LPWSTR) arg2);
  else
    return OemToCharA (arg1, (LPSTR) arg2);
}

HDESK
qxeOpenDesktop (Extbyte * arg1, DWORD arg2, BOOL arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return OpenDesktopW ((LPWSTR) arg1, arg2, arg3, arg4);
  else
    return OpenDesktopA ((LPSTR) arg1, arg2, arg3, arg4);
}

HWINSTA
qxeOpenWindowStation (Extbyte * arg1, BOOL arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return OpenWindowStationW ((LPWSTR) arg1, arg2, arg3);
  else
    return OpenWindowStationA ((LPSTR) arg1, arg2, arg3);
}

BOOL
qxePeekMessage (LPMSG arg1, HWND arg2, UINT arg3, UINT arg4, UINT arg5)
{
  if (XEUNICODE_P)
    return PeekMessageW (arg1, arg2, arg3, arg4, arg5);
  else
    return PeekMessageA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxePostMessage (HWND arg1, UINT arg2, WPARAM arg3, LPARAM arg4)
{
  if (XEUNICODE_P)
    return PostMessageW (arg1, arg2, arg3, arg4);
  else
    return PostMessageA (arg1, arg2, arg3, arg4);
}

BOOL
qxePostThreadMessage (DWORD arg1, UINT arg2, WPARAM arg3, LPARAM arg4)
{
  if (XEUNICODE_P)
    return PostThreadMessageW (arg1, arg2, arg3, arg4);
  else
    return PostThreadMessageA (arg1, arg2, arg3, arg4);
}

/* Error if RealGetWindowClass used: NT 5.0+ only */

/* Skipping RegisterClass because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASS */

/* Skipping RegisterClassEx because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASSEX; NT 4.0+ only */

UINT
qxeRegisterClipboardFormat (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return RegisterClipboardFormatW ((LPCWSTR) arg1);
  else
    return RegisterClipboardFormatA ((LPCSTR) arg1);
}

/* Error if RegisterDeviceNotification used: NT 5.0+ only */

UINT
qxeRegisterWindowMessage (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return RegisterWindowMessageW ((LPCWSTR) arg1);
  else
    return RegisterWindowMessageA ((LPCSTR) arg1);
}

HANDLE
qxeRemoveProp (HWND arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return RemovePropW (arg1, (LPCWSTR) arg2);
  else
    return RemovePropA (arg1, (LPCSTR) arg2);
}

LONG
qxeSendDlgItemMessage (HWND arg1, int arg2, UINT arg3, WPARAM arg4, LPARAM arg5)
{
  if (XEUNICODE_P)
    return SendDlgItemMessageW (arg1, arg2, arg3, arg4, arg5);
  else
    return SendDlgItemMessageA (arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeSendMessageCallback (HWND arg1, UINT arg2, WPARAM arg3, LPARAM arg4, SENDASYNCPROC arg5, DWORD arg6)
{
  if (XEUNICODE_P)
    return SendMessageCallbackW (arg1, arg2, arg3, arg4, arg5, arg6);
  else
    return SendMessageCallbackA (arg1, arg2, arg3, arg4, arg5, arg6);
}

/* Error if SendMessageTimeout used: VS6 has erroneous seventh parameter DWORD_PTR instead of PDWORD_PTR */

/* Skipping SendMessage because split messages and structures */

BOOL
qxeSendNotifyMessage (HWND arg1, UINT arg2, WPARAM arg3, LPARAM arg4)
{
  if (XEUNICODE_P)
    return SendNotifyMessageW (arg1, arg2, arg3, arg4);
  else
    return SendNotifyMessageA (arg1, arg2, arg3, arg4);
}

DWORD
qxeSetClassLong (HWND arg1, int arg2, LONG arg3)
{
  if (XEUNICODE_P)
    return SetClassLongW (arg1, arg2, arg3);
  else
    return SetClassLongA (arg1, arg2, arg3);
}

/* Error if SetClassLongPtr used: Function needs review to determine how to handle it */

BOOL
qxeSetDlgItemText (HWND arg1, int arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return SetDlgItemTextW (arg1, arg2, (LPCWSTR) arg3);
  else
    return SetDlgItemTextA (arg1, arg2, (LPCSTR) arg3);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeSetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPCMENUITEMINFOW arg4)
{
  if (XEUNICODE_P)
    return SetMenuItemInfoW (arg1, arg2, arg3, arg4);
  else
    return SetMenuItemInfoA (arg1, arg2, arg3, (LPCMENUITEMINFOA) arg4);
}

BOOL
qxeSetProp (HWND arg1, const Extbyte * arg2, HANDLE arg3)
{
  if (XEUNICODE_P)
    return SetPropW (arg1, (LPCWSTR) arg2, arg3);
  else
    return SetPropA (arg1, (LPCSTR) arg2, arg3);
}

BOOL
qxeSetUserObjectInformation (HANDLE arg1, int arg2, PVOID arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return SetUserObjectInformationW (arg1, arg2, arg3, arg4);
  else
    return SetUserObjectInformationA (arg1, arg2, arg3, arg4);
}

LONG
qxeSetWindowLong (HWND arg1, int arg2, LONG arg3)
{
  if (XEUNICODE_P)
    return SetWindowLongW (arg1, arg2, arg3);
  else
    return SetWindowLongA (arg1, arg2, arg3);
}

/* Error if SetWindowLongPtr used: Function needs review to determine how to handle it */

/* Error if SetWindowsHook used: obsolete; two versions, STRICT and non-STRICT */

HHOOK
qxeSetWindowsHookEx (int arg1, HOOKPROC arg2, HINSTANCE arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return SetWindowsHookExW (arg1, arg2, arg3, arg4);
  else
    return SetWindowsHookExA (arg1, arg2, arg3, arg4);
}

BOOL
qxeSetWindowText (HWND arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return SetWindowTextW (arg1, (LPCWSTR) arg2);
  else
    return SetWindowTextA (arg1, (LPCSTR) arg2);
}

/* NOTE: probs w/ICONMETRICS, NONCLIENTMETRICS */
BOOL
qxeSystemParametersInfo (UINT arg1, UINT arg2, PVOID arg3, UINT arg4)
{
  if (XEUNICODE_P)
    return SystemParametersInfoW (arg1, arg2, arg3, arg4);
  else
    return SystemParametersInfoA (arg1, arg2, arg3, arg4);
}

LONG
qxeTabbedTextOut (HDC arg1, int arg2, int arg3, const Extbyte * arg4, int arg5, int arg6, LPINT arg7, int arg8)
{
  if (XEUNICODE_P)
    return TabbedTextOutW (arg1, arg2, arg3, (LPCWSTR) arg4, arg5, arg6, arg7, arg8);
  else
    return TabbedTextOutA (arg1, arg2, arg3, (LPCSTR) arg4, arg5, arg6, arg7, arg8);
}

int
qxeTranslateAccelerator (HWND arg1, HACCEL arg2, LPMSG arg3)
{
  if (XEUNICODE_P)
    return TranslateAcceleratorW (arg1, arg2, arg3);
  else
    return TranslateAcceleratorA (arg1, arg2, arg3);
}

/* Skipping UnregisterClass because need to intercept for reasons related to RegisterClass */

/* Error if VkKeyScanEx used: split CHAR; NT 4.0+ only */

/* Skipping VkKeyScan because split CHAR */

BOOL
qxeWinHelp (HWND arg1, const Extbyte * arg2, UINT arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return WinHelpW (arg1, (LPCWSTR) arg2, arg3, arg4);
  else
    return WinHelpA (arg1, (LPCSTR) arg2, arg3, arg4);
}

int
qxewvsprintf (Extbyte * arg1, const Extbyte * arg2, va_list arglist)
{
  if (XEUNICODE_P)
    return wvsprintfW ((LPWSTR) arg1, (LPCWSTR) arg2, arglist);
  else
    return wvsprintfA ((LPSTR) arg1, (LPCSTR) arg2, arglist);
}


/*----------------------------------------------------------------------*/
/*                       Processing file DDEML.H                        */
/*----------------------------------------------------------------------*/

/* NOTE: former error in Cygwin prototype, but no more (Cygwin 1.7, 1-30-10) */
HSZ
qxeDdeCreateStringHandle (DWORD arg1, const Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return DdeCreateStringHandleW (arg1, (LPCWSTR) arg2, arg3);
  else
    return DdeCreateStringHandleA (arg1, (LPCSTR) arg2, arg3);
}

UINT
qxeDdeInitialize (PDWORD arg1, PFNCALLBACK arg2, DWORD arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return DdeInitializeW (arg1, arg2, arg3, arg4);
  else
    return DdeInitializeA (arg1, arg2, arg3, arg4);
}

DWORD
qxeDdeQueryString (DWORD arg1, HSZ arg2, Extbyte * arg3, DWORD arg4, int arg5)
{
  if (XEUNICODE_P)
    return DdeQueryStringW (arg1, arg2, (LPWSTR) arg3, arg4, arg5);
  else
    return DdeQueryStringA (arg1, arg2, (LPSTR) arg3, arg4, arg5);
}


/*----------------------------------------------------------------------*/
/*                       Processing file WINREG.H                       */
/*----------------------------------------------------------------------*/

/* NOTE: error arg 1, Cygwin prototype, extra const.
   NOTE: Prototype manually overridden.
         Header file claims:
           WINADVAPI BOOL WINAPI AbortSystemShutdown(LPCWSTR)
         Overridden with:
           BOOL AbortSystemShutdown(LPWSTR)
         Differences in return-type qualifiers, e.g. WINAPI, are not important.
 */
BOOL
qxeAbortSystemShutdown (Extbyte * arg1)
{
  if (XEUNICODE_P)
    return AbortSystemShutdownW ((LPWSTR) arg1);
  else
    return AbortSystemShutdownA ((LPSTR) arg1);
}

BOOL
qxeInitiateSystemShutdown (Extbyte * arg1, Extbyte * arg2, DWORD arg3, BOOL arg4, BOOL arg5)
{
  if (XEUNICODE_P)
    return InitiateSystemShutdownW ((LPWSTR) arg1, (LPWSTR) arg2, arg3, arg4, arg5);
  else
    return InitiateSystemShutdownA ((LPSTR) arg1, (LPSTR) arg2, arg3, arg4, arg5);
}

/* NOTE: former error in Cygwin prototype, but no more (Cygwin 1.7, 1-30-10) */
LONG
qxeRegConnectRegistry (const Extbyte * arg1, HKEY arg2, PHKEY arg3)
{
  if (XEUNICODE_P)
    return RegConnectRegistryW ((LPCWSTR) arg1, arg2, arg3);
  else
    return RegConnectRegistryA ((LPCSTR) arg1, arg2, arg3);
}

LONG
qxeRegCreateKeyEx (HKEY arg1, const Extbyte * arg2, DWORD arg3, Extbyte * arg4, DWORD arg5, REGSAM arg6, LPSECURITY_ATTRIBUTES arg7, PHKEY arg8, PDWORD arg9)
{
  if (XEUNICODE_P)
    return RegCreateKeyExW (arg1, (LPCWSTR) arg2, arg3, (LPWSTR) arg4, arg5, arg6, arg7, arg8, arg9);
  else
    return RegCreateKeyExA (arg1, (LPCSTR) arg2, arg3, (LPSTR) arg4, arg5, arg6, arg7, arg8, arg9);
}

LONG
qxeRegCreateKey (HKEY arg1, const Extbyte * arg2, PHKEY arg3)
{
  if (XEUNICODE_P)
    return RegCreateKeyW (arg1, (LPCWSTR) arg2, arg3);
  else
    return RegCreateKeyA (arg1, (LPCSTR) arg2, arg3);
}

LONG
qxeRegDeleteKey (HKEY arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return RegDeleteKeyW (arg1, (LPCWSTR) arg2);
  else
    return RegDeleteKeyA (arg1, (LPCSTR) arg2);
}

/* Error if RegDeleteKeyEx used: Function needs review to determine how to handle it */

LONG
qxeRegDeleteValue (HKEY arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return RegDeleteValueW (arg1, (LPCWSTR) arg2);
  else
    return RegDeleteValueA (arg1, (LPCSTR) arg2);
}

LONG
qxeRegEnumKey (HKEY arg1, DWORD arg2, Extbyte * arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return RegEnumKeyW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return RegEnumKeyA (arg1, arg2, (LPSTR) arg3, arg4);
}

LONG
qxeRegEnumKeyEx (HKEY arg1, DWORD arg2, Extbyte * arg3, PDWORD arg4, PDWORD arg5, Extbyte * arg6, PDWORD arg7, PFILETIME arg8)
{
  if (XEUNICODE_P)
    return RegEnumKeyExW (arg1, arg2, (LPWSTR) arg3, arg4, arg5, (LPWSTR) arg6, arg7, arg8);
  else
    return RegEnumKeyExA (arg1, arg2, (LPSTR) arg3, arg4, arg5, (LPSTR) arg6, arg7, arg8);
}

LONG
qxeRegEnumValue (HKEY arg1, DWORD arg2, Extbyte * arg3, PDWORD arg4, PDWORD arg5, PDWORD arg6, LPBYTE arg7, PDWORD arg8)
{
  if (XEUNICODE_P)
    return RegEnumValueW (arg1, arg2, (LPWSTR) arg3, arg4, arg5, arg6, arg7, arg8);
  else
    return RegEnumValueA (arg1, arg2, (LPSTR) arg3, arg4, arg5, arg6, arg7, arg8);
}

LONG
qxeRegLoadKey (HKEY arg1, const Extbyte * arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return RegLoadKeyW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3);
  else
    return RegLoadKeyA (arg1, (LPCSTR) arg2, (LPCSTR) arg3);
}

LONG
qxeRegOpenKeyEx (HKEY arg1, const Extbyte * arg2, DWORD arg3, REGSAM arg4, PHKEY arg5)
{
  if (XEUNICODE_P)
    return RegOpenKeyExW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return RegOpenKeyExA (arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}

LONG
qxeRegOpenKey (HKEY arg1, const Extbyte * arg2, PHKEY arg3)
{
  if (XEUNICODE_P)
    return RegOpenKeyW (arg1, (LPCWSTR) arg2, arg3);
  else
    return RegOpenKeyA (arg1, (LPCSTR) arg2, arg3);
}

LONG
qxeRegQueryInfoKey (HKEY arg1, Extbyte * arg2, PDWORD arg3, PDWORD arg4, PDWORD arg5, PDWORD arg6, PDWORD arg7, PDWORD arg8, PDWORD arg9, PDWORD arg10, PDWORD arg11, PFILETIME arg12)
{
  if (XEUNICODE_P)
    return RegQueryInfoKeyW (arg1, (LPWSTR) arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  else
    return RegQueryInfoKeyA (arg1, (LPSTR) arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

LONG
qxeRegQueryMultipleValues (HKEY arg1, PVALENTW arg2, DWORD arg3, Extbyte * arg4, LPDWORD arg5)
{
  if (XEUNICODE_P)
    return RegQueryMultipleValuesW (arg1, arg2, arg3, (LPWSTR) arg4, arg5);
  else
    return RegQueryMultipleValuesA (arg1, (PVALENTA) arg2, arg3, (LPSTR) arg4, arg5);
}

LONG
qxeRegQueryValueEx (HKEY arg1, const Extbyte * arg2, LPDWORD arg3, LPDWORD arg4, LPBYTE arg5, LPDWORD arg6)
{
  if (XEUNICODE_P)
    return RegQueryValueExW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return RegQueryValueExA (arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6);
}

LONG
qxeRegQueryValue (HKEY arg1, const Extbyte * arg2, Extbyte * arg3, PLONG arg4)
{
  if (XEUNICODE_P)
    return RegQueryValueW (arg1, (LPCWSTR) arg2, (LPWSTR) arg3, arg4);
  else
    return RegQueryValueA (arg1, (LPCSTR) arg2, (LPSTR) arg3, arg4);
}

LONG
qxeRegReplaceKey (HKEY arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return RegReplaceKeyW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPCWSTR) arg4);
  else
    return RegReplaceKeyA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPCSTR) arg4);
}

LONG
qxeRegRestoreKey (HKEY arg1, const Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return RegRestoreKeyW (arg1, (LPCWSTR) arg2, arg3);
  else
    return RegRestoreKeyA (arg1, (LPCSTR) arg2, arg3);
}

LONG
qxeRegSaveKey (HKEY arg1, const Extbyte * arg2, LPSECURITY_ATTRIBUTES arg3)
{
  if (XEUNICODE_P)
    return RegSaveKeyW (arg1, (LPCWSTR) arg2, arg3);
  else
    return RegSaveKeyA (arg1, (LPCSTR) arg2, arg3);
}

LONG
qxeRegSetValueEx (HKEY arg1, const Extbyte * arg2, DWORD arg3, DWORD arg4, const BYTE* arg5, DWORD arg6)
{
  if (XEUNICODE_P)
    return RegSetValueExW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return RegSetValueExA (arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6);
}

LONG
qxeRegSetValue (HKEY arg1, const Extbyte * arg2, DWORD arg3, const Extbyte * arg4, DWORD arg5)
{
  if (XEUNICODE_P)
    return RegSetValueW (arg1, (LPCWSTR) arg2, arg3, (LPCWSTR) arg4, arg5);
  else
    return RegSetValueA (arg1, (LPCSTR) arg2, arg3, (LPCSTR) arg4, arg5);
}

LONG
qxeRegUnLoadKey (HKEY arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return RegUnLoadKeyW (arg1, (LPCWSTR) arg2);
  else
    return RegUnLoadKeyA (arg1, (LPCSTR) arg2);
}


/*----------------------------------------------------------------------*/
/*                       Processing file WINNLS.H                       */
/*----------------------------------------------------------------------*/

/* Error if CompareString used: not used, not examined yet */

/* Error if EnumCalendarInfo used: not used, not examined yet */

/* Error if EnumDateFormats used: not used, not examined yet */

/* Error if EnumSystemCodePages used: not used, not examined yet */

/* Error if EnumSystemLocales used: not used, not examined yet */

/* Error if EnumTimeFormats used: not used, not examined yet */

/* Error if FoldString used: not used, not examined yet */

/* Error if GetCalendarInfo used: Function needs review to determine how to handle it */

/* Error if GetCPInfoEx used: not used, not examined yet */

/* Error if GetCurrencyFormat used: not used, not examined yet */

/* Error if GetDateFormat used: not used, not examined yet */

/* Error if GetGeoInfo used: Function needs review to determine how to handle it */

int
qxeGetLocaleInfo (LCID arg1, LCTYPE arg2, Extbyte * arg3, int arg4)
{
  if (XEUNICODE_P)
    return GetLocaleInfoW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return GetLocaleInfoA (arg1, arg2, (LPSTR) arg3, arg4);
}

/* Error if GetNumberFormat used: not used, not examined yet */

/* Error if GetStringType used: no such fun; A and W versions have different nos. of args */

/* Error if GetStringTypeEx used: not used, not examined yet */

/* Error if GetTimeFormat used: not used, not examined yet */

/* Error if LCMapString used: not used, not examined yet */

/* Error if SetCalendarInfo used: Function needs review to determine how to handle it */

BOOL
qxeSetLocaleInfo (LCID arg1, LCTYPE arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return SetLocaleInfoW (arg1, arg2, (LPCWSTR) arg3);
  else
    return SetLocaleInfoA (arg1, arg2, (LPCSTR) arg3);
}

/* Error if EnumCalendarInfoEx used: not used, not examined yet */

/* Error if EnumDateFormatsEx used: not used, not examined yet */

/* Error if EnumSystemLanguageGroups used: Function needs review to determine how to handle it */

/* Error if EnumLanguageGroupLocales used: Function needs review to determine how to handle it */

/* Error if EnumUILanguages used: Function needs review to determine how to handle it */


/*----------------------------------------------------------------------*/
/*                       Processing file WINGDI.H                       */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

int
qxeAddFontResource (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return AddFontResourceW ((LPCWSTR) arg1);
  else
    return AddFontResourceA ((LPCSTR) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddFontResourceEx used: NT 5.0+ only */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HENHMETAFILE
qxeCopyEnhMetaFile (HENHMETAFILE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return CopyEnhMetaFileW (arg1, (LPCWSTR) arg2);
  else
    return CopyEnhMetaFileA (arg1, (LPCSTR) arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HMETAFILE
qxeCopyMetaFile (HMETAFILE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return CopyMetaFileW (arg1, (LPCWSTR) arg2);
  else
    return CopyMetaFileA (arg1, (LPCSTR) arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if CreateColorSpace used: split-sized LPLOGCOLORSPACE; NT 4.0+ only */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping CreateDC because split-sized DEVMODE */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HDC
qxeCreateEnhMetaFile (HDC arg1, const Extbyte * arg2, LPCRECT arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return CreateEnhMetaFileW (arg1, (LPCWSTR) arg2, arg3, (LPCWSTR) arg4);
  else
    return CreateEnhMetaFileA (arg1, (LPCSTR) arg2, arg3, (LPCSTR) arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HFONT
qxeCreateFont (int arg1, int arg2, int arg3, int arg4, int arg5, DWORD arg6, DWORD arg7, DWORD arg8, DWORD arg9, DWORD arg10, DWORD arg11, DWORD arg12, DWORD arg13, const Extbyte * arg14)
{
  if (XEUNICODE_P)
    return CreateFontW (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, (LPCWSTR) arg14);
  else
    return CreateFontA (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, (LPCSTR) arg14);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping CreateFontIndirect because split-sized LOGFONT */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping CreateIC because split-sized DEVMODE */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HDC
qxeCreateMetaFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return CreateMetaFileW ((LPCWSTR) arg1);
  else
    return CreateMetaFileA ((LPCSTR) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeCreateScalableFontResource (DWORD arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return CreateScalableFontResourceW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPCWSTR) arg4);
  else
    return CreateScalableFontResourceA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPCSTR) arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping DeviceCapabilities because split-sized DEVMODE */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumFontFamilies used: split-complex FONTENUMPROC */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping EnumFontFamiliesEx because split-complex FONTENUMPROC; NT 4.0+ only */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumFonts used: split-complex FONTENUMPROC */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: NT 4.0+ only */
int
qxeEnumICMProfiles (HDC arg1, ICMENUMPROCW arg2, LPARAM arg3)
{
  if (XEUNICODE_P)
    return EnumICMProfilesW (arg1, arg2, arg3);
  else
    return EnumICMProfilesA (arg1, (ICMENUMPROCA) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeExtTextOut (HDC arg1, int arg2, int arg3, UINT arg4, LPCRECT arg5, const Extbyte * arg6, UINT arg7, const INT* arg8)
{
  if (XEUNICODE_P)
    return ExtTextOutW (arg1, arg2, arg3, arg4, arg5, (LPCWSTR) arg6, arg7, arg8);
  else
    return ExtTextOutA (arg1, arg2, arg3, arg4, arg5, (LPCSTR) arg6, arg7, arg8);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetCharABCWidths (HDC arg1, UINT arg2, UINT arg3, LPABC arg4)
{
  if (XEUNICODE_P)
    return GetCharABCWidthsW (arg1, arg2, arg3, arg4);
  else
    return GetCharABCWidthsA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetCharABCWidthsFloat (HDC arg1, UINT arg2, UINT arg3, LPABCFLOAT arg4)
{
  if (XEUNICODE_P)
    return GetCharABCWidthsFloatW (arg1, arg2, arg3, arg4);
  else
    return GetCharABCWidthsFloatA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: NT 4.0+ only */
DWORD
qxeGetCharacterPlacement (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPGCP_RESULTSW arg5, DWORD arg6)
{
  if (XEUNICODE_P)
    return GetCharacterPlacementW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return GetCharacterPlacementA (arg1, (LPCSTR) arg2, arg3, arg4, (LPGCP_RESULTSA) arg5, arg6);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetCharWidth32 (HDC arg1, UINT arg2, UINT arg3, LPINT arg4)
{
  if (XEUNICODE_P)
    return GetCharWidth32W (arg1, arg2, arg3, arg4);
  else
    return GetCharWidth32A (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetCharWidth (HDC arg1, UINT arg2, UINT arg3, LPINT arg4)
{
  if (XEUNICODE_P)
    return GetCharWidthW (arg1, arg2, arg3, arg4);
  else
    return GetCharWidthA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetCharWidthFloat (HDC arg1, UINT arg2, UINT arg3, PFLOAT arg4)
{
  if (XEUNICODE_P)
    return GetCharWidthFloatW (arg1, arg2, arg3, arg4);
  else
    return GetCharWidthFloatA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HENHMETAFILE
qxeGetEnhMetaFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetEnhMetaFileW ((LPCWSTR) arg1);
  else
    return GetEnhMetaFileA ((LPCSTR) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeGetEnhMetaFileDescription (HENHMETAFILE arg1, UINT arg2, Extbyte * arg3)
{
  if (XEUNICODE_P)
    return GetEnhMetaFileDescriptionW (arg1, arg2, (LPWSTR) arg3);
  else
    return GetEnhMetaFileDescriptionA (arg1, arg2, (LPSTR) arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeGetGlyphOutline (HDC arg1, UINT arg2, UINT arg3, LPGLYPHMETRICS arg4, DWORD arg5, PVOID arg6, const MAT2* arg7)
{
  if (XEUNICODE_P)
    return GetGlyphOutlineW (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return GetGlyphOutlineA (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: NT 4.0+ only, former error in Cygwin prototype but no more (Cygwin 1.7, 1-30-10) */
BOOL
qxeGetICMProfile (HDC arg1, LPDWORD arg2, Extbyte * arg3)
{
  if (XEUNICODE_P)
    return GetICMProfileW (arg1, arg2, (LPWSTR) arg3);
  else
    return GetICMProfileA (arg1, arg2, (LPSTR) arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeGetKerningPairs (HDC arg1, DWORD arg2, LPKERNINGPAIR arg3)
{
  if (XEUNICODE_P)
    return GetKerningPairsW (arg1, arg2, arg3);
  else
    return GetKerningPairsA (arg1, arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetLogColorSpace used: split-sized LPLOGCOLORSPACE; NT 4.0+ only */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HMETAFILE
qxeGetMetaFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetMetaFileW ((LPCWSTR) arg1);
  else
    return GetMetaFileA ((LPCSTR) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping GetObject because split-sized LOGFONT */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetOutlineTextMetrics used: split-sized LPOUTLINETEXTMETRIC */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetTextExtentExPoint (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPINT arg5, LPINT arg6, LPSIZE arg7)
{
  if (XEUNICODE_P)
    return GetTextExtentExPointW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return GetTextExtentExPointA (arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6, arg7);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetTextExtentPoint (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4)
{
  if (XEUNICODE_P)
    return GetTextExtentPointW (arg1, (LPCWSTR) arg2, arg3, arg4);
  else
    return GetTextExtentPointA (arg1, (LPCSTR) arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetTextExtentPoint32 (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4)
{
  if (XEUNICODE_P)
    return GetTextExtentPoint32W (arg1, (LPCWSTR) arg2, arg3, arg4);
  else
    return GetTextExtentPoint32A (arg1, (LPCSTR) arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

int
qxeGetTextFace (HDC arg1, int arg2, Extbyte * arg3)
{
  if (XEUNICODE_P)
    return GetTextFaceW (arg1, arg2, (LPWSTR) arg3);
  else
    return GetTextFaceA (arg1, arg2, (LPSTR) arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping GetTextMetrics because split-sized LPTEXTMETRIC */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxePolyTextOut (HDC arg1, const POLYTEXTW* arg2, int arg3)
{
  if (XEUNICODE_P)
    return PolyTextOutW (arg1, arg2, arg3);
  else
    return PolyTextOutA (arg1, (const POLYTEXTA*) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeRemoveFontResource (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return RemoveFontResourceW ((LPCWSTR) arg1);
  else
    return RemoveFontResourceA ((LPCSTR) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if RemoveFontResourceEx used: NT 5.0+ only */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping ResetDC because split-sized DEVMODE */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: NT 4.0+ only */
BOOL
qxeSetICMProfile (HDC arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return SetICMProfileW (arg1, (LPWSTR) arg2);
  else
    return SetICMProfileA (arg1, (LPSTR) arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

int
qxeStartDoc (HDC arg1, const DOCINFOW* arg2)
{
  if (XEUNICODE_P)
    return StartDocW (arg1, arg2);
  else
    return StartDocA (arg1, (const DOCINFOA*) arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeTextOut (HDC arg1, int arg2, int arg3, const Extbyte * arg4, int arg5)
{
  if (XEUNICODE_P)
    return TextOutW (arg1, arg2, arg3, (LPCWSTR) arg4, arg5);
  else
    return TextOutA (arg1, arg2, arg3, (LPCSTR) arg4, arg5);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping UpdateICMRegKey because NT 4.0+ only, error in Cygwin prototype */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if wglUseFontBitmaps used: causes link error */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if wglUseFontOutlines used: causes link error */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetGlyphIndices used: NT 5.0+ only */

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                       Processing file SHLOBJ.H                       */
/*----------------------------------------------------------------------*/

/* Skipping SHBrowseForFolder because need to intercept callback for SendMessage */

/* Skipping SHGetDataFromIDList because split-sized WIN32_FIND_DATA or split-simple NETRESOURCE, missing from Cygwin libraries */

BOOL
qxeSHGetPathFromIDList (LPCITEMIDLIST arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return SHGetPathFromIDListW (arg1, (LPWSTR) arg2);
  else
    return SHGetPathFromIDListA (arg1, (LPSTR) arg2);
}

/* Skipping SHGetSpecialFolderPath because error in Cygwin prototype, missing from Cygwin libraries */

/* Error if SHGetFolderPath used: Function needs review to determine how to handle it */

/* Error if SHGetIconOverlayIndex used: Function needs review to determine how to handle it */

/* Error if SHCreateDirectoryEx used: Function needs review to determine how to handle it */

/* Error if SHGetFolderPathAndSubDir used: Function needs review to determine how to handle it */


/*----------------------------------------------------------------------*/
/*                      Processing file COMMDLG.H                       */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeChooseColor (LPCHOOSECOLORW arg1)
{
  if (XEUNICODE_P)
    return ChooseColorW (arg1);
  else
    return ChooseColorA ((LPCHOOSECOLORA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ChooseFont used: split-sized LPLOGFONT in LPCHOOSEFONT */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HWND
qxeFindText (LPFINDREPLACEW arg1)
{
  if (XEUNICODE_P)
    return FindTextW (arg1);
  else
    return FindTextA ((LPFINDREPLACEA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

short
qxeGetFileTitle (const Extbyte * arg1, Extbyte * arg2, WORD arg3)
{
  if (XEUNICODE_P)
    return GetFileTitleW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return GetFileTitleA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetOpenFileName (LPOPENFILENAMEW arg1)
{
  if (XEUNICODE_P)
    return GetOpenFileNameW (arg1);
  else
    return GetOpenFileNameA ((LPOPENFILENAMEA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeGetSaveFileName (LPOPENFILENAMEW arg1)
{
  if (XEUNICODE_P)
    return GetSaveFileNameW (arg1);
  else
    return GetSaveFileNameA ((LPOPENFILENAMEA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping PageSetupDlg because LPPAGESETUPDLG with split-sized DEVMODE handle */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping PrintDlg because LPPRINTDLG with split-sized DEVMODE handle */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

HWND
qxeReplaceText (LPFINDREPLACEW arg1)
{
  if (XEUNICODE_P)
    return ReplaceTextW (arg1);
  else
    return ReplaceTextA ((LPFINDREPLACEA) arg1);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if PrintDlgEx used: Function needs review to determine how to handle it */

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                        Processing file IMM.H                         */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

HKL
qxeImmInstallIME (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return ImmInstallIMEW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return ImmInstallIMEA ((LPCSTR) arg1, (LPCSTR) arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeImmGetDescription (HKL arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return ImmGetDescriptionW (arg1, (LPWSTR) arg2, arg3);
  else
    return ImmGetDescriptionA (arg1, (LPSTR) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeImmGetIMEFileName (HKL arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return ImmGetIMEFileNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return ImmGetIMEFileNameA (arg1, (LPSTR) arg2, arg3);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

LONG
qxeImmGetCompositionString (HIMC arg1, DWORD arg2, PVOID arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return ImmGetCompositionStringW (arg1, arg2, arg3, arg4);
  else
    return ImmGetCompositionStringA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping ImmSetCompositionString because different prototypes in VC6 and VC7 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetCandidateListCount (HIMC arg1, PDWORD arg2)
{
  if (XEUNICODE_P)
    return ImmGetCandidateListCountW (arg1, arg2);
  else
    return ImmGetCandidateListCountA (arg1, arg2);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetCandidateList (HIMC arg1, DWORD arg2, PCANDIDATELIST arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return ImmGetCandidateListW (arg1, arg2, arg3, arg4);
  else
    return ImmGetCandidateListA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetGuideLine (HIMC arg1, DWORD arg2, Extbyte * arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return ImmGetGuideLineW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return ImmGetGuideLineA (arg1, arg2, (LPSTR) arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping ImmGetCompositionFont because split-sized LOGFONT */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping ImmSetCompositionFont because split-sized LOGFONT */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: // split-simple REGISTERWORD */
BOOL
qxeImmConfigureIME (HKL arg1, HWND arg2, DWORD arg3, PVOID arg4)
{
  if (XEUNICODE_P)
    return ImmConfigureIMEW (arg1, arg2, arg3, arg4);
  else
    return ImmConfigureIMEA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: // strings of various sorts */
LRESULT
qxeImmEscape (HKL arg1, HIMC arg2, UINT arg3, PVOID arg4)
{
  if (XEUNICODE_P)
    return ImmEscapeW (arg1, arg2, arg3, arg4);
  else
    return ImmEscapeA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetConversionList (HKL arg1, HIMC arg2, const Extbyte * arg3, PCANDIDATELIST arg4, DWORD arg5, UINT arg6)
{
  if (XEUNICODE_P)
    return ImmGetConversionListW (arg1, arg2, (LPCWSTR) arg3, arg4, arg5, arg6);
  else
    return ImmGetConversionListA (arg1, arg2, (LPCSTR) arg3, arg4, arg5, arg6);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeImmIsUIMessage (HWND arg1, UINT arg2, WPARAM arg3, LPARAM arg4)
{
  if (XEUNICODE_P)
    return ImmIsUIMessageW (arg1, arg2, arg3, arg4);
  else
    return ImmIsUIMessageA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeImmRegisterWord (HKL arg1, const Extbyte * arg2, DWORD arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return ImmRegisterWordW (arg1, (LPCWSTR) arg2, arg3, (LPCWSTR) arg4);
  else
    return ImmRegisterWordA (arg1, (LPCSTR) arg2, arg3, (LPCSTR) arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeImmUnregisterWord (HKL arg1, const Extbyte * arg2, DWORD arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return ImmUnregisterWordW (arg1, (LPCWSTR) arg2, arg3, (LPCWSTR) arg4);
  else
    return ImmUnregisterWordA (arg1, (LPCSTR) arg2, arg3, (LPCSTR) arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ImmGetRegisterWordStyle used: split-sized STYLEBUF */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeImmEnumRegisterWord (HKL arg1, REGISTERWORDENUMPROCW arg2, const Extbyte * arg3, DWORD arg4, const Extbyte * arg5, PVOID arg6)
{
  if (XEUNICODE_P)
    return ImmEnumRegisterWordW (arg1, arg2, (LPCWSTR) arg3, arg4, (LPCWSTR) arg5, arg6);
  else
    return ImmEnumRegisterWordA (arg1, (REGISTERWORDENUMPROCA) arg2, (LPCSTR) arg3, arg4, (LPCSTR) arg5, arg6);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ImmGetImeMenuItems used: split-sized IMEMENUITEMINFO */

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                      Processing file WINBASE.H                       */
/*----------------------------------------------------------------------*/

BOOL
qxeAccessCheckAndAuditAlarm (const Extbyte * arg1, LPVOID arg2, Extbyte * arg3, Extbyte * arg4, PSECURITY_DESCRIPTOR arg5, DWORD arg6, PGENERIC_MAPPING arg7, BOOL arg8, PDWORD arg9, PBOOL arg10, PBOOL arg11)
{
  if (XEUNICODE_P)
    return AccessCheckAndAuditAlarmW ((LPCWSTR) arg1, arg2, (LPWSTR) arg3, (LPWSTR) arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  else
    return AccessCheckAndAuditAlarmA ((LPCSTR) arg1, arg2, (LPSTR) arg3, (LPSTR) arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

ATOM
qxeAddAtom (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return AddAtomW ((LPCWSTR) arg1);
  else
    return AddAtomA ((LPCSTR) arg1);
}

BOOL
qxeBackupEventLog (HANDLE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return BackupEventLogW (arg1, (LPCWSTR) arg2);
  else
    return BackupEventLogA (arg1, (LPCSTR) arg2);
}

HANDLE
qxeBeginUpdateResource (const Extbyte * arg1, BOOL arg2)
{
  if (XEUNICODE_P)
    return BeginUpdateResourceW ((LPCWSTR) arg1, arg2);
  else
    return BeginUpdateResourceA ((LPCSTR) arg1, arg2);
}

BOOL
qxeBuildCommDCB (const Extbyte * arg1, LPDCB arg2)
{
  if (XEUNICODE_P)
    return BuildCommDCBW ((LPCWSTR) arg1, arg2);
  else
    return BuildCommDCBA ((LPCSTR) arg1, arg2);
}

BOOL
qxeBuildCommDCBAndTimeouts (const Extbyte * arg1, LPDCB arg2, LPCOMMTIMEOUTS arg3)
{
  if (XEUNICODE_P)
    return BuildCommDCBAndTimeoutsW ((LPCWSTR) arg1, arg2, arg3);
  else
    return BuildCommDCBAndTimeoutsA ((LPCSTR) arg1, arg2, arg3);
}

BOOL
qxeCallNamedPipe (const Extbyte * arg1, PVOID arg2, DWORD arg3, PVOID arg4, DWORD arg5, PDWORD arg6, DWORD arg7)
{
  if (XEUNICODE_P)
    return CallNamedPipeW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return CallNamedPipeA ((LPCSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

/* Error if CheckNameLegalDOS8Dot3 used: Function needs review to determine how to handle it */

BOOL
qxeClearEventLog (HANDLE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return ClearEventLogW (arg1, (LPCWSTR) arg2);
  else
    return ClearEventLogA (arg1, (LPCSTR) arg2);
}

BOOL
qxeCommConfigDialog (const Extbyte * arg1, HWND arg2, LPCOMMCONFIG arg3)
{
  if (XEUNICODE_P)
    return CommConfigDialogW ((LPCWSTR) arg1, arg2, arg3);
  else
    return CommConfigDialogA ((LPCSTR) arg1, arg2, arg3);
}

BOOL
qxeCopyFile (const Extbyte * arg1, const Extbyte * arg2, BOOL arg3)
{
  if (XEUNICODE_P)
    return CopyFileW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3);
  else
    return CopyFileA ((LPCSTR) arg1, (LPCSTR) arg2, arg3);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeCopyFileEx (const Extbyte * arg1, const Extbyte * arg2, LPPROGRESS_ROUTINE arg3, LPVOID arg4, LPBOOL arg5, DWORD arg6)
{
  if (XEUNICODE_P)
    return CopyFileExW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return CopyFileExA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6);
}

/* Error if CreateActCtx used: Function needs review to determine how to handle it */

BOOL
qxeCreateDirectory (const Extbyte * arg1, LPSECURITY_ATTRIBUTES arg2)
{
  if (XEUNICODE_P)
    return CreateDirectoryW ((LPCWSTR) arg1, arg2);
  else
    return CreateDirectoryA ((LPCSTR) arg1, arg2);
}

BOOL
qxeCreateDirectoryEx (const Extbyte * arg1, const Extbyte * arg2, LPSECURITY_ATTRIBUTES arg3)
{
  if (XEUNICODE_P)
    return CreateDirectoryExW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3);
  else
    return CreateDirectoryExA ((LPCSTR) arg1, (LPCSTR) arg2, arg3);
}

HANDLE
qxeCreateEvent (LPSECURITY_ATTRIBUTES arg1, BOOL arg2, BOOL arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return CreateEventW (arg1, arg2, arg3, (LPCWSTR) arg4);
  else
    return CreateEventA (arg1, arg2, arg3, (LPCSTR) arg4);
}

HANDLE
qxeCreateFile (const Extbyte * arg1, DWORD arg2, DWORD arg3, LPSECURITY_ATTRIBUTES arg4, DWORD arg5, DWORD arg6, HANDLE arg7)
{
  if (XEUNICODE_P)
    return CreateFileW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return CreateFileA ((LPCSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

HANDLE
qxeCreateFileMapping (HANDLE arg1, LPSECURITY_ATTRIBUTES arg2, DWORD arg3, DWORD arg4, DWORD arg5, const Extbyte * arg6)
{
  if (XEUNICODE_P)
    return CreateFileMappingW (arg1, arg2, arg3, arg4, arg5, (LPCWSTR) arg6);
  else
    return CreateFileMappingA (arg1, arg2, arg3, arg4, arg5, (LPCSTR) arg6);
}

/* Error if CreateHardLink used: NT 5.0+ only */

/* Error if CreateJobObject used: NT 5.0+ only */

HANDLE
qxeCreateMailslot (const Extbyte * arg1, DWORD arg2, DWORD arg3, LPSECURITY_ATTRIBUTES arg4)
{
  if (XEUNICODE_P)
    return CreateMailslotW ((LPCWSTR) arg1, arg2, arg3, arg4);
  else
    return CreateMailslotA ((LPCSTR) arg1, arg2, arg3, arg4);
}

HANDLE
qxeCreateMutex (LPSECURITY_ATTRIBUTES arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return CreateMutexW (arg1, arg2, (LPCWSTR) arg3);
  else
    return CreateMutexA (arg1, arg2, (LPCSTR) arg3);
}

HANDLE
qxeCreateNamedPipe (const Extbyte * arg1, DWORD arg2, DWORD arg3, DWORD arg4, DWORD arg5, DWORD arg6, DWORD arg7, LPSECURITY_ATTRIBUTES arg8)
{
  if (XEUNICODE_P)
    return CreateNamedPipeW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  else
    return CreateNamedPipeA ((LPCSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

BOOL
qxeCreateProcess (const Extbyte * arg1, Extbyte * arg2, LPSECURITY_ATTRIBUTES arg3, LPSECURITY_ATTRIBUTES arg4, BOOL arg5, DWORD arg6, PVOID arg7, const Extbyte * arg8, LPSTARTUPINFOW arg9, LPPROCESS_INFORMATION arg10)
{
  if (XEUNICODE_P)
    return CreateProcessW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3, arg4, arg5, arg6, arg7, (LPCWSTR) arg8, arg9, arg10);
  else
    return CreateProcessA ((LPCSTR) arg1, (LPSTR) arg2, arg3, arg4, arg5, arg6, arg7, (LPCSTR) arg8, (LPSTARTUPINFOA) arg9, arg10);
}

BOOL
qxeCreateProcessAsUser (HANDLE arg1, const Extbyte * arg2, Extbyte * arg3, LPSECURITY_ATTRIBUTES arg4, LPSECURITY_ATTRIBUTES arg5, BOOL arg6, DWORD arg7, PVOID arg8, const Extbyte * arg9, LPSTARTUPINFOW arg10, LPPROCESS_INFORMATION arg11)
{
  if (XEUNICODE_P)
    return CreateProcessAsUserW (arg1, (LPCWSTR) arg2, (LPWSTR) arg3, arg4, arg5, arg6, arg7, arg8, (LPCWSTR) arg9, arg10, arg11);
  else
    return CreateProcessAsUserA (arg1, (LPCSTR) arg2, (LPSTR) arg3, arg4, arg5, arg6, arg7, arg8, (LPCSTR) arg9, (LPSTARTUPINFOA) arg10, arg11);
}

/* Error if CreateProcessWithLogon used: Function needs review to determine how to handle it */

HANDLE
qxeCreateSemaphore (LPSECURITY_ATTRIBUTES arg1, LONG arg2, LONG arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return CreateSemaphoreW (arg1, arg2, arg3, (LPCWSTR) arg4);
  else
    return CreateSemaphoreA (arg1, arg2, arg3, (LPCSTR) arg4);
}

HANDLE
qxeCreateWaitableTimer (LPSECURITY_ATTRIBUTES arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return CreateWaitableTimerW (arg1, arg2, (LPCWSTR) arg3);
  else
    return CreateWaitableTimerA (arg1, arg2, (LPCSTR) arg3);
}

BOOL
qxeDefineDosDevice (DWORD arg1, const Extbyte * arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return DefineDosDeviceW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3);
  else
    return DefineDosDeviceA (arg1, (LPCSTR) arg2, (LPCSTR) arg3);
}

BOOL
qxeDeleteFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return DeleteFileW ((LPCWSTR) arg1);
  else
    return DeleteFileA ((LPCSTR) arg1);
}

/* Error if DeleteVolumeMountPoint used: Function needs review to determine how to handle it */

/* Error if DnsHostnameToComputerName used: Function needs review to determine how to handle it */

#if !defined (CYGWIN_HEADERS)

/* Error if EncryptFile used: Win2K+ only */

#endif /* !defined (CYGWIN_HEADERS) */

BOOL
qxeEndUpdateResource (HANDLE arg1, BOOL arg2)
{
  if (XEUNICODE_P)
    return EndUpdateResourceW (arg1, arg2);
  else
    return EndUpdateResourceA (arg1, arg2);
}

/* Skipping EnumResourceLanguages because different prototypes in VC6 and VC7 */

/* Skipping EnumResourceNames because different prototypes in VC6 and VC7 */

/* Skipping EnumResourceTypes because different prototypes in VC6 and VC7 */

DWORD
qxeExpandEnvironmentStrings (const Extbyte * arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return ExpandEnvironmentStringsW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return ExpandEnvironmentStringsA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

void
qxeFatalAppExit (UINT arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    FatalAppExitW (arg1, (LPCWSTR) arg2);
  else
    FatalAppExitA (arg1, (LPCSTR) arg2);
}

/* Error if FileEncryptionStatus used: Function needs review to determine how to handle it */

/* Error if FindActCtxSectionString used: Function needs review to determine how to handle it */

ATOM
qxeFindAtom (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return FindAtomW ((LPCWSTR) arg1);
  else
    return FindAtomA ((LPCSTR) arg1);
}

HANDLE
qxeFindFirstChangeNotification (const Extbyte * arg1, BOOL arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return FindFirstChangeNotificationW ((LPCWSTR) arg1, arg2, arg3);
  else
    return FindFirstChangeNotificationA ((LPCSTR) arg1, arg2, arg3);
}

/* Skipping FindFirstFile because split-sized LPWIN32_FIND_DATA */

/* Error if FindFirstFileEx used: split-sized LPWIN32_FIND_DATA; not used, NT 4.0+ only */

/* Error if FindFirstVolume used: Function needs review to determine how to handle it */

/* Error if FindFirstVolumeMountPoint used: Function needs review to determine how to handle it */

/* Skipping FindNextFile because split-sized LPWIN32_FIND_DATA */

/* Error if FindNextVolume used: Function needs review to determine how to handle it */

/* Error if FindNextVolumeMountPoint used: Function needs review to determine how to handle it */

HRSRC
qxeFindResource (HINSTANCE arg1, const Extbyte * arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return FindResourceW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3);
  else
    return FindResourceA (arg1, (LPCSTR) arg2, (LPCSTR) arg3);
}

HRSRC
qxeFindResourceEx (HINSTANCE arg1, const Extbyte * arg2, const Extbyte * arg3, WORD arg4)
{
  if (XEUNICODE_P)
    return FindResourceExW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4);
  else
    return FindResourceExA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4);
}

/* Error if GetFirmwareEnvironmentVariable used: Function needs review to determine how to handle it */

DWORD
qxeFormatMessage (DWORD arg1, PCVOID arg2, DWORD arg3, DWORD arg4, Extbyte * arg5, DWORD arg6, va_list* arg7)
{
  if (XEUNICODE_P)
    return FormatMessageW (arg1, arg2, arg3, arg4, (LPWSTR) arg5, arg6, arg7);
  else
    return FormatMessageA (arg1, arg2, arg3, arg4, (LPSTR) arg5, arg6, arg7);
}

BOOL
qxeFreeEnvironmentStrings (Extbyte * arg1)
{
  if (XEUNICODE_P)
    return FreeEnvironmentStringsW ((LPWSTR) arg1);
  else
    return FreeEnvironmentStringsA ((LPSTR) arg1);
}

UINT
qxeGetAtomName (ATOM arg1, Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return GetAtomNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return GetAtomNameA (arg1, (LPSTR) arg2, arg3);
}

BOOL
qxeGetBinaryType (const Extbyte * arg1, PDWORD arg2)
{
  if (XEUNICODE_P)
    return GetBinaryTypeW ((LPCWSTR) arg1, arg2);
  else
    return GetBinaryTypeA ((LPCSTR) arg1, arg2);
}

Extbyte *
qxeGetCommandLine (void)
{
  if (XEUNICODE_P)
    return (Extbyte *) GetCommandLineW ();
  else
    return (Extbyte *) GetCommandLineA ();
}

DWORD
qxeGetCompressedFileSize (const Extbyte * arg1, PDWORD arg2)
{
  if (XEUNICODE_P)
    return GetCompressedFileSizeW ((LPCWSTR) arg1, arg2);
  else
    return GetCompressedFileSizeA ((LPCSTR) arg1, arg2);
}

BOOL
qxeGetComputerName (Extbyte * arg1, PDWORD arg2)
{
  if (XEUNICODE_P)
    return GetComputerNameW ((LPWSTR) arg1, arg2);
  else
    return GetComputerNameA ((LPSTR) arg1, arg2);
}

/* Error if GetComputerNameEx used: Function needs review to determine how to handle it */

DWORD
qxeGetCurrentDirectory (DWORD arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return GetCurrentDirectoryW (arg1, (LPWSTR) arg2);
  else
    return GetCurrentDirectoryA (arg1, (LPSTR) arg2);
}

/* Error if GetCurrentHwProfile used: split-sized LPHW_PROFILE_INFO; NT 4.0+ only */

BOOL
qxeGetDefaultCommConfig (const Extbyte * arg1, LPCOMMCONFIG arg2, PDWORD arg3)
{
  if (XEUNICODE_P)
    return GetDefaultCommConfigW ((LPCWSTR) arg1, arg2, arg3);
  else
    return GetDefaultCommConfigA ((LPCSTR) arg1, arg2, arg3);
}

BOOL
qxeGetDiskFreeSpace (const Extbyte * arg1, PDWORD arg2, PDWORD arg3, PDWORD arg4, PDWORD arg5)
{
  if (XEUNICODE_P)
    return GetDiskFreeSpaceW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5);
  else
    return GetDiskFreeSpaceA ((LPCSTR) arg1, arg2, arg3, arg4, arg5);
}

BOOL
qxeGetDiskFreeSpaceEx (const Extbyte * arg1, PULARGE_INTEGER arg2, PULARGE_INTEGER arg3, PULARGE_INTEGER arg4)
{
  if (XEUNICODE_P)
    return GetDiskFreeSpaceExW ((LPCWSTR) arg1, arg2, arg3, arg4);
  else
    return GetDiskFreeSpaceExA ((LPCSTR) arg1, arg2, arg3, arg4);
}

/* Error if GetDllDirectory used: Function needs review to determine how to handle it */

UINT
qxeGetDriveType (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetDriveTypeW ((LPCWSTR) arg1);
  else
    return GetDriveTypeA ((LPCSTR) arg1);
}

DWORD
qxeGetEnvironmentVariable (const Extbyte * arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return GetEnvironmentVariableW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return GetEnvironmentVariableA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

DWORD
qxeGetFileAttributes (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetFileAttributesW ((LPCWSTR) arg1);
  else
    return GetFileAttributesA ((LPCSTR) arg1);
}

BOOL
qxeGetFileAttributesEx (const Extbyte * arg1, GET_FILEEX_INFO_LEVELS arg2, PVOID arg3)
{
  if (XEUNICODE_P)
    return GetFileAttributesExW ((LPCWSTR) arg1, arg2, arg3);
  else
    return GetFileAttributesExA ((LPCSTR) arg1, arg2, arg3);
}

BOOL
qxeGetFileSecurity (const Extbyte * arg1, SECURITY_INFORMATION arg2, PSECURITY_DESCRIPTOR arg3, DWORD arg4, PDWORD arg5)
{
  if (XEUNICODE_P)
    return GetFileSecurityW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5);
  else
    return GetFileSecurityA ((LPCSTR) arg1, arg2, arg3, arg4, arg5);
}

DWORD
qxeGetFullPathName (const Extbyte * arg1, DWORD arg2, Extbyte * arg3, Extbyte ** arg4)
{
  if (XEUNICODE_P)
    return GetFullPathNameW ((LPCWSTR) arg1, arg2, (LPWSTR) arg3, (LPWSTR*) arg4);
  else
    return GetFullPathNameA ((LPCSTR) arg1, arg2, (LPSTR) arg3, (LPSTR*) arg4);
}

DWORD
qxeGetLogicalDriveStrings (DWORD arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return GetLogicalDriveStringsW (arg1, (LPWSTR) arg2);
  else
    return GetLogicalDriveStringsA (arg1, (LPSTR) arg2);
}

/* Error if GetLongPathName used: Win98/2K+ only */

DWORD
qxeGetModuleFileName (HINSTANCE arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return GetModuleFileNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return GetModuleFileNameA (arg1, (LPSTR) arg2, arg3);
}

HMODULE
qxeGetModuleHandle (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetModuleHandleW ((LPCWSTR) arg1);
  else
    return GetModuleHandleA ((LPCSTR) arg1);
}

/* Error if GetModuleHandleEx used: Function needs review to determine how to handle it */

BOOL
qxeGetNamedPipeHandleState (HANDLE arg1, PDWORD arg2, PDWORD arg3, PDWORD arg4, PDWORD arg5, Extbyte * arg6, DWORD arg7)
{
  if (XEUNICODE_P)
    return GetNamedPipeHandleStateW (arg1, arg2, arg3, arg4, arg5, (LPWSTR) arg6, arg7);
  else
    return GetNamedPipeHandleStateA (arg1, arg2, arg3, arg4, arg5, (LPSTR) arg6, arg7);
}

UINT
qxeGetPrivateProfileInt (const Extbyte * arg1, const Extbyte * arg2, INT arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return GetPrivateProfileIntW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, (LPCWSTR) arg4);
  else
    return GetPrivateProfileIntA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, (LPCSTR) arg4);
}

DWORD
qxeGetPrivateProfileSection (const Extbyte * arg1, Extbyte * arg2, DWORD arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return GetPrivateProfileSectionW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3, (LPCWSTR) arg4);
  else
    return GetPrivateProfileSectionA ((LPCSTR) arg1, (LPSTR) arg2, arg3, (LPCSTR) arg4);
}

DWORD
qxeGetPrivateProfileSectionNames (Extbyte * arg1, DWORD arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return GetPrivateProfileSectionNamesW ((LPWSTR) arg1, arg2, (LPCWSTR) arg3);
  else
    return GetPrivateProfileSectionNamesA ((LPSTR) arg1, arg2, (LPCSTR) arg3);
}

DWORD
qxeGetPrivateProfileString (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3, Extbyte * arg4, DWORD arg5, const Extbyte * arg6)
{
  if (XEUNICODE_P)
    return GetPrivateProfileStringW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPWSTR) arg4, arg5, (LPCWSTR) arg6);
  else
    return GetPrivateProfileStringA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPSTR) arg4, arg5, (LPCSTR) arg6);
}

BOOL
qxeGetPrivateProfileStruct (const Extbyte * arg1, const Extbyte * arg2, LPVOID arg3, UINT arg4, const Extbyte * arg5)
{
  if (XEUNICODE_P)
    return GetPrivateProfileStructW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, arg4, (LPCWSTR) arg5);
  else
    return GetPrivateProfileStructA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, arg4, (LPCSTR) arg5);
}

UINT
qxeGetProfileInt (const Extbyte * arg1, const Extbyte * arg2, INT arg3)
{
  if (XEUNICODE_P)
    return GetProfileIntW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3);
  else
    return GetProfileIntA ((LPCSTR) arg1, (LPCSTR) arg2, arg3);
}

DWORD
qxeGetProfileSection (const Extbyte * arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return GetProfileSectionW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return GetProfileSectionA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

DWORD
qxeGetProfileString (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3, Extbyte * arg4, DWORD arg5)
{
  if (XEUNICODE_P)
    return GetProfileStringW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPWSTR) arg4, arg5);
  else
    return GetProfileStringA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPSTR) arg4, arg5);
}

DWORD
qxeGetShortPathName (const Extbyte * arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return GetShortPathNameW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return GetShortPathNameA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

VOID
qxeGetStartupInfo (LPSTARTUPINFOW arg1)
{
  if (XEUNICODE_P)
    GetStartupInfoW (arg1);
  else
    GetStartupInfoA ((LPSTARTUPINFOA) arg1);
}

UINT
qxeGetSystemDirectory (Extbyte * arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return GetSystemDirectoryW ((LPWSTR) arg1, arg2);
  else
    return GetSystemDirectoryA ((LPSTR) arg1, arg2);
}

/* Error if GetSystemWindowsDirectory used: Function needs review to determine how to handle it */

/* Error if GetSystemWow64Directory used: Function needs review to determine how to handle it */

UINT
qxeGetTempFileName (const Extbyte * arg1, const Extbyte * arg2, UINT arg3, Extbyte * arg4)
{
  if (XEUNICODE_P)
    return GetTempFileNameW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, (LPWSTR) arg4);
  else
    return GetTempFileNameA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, (LPSTR) arg4);
}

DWORD
qxeGetTempPath (DWORD arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return GetTempPathW (arg1, (LPWSTR) arg2);
  else
    return GetTempPathA (arg1, (LPSTR) arg2);
}

BOOL
qxeGetUserName (Extbyte * arg1, PDWORD arg2)
{
  if (XEUNICODE_P)
    return GetUserNameW ((LPWSTR) arg1, arg2);
  else
    return GetUserNameA ((LPSTR) arg1, arg2);
}

/* Error if GetVersionEx used: split-sized LPOSVERSIONINFO */

BOOL
qxeGetVolumeInformation (const Extbyte * arg1, Extbyte * arg2, DWORD arg3, PDWORD arg4, PDWORD arg5, PDWORD arg6, Extbyte * arg7, DWORD arg8)
{
  if (XEUNICODE_P)
    return GetVolumeInformationW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3, arg4, arg5, arg6, (LPWSTR) arg7, arg8);
  else
    return GetVolumeInformationA ((LPCSTR) arg1, (LPSTR) arg2, arg3, arg4, arg5, arg6, (LPSTR) arg7, arg8);
}

/* Error if GetVolumeNameForVolumeMountPoint used: Function needs review to determine how to handle it */

/* Error if GetVolumePathName used: Function needs review to determine how to handle it */

/* Error if GetVolumePathNamesForVolumeName used: Function needs review to determine how to handle it */

UINT
qxeGetWindowsDirectory (Extbyte * arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return GetWindowsDirectoryW ((LPWSTR) arg1, arg2);
  else
    return GetWindowsDirectoryA ((LPSTR) arg1, arg2);
}

ATOM
qxeGlobalAddAtom (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GlobalAddAtomW ((LPCWSTR) arg1);
  else
    return GlobalAddAtomA ((LPCSTR) arg1);
}

ATOM
qxeGlobalFindAtom (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GlobalFindAtomW ((LPCWSTR) arg1);
  else
    return GlobalFindAtomA ((LPCSTR) arg1);
}

UINT
qxeGlobalGetAtomName (ATOM arg1, Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return GlobalGetAtomNameW (arg1, (LPWSTR) arg2, arg3);
  else
    return GlobalGetAtomNameA (arg1, (LPSTR) arg2, arg3);
}

BOOL
qxeIsBadStringPtr (const Extbyte * arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return IsBadStringPtrW ((LPCWSTR) arg1, arg2);
  else
    return IsBadStringPtrA ((LPCSTR) arg1, arg2);
}

HINSTANCE
qxeLoadLibraryEx (const Extbyte * arg1, HANDLE arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return LoadLibraryExW ((LPCWSTR) arg1, arg2, arg3);
  else
    return LoadLibraryExA ((LPCSTR) arg1, arg2, arg3);
}

HINSTANCE
qxeLoadLibrary (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return LoadLibraryW ((LPCWSTR) arg1);
  else
    return LoadLibraryA ((LPCSTR) arg1);
}

BOOL
qxeLogonUser (Extbyte * arg1, Extbyte * arg2, Extbyte * arg3, DWORD arg4, DWORD arg5, PHANDLE arg6)
{
  if (XEUNICODE_P)
    return LogonUserW ((LPWSTR) arg1, (LPWSTR) arg2, (LPWSTR) arg3, arg4, arg5, arg6);
  else
    return LogonUserA ((LPSTR) arg1, (LPSTR) arg2, (LPSTR) arg3, arg4, arg5, arg6);
}

BOOL
qxeLookupAccountName (const Extbyte * arg1, const Extbyte * arg2, PSID arg3, PDWORD arg4, Extbyte * arg5, PDWORD arg6, PSID_NAME_USE arg7)
{
  if (XEUNICODE_P)
    return LookupAccountNameW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, arg4, (LPWSTR) arg5, arg6, arg7);
  else
    return LookupAccountNameA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, arg4, (LPSTR) arg5, arg6, arg7);
}

BOOL
qxeLookupAccountSid (const Extbyte * arg1, PSID arg2, Extbyte * arg3, PDWORD arg4, Extbyte * arg5, PDWORD arg6, PSID_NAME_USE arg7)
{
  if (XEUNICODE_P)
    return LookupAccountSidW ((LPCWSTR) arg1, arg2, (LPWSTR) arg3, arg4, (LPWSTR) arg5, arg6, arg7);
  else
    return LookupAccountSidA ((LPCSTR) arg1, arg2, (LPSTR) arg3, arg4, (LPSTR) arg5, arg6, arg7);
}

BOOL
qxeLookupPrivilegeDisplayName (const Extbyte * arg1, const Extbyte * arg2, Extbyte * arg3, PDWORD arg4, PDWORD arg5)
{
  if (XEUNICODE_P)
    return LookupPrivilegeDisplayNameW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPWSTR) arg3, arg4, arg5);
  else
    return LookupPrivilegeDisplayNameA ((LPCSTR) arg1, (LPCSTR) arg2, (LPSTR) arg3, arg4, arg5);
}

BOOL
qxeLookupPrivilegeName (const Extbyte * arg1, PLUID arg2, Extbyte * arg3, PDWORD arg4)
{
  if (XEUNICODE_P)
    return LookupPrivilegeNameW ((LPCWSTR) arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return LookupPrivilegeNameA ((LPCSTR) arg1, arg2, (LPSTR) arg3, arg4);
}

BOOL
qxeLookupPrivilegeValue (const Extbyte * arg1, const Extbyte * arg2, PLUID arg3)
{
  if (XEUNICODE_P)
    return LookupPrivilegeValueW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3);
  else
    return LookupPrivilegeValueA ((LPCSTR) arg1, (LPCSTR) arg2, arg3);
}

Extbyte *
qxelstrcat (Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return (Extbyte *) lstrcatW ((LPWSTR) arg1, (LPCWSTR) arg2);
  else
    return (Extbyte *) lstrcatA ((LPSTR) arg1, (LPCSTR) arg2);
}

int
qxelstrcmpi (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return lstrcmpiW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return lstrcmpiA ((LPCSTR) arg1, (LPCSTR) arg2);
}

int
qxelstrcmp (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return lstrcmpW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return lstrcmpA ((LPCSTR) arg1, (LPCSTR) arg2);
}

Extbyte *
qxelstrcpyn (Extbyte * arg1, const Extbyte * arg2, int arg3)
{
  if (XEUNICODE_P)
    return (Extbyte *) lstrcpynW ((LPWSTR) arg1, (LPCWSTR) arg2, arg3);
  else
    return (Extbyte *) lstrcpynA ((LPSTR) arg1, (LPCSTR) arg2, arg3);
}

Extbyte *
qxelstrcpy (Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return (Extbyte *) lstrcpyW ((LPWSTR) arg1, (LPCWSTR) arg2);
  else
    return (Extbyte *) lstrcpyA ((LPSTR) arg1, (LPCSTR) arg2);
}

int
qxelstrlen (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return lstrlenW ((LPCWSTR) arg1);
  else
    return lstrlenA ((LPCSTR) arg1);
}

BOOL
qxeMoveFileEx (const Extbyte * arg1, const Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return MoveFileExW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3);
  else
    return MoveFileExA ((LPCSTR) arg1, (LPCSTR) arg2, arg3);
}

BOOL
qxeMoveFile (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return MoveFileW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return MoveFileA ((LPCSTR) arg1, (LPCSTR) arg2);
}

BOOL
qxeObjectCloseAuditAlarm (const Extbyte * arg1, PVOID arg2, BOOL arg3)
{
  if (XEUNICODE_P)
    return ObjectCloseAuditAlarmW ((LPCWSTR) arg1, arg2, arg3);
  else
    return ObjectCloseAuditAlarmA ((LPCSTR) arg1, arg2, arg3);
}

BOOL
qxeObjectDeleteAuditAlarm (const Extbyte * arg1, PVOID arg2, BOOL arg3)
{
  if (XEUNICODE_P)
    return ObjectDeleteAuditAlarmW ((LPCWSTR) arg1, arg2, arg3);
  else
    return ObjectDeleteAuditAlarmA ((LPCSTR) arg1, arg2, arg3);
}

BOOL
qxeObjectOpenAuditAlarm (const Extbyte * arg1, PVOID arg2, Extbyte * arg3, Extbyte * arg4, PSECURITY_DESCRIPTOR arg5, HANDLE arg6, DWORD arg7, DWORD arg8, PPRIVILEGE_SET arg9, BOOL arg10, BOOL arg11, PBOOL arg12)
{
  if (XEUNICODE_P)
    return ObjectOpenAuditAlarmW ((LPCWSTR) arg1, arg2, (LPWSTR) arg3, (LPWSTR) arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  else
    return ObjectOpenAuditAlarmA ((LPCSTR) arg1, arg2, (LPSTR) arg3, (LPSTR) arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

BOOL
qxeObjectPrivilegeAuditAlarm (const Extbyte * arg1, PVOID arg2, HANDLE arg3, DWORD arg4, PPRIVILEGE_SET arg5, BOOL arg6)
{
  if (XEUNICODE_P)
    return ObjectPrivilegeAuditAlarmW ((LPCWSTR) arg1, arg2, arg3, arg4, arg5, arg6);
  else
    return ObjectPrivilegeAuditAlarmA ((LPCSTR) arg1, arg2, arg3, arg4, arg5, arg6);
}

HANDLE
qxeOpenBackupEventLog (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return OpenBackupEventLogW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return OpenBackupEventLogA ((LPCSTR) arg1, (LPCSTR) arg2);
}

HANDLE
qxeOpenEventLog (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return OpenEventLogW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return OpenEventLogA ((LPCSTR) arg1, (LPCSTR) arg2);
}

HANDLE
qxeOpenEvent (DWORD arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return OpenEventW (arg1, arg2, (LPCWSTR) arg3);
  else
    return OpenEventA (arg1, arg2, (LPCSTR) arg3);
}

HANDLE
qxeOpenFileMapping (DWORD arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return OpenFileMappingW (arg1, arg2, (LPCWSTR) arg3);
  else
    return OpenFileMappingA (arg1, arg2, (LPCSTR) arg3);
}

HANDLE
qxeOpenMutex (DWORD arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return OpenMutexW (arg1, arg2, (LPCWSTR) arg3);
  else
    return OpenMutexA (arg1, arg2, (LPCSTR) arg3);
}

HANDLE
qxeOpenSemaphore (DWORD arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return OpenSemaphoreW (arg1, arg2, (LPCWSTR) arg3);
  else
    return OpenSemaphoreA (arg1, arg2, (LPCSTR) arg3);
}

HANDLE
qxeOpenWaitableTimer (DWORD arg1, BOOL arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return OpenWaitableTimerW (arg1, arg2, (LPCWSTR) arg3);
  else
    return OpenWaitableTimerA (arg1, arg2, (LPCSTR) arg3);
}

void
qxeOutputDebugString (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    OutputDebugStringW ((LPCWSTR) arg1);
  else
    OutputDebugStringA ((LPCSTR) arg1);
}

BOOL
qxePrivilegedServiceAuditAlarm (const Extbyte * arg1, const Extbyte * arg2, HANDLE arg3, PPRIVILEGE_SET arg4, BOOL arg5)
{
  if (XEUNICODE_P)
    return PrivilegedServiceAuditAlarmW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, arg4, arg5);
  else
    return PrivilegedServiceAuditAlarmA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, arg4, arg5);
}

/* Error if QueryActCtx used: Function needs review to determine how to handle it */

DWORD
qxeQueryDosDevice (const Extbyte * arg1, Extbyte * arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return QueryDosDeviceW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return QueryDosDeviceA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

/* Error if ReadDirectoryChanges used: Unicode-only */

BOOL
qxeReadEventLog (HANDLE arg1, DWORD arg2, DWORD arg3, PVOID arg4, DWORD arg5, DWORD * arg6, DWORD * arg7)
{
  if (XEUNICODE_P)
    return ReadEventLogW (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return ReadEventLogA (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

HANDLE
qxeRegisterEventSource (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return RegisterEventSourceW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return RegisterEventSourceA ((LPCSTR) arg1, (LPCSTR) arg2);
}

BOOL
qxeRemoveDirectory (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return RemoveDirectoryW ((LPCWSTR) arg1);
  else
    return RemoveDirectoryA ((LPCSTR) arg1);
}

/* Error if ReplaceFile used: Function needs review to determine how to handle it */

BOOL
qxeReportEvent (HANDLE arg1, WORD arg2, WORD arg3, DWORD arg4, PSID arg5, WORD arg6, DWORD arg7, const Extbyte ** arg8, PVOID arg9)
{
  if (XEUNICODE_P)
    return ReportEventW (arg1, arg2, arg3, arg4, arg5, arg6, arg7, (LPCWSTR*) arg8, arg9);
  else
    return ReportEventA (arg1, arg2, arg3, arg4, arg5, arg6, arg7, (LPCSTR*) arg8, arg9);
}

DWORD
qxeSearchPath (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3, DWORD arg4, Extbyte * arg5, Extbyte ** arg6)
{
  if (XEUNICODE_P)
    return SearchPathW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4, (LPWSTR) arg5, (LPWSTR*) arg6);
  else
    return SearchPathA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4, (LPSTR) arg5, (LPSTR*) arg6);
}

BOOL
qxeSetComputerName (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return SetComputerNameW ((LPCWSTR) arg1);
  else
    return SetComputerNameA ((LPCSTR) arg1);
}

/* Error if SetComputerNameEx used: Function needs review to determine how to handle it */

BOOL
qxeSetCurrentDirectory (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return SetCurrentDirectoryW ((LPCWSTR) arg1);
  else
    return SetCurrentDirectoryA ((LPCSTR) arg1);
}

BOOL
qxeSetDefaultCommConfig (const Extbyte * arg1, LPCOMMCONFIG arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return SetDefaultCommConfigW ((LPCWSTR) arg1, arg2, arg3);
  else
    return SetDefaultCommConfigA ((LPCSTR) arg1, arg2, arg3);
}

/* Error if SetDllDirectory used: Function needs review to determine how to handle it */

BOOL
qxeSetEnvironmentVariable (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return SetEnvironmentVariableW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return SetEnvironmentVariableA ((LPCSTR) arg1, (LPCSTR) arg2);
}

BOOL
qxeSetFileAttributes (const Extbyte * arg1, DWORD arg2)
{
  if (XEUNICODE_P)
    return SetFileAttributesW ((LPCWSTR) arg1, arg2);
  else
    return SetFileAttributesA ((LPCSTR) arg1, arg2);
}

BOOL
qxeSetFileSecurity (const Extbyte * arg1, SECURITY_INFORMATION arg2, PSECURITY_DESCRIPTOR arg3)
{
  if (XEUNICODE_P)
    return SetFileSecurityW ((LPCWSTR) arg1, arg2, arg3);
  else
    return SetFileSecurityA ((LPCSTR) arg1, arg2, arg3);
}

/* Error if SetFileShortName used: Function needs review to determine how to handle it */

/* Error if SetFirmwareEnvironmentVariable used: Function needs review to determine how to handle it */

BOOL
qxeSetVolumeLabel (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return SetVolumeLabelW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return SetVolumeLabelA ((LPCSTR) arg1, (LPCSTR) arg2);
}

/* Error if SetVolumeMountPoint used: Function needs review to determine how to handle it */

BOOL
qxeUpdateResource (HANDLE arg1, const Extbyte * arg2, const Extbyte * arg3, WORD arg4, PVOID arg5, DWORD arg6)
{
  if (XEUNICODE_P)
    return UpdateResourceW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, arg4, arg5, arg6);
  else
    return UpdateResourceA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, arg4, arg5, arg6);
}

/* Error if VerifyVersionInfo used: Function needs review to determine how to handle it */

BOOL
qxeWaitNamedPipe (const Extbyte * arg1, DWORD arg2)
{
  if (XEUNICODE_P)
    return WaitNamedPipeW ((LPCWSTR) arg1, arg2);
  else
    return WaitNamedPipeA ((LPCSTR) arg1, arg2);
}

BOOL
qxeWritePrivateProfileSection (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return WritePrivateProfileSectionW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3);
  else
    return WritePrivateProfileSectionA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3);
}

BOOL
qxeWritePrivateProfileString (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return WritePrivateProfileStringW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPCWSTR) arg4);
  else
    return WritePrivateProfileStringA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPCSTR) arg4);
}

BOOL
qxeWritePrivateProfileStruct (const Extbyte * arg1, const Extbyte * arg2, LPVOID arg3, UINT arg4, const Extbyte * arg5)
{
  if (XEUNICODE_P)
    return WritePrivateProfileStructW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, arg4, (LPCWSTR) arg5);
  else
    return WritePrivateProfileStructA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, arg4, (LPCSTR) arg5);
}

BOOL
qxeWriteProfileSection (const Extbyte * arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return WriteProfileSectionW ((LPCWSTR) arg1, (LPCWSTR) arg2);
  else
    return WriteProfileSectionA ((LPCSTR) arg1, (LPCSTR) arg2);
}

BOOL
qxeWriteProfileString (const Extbyte * arg1, const Extbyte * arg2, const Extbyte * arg3)
{
  if (XEUNICODE_P)
    return WriteProfileStringW ((LPCWSTR) arg1, (LPCWSTR) arg2, (LPCWSTR) arg3);
  else
    return WriteProfileStringA ((LPCSTR) arg1, (LPCSTR) arg2, (LPCSTR) arg3);
}


/*----------------------------------------------------------------------*/
/*                       Processing file ACLAPI.h                       */
/*----------------------------------------------------------------------*/

/* Error if BuildExplicitAccessWithName used: Function needs review to determine how to handle it */

/* Error if BuildSecurityDescriptor used: Function needs review to determine how to handle it */

/* Error if BuildTrusteeWithName used: Function needs review to determine how to handle it */

/* Error if BuildTrusteeWithObjectsAndName used: Function needs review to determine how to handle it */

/* Error if BuildTrusteeWithObjectsAndSid used: Function needs review to determine how to handle it */

/* Error if BuildTrusteeWithSid used: Function needs review to determine how to handle it */

/* Error if GetAuditedPermissionsFromAcl used: Function needs review to determine how to handle it */

/* Error if GetEffectiveRightsFromAcl used: Function needs review to determine how to handle it */

/* Error if GetExplicitEntriesFromAcl used: Function needs review to determine how to handle it */

DWORD
qxeGetNamedSecurityInfo (Extbyte * arg1, SE_OBJECT_TYPE arg2, SECURITY_INFORMATION arg3, PSID* arg4, PSID* arg5, PACL* arg6, PACL* arg7, PSECURITY_DESCRIPTOR* arg8)
{
  if (XEUNICODE_P)
    return GetNamedSecurityInfoW ((LPWSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  else
    return GetNamedSecurityInfoA ((LPSTR) arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

/* Error if GetTrusteeForm used: Function needs review to determine how to handle it */

/* Error if GetTrusteeName used: Function needs review to determine how to handle it */

/* Error if GetTrusteeType used: Function needs review to determine how to handle it */

/* Error if LookupSecurityDescriptorParts used: Function needs review to determine how to handle it */

/* Error if SetEntriesInAcl used: Function needs review to determine how to handle it */

/* Error if SetNamedSecurityInfo used: Function needs review to determine how to handle it */

/* Error if BuildImpersonateExplicitAccessWithName used: Function needs review to determine how to handle it */

/* Error if BuildImpersonateTrustee used: Function needs review to determine how to handle it */

/* Error if GetMultipleTrustee used: Function needs review to determine how to handle it */

/* Error if GetMultipleTrusteeOperation used: Function needs review to determine how to handle it */


/*----------------------------------------------------------------------*/
/*                      Processing file MMSYSTEM.H                      */
/*----------------------------------------------------------------------*/

BOOL
qxesndPlaySound (const Extbyte * arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return sndPlaySoundW ((LPCWSTR) arg1, arg2);
  else
    return sndPlaySoundA ((LPCSTR) arg1, arg2);
}

BOOL
qxePlaySound (const Extbyte * arg1, HMODULE arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return PlaySoundW ((LPCWSTR) arg1, arg2, arg3);
  else
    return PlaySoundA ((LPCSTR) arg1, arg2, arg3);
}

/* Error if waveOutGetDevCaps used: split-sized LPWAVEOUTCAPS */

MMRESULT
qxewaveOutGetErrorText (MMRESULT arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return waveOutGetErrorTextW (arg1, (LPWSTR) arg2, arg3);
  else
    return waveOutGetErrorTextA (arg1, (LPSTR) arg2, arg3);
}

/* Error if waveInGetDevCaps used: split-sized LPWAVEINCAPS */

MMRESULT
qxewaveInGetErrorText (MMRESULT arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return waveInGetErrorTextW (arg1, (LPWSTR) arg2, arg3);
  else
    return waveInGetErrorTextA (arg1, (LPSTR) arg2, arg3);
}

/* Error if midiOutGetDevCaps used: split-sized LPMIDIOUTCAPS */

MMRESULT
qxemidiOutGetErrorText (MMRESULT arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return midiOutGetErrorTextW (arg1, (LPWSTR) arg2, arg3);
  else
    return midiOutGetErrorTextA (arg1, (LPSTR) arg2, arg3);
}

/* Error if midiInGetDevCaps used: split-sized LPMIDIOUTCAPS */

MMRESULT
qxemidiInGetErrorText (MMRESULT arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return midiInGetErrorTextW (arg1, (LPWSTR) arg2, arg3);
  else
    return midiInGetErrorTextA (arg1, (LPSTR) arg2, arg3);
}

/* Error if auxGetDevCaps used: split-sized LPAUXCAPS */

/* Error if mixerGetDevCaps used: split-sized LPMIXERCAPS */

/* Error if mixerGetLineInfo used: split-sized LPMIXERLINE */

/* Error if mixerGetLineControls used: split-sized LPMIXERCONTROL */

/* Error if mixerGetControlDetails used: split-sized LPMIXERCONTROL in LPMIXERLINECONTROLS in LPMIXERCONTROLDETAILS */

/* Error if joyGetDevCaps used: split-sized LPJOYCAPS */

FOURCC
qxemmioStringToFOURCC (const Extbyte * arg1, UINT arg2)
{
  if (XEUNICODE_P)
    return mmioStringToFOURCCW ((LPCWSTR) arg1, arg2);
  else
    return mmioStringToFOURCCA ((LPCSTR) arg1, arg2);
}

LPMMIOPROC
qxemmioInstallIOProc (FOURCC arg1, LPMMIOPROC arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return mmioInstallIOProcW (arg1, arg2, arg3);
  else
    return mmioInstallIOProcA (arg1, arg2, arg3);
}

HMMIO
qxemmioOpen (Extbyte * arg1, LPMMIOINFO arg2, DWORD arg3)
{
  if (XEUNICODE_P)
    return mmioOpenW ((LPWSTR) arg1, arg2, arg3);
  else
    return mmioOpenA ((LPSTR) arg1, arg2, arg3);
}

MMRESULT
qxemmioRename (const Extbyte * arg1, const Extbyte * arg2, LPCMMIOINFO arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return mmioRenameW ((LPCWSTR) arg1, (LPCWSTR) arg2, arg3, arg4);
  else
    return mmioRenameA ((LPCSTR) arg1, (LPCSTR) arg2, arg3, arg4);
}

MCIERROR
qxemciSendCommand (MCIDEVICEID arg1, UINT arg2, DWORD arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return mciSendCommandW (arg1, arg2, arg3, arg4);
  else
    return mciSendCommandA (arg1, arg2, arg3, arg4);
}

MCIERROR
qxemciSendString (const Extbyte * arg1, Extbyte * arg2, UINT arg3, HWND arg4)
{
  if (XEUNICODE_P)
    return mciSendStringW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3, arg4);
  else
    return mciSendStringA ((LPCSTR) arg1, (LPSTR) arg2, arg3, arg4);
}

MCIDEVICEID
qxemciGetDeviceID (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return mciGetDeviceIDW ((LPCWSTR) arg1);
  else
    return mciGetDeviceIDA ((LPCSTR) arg1);
}

#if !defined (MINGW)

/* Error if mciGetDeviceIDFromElementID used: missing from Win98se version of ADVAPI32.dll */

#endif /* !defined (MINGW) */

BOOL
qxemciGetErrorString (MCIERROR arg1, Extbyte * arg2, UINT arg3)
{
  if (XEUNICODE_P)
    return mciGetErrorStringW (arg1, (LPWSTR) arg2, arg3);
  else
    return mciGetErrorStringA (arg1, (LPSTR) arg2, arg3);
}

