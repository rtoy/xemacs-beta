/* Automatically-generated Unicode-encapsulation file,
   using the command

   ../lib-src/make-mswin-unicode.pl --c-output intl-auto-encap-win32.c --h-output intl-auto-encap-win32.h intl-encap-win32.c

   Do not edit.  See `make-mswin-unicode.pl'.
*/

#include <config.h>
#include "lisp.h"

#include "syswindows.h"


/*----------------------------------------------------------------------*/
/*                      Processing file WINSPOOL.H                      */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

/* NOTE: #### problems with DEVMODE pointer in PRINTER_INFO_2 */
BOOL
qxeEnumPrinters (DWORD Flags, Extbyte * Name, DWORD Level, LPBYTE pPrinterEnum, DWORD cbBuf, LPDWORD pcbNeeded, LPDWORD pcReturned)
{
  if (XEUNICODE_P)
    return EnumPrintersW (Flags, (LPWSTR) Name, Level, pPrinterEnum, cbBuf, pcbNeeded, pcReturned);
  else
    return EnumPrintersA (Flags, (LPSTR) Name, Level, pPrinterEnum, cbBuf, pcbNeeded, pcReturned);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping OpenPrinter because split-sized DEVMODE pointer in split PRINTER_DEFAULTS */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ResetPrinter used: split-sized DEVMODE pointer in split PRINTER_DEFAULTS */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetJob used: split-sized DEVMODE pointer in split JOB_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetJob used: split-sized DEVMODE pointer in split JOB_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumJobs used: split-sized DEVMODE pointer in split JOB_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinter used: split-sized DEVMODE pointer in split PRINTER_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPrinter used: split-sized DEVMODE pointer in split PRINTER_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinter used: split-sized DEVMODE pointer in split PRINTER_INFO_2 */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinterDriver used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinterDriverEx used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrinterDrivers used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterDriver used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterDriverDirectory used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterDriver used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterDriverEx used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPerMachineConnection used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePerMachineConnection used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPerMachineConnections used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrintProcessor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrintProcessors used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrintProcessorDirectory used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrintProcessorDatatypes used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrintProcessor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if StartDocPrinter used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddJob used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Skipping DocumentProperties because split-sized DEVMODE, error in Cygwin prototype */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AdvancedDocumentProperties used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterDataEx used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrinterDataEx used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPrinterKey used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPrinterDataEx used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterDataEx used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterKey used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if PrinterMessageBox used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeleteForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetForm used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumForms used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumMonitors used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddMonitor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeleteMonitor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if EnumPorts used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ConfigurePort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if XcvData used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPort used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrinterConnection used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrinterConnection used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if AddPrintProvidor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if DeletePrintProvidor used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if SetPrinterHTMLView used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if GetPrinterHTMLView used: not used, complicated interface with split structures */

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                      Processing file WINNETWK.H                      */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetAddConnection (const Extbyte * lpRemoteName, const Extbyte * lpPassword, const Extbyte * lpLocalName)
{
  if (XEUNICODE_P)
    return WNetAddConnectionW ((LPCWSTR) lpRemoteName, (LPCWSTR) lpPassword, (LPCWSTR) lpLocalName);
  else
    return WNetAddConnectionA ((LPCSTR) lpRemoteName, (LPCSTR) lpPassword, (LPCSTR) lpLocalName);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetAddConnection2 (LPNETRESOURCEW lpNetResource, const Extbyte * lpPassword, const Extbyte * lpUserName, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return WNetAddConnection2W (lpNetResource, (LPCWSTR) lpPassword, (LPCWSTR) lpUserName, dwFlags);
  else
    return WNetAddConnection2A ((LPNETRESOURCEA) lpNetResource, (LPCSTR) lpPassword, (LPCSTR) lpUserName, dwFlags);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetAddConnection3 (HWND hwndOwner, LPNETRESOURCEW lpNetResource, const Extbyte * lpPassword, const Extbyte * lpUserName, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return WNetAddConnection3W (hwndOwner, lpNetResource, (LPCWSTR) lpPassword, (LPCWSTR) lpUserName, dwFlags);
  else
    return WNetAddConnection3A (hwndOwner, (LPNETRESOURCEA) lpNetResource, (LPCSTR) lpPassword, (LPCSTR) lpUserName, dwFlags);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetCancelConnection (const Extbyte * lpName, BOOL fForce)
{
  if (XEUNICODE_P)
    return WNetCancelConnectionW ((LPCWSTR) lpName, fForce);
  else
    return WNetCancelConnectionA ((LPCSTR) lpName, fForce);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetCancelConnection2 (const Extbyte * lpName, DWORD dwFlags, BOOL fForce)
{
  if (XEUNICODE_P)
    return WNetCancelConnection2W ((LPCWSTR) lpName, dwFlags, fForce);
  else
    return WNetCancelConnection2A ((LPCSTR) lpName, dwFlags, fForce);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetConnection (const Extbyte * lpLocalName, Extbyte * lpRemoteName, LPDWORD lpnLength)
{
  if (XEUNICODE_P)
    return WNetGetConnectionW ((LPCWSTR) lpLocalName, (LPWSTR) lpRemoteName, lpnLength);
  else
    return WNetGetConnectionA ((LPCSTR) lpLocalName, (LPSTR) lpRemoteName, lpnLength);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetUseConnection (HWND hwndOwner, LPNETRESOURCEW lpNetResource, const Extbyte * lpUserID, const Extbyte * lpPassword, DWORD dwFlags, Extbyte * lpAccessName, LPDWORD lpBufferSize, LPDWORD lpResult)
{
  if (XEUNICODE_P)
    return WNetUseConnectionW (hwndOwner, lpNetResource, (LPCWSTR) lpUserID, (LPCWSTR) lpPassword, dwFlags, (LPWSTR) lpAccessName, lpBufferSize, lpResult);
  else
    return WNetUseConnectionA (hwndOwner, (LPNETRESOURCEA) lpNetResource, (LPCSTR) lpUserID, (LPCSTR) lpPassword, dwFlags, (LPSTR) lpAccessName, lpBufferSize, lpResult);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* NOTE: contains split-simple LPNETRESOURCE */
DWORD 
qxeWNetConnectionDialog1 (LPCONNECTDLGSTRUCTW lpConnDlgStruct)
{
  if (XEUNICODE_P)
    return WNetConnectionDialog1W (lpConnDlgStruct);
  else
    return WNetConnectionDialog1A ((LPCONNECTDLGSTRUCTA) lpConnDlgStruct);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetDisconnectDialog1 (LPDISCDLGSTRUCTW lpConnDlgStruct)
{
  if (XEUNICODE_P)
    return WNetDisconnectDialog1W (lpConnDlgStruct);
  else
    return WNetDisconnectDialog1A ((LPDISCDLGSTRUCTA) lpConnDlgStruct);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetOpenEnum (DWORD dwScope, DWORD dwType, DWORD dwUsage, LPNETRESOURCEW lpNetResource, LPHANDLE lphEnum)
{
  if (XEUNICODE_P)
    return WNetOpenEnumW (dwScope, dwType, dwUsage, lpNetResource, lphEnum);
  else
    return WNetOpenEnumA (dwScope, dwType, dwUsage, (LPNETRESOURCEA) lpNetResource, lphEnum);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetEnumResource (HANDLE hEnum, LPDWORD lpcCount, LPVOID lpBuffer, LPDWORD lpBufferSize)
{
  if (XEUNICODE_P)
    return WNetEnumResourceW (hEnum, lpcCount, lpBuffer, lpBufferSize);
  else
    return WNetEnumResourceA (hEnum, lpcCount, lpBuffer, lpBufferSize);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetUniversalName (const Extbyte * lpLocalPath, DWORD dwInfoLevel, LPVOID lpBuffer, LPDWORD lpBufferSize)
{
  if (XEUNICODE_P)
    return WNetGetUniversalNameW ((LPCWSTR) lpLocalPath, dwInfoLevel, lpBuffer, lpBufferSize);
  else
    return WNetGetUniversalNameA ((LPCSTR) lpLocalPath, dwInfoLevel, lpBuffer, lpBufferSize);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetUser (const Extbyte * lpName, Extbyte * lpUserName, LPDWORD lpnLength)
{
  if (XEUNICODE_P)
    return WNetGetUserW ((LPCWSTR) lpName, (LPWSTR) lpUserName, lpnLength);
  else
    return WNetGetUserA ((LPCSTR) lpName, (LPSTR) lpUserName, lpnLength);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetProviderName (DWORD dwNetType, Extbyte * lpProviderName, LPDWORD lpBufferSize)
{
  if (XEUNICODE_P)
    return WNetGetProviderNameW (dwNetType, (LPWSTR) lpProviderName, lpBufferSize);
  else
    return WNetGetProviderNameA (dwNetType, (LPSTR) lpProviderName, lpBufferSize);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetNetworkInformation (const Extbyte * lpProvider, LPNETINFOSTRUCT lpNetInfoStruct)
{
  if (XEUNICODE_P)
    return WNetGetNetworkInformationW ((LPCWSTR) lpProvider, lpNetInfoStruct);
  else
    return WNetGetNetworkInformationA ((LPCSTR) lpProvider, lpNetInfoStruct);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeWNetGetLastError (LPDWORD lpError, Extbyte * lpErrorBuf, DWORD nErrorBufSize, Extbyte * lpNameBuf, DWORD nNameBufSize)
{
  if (XEUNICODE_P)
    return WNetGetLastErrorW (lpError, (LPWSTR) lpErrorBuf, nErrorBufSize, (LPWSTR) lpNameBuf, nNameBufSize);
  else
    return WNetGetLastErrorA (lpError, (LPSTR) lpErrorBuf, nErrorBufSize, (LPSTR) lpNameBuf, nNameBufSize);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD 
qxeMultinetGetConnectionPerformance (LPNETRESOURCEW lpNetResource, LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct)
{
  if (XEUNICODE_P)
    return MultinetGetConnectionPerformanceW (lpNetResource, lpNetConnectInfoStruct);
  else
    return MultinetGetConnectionPerformanceA ((LPNETRESOURCEA) lpNetResource, lpNetConnectInfoStruct);
}

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                       Processing file WINREG.H                       */
/*----------------------------------------------------------------------*/

/* Skipping RegConnectRegistry because error in Cygwin prototype */

LONG

qxeRegCreateKey (HKEY hKey, const Extbyte * lpSubKey, PHKEY phkResult)
{
  if (XEUNICODE_P)
    return RegCreateKeyW (hKey, (LPCWSTR) lpSubKey, phkResult);
  else
    return RegCreateKeyA (hKey, (LPCSTR) lpSubKey, phkResult);
}

LONG

qxeRegCreateKeyEx (HKEY hKey, const Extbyte * lpSubKey, DWORD Reserved, Extbyte * lpClass, DWORD dwOptions, REGSAM samDesired, LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition)
{
  if (XEUNICODE_P)
    return RegCreateKeyExW (hKey, (LPCWSTR) lpSubKey, Reserved, (LPWSTR) lpClass, dwOptions, samDesired, lpSecurityAttributes, phkResult, lpdwDisposition);
  else
    return RegCreateKeyExA (hKey, (LPCSTR) lpSubKey, Reserved, (LPSTR) lpClass, dwOptions, samDesired, lpSecurityAttributes, phkResult, lpdwDisposition);
}

LONG

qxeRegDeleteKey (HKEY hKey, const Extbyte * lpSubKey)
{
  if (XEUNICODE_P)
    return RegDeleteKeyW (hKey, (LPCWSTR) lpSubKey);
  else
    return RegDeleteKeyA (hKey, (LPCSTR) lpSubKey);
}

LONG

qxeRegDeleteValue (HKEY hKey, const Extbyte * lpValueName)
{
  if (XEUNICODE_P)
    return RegDeleteValueW (hKey, (LPCWSTR) lpValueName);
  else
    return RegDeleteValueA (hKey, (LPCSTR) lpValueName);
}

LONG

qxeRegEnumKey (HKEY hKey, DWORD dwIndex, Extbyte * lpName, DWORD cbName)
{
  if (XEUNICODE_P)
    return RegEnumKeyW (hKey, dwIndex, (LPWSTR) lpName, cbName);
  else
    return RegEnumKeyA (hKey, dwIndex, (LPSTR) lpName, cbName);
}

LONG

qxeRegEnumKeyEx (HKEY hKey, DWORD dwIndex, Extbyte * lpName, LPDWORD lpcbName, LPDWORD lpReserved, Extbyte * lpClass, LPDWORD lpcbClass, PFILETIME lpftLastWriteTime)
{
  if (XEUNICODE_P)
    return RegEnumKeyExW (hKey, dwIndex, (LPWSTR) lpName, lpcbName, lpReserved, (LPWSTR) lpClass, lpcbClass, lpftLastWriteTime);
  else
    return RegEnumKeyExA (hKey, dwIndex, (LPSTR) lpName, lpcbName, lpReserved, (LPSTR) lpClass, lpcbClass, lpftLastWriteTime);
}

LONG

qxeRegEnumValue (HKEY hKey, DWORD dwIndex, Extbyte * lpValueName, LPDWORD lpcbValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData)
{
  if (XEUNICODE_P)
    return RegEnumValueW (hKey, dwIndex, (LPWSTR) lpValueName, lpcbValueName, lpReserved, lpType, lpData, lpcbData);
  else
    return RegEnumValueA (hKey, dwIndex, (LPSTR) lpValueName, lpcbValueName, lpReserved, lpType, lpData, lpcbData);
}

LONG

qxeRegLoadKey (HKEY hKey, const Extbyte * lpSubKey, const Extbyte * lpFile)
{
  if (XEUNICODE_P)
    return RegLoadKeyW (hKey, (LPCWSTR) lpSubKey, (LPCWSTR) lpFile);
  else
    return RegLoadKeyA (hKey, (LPCSTR) lpSubKey, (LPCSTR) lpFile);
}

LONG

qxeRegOpenKey (HKEY hKey, const Extbyte * lpSubKey, PHKEY phkResult)
{
  if (XEUNICODE_P)
    return RegOpenKeyW (hKey, (LPCWSTR) lpSubKey, phkResult);
  else
    return RegOpenKeyA (hKey, (LPCSTR) lpSubKey, phkResult);
}

LONG

qxeRegOpenKeyEx (HKEY hKey, const Extbyte * lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult)
{
  if (XEUNICODE_P)
    return RegOpenKeyExW (hKey, (LPCWSTR) lpSubKey, ulOptions, samDesired, phkResult);
  else
    return RegOpenKeyExA (hKey, (LPCSTR) lpSubKey, ulOptions, samDesired, phkResult);
}

LONG

qxeRegQueryInfoKey (HKEY hKey, Extbyte * lpClass, LPDWORD lpcbClass, LPDWORD lpReserved, LPDWORD lpcSubKeys, LPDWORD lpcbMaxSubKeyLen, LPDWORD lpcbMaxClassLen, LPDWORD lpcValues, LPDWORD lpcbMaxValueNameLen, LPDWORD lpcbMaxValueLen, LPDWORD lpcbSecurityDescriptor, PFILETIME lpftLastWriteTime)
{
  if (XEUNICODE_P)
    return RegQueryInfoKeyW (hKey, (LPWSTR) lpClass, lpcbClass, lpReserved, lpcSubKeys, lpcbMaxSubKeyLen, lpcbMaxClassLen, lpcValues, lpcbMaxValueNameLen, lpcbMaxValueLen, lpcbSecurityDescriptor, lpftLastWriteTime);
  else
    return RegQueryInfoKeyA (hKey, (LPSTR) lpClass, lpcbClass, lpReserved, lpcSubKeys, lpcbMaxSubKeyLen, lpcbMaxClassLen, lpcValues, lpcbMaxValueNameLen, lpcbMaxValueLen, lpcbSecurityDescriptor, lpftLastWriteTime);
}

LONG

qxeRegQueryValue (HKEY hKey, const Extbyte * lpSubKey, Extbyte * lpValue, PLONG lpcbValue)
{
  if (XEUNICODE_P)
    return RegQueryValueW (hKey, (LPCWSTR) lpSubKey, (LPWSTR) lpValue, lpcbValue);
  else
    return RegQueryValueA (hKey, (LPCSTR) lpSubKey, (LPSTR) lpValue, lpcbValue);
}

LONG

qxeRegQueryMultipleValues (HKEY hKey, PVALENTW val_list, DWORD num_vals, Extbyte * lpValueBuf, LPDWORD ldwTotsize)
{
  if (XEUNICODE_P)
    return RegQueryMultipleValuesW (hKey, val_list, num_vals, (LPWSTR) lpValueBuf, ldwTotsize);
  else
    return RegQueryMultipleValuesA (hKey, (PVALENTA) val_list, num_vals, (LPSTR) lpValueBuf, ldwTotsize);
}

LONG

qxeRegQueryValueEx (HKEY hKey, const Extbyte * lpValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData)
{
  if (XEUNICODE_P)
    return RegQueryValueExW (hKey, (LPCWSTR) lpValueName, lpReserved, lpType, lpData, lpcbData);
  else
    return RegQueryValueExA (hKey, (LPCSTR) lpValueName, lpReserved, lpType, lpData, lpcbData);
}

LONG

qxeRegReplaceKey (HKEY hKey, const Extbyte * lpSubKey, const Extbyte * lpNewFile, const Extbyte * lpOldFile)
{
  if (XEUNICODE_P)
    return RegReplaceKeyW (hKey, (LPCWSTR) lpSubKey, (LPCWSTR) lpNewFile, (LPCWSTR) lpOldFile);
  else
    return RegReplaceKeyA (hKey, (LPCSTR) lpSubKey, (LPCSTR) lpNewFile, (LPCSTR) lpOldFile);
}

LONG

qxeRegRestoreKey (HKEY hKey, const Extbyte * lpFile, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return RegRestoreKeyW (hKey, (LPCWSTR) lpFile, dwFlags);
  else
    return RegRestoreKeyA (hKey, (LPCSTR) lpFile, dwFlags);
}

LONG

qxeRegSaveKey (HKEY hKey, const Extbyte * lpFile, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
{
  if (XEUNICODE_P)
    return RegSaveKeyW (hKey, (LPCWSTR) lpFile, lpSecurityAttributes);
  else
    return RegSaveKeyA (hKey, (LPCSTR) lpFile, lpSecurityAttributes);
}

LONG

qxeRegSetValue (HKEY hKey, const Extbyte * lpSubKey, DWORD dwType, const Extbyte * lpData, DWORD cbData)
{
  if (XEUNICODE_P)
    return RegSetValueW (hKey, (LPCWSTR) lpSubKey, dwType, (LPCWSTR) lpData, cbData);
  else
    return RegSetValueA (hKey, (LPCSTR) lpSubKey, dwType, (LPCSTR) lpData, cbData);
}

LONG

qxeRegSetValueEx (HKEY hKey, const Extbyte * lpValueName, DWORD Reserved, DWORD dwType, CONST BYTE* lpData, DWORD cbData)
{
  if (XEUNICODE_P)
    return RegSetValueExW (hKey, (LPCWSTR) lpValueName, Reserved, dwType, lpData, cbData);
  else
    return RegSetValueExA (hKey, (LPCSTR) lpValueName, Reserved, dwType, lpData, cbData);
}

LONG

qxeRegUnLoadKey (HKEY hKey, const Extbyte * lpSubKey)
{
  if (XEUNICODE_P)
    return RegUnLoadKeyW (hKey, (LPCWSTR) lpSubKey);
  else
    return RegUnLoadKeyA (hKey, (LPCSTR) lpSubKey);
}

BOOL

qxeInitiateSystemShutdown (Extbyte * lpMachineName, Extbyte * lpMessage, DWORD dwTimeout, BOOL bForceAppsClosed, BOOL bRebootAfterShutdown)
{
  if (XEUNICODE_P)
    return InitiateSystemShutdownW ((LPWSTR) lpMachineName, (LPWSTR) lpMessage, dwTimeout, bForceAppsClosed, bRebootAfterShutdown);
  else
    return InitiateSystemShutdownA ((LPSTR) lpMachineName, (LPSTR) lpMessage, dwTimeout, bForceAppsClosed, bRebootAfterShutdown);
}

BOOL

qxeAbortSystemShutdown (Extbyte * lpMachineName)
{
  if (XEUNICODE_P)
    return AbortSystemShutdownW ((LPWSTR) lpMachineName);
  else
    return AbortSystemShutdownA ((LPSTR) lpMachineName);
}


/*----------------------------------------------------------------------*/
/*                      Processing file SHELLAPI.H                      */
/*----------------------------------------------------------------------*/

UINT 
qxeDragQueryFile (HDROP arg1, UINT arg2, Extbyte * arg3, UINT arg4)
{
  if (XEUNICODE_P)
    return DragQueryFileW (arg1, arg2, (LPWSTR) arg3, arg4);
  else
    return DragQueryFileA (arg1, arg2, (LPSTR) arg3, arg4);
}

HINSTANCE 
qxeShellExecute (HWND hwnd, const Extbyte * lpOperation, const Extbyte * lpFile, const Extbyte * lpParameters, const Extbyte * lpDirectory, INT nShowCmd)
{
  if (XEUNICODE_P)
    return ShellExecuteW (hwnd, (LPCWSTR) lpOperation, (LPCWSTR) lpFile, (LPCWSTR) lpParameters, (LPCWSTR) lpDirectory, nShowCmd);
  else
    return ShellExecuteA (hwnd, (LPCSTR) lpOperation, (LPCSTR) lpFile, (LPCSTR) lpParameters, (LPCSTR) lpDirectory, nShowCmd);
}

HINSTANCE 
qxeFindExecutable (const Extbyte * lpFile, const Extbyte * lpDirectory, Extbyte * lpResult)
{
  if (XEUNICODE_P)
    return FindExecutableW ((LPCWSTR) lpFile, (LPCWSTR) lpDirectory, (LPWSTR) lpResult);
  else
    return FindExecutableA ((LPCSTR) lpFile, (LPCSTR) lpDirectory, (LPSTR) lpResult);
}

/* Error if CommandLineToArgv used: Unicode-only */

INT       
qxeShellAbout (HWND hWnd, const Extbyte * szApp, const Extbyte * szOtherStuff, HICON hIcon)
{
  if (XEUNICODE_P)
    return ShellAboutW (hWnd, (LPCWSTR) szApp, (LPCWSTR) szOtherStuff, hIcon);
  else
    return ShellAboutA (hWnd, (LPCSTR) szApp, (LPCSTR) szOtherStuff, hIcon);
}

HICON     
qxeExtractAssociatedIcon (HINSTANCE hInst, Extbyte * lpIconPath, LPWORD lpiIcon)
{
  if (XEUNICODE_P)
    return ExtractAssociatedIconW (hInst, (LPWSTR) lpIconPath, lpiIcon);
  else
    return ExtractAssociatedIconA (hInst, (LPSTR) lpIconPath, lpiIcon);
}

HICON     
qxeExtractIcon (HINSTANCE hInst, const Extbyte * lpszExeFileName, UINT nIconIndex)
{
  if (XEUNICODE_P)
    return ExtractIconW (hInst, (LPCWSTR) lpszExeFileName, nIconIndex);
  else
    return ExtractIconA (hInst, (LPCSTR) lpszExeFileName, nIconIndex);
}

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
DWORD   
qxeDoEnvironmentSubst (Extbyte * szString, UINT cbString)
{
  if (XEUNICODE_P)
    return DoEnvironmentSubstW ((LPWSTR) szString, cbString);
  else
    return DoEnvironmentSubstA ((LPSTR) szString, cbString);
}

#endif /* !defined (CYGWIN_HEADERS) */

/* Error if FindEnvironmentString used: causes link error; NT 4.0+ only */

/* Skipping ExtractIconEx because NT 4.0+ only, error in Cygwin prototype */

/* NOTE: NT 4.0+ only */
int
qxeSHFileOperation (LPSHFILEOPSTRUCTW lpFileOp)
{
  if (XEUNICODE_P)
    return SHFileOperationW (lpFileOp);
  else
    return SHFileOperationA ((LPSHFILEOPSTRUCTA) lpFileOp);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeShellExecuteEx (LPSHELLEXECUTEINFOW lpExecInfo)
{
  if (XEUNICODE_P)
    return ShellExecuteExW (lpExecInfo);
  else
    return ShellExecuteExA ((LPSHELLEXECUTEINFOA) lpExecInfo);
}

/* Error if WinExecError used: causes link error; NT 4.0+ only */

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
HRESULT
qxeSHQueryRecycleBin (const Extbyte * pszRootPath, LPSHQUERYRBINFO pSHQueryRBInfo)
{
  if (XEUNICODE_P)
    return SHQueryRecycleBinW ((LPCWSTR) pszRootPath, pSHQueryRBInfo);
  else
    return SHQueryRecycleBinA ((LPCSTR) pszRootPath, pSHQueryRBInfo);
}

#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
HRESULT
qxeSHEmptyRecycleBin (HWND hwnd, const Extbyte * pszRootPath, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return SHEmptyRecycleBinW (hwnd, (LPCWSTR) pszRootPath, dwFlags);
  else
    return SHEmptyRecycleBinA (hwnd, (LPCSTR) pszRootPath, dwFlags);
}

#endif /* !defined (CYGWIN_HEADERS) */

/* Error if Shell_NotifyIcon used: split-sized NOTIFYICONDATA, NT 4.0+ only */

/* Skipping SHGetFileInfo because split-sized SHFILEINFO, NT 4.0+ only */

/* Error if SHGetDiskFreeSpace used: causes link error; NT 4.0+ only */

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
BOOL
qxeSHGetNewLinkInfo (const Extbyte * pszLinkTo, const Extbyte * pszDir, Extbyte * pszName, BOOL * pfMustCopy, UINT uFlags)
{
  if (XEUNICODE_P)
    return SHGetNewLinkInfoW ((LPCWSTR) pszLinkTo, (LPCWSTR) pszDir, (LPWSTR) pszName, pfMustCopy, uFlags);
  else
    return SHGetNewLinkInfoA ((LPCSTR) pszLinkTo, (LPCSTR) pszDir, (LPSTR) pszName, pfMustCopy, uFlags);
}

#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)

/* NOTE: NT 4.0+ only */
BOOL
qxeSHInvokePrinterCommand (HWND hwnd, UINT uAction, const Extbyte * lpBuf1, const Extbyte * lpBuf2, BOOL fModal)
{
  if (XEUNICODE_P)
    return SHInvokePrinterCommandW (hwnd, uAction, (LPCWSTR) lpBuf1, (LPCWSTR) lpBuf2, fModal);
  else
    return SHInvokePrinterCommandA (hwnd, uAction, (LPCSTR) lpBuf1, (LPCSTR) lpBuf2, fModal);
}

#endif /* !defined (CYGWIN_HEADERS) */


/*----------------------------------------------------------------------*/
/*                       Processing file WINCON.H                       */
/*----------------------------------------------------------------------*/

BOOL
qxePeekConsoleInput (HANDLE hConsoleInput, PINPUT_RECORD lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsRead)
{
  if (XEUNICODE_P)
    return PeekConsoleInputW (hConsoleInput, lpBuffer, nLength, lpNumberOfEventsRead);
  else
    return PeekConsoleInputA (hConsoleInput, lpBuffer, nLength, lpNumberOfEventsRead);
}

BOOL
qxeReadConsoleInput (HANDLE hConsoleInput, PINPUT_RECORD lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsRead)
{
  if (XEUNICODE_P)
    return ReadConsoleInputW (hConsoleInput, lpBuffer, nLength, lpNumberOfEventsRead);
  else
    return ReadConsoleInputA (hConsoleInput, lpBuffer, nLength, lpNumberOfEventsRead);
}

BOOL
qxeWriteConsoleInput (HANDLE hConsoleInput, CONST INPUT_RECORD * lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsWritten)
{
  if (XEUNICODE_P)
    return WriteConsoleInputW (hConsoleInput, lpBuffer, nLength, lpNumberOfEventsWritten);
  else
    return WriteConsoleInputA (hConsoleInput, lpBuffer, nLength, lpNumberOfEventsWritten);
}

BOOL
qxeReadConsoleOutput (HANDLE hConsoleOutput, PCHAR_INFO lpBuffer, COORD dwBufferSize, COORD dwBufferCoord, PSMALL_RECT lpReadRegion)
{
  if (XEUNICODE_P)
    return ReadConsoleOutputW (hConsoleOutput, lpBuffer, dwBufferSize, dwBufferCoord, lpReadRegion);
  else
    return ReadConsoleOutputA (hConsoleOutput, lpBuffer, dwBufferSize, dwBufferCoord, lpReadRegion);
}

BOOL
qxeWriteConsoleOutput (HANDLE hConsoleOutput, CONST CHAR_INFO * lpBuffer, COORD dwBufferSize, COORD dwBufferCoord, PSMALL_RECT lpWriteRegion)
{
  if (XEUNICODE_P)
    return WriteConsoleOutputW (hConsoleOutput, lpBuffer, dwBufferSize, dwBufferCoord, lpWriteRegion);
  else
    return WriteConsoleOutputA (hConsoleOutput, lpBuffer, dwBufferSize, dwBufferCoord, lpWriteRegion);
}

BOOL
qxeReadConsoleOutputCharacter (HANDLE hConsoleOutput, Extbyte * lpCharacter, DWORD nLength, COORD dwReadCoord, LPDWORD lpNumberOfCharsRead)
{
  if (XEUNICODE_P)
    return ReadConsoleOutputCharacterW (hConsoleOutput, (LPWSTR) lpCharacter, nLength, dwReadCoord, lpNumberOfCharsRead);
  else
    return ReadConsoleOutputCharacterA (hConsoleOutput, (LPSTR) lpCharacter, nLength, dwReadCoord, lpNumberOfCharsRead);
}

BOOL
qxeWriteConsoleOutputCharacter (HANDLE hConsoleOutput, const Extbyte * lpCharacter, DWORD nLength, COORD dwWriteCoord, LPDWORD lpNumberOfCharsWritten)
{
  if (XEUNICODE_P)
    return WriteConsoleOutputCharacterW (hConsoleOutput, (LPCWSTR) lpCharacter, nLength, dwWriteCoord, lpNumberOfCharsWritten);
  else
    return WriteConsoleOutputCharacterA (hConsoleOutput, (LPCSTR) lpCharacter, nLength, dwWriteCoord, lpNumberOfCharsWritten);
}

/* Error if FillConsoleOutputCharacter used: split CHAR */

BOOL
qxeScrollConsoleScreenBuffer (HANDLE hConsoleOutput, CONST SMALL_RECT * lpScrollRectangle, CONST SMALL_RECT * lpClipRectangle, COORD dwDestinationOrigin, CONST CHAR_INFO * lpFill)
{
  if (XEUNICODE_P)
    return ScrollConsoleScreenBufferW (hConsoleOutput, lpScrollRectangle, lpClipRectangle, dwDestinationOrigin, lpFill);
  else
    return ScrollConsoleScreenBufferA (hConsoleOutput, lpScrollRectangle, lpClipRectangle, dwDestinationOrigin, lpFill);
}

DWORD
qxeGetConsoleTitle (Extbyte * lpConsoleTitle, DWORD nSize)
{
  if (XEUNICODE_P)
    return GetConsoleTitleW ((LPWSTR) lpConsoleTitle, nSize);
  else
    return GetConsoleTitleA ((LPSTR) lpConsoleTitle, nSize);
}

BOOL
qxeSetConsoleTitle (const Extbyte * lpConsoleTitle)
{
  if (XEUNICODE_P)
    return SetConsoleTitleW ((LPCWSTR) lpConsoleTitle);
  else
    return SetConsoleTitleA ((LPCSTR) lpConsoleTitle);
}

BOOL
qxeReadConsole (HANDLE hConsoleInput, LPVOID lpBuffer, DWORD nNumberOfCharsToRead, LPDWORD lpNumberOfCharsRead, LPVOID lpReserved)
{
  if (XEUNICODE_P)
    return ReadConsoleW (hConsoleInput, lpBuffer, nNumberOfCharsToRead, lpNumberOfCharsRead, lpReserved);
  else
    return ReadConsoleA (hConsoleInput, lpBuffer, nNumberOfCharsToRead, lpNumberOfCharsRead, lpReserved);
}

BOOL
qxeWriteConsole (HANDLE hConsoleOutput, CONST VOID * lpBuffer, DWORD nNumberOfCharsToWrite, LPDWORD lpNumberOfCharsWritten, LPVOID lpReserved)
{
  if (XEUNICODE_P)
    return WriteConsoleW (hConsoleOutput, lpBuffer, nNumberOfCharsToWrite, lpNumberOfCharsWritten, lpReserved);
  else
    return WriteConsoleA (hConsoleOutput, lpBuffer, nNumberOfCharsToWrite, lpNumberOfCharsWritten, lpReserved);
}


/*----------------------------------------------------------------------*/
/*                        Processing file IMM.H                         */
/*----------------------------------------------------------------------*/

#if defined (HAVE_MS_WINDOWS)

HKL
qxeImmInstallIME (const Extbyte * lpszIMEFileName, const Extbyte * lpszLayoutText)
{
  if (XEUNICODE_P)
    return ImmInstallIMEW ((LPCWSTR) lpszIMEFileName, (LPCWSTR) lpszLayoutText);
  else
    return ImmInstallIMEA ((LPCSTR) lpszIMEFileName, (LPCSTR) lpszLayoutText);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeImmGetDescription (HKL arg1, Extbyte * arg2, UINT uBufLen)
{
  if (XEUNICODE_P)
    return ImmGetDescriptionW (arg1, (LPWSTR) arg2, uBufLen);
  else
    return ImmGetDescriptionA (arg1, (LPSTR) arg2, uBufLen);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeImmGetIMEFileName (HKL arg1, Extbyte * arg2, UINT uBufLen)
{
  if (XEUNICODE_P)
    return ImmGetIMEFileNameW (arg1, (LPWSTR) arg2, uBufLen);
  else
    return ImmGetIMEFileNameA (arg1, (LPSTR) arg2, uBufLen);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

LONG
qxeImmGetCompositionString (HIMC arg1, DWORD arg2, LPVOID arg3, DWORD arg4)
{
  if (XEUNICODE_P)
    return ImmGetCompositionStringW (arg1, arg2, arg3, arg4);
  else
    return ImmGetCompositionStringA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPCVOID lpComp, DWORD arg4, LPCVOID lpRead, DWORD arg6)
{
  if (XEUNICODE_P)
    return ImmSetCompositionStringW (arg1, dwIndex, lpComp, arg4, lpRead, arg6);
  else
    return ImmSetCompositionStringA (arg1, dwIndex, lpComp, arg4, lpRead, arg6);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetCandidateListCount (HIMC arg1, LPDWORD lpdwListCount)
{
  if (XEUNICODE_P)
    return ImmGetCandidateListCountW (arg1, lpdwListCount);
  else
    return ImmGetCandidateListCountA (arg1, lpdwListCount);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetCandidateList (HIMC arg1, DWORD deIndex, LPCANDIDATELIST arg3, DWORD dwBufLen)
{
  if (XEUNICODE_P)
    return ImmGetCandidateListW (arg1, deIndex, arg3, dwBufLen);
  else
    return ImmGetCandidateListA (arg1, deIndex, arg3, dwBufLen);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetGuideLine (HIMC arg1, DWORD dwIndex, Extbyte * arg3, DWORD dwBufLen)
{
  if (XEUNICODE_P)
    return ImmGetGuideLineW (arg1, dwIndex, (LPWSTR) arg3, dwBufLen);
  else
    return ImmGetGuideLineA (arg1, dwIndex, (LPSTR) arg3, dwBufLen);
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
qxeImmConfigureIME (HKL arg1, HWND arg2, DWORD arg3, LPVOID arg4)
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
qxeImmEscape (HKL arg1, HIMC arg2, UINT arg3, LPVOID arg4)
{
  if (XEUNICODE_P)
    return ImmEscapeW (arg1, arg2, arg3, arg4);
  else
    return ImmEscapeA (arg1, arg2, arg3, arg4);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

DWORD
qxeImmGetConversionList (HKL arg1, HIMC arg2, const Extbyte * arg3, LPCANDIDATELIST arg4, DWORD dwBufLen, UINT uFlag)
{
  if (XEUNICODE_P)
    return ImmGetConversionListW (arg1, arg2, (LPCWSTR) arg3, arg4, dwBufLen, uFlag);
  else
    return ImmGetConversionListA (arg1, arg2, (LPCSTR) arg3, arg4, dwBufLen, uFlag);
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
qxeImmRegisterWord (HKL arg1, const Extbyte * lpszReading, DWORD arg3, const Extbyte * lpszRegister)
{
  if (XEUNICODE_P)
    return ImmRegisterWordW (arg1, (LPCWSTR) lpszReading, arg3, (LPCWSTR) lpszRegister);
  else
    return ImmRegisterWordA (arg1, (LPCSTR) lpszReading, arg3, (LPCSTR) lpszRegister);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

BOOL
qxeImmUnregisterWord (HKL arg1, const Extbyte * lpszReading, DWORD arg3, const Extbyte * lpszUnregister)
{
  if (XEUNICODE_P)
    return ImmUnregisterWordW (arg1, (LPCWSTR) lpszReading, arg3, (LPCWSTR) lpszUnregister);
  else
    return ImmUnregisterWordA (arg1, (LPCSTR) lpszReading, arg3, (LPCSTR) lpszUnregister);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ImmGetRegisterWordStyle used: split-sized STYLEBUF */

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

UINT
qxeImmEnumRegisterWord (HKL arg1, REGISTERWORDENUMPROCW arg2, const Extbyte * lpszReading, DWORD arg4, const Extbyte * lpszRegister, LPVOID arg6)
{
  if (XEUNICODE_P)
    return ImmEnumRegisterWordW (arg1, arg2, (LPCWSTR) lpszReading, arg4, (LPCWSTR) lpszRegister, arg6);
  else
    return ImmEnumRegisterWordA (arg1, (REGISTERWORDENUMPROCA) arg2, (LPCSTR) lpszReading, arg4, (LPCSTR) lpszRegister, arg6);
}

#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)

/* Error if ImmGetImeMenuItems used: split-sized IMEMENUITEMINFO */

#endif /* defined (HAVE_MS_WINDOWS) */


/*----------------------------------------------------------------------*/
/*                       Processing file DDEML.H                        */
/*----------------------------------------------------------------------*/

UINT
qxeDdeInitialize (LPDWORD pidInst, PFNCALLBACK pfnCallback, DWORD afCmd, DWORD ulRes)
{
  if (XEUNICODE_P)
    return DdeInitializeW (pidInst, pfnCallback, afCmd, ulRes);
  else
    return DdeInitializeA (pidInst, pfnCallback, afCmd, ulRes);
}

HSZ
qxeDdeCreateStringHandle (DWORD idInst, const Extbyte * psz, int iCodePage)
{
  if (XEUNICODE_P)
    return DdeCreateStringHandleW (idInst, (LPCWSTR) psz, iCodePage);
  else
    return DdeCreateStringHandleA (idInst, (LPCSTR) psz, iCodePage);
}

DWORD
qxeDdeQueryString (DWORD idInst, HSZ hsz, Extbyte * psz, DWORD cchMax, int iCodePage)
{
  if (XEUNICODE_P)
    return DdeQueryStringW (idInst, hsz, (LPWSTR) psz, cchMax, iCodePage);
  else
    return DdeQueryStringA (idInst, hsz, (LPSTR) psz, cchMax, iCodePage);
}


/*----------------------------------------------------------------------*/
/*                      Processing file WINUSER.H                       */
/*----------------------------------------------------------------------*/

int
qxewvsprintf (Extbyte * arg1, const Extbyte * arg2, va_list arglist)
{
  if (XEUNICODE_P)
    return wvsprintfW ((LPWSTR) arg1, (LPCWSTR) arg2, arglist);
  else
    return wvsprintfA ((LPSTR) arg1, (LPCSTR) arg2, arglist);
}

HKL
qxeLoadKeyboardLayout (const Extbyte * pwszKLID, UINT Flags)
{
  if (XEUNICODE_P)
    return LoadKeyboardLayoutW ((LPCWSTR) pwszKLID, Flags);
  else
    return LoadKeyboardLayoutA ((LPCSTR) pwszKLID, Flags);
}

BOOL
qxeGetKeyboardLayoutName (Extbyte * pwszKLID)
{
  if (XEUNICODE_P)
    return GetKeyboardLayoutNameW ((LPWSTR) pwszKLID);
  else
    return GetKeyboardLayoutNameA ((LPSTR) pwszKLID);
}

/* Error if CreateDesktop used: split-sized LPDEVMODE */

HDESK
qxeOpenDesktop (Extbyte * lpszDesktop, DWORD dwFlags, BOOL fInherit, ACCESS_MASK dwDesiredAccess)
{
  if (XEUNICODE_P)
    return OpenDesktopW ((LPWSTR) lpszDesktop, dwFlags, fInherit, dwDesiredAccess);
  else
    return OpenDesktopA ((LPSTR) lpszDesktop, dwFlags, fInherit, dwDesiredAccess);
}

/* NOTE: // callback fun differs only in string pointer type */
BOOL
qxeEnumDesktops (HWINSTA hwinsta, DESKTOPENUMPROCW lpEnumFunc, LPARAM lParam)
{
  if (XEUNICODE_P)
    return EnumDesktopsW (hwinsta, lpEnumFunc, lParam);
  else
    return EnumDesktopsA (hwinsta, (DESKTOPENUMPROCA) lpEnumFunc, lParam);
}

HWINSTA
qxeCreateWindowStation (Extbyte * lpwinsta, DWORD dwReserved, ACCESS_MASK dwDesiredAccess, LPSECURITY_ATTRIBUTES lpsa)
{
  if (XEUNICODE_P)
    return CreateWindowStationW ((LPWSTR) lpwinsta, dwReserved, dwDesiredAccess, lpsa);
  else
    return CreateWindowStationA ((LPSTR) lpwinsta, dwReserved, dwDesiredAccess, lpsa);
}

HWINSTA
qxeOpenWindowStation (Extbyte * lpszWinSta, BOOL fInherit, ACCESS_MASK dwDesiredAccess)
{
  if (XEUNICODE_P)
    return OpenWindowStationW ((LPWSTR) lpszWinSta, fInherit, dwDesiredAccess);
  else
    return OpenWindowStationA ((LPSTR) lpszWinSta, fInherit, dwDesiredAccess);
}

/* NOTE: // callback fun differs only in string pointer type */
BOOL
qxeEnumWindowStations (WINSTAENUMPROCW lpEnumFunc, LPARAM lParam)
{
  if (XEUNICODE_P)
    return EnumWindowStationsW (lpEnumFunc, lParam);
  else
    return EnumWindowStationsA ((WINSTAENUMPROCA) lpEnumFunc, lParam);
}

BOOL
qxeGetUserObjectInformation (HANDLE hObj, int nIndex, PVOID pvInfo, DWORD nLength, LPDWORD lpnLengthNeeded)
{
  if (XEUNICODE_P)
    return GetUserObjectInformationW (hObj, nIndex, pvInfo, nLength, lpnLengthNeeded);
  else
    return GetUserObjectInformationA (hObj, nIndex, pvInfo, nLength, lpnLengthNeeded);
}

BOOL
qxeSetUserObjectInformation (HANDLE hObj, int nIndex, PVOID pvInfo, DWORD nLength)
{
  if (XEUNICODE_P)
    return SetUserObjectInformationW (hObj, nIndex, pvInfo, nLength);
  else
    return SetUserObjectInformationA (hObj, nIndex, pvInfo, nLength);
}

UINT
qxeRegisterWindowMessage (const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return RegisterWindowMessageW ((LPCWSTR) lpString);
  else
    return RegisterWindowMessageA ((LPCSTR) lpString);
}

BOOL
qxeGetMessage (LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax)
{
  if (XEUNICODE_P)
    return GetMessageW (lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax);
  else
    return GetMessageA (lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax);
}

LONG
qxeDispatchMessage (CONST MSG * lpMsg)
{
  if (XEUNICODE_P)
    return DispatchMessageW (lpMsg);
  else
    return DispatchMessageA (lpMsg);
}

BOOL
qxePeekMessage (LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax, UINT wRemoveMsg)
{
  if (XEUNICODE_P)
    return PeekMessageW (lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax, wRemoveMsg);
  else
    return PeekMessageA (lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax, wRemoveMsg);
}

/* Skipping SendMessage because split messages and structures */

LRESULT
qxeSendMessageTimeout (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam, UINT fuFlags, UINT uTimeout, LPDWORD lpdwResult)
{
  if (XEUNICODE_P)
    return SendMessageTimeoutW (hWnd, Msg, wParam, lParam, fuFlags, uTimeout, lpdwResult);
  else
    return SendMessageTimeoutA (hWnd, Msg, wParam, lParam, fuFlags, uTimeout, lpdwResult);
}

BOOL
qxeSendNotifyMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return SendNotifyMessageW (hWnd, Msg, wParam, lParam);
  else
    return SendNotifyMessageA (hWnd, Msg, wParam, lParam);
}

BOOL
qxeSendMessageCallback (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam, SENDASYNCPROC lpResultCallBack, DWORD dwData)
{
  if (XEUNICODE_P)
    return SendMessageCallbackW (hWnd, Msg, wParam, lParam, lpResultCallBack, dwData);
  else
    return SendMessageCallbackA (hWnd, Msg, wParam, lParam, lpResultCallBack, dwData);
}

/* Error if BroadcastSystemMessage used: win95 version not split; NT 4.0+ only */

/* Error if RegisterDeviceNotification used: NT 5.0+ only */

BOOL
qxePostMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return PostMessageW (hWnd, Msg, wParam, lParam);
  else
    return PostMessageA (hWnd, Msg, wParam, lParam);
}

BOOL
qxePostThreadMessage (DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return PostThreadMessageW (idThread, Msg, wParam, lParam);
  else
    return PostThreadMessageA (idThread, Msg, wParam, lParam);
}

/* Skipping DefWindowProc because return value is conditionalized on _MAC, messes up parser */

/* Error if CallWindowProc used: two versions, STRICT and non-STRICT */

/* Error if CallWindowProc used: two versions, STRICT and non-STRICT */

/* Skipping RegisterClass because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASS */

/* Skipping UnregisterClass because need to intercept for reasons related to RegisterClass */

BOOL
qxeGetClassInfo (HINSTANCE hInstance, const Extbyte * lpClassName, LPWNDCLASSW lpWndClass)
{
  if (XEUNICODE_P)
    return GetClassInfoW (hInstance, (LPCWSTR) lpClassName, lpWndClass);
  else
    return GetClassInfoA (hInstance, (LPCSTR) lpClassName, (LPWNDCLASSA) lpWndClass);
}

/* Skipping RegisterClassEx because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASSEX; NT 4.0+ only */

/* NOTE: NT 4.0+ only */
BOOL
qxeGetClassInfoEx (HINSTANCE arg1, const Extbyte * arg2, LPWNDCLASSEXW arg3)
{
  if (XEUNICODE_P)
    return GetClassInfoExW (arg1, (LPCWSTR) arg2, arg3);
  else
    return GetClassInfoExA (arg1, (LPCSTR) arg2, (LPWNDCLASSEXA) arg3);
}

HWND
qxeCreateWindowEx (DWORD dwExStyle, const Extbyte * lpClassName, const Extbyte * lpWindowName, DWORD dwStyle, int X, int Y, int nWidth, int nHeight, HWND hWndParent, HMENU hMenu, HINSTANCE hInstance, LPVOID lpParam)
{
  if (XEUNICODE_P)
    return CreateWindowExW (dwExStyle, (LPCWSTR) lpClassName, (LPCWSTR) lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
  else
    return CreateWindowExA (dwExStyle, (LPCSTR) lpClassName, (LPCSTR) lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
}

HWND
qxeCreateDialogParam (HINSTANCE hInstance, const Extbyte * lpTemplateName, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam)
{
  if (XEUNICODE_P)
    return CreateDialogParamW (hInstance, (LPCWSTR) lpTemplateName, hWndParent, lpDialogFunc, dwInitParam);
  else
    return CreateDialogParamA (hInstance, (LPCSTR) lpTemplateName, hWndParent, lpDialogFunc, dwInitParam);
}

/* NOTE: error in Cygwin prototype (no split) but fixable with typedef */
HWND
qxeCreateDialogIndirectParam (HINSTANCE hInstance, LPCDLGTEMPLATEW lpTemplate, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam)
{
  if (XEUNICODE_P)
    return CreateDialogIndirectParamW (hInstance, lpTemplate, hWndParent, lpDialogFunc, dwInitParam);
  else
    return CreateDialogIndirectParamA (hInstance, (LPCDLGTEMPLATEA) lpTemplate, hWndParent, lpDialogFunc, dwInitParam);
}

int
qxeDialogBoxParam (HINSTANCE hInstance, const Extbyte * lpTemplateName, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam)
{
  if (XEUNICODE_P)
    return DialogBoxParamW (hInstance, (LPCWSTR) lpTemplateName, hWndParent, lpDialogFunc, dwInitParam);
  else
    return DialogBoxParamA (hInstance, (LPCSTR) lpTemplateName, hWndParent, lpDialogFunc, dwInitParam);
}

/* NOTE: error in Cygwin prototype (no split) but fixable with typedef */
int
qxeDialogBoxIndirectParam (HINSTANCE hInstance, LPCDLGTEMPLATEW hDialogTemplate, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam)
{
  if (XEUNICODE_P)
    return DialogBoxIndirectParamW (hInstance, hDialogTemplate, hWndParent, lpDialogFunc, dwInitParam);
  else
    return DialogBoxIndirectParamA (hInstance, (LPCDLGTEMPLATEA) hDialogTemplate, hWndParent, lpDialogFunc, dwInitParam);
}

BOOL
qxeSetDlgItemText (HWND hDlg, int nIDDlgItem, const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return SetDlgItemTextW (hDlg, nIDDlgItem, (LPCWSTR) lpString);
  else
    return SetDlgItemTextA (hDlg, nIDDlgItem, (LPCSTR) lpString);
}

UINT
qxeGetDlgItemText (HWND hDlg, int nIDDlgItem, Extbyte * lpString, int nMaxCount)
{
  if (XEUNICODE_P)
    return GetDlgItemTextW (hDlg, nIDDlgItem, (LPWSTR) lpString, nMaxCount);
  else
    return GetDlgItemTextA (hDlg, nIDDlgItem, (LPSTR) lpString, nMaxCount);
}

LONG
qxeSendDlgItemMessage (HWND hDlg, int nIDDlgItem, UINT Msg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return SendDlgItemMessageW (hDlg, nIDDlgItem, Msg, wParam, lParam);
  else
    return SendDlgItemMessageA (hDlg, nIDDlgItem, Msg, wParam, lParam);
}

/* Error if DefDlgProc used: return value is conditionalized on _MAC, messes up parser */

#if !defined (CYGWIN_HEADERS)

BOOL
qxeCallMsgFilter (LPMSG lpMsg, int nCode)
{
  if (XEUNICODE_P)
    return CallMsgFilterW (lpMsg, nCode);
  else
    return CallMsgFilterA (lpMsg, nCode);
}

#endif /* !defined (CYGWIN_HEADERS) */

UINT
qxeRegisterClipboardFormat (const Extbyte * lpszFormat)
{
  if (XEUNICODE_P)
    return RegisterClipboardFormatW ((LPCWSTR) lpszFormat);
  else
    return RegisterClipboardFormatA ((LPCSTR) lpszFormat);
}

int
qxeGetClipboardFormatName (UINT format, Extbyte * lpszFormatName, int cchMaxCount)
{
  if (XEUNICODE_P)
    return GetClipboardFormatNameW (format, (LPWSTR) lpszFormatName, cchMaxCount);
  else
    return GetClipboardFormatNameA (format, (LPSTR) lpszFormatName, cchMaxCount);
}

BOOL
qxeCharToOem (const Extbyte * lpszSrc, LPSTR lpszDst)
{
  if (XEUNICODE_P)
    return CharToOemW ((LPCWSTR) lpszSrc, lpszDst);
  else
    return CharToOemA ((LPCSTR) lpszSrc, lpszDst);
}

BOOL
qxeOemToChar (LPCSTR lpszSrc, Extbyte * lpszDst)
{
  if (XEUNICODE_P)
    return OemToCharW (lpszSrc, (LPWSTR) lpszDst);
  else
    return OemToCharA (lpszSrc, (LPSTR) lpszDst);
}

BOOL
qxeCharToOemBuff (const Extbyte * lpszSrc, LPSTR lpszDst, DWORD cchDstLength)
{
  if (XEUNICODE_P)
    return CharToOemBuffW ((LPCWSTR) lpszSrc, lpszDst, cchDstLength);
  else
    return CharToOemBuffA ((LPCSTR) lpszSrc, lpszDst, cchDstLength);
}

BOOL
qxeOemToCharBuff (LPCSTR lpszSrc, Extbyte * lpszDst, DWORD cchDstLength)
{
  if (XEUNICODE_P)
    return OemToCharBuffW (lpszSrc, (LPWSTR) lpszDst, cchDstLength);
  else
    return OemToCharBuffA (lpszSrc, (LPSTR) lpszDst, cchDstLength);
}

Extbyte *
qxeCharUpper (Extbyte * lpsz)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharUpperW ((LPWSTR) lpsz);
  else
    return (Extbyte *) CharUpperA ((LPSTR) lpsz);
}

DWORD
qxeCharUpperBuff (Extbyte * lpsz, DWORD cchLength)
{
  if (XEUNICODE_P)
    return CharUpperBuffW ((LPWSTR) lpsz, cchLength);
  else
    return CharUpperBuffA ((LPSTR) lpsz, cchLength);
}

Extbyte *
qxeCharLower (Extbyte * lpsz)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharLowerW ((LPWSTR) lpsz);
  else
    return (Extbyte *) CharLowerA ((LPSTR) lpsz);
}

DWORD
qxeCharLowerBuff (Extbyte * lpsz, DWORD cchLength)
{
  if (XEUNICODE_P)
    return CharLowerBuffW ((LPWSTR) lpsz, cchLength);
  else
    return CharLowerBuffA ((LPSTR) lpsz, cchLength);
}

Extbyte *
qxeCharNext (const Extbyte * lpsz)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharNextW ((LPCWSTR) lpsz);
  else
    return (Extbyte *) CharNextA ((LPCSTR) lpsz);
}

Extbyte *
qxeCharPrev (const Extbyte * lpszStart, const Extbyte * lpszCurrent)
{
  if (XEUNICODE_P)
    return (Extbyte *) CharPrevW ((LPCWSTR) lpszStart, (LPCWSTR) lpszCurrent);
  else
    return (Extbyte *) CharPrevA ((LPCSTR) lpszStart, (LPCSTR) lpszCurrent);
}

/* Error if IsCharAlpha used: split CHAR */

/* Error if IsCharAlphaNumeric used: split CHAR */

/* Error if IsCharUpper used: split CHAR */

/* Error if IsCharLower used: split CHAR */

int
qxeGetKeyNameText (LONG lParam, Extbyte * lpString, int nSize)
{
  if (XEUNICODE_P)
    return GetKeyNameTextW (lParam, (LPWSTR) lpString, nSize);
  else
    return GetKeyNameTextA (lParam, (LPSTR) lpString, nSize);
}

/* Skipping VkKeyScan because split CHAR */

/* Error if VkKeyScanEx used: split CHAR; NT 4.0+ only */

UINT
qxeMapVirtualKey (UINT uCode, UINT uMapType)
{
  if (XEUNICODE_P)
    return MapVirtualKeyW (uCode, uMapType);
  else
    return MapVirtualKeyA (uCode, uMapType);
}

/* NOTE: NT 4.0+ only */
UINT
qxeMapVirtualKeyEx (UINT uCode, UINT uMapType, HKL dwhkl)
{
  if (XEUNICODE_P)
    return MapVirtualKeyExW (uCode, uMapType, dwhkl);
  else
    return MapVirtualKeyExA (uCode, uMapType, dwhkl);
}

HACCEL
qxeLoadAccelerators (HINSTANCE hInstance, const Extbyte * lpTableName)
{
  if (XEUNICODE_P)
    return LoadAcceleratorsW (hInstance, (LPCWSTR) lpTableName);
  else
    return LoadAcceleratorsA (hInstance, (LPCSTR) lpTableName);
}

HACCEL
qxeCreateAcceleratorTable (LPACCEL arg1, int arg2)
{
  if (XEUNICODE_P)
    return CreateAcceleratorTableW (arg1, arg2);
  else
    return CreateAcceleratorTableA (arg1, arg2);
}

int
qxeCopyAcceleratorTable (HACCEL hAccelSrc, LPACCEL lpAccelDst, int cAccelEntries)
{
  if (XEUNICODE_P)
    return CopyAcceleratorTableW (hAccelSrc, lpAccelDst, cAccelEntries);
  else
    return CopyAcceleratorTableA (hAccelSrc, lpAccelDst, cAccelEntries);
}

int
qxeTranslateAccelerator (HWND hWnd, HACCEL hAccTable, LPMSG lpMsg)
{
  if (XEUNICODE_P)
    return TranslateAcceleratorW (hWnd, hAccTable, lpMsg);
  else
    return TranslateAcceleratorA (hWnd, hAccTable, lpMsg);
}

HMENU
qxeLoadMenu (HINSTANCE hInstance, const Extbyte * lpMenuName)
{
  if (XEUNICODE_P)
    return LoadMenuW (hInstance, (LPCWSTR) lpMenuName);
  else
    return LoadMenuA (hInstance, (LPCSTR) lpMenuName);
}

HMENU
qxeLoadMenuIndirect (CONST MENUTEMPLATEW * lpMenuTemplate)
{
  if (XEUNICODE_P)
    return LoadMenuIndirectW (lpMenuTemplate);
  else
    return LoadMenuIndirectA ((CONST MENUTEMPLATEA *) lpMenuTemplate);
}

BOOL
qxeChangeMenu (HMENU hMenu, UINT cmd, const Extbyte * lpszNewItem, UINT cmdInsert, UINT flags)
{
  if (XEUNICODE_P)
    return ChangeMenuW (hMenu, cmd, (LPCWSTR) lpszNewItem, cmdInsert, flags);
  else
    return ChangeMenuA (hMenu, cmd, (LPCSTR) lpszNewItem, cmdInsert, flags);
}

int
qxeGetMenuString (HMENU hMenu, UINT uIDItem, Extbyte * lpString, int nMaxCount, UINT uFlag)
{
  if (XEUNICODE_P)
    return GetMenuStringW (hMenu, uIDItem, (LPWSTR) lpString, nMaxCount, uFlag);
  else
    return GetMenuStringA (hMenu, uIDItem, (LPSTR) lpString, nMaxCount, uFlag);
}

BOOL
qxeInsertMenu (HMENU hMenu, UINT uPosition, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem)
{
  if (XEUNICODE_P)
    return InsertMenuW (hMenu, uPosition, uFlags, uIDNewItem, (LPCWSTR) lpNewItem);
  else
    return InsertMenuA (hMenu, uPosition, uFlags, uIDNewItem, (LPCSTR) lpNewItem);
}

BOOL
qxeAppendMenu (HMENU hMenu, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem)
{
  if (XEUNICODE_P)
    return AppendMenuW (hMenu, uFlags, uIDNewItem, (LPCWSTR) lpNewItem);
  else
    return AppendMenuA (hMenu, uFlags, uIDNewItem, (LPCSTR) lpNewItem);
}

BOOL
qxeModifyMenu (HMENU hMnu, UINT uPosition, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem)
{
  if (XEUNICODE_P)
    return ModifyMenuW (hMnu, uPosition, uFlags, uIDNewItem, (LPCWSTR) lpNewItem);
  else
    return ModifyMenuA (hMnu, uPosition, uFlags, uIDNewItem, (LPCSTR) lpNewItem);
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

/* NOTE: NT 4.0+ only */
BOOL
qxeGetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPMENUITEMINFOW arg4)
{
  if (XEUNICODE_P)
    return GetMenuItemInfoW (arg1, arg2, arg3, arg4);
  else
    return GetMenuItemInfoA (arg1, arg2, arg3, (LPMENUITEMINFOA) arg4);
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

int
qxeDrawText (HDC hDC, const Extbyte * lpString, int nCount, LPRECT lpRect, UINT uFormat)
{
  if (XEUNICODE_P)
    return DrawTextW (hDC, (LPCWSTR) lpString, nCount, lpRect, uFormat);
  else
    return DrawTextA (hDC, (LPCSTR) lpString, nCount, lpRect, uFormat);
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

BOOL
qxeGrayString (HDC hDC, HBRUSH hBrush, GRAYSTRINGPROC lpOutputFunc, LPARAM lpData, int nCount, int X, int Y, int nWidth, int nHeight)
{
  if (XEUNICODE_P)
    return GrayStringW (hDC, hBrush, lpOutputFunc, lpData, nCount, X, Y, nWidth, nHeight);
  else
    return GrayStringA (hDC, hBrush, lpOutputFunc, lpData, nCount, X, Y, nWidth, nHeight);
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

LONG
qxeTabbedTextOut (HDC hDC, int X, int Y, const Extbyte * lpString, int nCount, int nTabPositions, LPINT lpnTabStopPositions, int nTabOrigin)
{
  if (XEUNICODE_P)
    return TabbedTextOutW (hDC, X, Y, (LPCWSTR) lpString, nCount, nTabPositions, lpnTabStopPositions, nTabOrigin);
  else
    return TabbedTextOutA (hDC, X, Y, (LPCSTR) lpString, nCount, nTabPositions, lpnTabStopPositions, nTabOrigin);
}

DWORD
qxeGetTabbedTextExtent (HDC hDC, const Extbyte * lpString, int nCount, int nTabPositions, LPINT lpnTabStopPositions)
{
  if (XEUNICODE_P)
    return GetTabbedTextExtentW (hDC, (LPCWSTR) lpString, nCount, nTabPositions, lpnTabStopPositions);
  else
    return GetTabbedTextExtentA (hDC, (LPCSTR) lpString, nCount, nTabPositions, lpnTabStopPositions);
}

BOOL
qxeSetProp (HWND hWnd, const Extbyte * lpString, HANDLE hData)
{
  if (XEUNICODE_P)
    return SetPropW (hWnd, (LPCWSTR) lpString, hData);
  else
    return SetPropA (hWnd, (LPCSTR) lpString, hData);
}

HANDLE
qxeGetProp (HWND hWnd, const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return GetPropW (hWnd, (LPCWSTR) lpString);
  else
    return GetPropA (hWnd, (LPCSTR) lpString);
}

HANDLE
qxeRemoveProp (HWND hWnd, const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return RemovePropW (hWnd, (LPCWSTR) lpString);
  else
    return RemovePropA (hWnd, (LPCSTR) lpString);
}

/* NOTE: // callback fun differs only in string pointer type */
int
qxeEnumPropsEx (HWND hWnd, PROPENUMPROCEXW lpEnumFunc, LPARAM lParam)
{
  if (XEUNICODE_P)
    return EnumPropsExW (hWnd, lpEnumFunc, lParam);
  else
    return EnumPropsExA (hWnd, (PROPENUMPROCEXA) lpEnumFunc, lParam);
}

/* NOTE: // callback fun differs only in string pointer type */
int
qxeEnumProps (HWND hWnd, PROPENUMPROCW lpEnumFunc)
{
  if (XEUNICODE_P)
    return EnumPropsW (hWnd, lpEnumFunc);
  else
    return EnumPropsA (hWnd, (PROPENUMPROCA) lpEnumFunc);
}

BOOL
qxeSetWindowText (HWND hWnd, const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return SetWindowTextW (hWnd, (LPCWSTR) lpString);
  else
    return SetWindowTextA (hWnd, (LPCSTR) lpString);
}

int
qxeGetWindowText (HWND hWnd, Extbyte * lpString, int nMaxCount)
{
  if (XEUNICODE_P)
    return GetWindowTextW (hWnd, (LPWSTR) lpString, nMaxCount);
  else
    return GetWindowTextA (hWnd, (LPSTR) lpString, nMaxCount);
}

int
qxeGetWindowTextLength (HWND hWnd)
{
  if (XEUNICODE_P)
    return GetWindowTextLengthW (hWnd);
  else
    return GetWindowTextLengthA (hWnd);
}

int
qxeMessageBox (HWND hWnd, const Extbyte * lpText, const Extbyte * lpCaption, UINT uType)
{
  if (XEUNICODE_P)
    return MessageBoxW (hWnd, (LPCWSTR) lpText, (LPCWSTR) lpCaption, uType);
  else
    return MessageBoxA (hWnd, (LPCSTR) lpText, (LPCSTR) lpCaption, uType);
}

int
qxeMessageBoxEx (HWND hWnd, const Extbyte * lpText, const Extbyte * lpCaption, UINT uType, WORD wLanguageId)
{
  if (XEUNICODE_P)
    return MessageBoxExW (hWnd, (LPCWSTR) lpText, (LPCWSTR) lpCaption, uType, wLanguageId);
  else
    return MessageBoxExA (hWnd, (LPCSTR) lpText, (LPCSTR) lpCaption, uType, wLanguageId);
}

/* NOTE: NT 4.0+ only */
int
qxeMessageBoxIndirect (LPMSGBOXPARAMSW arg1)
{
  if (XEUNICODE_P)
    return MessageBoxIndirectW (arg1);
  else
    return MessageBoxIndirectA ((LPMSGBOXPARAMSA) arg1);
}

LONG
qxeGetWindowLong (HWND hWnd, int nIndex)
{
  if (XEUNICODE_P)
    return GetWindowLongW (hWnd, nIndex);
  else
    return GetWindowLongA (hWnd, nIndex);
}

LONG
qxeSetWindowLong (HWND hWnd, int nIndex, LONG dwNewLong)
{
  if (XEUNICODE_P)
    return SetWindowLongW (hWnd, nIndex, dwNewLong);
  else
    return SetWindowLongA (hWnd, nIndex, dwNewLong);
}

DWORD
qxeGetClassLong (HWND hWnd, int nIndex)
{
  if (XEUNICODE_P)
    return GetClassLongW (hWnd, nIndex);
  else
    return GetClassLongA (hWnd, nIndex);
}

DWORD
qxeSetClassLong (HWND hWnd, int nIndex, LONG dwNewLong)
{
  if (XEUNICODE_P)
    return SetClassLongW (hWnd, nIndex, dwNewLong);
  else
    return SetClassLongA (hWnd, nIndex, dwNewLong);
}

HWND
qxeFindWindow (const Extbyte * lpClassName, const Extbyte * lpWindowName)
{
  if (XEUNICODE_P)
    return FindWindowW ((LPCWSTR) lpClassName, (LPCWSTR) lpWindowName);
  else
    return FindWindowA ((LPCSTR) lpClassName, (LPCSTR) lpWindowName);
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

int
qxeGetClassName (HWND hWnd, Extbyte * lpClassName, int nMaxCount)
{
  if (XEUNICODE_P)
    return GetClassNameW (hWnd, (LPWSTR) lpClassName, nMaxCount);
  else
    return GetClassNameA (hWnd, (LPSTR) lpClassName, nMaxCount);
}

/* Error if SetWindowsHook used: obsolete; two versions, STRICT and non-STRICT */

/* Error if SetWindowsHook used: obsolete; two versions, STRICT and non-STRICT */

HHOOK
qxeSetWindowsHookEx (int idHook, HOOKPROC lpfn, HINSTANCE hmod, DWORD dwThreadId)
{
  if (XEUNICODE_P)
    return SetWindowsHookExW (idHook, lpfn, hmod, dwThreadId);
  else
    return SetWindowsHookExA (idHook, lpfn, hmod, dwThreadId);
}

HBITMAP
qxeLoadBitmap (HINSTANCE hInstance, const Extbyte * lpBitmapName)
{
  if (XEUNICODE_P)
    return LoadBitmapW (hInstance, (LPCWSTR) lpBitmapName);
  else
    return LoadBitmapA (hInstance, (LPCSTR) lpBitmapName);
}

HCURSOR
qxeLoadCursor (HINSTANCE hInstance, const Extbyte * lpCursorName)
{
  if (XEUNICODE_P)
    return LoadCursorW (hInstance, (LPCWSTR) lpCursorName);
  else
    return LoadCursorA (hInstance, (LPCSTR) lpCursorName);
}

HCURSOR
qxeLoadCursorFromFile (const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return LoadCursorFromFileW ((LPCWSTR) lpFileName);
  else
    return LoadCursorFromFileA ((LPCSTR) lpFileName);
}

HICON
qxeLoadIcon (HINSTANCE hInstance, const Extbyte * lpIconName)
{
  if (XEUNICODE_P)
    return LoadIconW (hInstance, (LPCWSTR) lpIconName);
  else
    return LoadIconA (hInstance, (LPCSTR) lpIconName);
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

int
qxeLoadString (HINSTANCE hInstance, UINT uID, Extbyte * lpBuffer, int nBufferMax)
{
  if (XEUNICODE_P)
    return LoadStringW (hInstance, uID, (LPWSTR) lpBuffer, nBufferMax);
  else
    return LoadStringA (hInstance, uID, (LPSTR) lpBuffer, nBufferMax);
}

BOOL
qxeIsDialogMessage (HWND hDlg, LPMSG lpMsg)
{
  if (XEUNICODE_P)
    return IsDialogMessageW (hDlg, lpMsg);
  else
    return IsDialogMessageA (hDlg, lpMsg);
}

int
qxeDlgDirList (HWND hDlg, Extbyte * lpPathSpec, int nIDListBox, int nIDStaticPath, UINT uFileType)
{
  if (XEUNICODE_P)
    return DlgDirListW (hDlg, (LPWSTR) lpPathSpec, nIDListBox, nIDStaticPath, uFileType);
  else
    return DlgDirListA (hDlg, (LPSTR) lpPathSpec, nIDListBox, nIDStaticPath, uFileType);
}

BOOL
qxeDlgDirSelectEx (HWND hDlg, Extbyte * lpString, int nCount, int nIDListBox)
{
  if (XEUNICODE_P)
    return DlgDirSelectExW (hDlg, (LPWSTR) lpString, nCount, nIDListBox);
  else
    return DlgDirSelectExA (hDlg, (LPSTR) lpString, nCount, nIDListBox);
}

int
qxeDlgDirListComboBox (HWND hDlg, Extbyte * lpPathSpec, int nIDComboBox, int nIDStaticPath, UINT uFiletype)
{
  if (XEUNICODE_P)
    return DlgDirListComboBoxW (hDlg, (LPWSTR) lpPathSpec, nIDComboBox, nIDStaticPath, uFiletype);
  else
    return DlgDirListComboBoxA (hDlg, (LPSTR) lpPathSpec, nIDComboBox, nIDStaticPath, uFiletype);
}

BOOL
qxeDlgDirSelectComboBoxEx (HWND hDlg, Extbyte * lpString, int nCount, int nIDComboBox)
{
  if (XEUNICODE_P)
    return DlgDirSelectComboBoxExW (hDlg, (LPWSTR) lpString, nCount, nIDComboBox);
  else
    return DlgDirSelectComboBoxExA (hDlg, (LPSTR) lpString, nCount, nIDComboBox);
}

LRESULT
qxeDefFrameProc (HWND hWnd, HWND hWndMDIClient, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  if (XEUNICODE_P)
    return DefFrameProcW (hWnd, hWndMDIClient, uMsg, wParam, lParam);
  else
    return DefFrameProcA (hWnd, hWndMDIClient, uMsg, wParam, lParam);
}

/* Error if DefMDIChildProc used: return value is conditionalized on _MAC, messes up parser */

HWND
qxeCreateMDIWindow (Extbyte * lpClassName, Extbyte * lpWindowName, DWORD dwStyle, int X, int Y, int nWidth, int nHeight, HWND hWndParent, HINSTANCE hInstance, LPARAM lParam)
{
  if (XEUNICODE_P)
    return CreateMDIWindowW ((LPWSTR) lpClassName, (LPWSTR) lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hInstance, lParam);
  else
    return CreateMDIWindowA ((LPSTR) lpClassName, (LPSTR) lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hInstance, lParam);
}

BOOL
qxeWinHelp (HWND hWndMain, const Extbyte * lpszHelp, UINT uCommand, DWORD dwData)
{
  if (XEUNICODE_P)
    return WinHelpW (hWndMain, (LPCWSTR) lpszHelp, uCommand, dwData);
  else
    return WinHelpA (hWndMain, (LPCSTR) lpszHelp, uCommand, dwData);
}

/* Error if ChangeDisplaySettings used: split-sized LPDEVMODE */

/* Error if ChangeDisplaySettingsEx used: split-sized LPDEVMODE; NT 5.0/Win98+ only */

/* Error if EnumDisplaySettings used: split-sized LPDEVMODE */

/* Error if EnumDisplayDevices used: split-sized PDISPLAY_DEVICE; NT 5.0+ only, no Win98 */

/* NOTE: probs w/ICONMETRICS, NONCLIENTMETRICS */
BOOL
qxeSystemParametersInfo (UINT uiAction, UINT uiParam, PVOID pvParam, UINT fWinIni)
{
  if (XEUNICODE_P)
    return SystemParametersInfoW (uiAction, uiParam, pvParam, fWinIni);
  else
    return SystemParametersInfoA (uiAction, uiParam, pvParam, fWinIni);
}

/* Error if GetMonitorInfo used: NT 5.0/Win98+ only */

/* Error if GetWindowModuleFileName used: NT 5.0+ only */

/* Error if RealGetWindowClass used: NT 5.0+ only */

/* Error if GetAltTabInfo used: NT 5.0+ only */


/*----------------------------------------------------------------------*/
/*                      Processing file MMSYSTEM.H                      */
/*----------------------------------------------------------------------*/

BOOL
qxesndPlaySound (const Extbyte * pszSound, UINT fuSound)
{
  if (XEUNICODE_P)
    return sndPlaySoundW ((LPCWSTR) pszSound, fuSound);
  else
    return sndPlaySoundA ((LPCSTR) pszSound, fuSound);
}

BOOL
qxePlaySound (const Extbyte * pszSound, HMODULE hmod, DWORD fdwSound)
{
  if (XEUNICODE_P)
    return PlaySoundW ((LPCWSTR) pszSound, hmod, fdwSound);
  else
    return PlaySoundA ((LPCSTR) pszSound, hmod, fdwSound);
}

/* Error if waveOutGetDevCaps used: split-sized LPWAVEOUTCAPS */

MMRESULT
qxewaveOutGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText)
{
  if (XEUNICODE_P)
    return waveOutGetErrorTextW (mmrError, (LPWSTR) pszText, cchText);
  else
    return waveOutGetErrorTextA (mmrError, (LPSTR) pszText, cchText);
}

/* Error if waveInGetDevCaps used: split-sized LPWAVEINCAPS */

MMRESULT
qxewaveInGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText)
{
  if (XEUNICODE_P)
    return waveInGetErrorTextW (mmrError, (LPWSTR) pszText, cchText);
  else
    return waveInGetErrorTextA (mmrError, (LPSTR) pszText, cchText);
}

/* Error if midiOutGetDevCaps used: split-sized LPMIDIOUTCAPS */

MMRESULT
qxemidiOutGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText)
{
  if (XEUNICODE_P)
    return midiOutGetErrorTextW (mmrError, (LPWSTR) pszText, cchText);
  else
    return midiOutGetErrorTextA (mmrError, (LPSTR) pszText, cchText);
}

/* Error if midiInGetDevCaps used: split-sized LPMIDIOUTCAPS */

MMRESULT
qxemidiInGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText)
{
  if (XEUNICODE_P)
    return midiInGetErrorTextW (mmrError, (LPWSTR) pszText, cchText);
  else
    return midiInGetErrorTextA (mmrError, (LPSTR) pszText, cchText);
}

/* Error if auxGetDevCaps used: split-sized LPAUXCAPS */

/* Error if mixerGetDevCaps used: split-sized LPMIXERCAPS */

/* Error if mixerGetLineInfo used: split-sized LPMIXERLINE */

/* Error if mixerGetLineControls used: split-sized LPMIXERCONTROL */

/* Error if mixerGetControlDetails used: split-sized LPMIXERCONTROL in LPMIXERLINECONTROLS in LPMIXERCONTROLDETAILS */

/* Error if joyGetDevCaps used: split-sized LPJOYCAPS */

FOURCC
qxemmioStringToFOURCC (const Extbyte * sz, UINT uFlags)
{
  if (XEUNICODE_P)
    return mmioStringToFOURCCW ((LPCWSTR) sz, uFlags);
  else
    return mmioStringToFOURCCA ((LPCSTR) sz, uFlags);
}

LPMMIOPROC
qxemmioInstallIOProc (FOURCC fccIOProc, LPMMIOPROC pIOProc, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return mmioInstallIOProcW (fccIOProc, pIOProc, dwFlags);
  else
    return mmioInstallIOProcA (fccIOProc, pIOProc, dwFlags);
}

HMMIO
qxemmioOpen (Extbyte * pszFileName, LPMMIOINFO pmmioinfo, DWORD fdwOpen)
{
  if (XEUNICODE_P)
    return mmioOpenW ((LPWSTR) pszFileName, pmmioinfo, fdwOpen);
  else
    return mmioOpenA ((LPSTR) pszFileName, pmmioinfo, fdwOpen);
}

MMRESULT
qxemmioRename (const Extbyte * pszFileName, const Extbyte * pszNewFileName, LPCMMIOINFO pmmioinfo, DWORD fdwRename)
{
  if (XEUNICODE_P)
    return mmioRenameW ((LPCWSTR) pszFileName, (LPCWSTR) pszNewFileName, pmmioinfo, fdwRename);
  else
    return mmioRenameA ((LPCSTR) pszFileName, (LPCSTR) pszNewFileName, pmmioinfo, fdwRename);
}

MCIERROR
qxemciSendCommand (MCIDEVICEID mciId, UINT uMsg, DWORD dwParam1, DWORD dwParam2)
{
  if (XEUNICODE_P)
    return mciSendCommandW (mciId, uMsg, dwParam1, dwParam2);
  else
    return mciSendCommandA (mciId, uMsg, dwParam1, dwParam2);
}

MCIERROR
qxemciSendString (const Extbyte * lpstrCommand, Extbyte * lpstrReturnString, UINT uReturnLength, HWND hwndCallback)
{
  if (XEUNICODE_P)
    return mciSendStringW ((LPCWSTR) lpstrCommand, (LPWSTR) lpstrReturnString, uReturnLength, hwndCallback);
  else
    return mciSendStringA ((LPCSTR) lpstrCommand, (LPSTR) lpstrReturnString, uReturnLength, hwndCallback);
}

MCIDEVICEID
qxemciGetDeviceID (const Extbyte * pszDevice)
{
  if (XEUNICODE_P)
    return mciGetDeviceIDW ((LPCWSTR) pszDevice);
  else
    return mciGetDeviceIDA ((LPCSTR) pszDevice);
}

#if !defined (MINGW)

MCIDEVICEID
qxemciGetDeviceIDFromElementID (DWORD dwElementID, const Extbyte * lpstrType)
{
  if (XEUNICODE_P)
    return mciGetDeviceIDFromElementIDW (dwElementID, (LPCWSTR) lpstrType);
  else
    return mciGetDeviceIDFromElementIDA (dwElementID, (LPCSTR) lpstrType);
}

#endif /* !defined (MINGW) */

BOOL
qxemciGetErrorString (MCIERROR mcierr, Extbyte * pszText, UINT cchText)
{
  if (XEUNICODE_P)
    return mciGetErrorStringW (mcierr, (LPWSTR) pszText, cchText);
  else
    return mciGetErrorStringA (mcierr, (LPSTR) pszText, cchText);
}


/*----------------------------------------------------------------------*/
/*                      Processing file WINBASE.H                       */
/*----------------------------------------------------------------------*/

BOOL
qxeGetBinaryType (const Extbyte * lpApplicationName, LPDWORD lpBinaryType)
{
  if (XEUNICODE_P)
    return GetBinaryTypeW ((LPCWSTR) lpApplicationName, lpBinaryType);
  else
    return GetBinaryTypeA ((LPCSTR) lpApplicationName, lpBinaryType);
}

DWORD
qxeGetShortPathName (const Extbyte * lpszLongPath, Extbyte * lpszShortPath, DWORD cchBuffer)
{
  if (XEUNICODE_P)
    return GetShortPathNameW ((LPCWSTR) lpszLongPath, (LPWSTR) lpszShortPath, cchBuffer);
  else
    return GetShortPathNameA ((LPCSTR) lpszLongPath, (LPSTR) lpszShortPath, cchBuffer);
}

DWORD
qxeGetLongPathName (const Extbyte * lpszShortPath, Extbyte * lpszLongPath, DWORD cchBuffer)
{
  if (XEUNICODE_P)
    return GetLongPathNameW ((LPCWSTR) lpszShortPath, (LPWSTR) lpszLongPath, cchBuffer);
  else
    return GetLongPathNameA ((LPCSTR) lpszShortPath, (LPSTR) lpszLongPath, cchBuffer);
}

Extbyte *
qxeGetEnvironmentStrings (void)
{
  if (XEUNICODE_P)
    return (Extbyte *) GetEnvironmentStringsW ();
  else
    return (Extbyte *) GetEnvironmentStringsA ();
}

BOOL
qxeFreeEnvironmentStrings (Extbyte * arg1)
{
  if (XEUNICODE_P)
    return FreeEnvironmentStringsW ((LPWSTR) arg1);
  else
    return FreeEnvironmentStringsA ((LPSTR) arg1);
}

DWORD
qxeFormatMessage (DWORD dwFlags, LPCVOID lpSource, DWORD dwMessageId, DWORD dwLanguageId, Extbyte * lpBuffer, DWORD nSize, va_list * Arguments)
{
  if (XEUNICODE_P)
    return FormatMessageW (dwFlags, lpSource, dwMessageId, dwLanguageId, (LPWSTR) lpBuffer, nSize, Arguments);
  else
    return FormatMessageA (dwFlags, lpSource, dwMessageId, dwLanguageId, (LPSTR) lpBuffer, nSize, Arguments);
}

HANDLE
qxeCreateMailslot (const Extbyte * lpName, DWORD nMaxMessageSize, DWORD lReadTimeout, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
{
  if (XEUNICODE_P)
    return CreateMailslotW ((LPCWSTR) lpName, nMaxMessageSize, lReadTimeout, lpSecurityAttributes);
  else
    return CreateMailslotA ((LPCSTR) lpName, nMaxMessageSize, lReadTimeout, lpSecurityAttributes);
}

#if !defined (CYGWIN_HEADERS)

BOOL
qxeEncryptFile (const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return EncryptFileW ((LPCWSTR) lpFileName);
  else
    return EncryptFileA ((LPCSTR) lpFileName);
}

#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)

BOOL
qxeDecryptFile (const Extbyte * lpFileName, DWORD dwReserved)
{
  if (XEUNICODE_P)
    return DecryptFileW ((LPCWSTR) lpFileName, dwReserved);
  else
    return DecryptFileA ((LPCSTR) lpFileName, dwReserved);
}

#endif /* !defined (CYGWIN_HEADERS) */

/* Error if OpenRaw used: error "The procedure entry point OpenRawW could not be located in the dynamic link library ADVAPI32.dll." */

/* Error if QueryRecoveryAgents used: split-sized LPRECOVERY_AGENT_INFORMATION */

int
qxelstrcmp (const Extbyte * lpString1, const Extbyte * lpString2)
{
  if (XEUNICODE_P)
    return lstrcmpW ((LPCWSTR) lpString1, (LPCWSTR) lpString2);
  else
    return lstrcmpA ((LPCSTR) lpString1, (LPCSTR) lpString2);
}

int
qxelstrcmpi (const Extbyte * lpString1, const Extbyte * lpString2)
{
  if (XEUNICODE_P)
    return lstrcmpiW ((LPCWSTR) lpString1, (LPCWSTR) lpString2);
  else
    return lstrcmpiA ((LPCSTR) lpString1, (LPCSTR) lpString2);
}

Extbyte *
qxelstrcpyn (Extbyte * lpString1, const Extbyte * lpString2, int iMaxLength)
{
  if (XEUNICODE_P)
    return (Extbyte *) lstrcpynW ((LPWSTR) lpString1, (LPCWSTR) lpString2, iMaxLength);
  else
    return (Extbyte *) lstrcpynA ((LPSTR) lpString1, (LPCSTR) lpString2, iMaxLength);
}

Extbyte *
qxelstrcpy (Extbyte * lpString1, const Extbyte * lpString2)
{
  if (XEUNICODE_P)
    return (Extbyte *) lstrcpyW ((LPWSTR) lpString1, (LPCWSTR) lpString2);
  else
    return (Extbyte *) lstrcpyA ((LPSTR) lpString1, (LPCSTR) lpString2);
}

Extbyte *
qxelstrcat (Extbyte * lpString1, const Extbyte * lpString2)
{
  if (XEUNICODE_P)
    return (Extbyte *) lstrcatW ((LPWSTR) lpString1, (LPCWSTR) lpString2);
  else
    return (Extbyte *) lstrcatA ((LPSTR) lpString1, (LPCSTR) lpString2);
}

int
qxelstrlen (const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return lstrlenW ((LPCWSTR) lpString);
  else
    return lstrlenA ((LPCSTR) lpString);
}

HANDLE
qxeCreateMutex (LPSECURITY_ATTRIBUTES lpMutexAttributes, BOOL bInitialOwner, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return CreateMutexW (lpMutexAttributes, bInitialOwner, (LPCWSTR) lpName);
  else
    return CreateMutexA (lpMutexAttributes, bInitialOwner, (LPCSTR) lpName);
}

HANDLE
qxeOpenMutex (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return OpenMutexW (dwDesiredAccess, bInheritHandle, (LPCWSTR) lpName);
  else
    return OpenMutexA (dwDesiredAccess, bInheritHandle, (LPCSTR) lpName);
}

HANDLE
qxeCreateEvent (LPSECURITY_ATTRIBUTES lpEventAttributes, BOOL bManualReset, BOOL bInitialState, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return CreateEventW (lpEventAttributes, bManualReset, bInitialState, (LPCWSTR) lpName);
  else
    return CreateEventA (lpEventAttributes, bManualReset, bInitialState, (LPCSTR) lpName);
}

HANDLE
qxeOpenEvent (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return OpenEventW (dwDesiredAccess, bInheritHandle, (LPCWSTR) lpName);
  else
    return OpenEventA (dwDesiredAccess, bInheritHandle, (LPCSTR) lpName);
}

HANDLE
qxeCreateSemaphore (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return CreateSemaphoreW (lpSemaphoreAttributes, lInitialCount, lMaximumCount, (LPCWSTR) lpName);
  else
    return CreateSemaphoreA (lpSemaphoreAttributes, lInitialCount, lMaximumCount, (LPCSTR) lpName);
}

HANDLE
qxeOpenSemaphore (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return OpenSemaphoreW (dwDesiredAccess, bInheritHandle, (LPCWSTR) lpName);
  else
    return OpenSemaphoreA (dwDesiredAccess, bInheritHandle, (LPCSTR) lpName);
}

HANDLE
qxeCreateWaitableTimer (LPSECURITY_ATTRIBUTES lpTimerAttributes, BOOL bManualReset, const Extbyte * lpTimerName)
{
  if (XEUNICODE_P)
    return CreateWaitableTimerW (lpTimerAttributes, bManualReset, (LPCWSTR) lpTimerName);
  else
    return CreateWaitableTimerA (lpTimerAttributes, bManualReset, (LPCSTR) lpTimerName);
}

HANDLE
qxeOpenWaitableTimer (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpTimerName)
{
  if (XEUNICODE_P)
    return OpenWaitableTimerW (dwDesiredAccess, bInheritHandle, (LPCWSTR) lpTimerName);
  else
    return OpenWaitableTimerA (dwDesiredAccess, bInheritHandle, (LPCSTR) lpTimerName);
}

HANDLE
qxeCreateFileMapping (HANDLE hFile, LPSECURITY_ATTRIBUTES lpFileMappingAttributes, DWORD flProtect, DWORD dwMaximumSizeHigh, DWORD dwMaximumSizeLow, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return CreateFileMappingW (hFile, lpFileMappingAttributes, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, (LPCWSTR) lpName);
  else
    return CreateFileMappingA (hFile, lpFileMappingAttributes, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, (LPCSTR) lpName);
}

HANDLE
qxeOpenFileMapping (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName)
{
  if (XEUNICODE_P)
    return OpenFileMappingW (dwDesiredAccess, bInheritHandle, (LPCWSTR) lpName);
  else
    return OpenFileMappingA (dwDesiredAccess, bInheritHandle, (LPCSTR) lpName);
}

DWORD
qxeGetLogicalDriveStrings (DWORD nBufferLength, Extbyte * lpBuffer)
{
  if (XEUNICODE_P)
    return GetLogicalDriveStringsW (nBufferLength, (LPWSTR) lpBuffer);
  else
    return GetLogicalDriveStringsA (nBufferLength, (LPSTR) lpBuffer);
}

HMODULE
qxeLoadLibrary (const Extbyte * lpLibFileName)
{
  if (XEUNICODE_P)
    return LoadLibraryW ((LPCWSTR) lpLibFileName);
  else
    return LoadLibraryA ((LPCSTR) lpLibFileName);
}

HMODULE
qxeLoadLibraryEx (const Extbyte * lpLibFileName, HANDLE hFile, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return LoadLibraryExW ((LPCWSTR) lpLibFileName, hFile, dwFlags);
  else
    return LoadLibraryExA ((LPCSTR) lpLibFileName, hFile, dwFlags);
}

DWORD
qxeGetModuleFileName (HMODULE hModule, Extbyte * lpFilename, DWORD nSize)
{
  if (XEUNICODE_P)
    return GetModuleFileNameW (hModule, (LPWSTR) lpFilename, nSize);
  else
    return GetModuleFileNameA (hModule, (LPSTR) lpFilename, nSize);
}

HMODULE
qxeGetModuleHandle (const Extbyte * lpModuleName)
{
  if (XEUNICODE_P)
    return GetModuleHandleW ((LPCWSTR) lpModuleName);
  else
    return GetModuleHandleA ((LPCSTR) lpModuleName);
}

BOOL
qxeCreateProcess (const Extbyte * lpApplicationName, Extbyte * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, const Extbyte * lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation)
{
  if (XEUNICODE_P)
    return CreateProcessW ((LPCWSTR) lpApplicationName, (LPWSTR) lpCommandLine, lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags, lpEnvironment, (LPCWSTR) lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
  else
    return CreateProcessA ((LPCSTR) lpApplicationName, (LPSTR) lpCommandLine, lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags, lpEnvironment, (LPCSTR) lpCurrentDirectory, (LPSTARTUPINFOA) lpStartupInfo, lpProcessInformation);
}

VOID
qxeFatalAppExit (UINT uAction, const Extbyte * lpMessageText)
{
  if (XEUNICODE_P)
    FatalAppExitW (uAction, (LPCWSTR) lpMessageText);
  else
    FatalAppExitA (uAction, (LPCSTR) lpMessageText);
}

VOID
qxeGetStartupInfo (LPSTARTUPINFOW lpStartupInfo)
{
  if (XEUNICODE_P)
    GetStartupInfoW (lpStartupInfo);
  else
    GetStartupInfoA ((LPSTARTUPINFOA) lpStartupInfo);
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
qxeGetEnvironmentVariable (const Extbyte * lpName, Extbyte * lpBuffer, DWORD nSize)
{
  if (XEUNICODE_P)
    return GetEnvironmentVariableW ((LPCWSTR) lpName, (LPWSTR) lpBuffer, nSize);
  else
    return GetEnvironmentVariableA ((LPCSTR) lpName, (LPSTR) lpBuffer, nSize);
}

BOOL
qxeSetEnvironmentVariable (const Extbyte * lpName, const Extbyte * lpValue)
{
  if (XEUNICODE_P)
    return SetEnvironmentVariableW ((LPCWSTR) lpName, (LPCWSTR) lpValue);
  else
    return SetEnvironmentVariableA ((LPCSTR) lpName, (LPCSTR) lpValue);
}

DWORD
qxeExpandEnvironmentStrings (const Extbyte * lpSrc, Extbyte * lpDst, DWORD nSize)
{
  if (XEUNICODE_P)
    return ExpandEnvironmentStringsW ((LPCWSTR) lpSrc, (LPWSTR) lpDst, nSize);
  else
    return ExpandEnvironmentStringsA ((LPCSTR) lpSrc, (LPSTR) lpDst, nSize);
}

VOID
qxeOutputDebugString (const Extbyte * lpOutputString)
{
  if (XEUNICODE_P)
    OutputDebugStringW ((LPCWSTR) lpOutputString);
  else
    OutputDebugStringA ((LPCSTR) lpOutputString);
}

HRSRC
qxeFindResource (HMODULE hModule, const Extbyte * lpName, const Extbyte * lpType)
{
  if (XEUNICODE_P)
    return FindResourceW (hModule, (LPCWSTR) lpName, (LPCWSTR) lpType);
  else
    return FindResourceA (hModule, (LPCSTR) lpName, (LPCSTR) lpType);
}

HRSRC
qxeFindResourceEx (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, WORD wLanguage)
{
  if (XEUNICODE_P)
    return FindResourceExW (hModule, (LPCWSTR) lpType, (LPCWSTR) lpName, wLanguage);
  else
    return FindResourceExA (hModule, (LPCSTR) lpType, (LPCSTR) lpName, wLanguage);
}

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

HANDLE
qxeBeginUpdateResource (const Extbyte * pFileName, BOOL bDeleteExistingResources)
{
  if (XEUNICODE_P)
    return BeginUpdateResourceW ((LPCWSTR) pFileName, bDeleteExistingResources);
  else
    return BeginUpdateResourceA ((LPCSTR) pFileName, bDeleteExistingResources);
}

BOOL
qxeUpdateResource (HANDLE hUpdate, const Extbyte * lpType, const Extbyte * lpName, WORD wLanguage, LPVOID lpData, DWORD cbData)
{
  if (XEUNICODE_P)
    return UpdateResourceW (hUpdate, (LPCWSTR) lpType, (LPCWSTR) lpName, wLanguage, lpData, cbData);
  else
    return UpdateResourceA (hUpdate, (LPCSTR) lpType, (LPCSTR) lpName, wLanguage, lpData, cbData);
}

BOOL
qxeEndUpdateResource (HANDLE hUpdate, BOOL fDiscard)
{
  if (XEUNICODE_P)
    return EndUpdateResourceW (hUpdate, fDiscard);
  else
    return EndUpdateResourceA (hUpdate, fDiscard);
}

ATOM
qxeGlobalAddAtom (const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return GlobalAddAtomW ((LPCWSTR) lpString);
  else
    return GlobalAddAtomA ((LPCSTR) lpString);
}

ATOM
qxeGlobalFindAtom (const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return GlobalFindAtomW ((LPCWSTR) lpString);
  else
    return GlobalFindAtomA ((LPCSTR) lpString);
}

UINT
qxeGlobalGetAtomName (ATOM nAtom, Extbyte * lpBuffer, int nSize)
{
  if (XEUNICODE_P)
    return GlobalGetAtomNameW (nAtom, (LPWSTR) lpBuffer, nSize);
  else
    return GlobalGetAtomNameA (nAtom, (LPSTR) lpBuffer, nSize);
}

ATOM
qxeAddAtom (const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return AddAtomW ((LPCWSTR) lpString);
  else
    return AddAtomA ((LPCSTR) lpString);
}

ATOM
qxeFindAtom (const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return FindAtomW ((LPCWSTR) lpString);
  else
    return FindAtomA ((LPCSTR) lpString);
}

UINT
qxeGetAtomName (ATOM nAtom, Extbyte * lpBuffer, int nSize)
{
  if (XEUNICODE_P)
    return GetAtomNameW (nAtom, (LPWSTR) lpBuffer, nSize);
  else
    return GetAtomNameA (nAtom, (LPSTR) lpBuffer, nSize);
}

UINT
qxeGetProfileInt (const Extbyte * lpAppName, const Extbyte * lpKeyName, INT nDefault)
{
  if (XEUNICODE_P)
    return GetProfileIntW ((LPCWSTR) lpAppName, (LPCWSTR) lpKeyName, nDefault);
  else
    return GetProfileIntA ((LPCSTR) lpAppName, (LPCSTR) lpKeyName, nDefault);
}

DWORD
qxeGetProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpDefault, Extbyte * lpReturnedString, DWORD nSize)
{
  if (XEUNICODE_P)
    return GetProfileStringW ((LPCWSTR) lpAppName, (LPCWSTR) lpKeyName, (LPCWSTR) lpDefault, (LPWSTR) lpReturnedString, nSize);
  else
    return GetProfileStringA ((LPCSTR) lpAppName, (LPCSTR) lpKeyName, (LPCSTR) lpDefault, (LPSTR) lpReturnedString, nSize);
}

BOOL
qxeWriteProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return WriteProfileStringW ((LPCWSTR) lpAppName, (LPCWSTR) lpKeyName, (LPCWSTR) lpString);
  else
    return WriteProfileStringA ((LPCSTR) lpAppName, (LPCSTR) lpKeyName, (LPCSTR) lpString);
}

DWORD
qxeGetProfileSection (const Extbyte * lpAppName, Extbyte * lpReturnedString, DWORD nSize)
{
  if (XEUNICODE_P)
    return GetProfileSectionW ((LPCWSTR) lpAppName, (LPWSTR) lpReturnedString, nSize);
  else
    return GetProfileSectionA ((LPCSTR) lpAppName, (LPSTR) lpReturnedString, nSize);
}

BOOL
qxeWriteProfileSection (const Extbyte * lpAppName, const Extbyte * lpString)
{
  if (XEUNICODE_P)
    return WriteProfileSectionW ((LPCWSTR) lpAppName, (LPCWSTR) lpString);
  else
    return WriteProfileSectionA ((LPCSTR) lpAppName, (LPCSTR) lpString);
}

UINT
qxeGetPrivateProfileInt (const Extbyte * lpAppName, const Extbyte * lpKeyName, INT nDefault, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return GetPrivateProfileIntW ((LPCWSTR) lpAppName, (LPCWSTR) lpKeyName, nDefault, (LPCWSTR) lpFileName);
  else
    return GetPrivateProfileIntA ((LPCSTR) lpAppName, (LPCSTR) lpKeyName, nDefault, (LPCSTR) lpFileName);
}

DWORD
qxeGetPrivateProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpDefault, Extbyte * lpReturnedString, DWORD nSize, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return GetPrivateProfileStringW ((LPCWSTR) lpAppName, (LPCWSTR) lpKeyName, (LPCWSTR) lpDefault, (LPWSTR) lpReturnedString, nSize, (LPCWSTR) lpFileName);
  else
    return GetPrivateProfileStringA ((LPCSTR) lpAppName, (LPCSTR) lpKeyName, (LPCSTR) lpDefault, (LPSTR) lpReturnedString, nSize, (LPCSTR) lpFileName);
}

BOOL
qxeWritePrivateProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpString, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return WritePrivateProfileStringW ((LPCWSTR) lpAppName, (LPCWSTR) lpKeyName, (LPCWSTR) lpString, (LPCWSTR) lpFileName);
  else
    return WritePrivateProfileStringA ((LPCSTR) lpAppName, (LPCSTR) lpKeyName, (LPCSTR) lpString, (LPCSTR) lpFileName);
}

DWORD
qxeGetPrivateProfileSection (const Extbyte * lpAppName, Extbyte * lpReturnedString, DWORD nSize, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return GetPrivateProfileSectionW ((LPCWSTR) lpAppName, (LPWSTR) lpReturnedString, nSize, (LPCWSTR) lpFileName);
  else
    return GetPrivateProfileSectionA ((LPCSTR) lpAppName, (LPSTR) lpReturnedString, nSize, (LPCSTR) lpFileName);
}

BOOL
qxeWritePrivateProfileSection (const Extbyte * lpAppName, const Extbyte * lpString, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return WritePrivateProfileSectionW ((LPCWSTR) lpAppName, (LPCWSTR) lpString, (LPCWSTR) lpFileName);
  else
    return WritePrivateProfileSectionA ((LPCSTR) lpAppName, (LPCSTR) lpString, (LPCSTR) lpFileName);
}

DWORD
qxeGetPrivateProfileSectionNames (Extbyte * lpszReturnBuffer, DWORD nSize, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return GetPrivateProfileSectionNamesW ((LPWSTR) lpszReturnBuffer, nSize, (LPCWSTR) lpFileName);
  else
    return GetPrivateProfileSectionNamesA ((LPSTR) lpszReturnBuffer, nSize, (LPCSTR) lpFileName);
}

BOOL
qxeGetPrivateProfileStruct (const Extbyte * lpszSection, const Extbyte * lpszKey, LPVOID lpStruct, UINT uSizeStruct, const Extbyte * szFile)
{
  if (XEUNICODE_P)
    return GetPrivateProfileStructW ((LPCWSTR) lpszSection, (LPCWSTR) lpszKey, lpStruct, uSizeStruct, (LPCWSTR) szFile);
  else
    return GetPrivateProfileStructA ((LPCSTR) lpszSection, (LPCSTR) lpszKey, lpStruct, uSizeStruct, (LPCSTR) szFile);
}

BOOL
qxeWritePrivateProfileStruct (const Extbyte * lpszSection, const Extbyte * lpszKey, LPVOID lpStruct, UINT uSizeStruct, const Extbyte * szFile)
{
  if (XEUNICODE_P)
    return WritePrivateProfileStructW ((LPCWSTR) lpszSection, (LPCWSTR) lpszKey, lpStruct, uSizeStruct, (LPCWSTR) szFile);
  else
    return WritePrivateProfileStructA ((LPCSTR) lpszSection, (LPCSTR) lpszKey, lpStruct, uSizeStruct, (LPCSTR) szFile);
}

UINT
qxeGetDriveType (const Extbyte * lpRootPathName)
{
  if (XEUNICODE_P)
    return GetDriveTypeW ((LPCWSTR) lpRootPathName);
  else
    return GetDriveTypeA ((LPCSTR) lpRootPathName);
}

UINT
qxeGetSystemDirectory (Extbyte * lpBuffer, UINT uSize)
{
  if (XEUNICODE_P)
    return GetSystemDirectoryW ((LPWSTR) lpBuffer, uSize);
  else
    return GetSystemDirectoryA ((LPSTR) lpBuffer, uSize);
}

DWORD
qxeGetTempPath (DWORD nBufferLength, Extbyte * lpBuffer)
{
  if (XEUNICODE_P)
    return GetTempPathW (nBufferLength, (LPWSTR) lpBuffer);
  else
    return GetTempPathA (nBufferLength, (LPSTR) lpBuffer);
}

UINT
qxeGetTempFileName (const Extbyte * lpPathName, const Extbyte * lpPrefixString, UINT uUnique, Extbyte * lpTempFileName)
{
  if (XEUNICODE_P)
    return GetTempFileNameW ((LPCWSTR) lpPathName, (LPCWSTR) lpPrefixString, uUnique, (LPWSTR) lpTempFileName);
  else
    return GetTempFileNameA ((LPCSTR) lpPathName, (LPCSTR) lpPrefixString, uUnique, (LPSTR) lpTempFileName);
}

UINT
qxeGetWindowsDirectory (Extbyte * lpBuffer, UINT uSize)
{
  if (XEUNICODE_P)
    return GetWindowsDirectoryW ((LPWSTR) lpBuffer, uSize);
  else
    return GetWindowsDirectoryA ((LPSTR) lpBuffer, uSize);
}

BOOL
qxeSetCurrentDirectory (const Extbyte * lpPathName)
{
  if (XEUNICODE_P)
    return SetCurrentDirectoryW ((LPCWSTR) lpPathName);
  else
    return SetCurrentDirectoryA ((LPCSTR) lpPathName);
}

DWORD
qxeGetCurrentDirectory (DWORD nBufferLength, Extbyte * lpBuffer)
{
  if (XEUNICODE_P)
    return GetCurrentDirectoryW (nBufferLength, (LPWSTR) lpBuffer);
  else
    return GetCurrentDirectoryA (nBufferLength, (LPSTR) lpBuffer);
}

BOOL
qxeGetDiskFreeSpace (const Extbyte * lpRootPathName, LPDWORD lpSectorsPerCluster, LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, LPDWORD lpTotalNumberOfClusters)
{
  if (XEUNICODE_P)
    return GetDiskFreeSpaceW ((LPCWSTR) lpRootPathName, lpSectorsPerCluster, lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters);
  else
    return GetDiskFreeSpaceA ((LPCSTR) lpRootPathName, lpSectorsPerCluster, lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters);
}

BOOL
qxeGetDiskFreeSpaceEx (const Extbyte * lpDirectoryName, PULARGE_INTEGER lpFreeBytesAvailableToCaller, PULARGE_INTEGER lpTotalNumberOfBytes, PULARGE_INTEGER lpTotalNumberOfFreeBytes)
{
  if (XEUNICODE_P)
    return GetDiskFreeSpaceExW ((LPCWSTR) lpDirectoryName, lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes, lpTotalNumberOfFreeBytes);
  else
    return GetDiskFreeSpaceExA ((LPCSTR) lpDirectoryName, lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes, lpTotalNumberOfFreeBytes);
}

BOOL
qxeCreateDirectory (const Extbyte * lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
{
  if (XEUNICODE_P)
    return CreateDirectoryW ((LPCWSTR) lpPathName, lpSecurityAttributes);
  else
    return CreateDirectoryA ((LPCSTR) lpPathName, lpSecurityAttributes);
}

BOOL
qxeCreateDirectoryEx (const Extbyte * lpTemplateDirectory, const Extbyte * lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
{
  if (XEUNICODE_P)
    return CreateDirectoryExW ((LPCWSTR) lpTemplateDirectory, (LPCWSTR) lpNewDirectory, lpSecurityAttributes);
  else
    return CreateDirectoryExA ((LPCSTR) lpTemplateDirectory, (LPCSTR) lpNewDirectory, lpSecurityAttributes);
}

BOOL
qxeRemoveDirectory (const Extbyte * lpPathName)
{
  if (XEUNICODE_P)
    return RemoveDirectoryW ((LPCWSTR) lpPathName);
  else
    return RemoveDirectoryA ((LPCSTR) lpPathName);
}

DWORD
qxeGetFullPathName (const Extbyte * lpFileName, DWORD nBufferLength, Extbyte * lpBuffer, Extbyte * * lpFilePart)
{
  if (XEUNICODE_P)
    return GetFullPathNameW ((LPCWSTR) lpFileName, nBufferLength, (LPWSTR) lpBuffer, (LPWSTR *) lpFilePart);
  else
    return GetFullPathNameA ((LPCSTR) lpFileName, nBufferLength, (LPSTR) lpBuffer, (LPSTR *) lpFilePart);
}

BOOL
qxeDefineDosDevice (DWORD dwFlags, const Extbyte * lpDeviceName, const Extbyte * lpTargetPath)
{
  if (XEUNICODE_P)
    return DefineDosDeviceW (dwFlags, (LPCWSTR) lpDeviceName, (LPCWSTR) lpTargetPath);
  else
    return DefineDosDeviceA (dwFlags, (LPCSTR) lpDeviceName, (LPCSTR) lpTargetPath);
}

DWORD
qxeQueryDosDevice (const Extbyte * lpDeviceName, Extbyte * lpTargetPath, DWORD ucchMax)
{
  if (XEUNICODE_P)
    return QueryDosDeviceW ((LPCWSTR) lpDeviceName, (LPWSTR) lpTargetPath, ucchMax);
  else
    return QueryDosDeviceA ((LPCSTR) lpDeviceName, (LPSTR) lpTargetPath, ucchMax);
}

HANDLE
qxeCreateFile (const Extbyte * lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile)
{
  if (XEUNICODE_P)
    return CreateFileW ((LPCWSTR) lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  else
    return CreateFileA ((LPCSTR) lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
}

BOOL
qxeSetFileAttributes (const Extbyte * lpFileName, DWORD dwFileAttributes)
{
  if (XEUNICODE_P)
    return SetFileAttributesW ((LPCWSTR) lpFileName, dwFileAttributes);
  else
    return SetFileAttributesA ((LPCSTR) lpFileName, dwFileAttributes);
}

DWORD
qxeGetFileAttributes (const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return GetFileAttributesW ((LPCWSTR) lpFileName);
  else
    return GetFileAttributesA ((LPCSTR) lpFileName);
}

BOOL
qxeGetFileAttributesEx (const Extbyte * lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation)
{
  if (XEUNICODE_P)
    return GetFileAttributesExW ((LPCWSTR) lpFileName, fInfoLevelId, lpFileInformation);
  else
    return GetFileAttributesExA ((LPCSTR) lpFileName, fInfoLevelId, lpFileInformation);
}

DWORD
qxeGetCompressedFileSize (const Extbyte * lpFileName, LPDWORD lpFileSizeHigh)
{
  if (XEUNICODE_P)
    return GetCompressedFileSizeW ((LPCWSTR) lpFileName, lpFileSizeHigh);
  else
    return GetCompressedFileSizeA ((LPCSTR) lpFileName, lpFileSizeHigh);
}

BOOL
qxeDeleteFile (const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return DeleteFileW ((LPCWSTR) lpFileName);
  else
    return DeleteFileA ((LPCSTR) lpFileName);
}

/* Error if FindFirstFileEx used: split-sized LPWIN32_FIND_DATA; not used, NT 4.0+ only */

/* Skipping FindFirstFile because split-sized LPWIN32_FIND_DATA */

/* Skipping FindNextFile because split-sized LPWIN32_FIND_DATA */

DWORD
qxeSearchPath (const Extbyte * lpPath, const Extbyte * lpFileName, const Extbyte * lpExtension, DWORD nBufferLength, Extbyte * lpBuffer, Extbyte * * lpFilePart)
{
  if (XEUNICODE_P)
    return SearchPathW ((LPCWSTR) lpPath, (LPCWSTR) lpFileName, (LPCWSTR) lpExtension, nBufferLength, (LPWSTR) lpBuffer, (LPWSTR *) lpFilePart);
  else
    return SearchPathA ((LPCSTR) lpPath, (LPCSTR) lpFileName, (LPCSTR) lpExtension, nBufferLength, (LPSTR) lpBuffer, (LPSTR *) lpFilePart);
}

BOOL
qxeCopyFile (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, BOOL bFailIfExists)
{
  if (XEUNICODE_P)
    return CopyFileW ((LPCWSTR) lpExistingFileName, (LPCWSTR) lpNewFileName, bFailIfExists);
  else
    return CopyFileA ((LPCSTR) lpExistingFileName, (LPCSTR) lpNewFileName, bFailIfExists);
}

/* NOTE: NT 4.0+ only */
BOOL
qxeCopyFileEx (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags)
{
  if (XEUNICODE_P)
    return CopyFileExW ((LPCWSTR) lpExistingFileName, (LPCWSTR) lpNewFileName, lpProgressRoutine, lpData, pbCancel, dwCopyFlags);
  else
    return CopyFileExA ((LPCSTR) lpExistingFileName, (LPCSTR) lpNewFileName, lpProgressRoutine, lpData, pbCancel, dwCopyFlags);
}

BOOL
qxeMoveFile (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName)
{
  if (XEUNICODE_P)
    return MoveFileW ((LPCWSTR) lpExistingFileName, (LPCWSTR) lpNewFileName);
  else
    return MoveFileA ((LPCSTR) lpExistingFileName, (LPCSTR) lpNewFileName);
}

BOOL
qxeMoveFileEx (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, DWORD dwFlags)
{
  if (XEUNICODE_P)
    return MoveFileExW ((LPCWSTR) lpExistingFileName, (LPCWSTR) lpNewFileName, dwFlags);
  else
    return MoveFileExA ((LPCSTR) lpExistingFileName, (LPCSTR) lpNewFileName, dwFlags);
}

/* Error if MoveFileWithProgress used: NT 5.0+ only */

/* Error if CreateHardLink used: NT 5.0+ only */

HANDLE
qxeCreateNamedPipe (const Extbyte * lpName, DWORD dwOpenMode, DWORD dwPipeMode, DWORD nMaxInstances, DWORD nOutBufferSize, DWORD nInBufferSize, DWORD nDefaultTimeOut, LPSECURITY_ATTRIBUTES lpSecurityAttributes)
{
  if (XEUNICODE_P)
    return CreateNamedPipeW ((LPCWSTR) lpName, dwOpenMode, dwPipeMode, nMaxInstances, nOutBufferSize, nInBufferSize, nDefaultTimeOut, lpSecurityAttributes);
  else
    return CreateNamedPipeA ((LPCSTR) lpName, dwOpenMode, dwPipeMode, nMaxInstances, nOutBufferSize, nInBufferSize, nDefaultTimeOut, lpSecurityAttributes);
}

BOOL
qxeGetNamedPipeHandleState (HANDLE hNamedPipe, LPDWORD lpState, LPDWORD lpCurInstances, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout, Extbyte * lpUserName, DWORD nMaxUserNameSize)
{
  if (XEUNICODE_P)
    return GetNamedPipeHandleStateW (hNamedPipe, lpState, lpCurInstances, lpMaxCollectionCount, lpCollectDataTimeout, (LPWSTR) lpUserName, nMaxUserNameSize);
  else
    return GetNamedPipeHandleStateA (hNamedPipe, lpState, lpCurInstances, lpMaxCollectionCount, lpCollectDataTimeout, (LPSTR) lpUserName, nMaxUserNameSize);
}

BOOL
qxeCallNamedPipe (const Extbyte * lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut)
{
  if (XEUNICODE_P)
    return CallNamedPipeW ((LPCWSTR) lpNamedPipeName, lpInBuffer, nInBufferSize, lpOutBuffer, nOutBufferSize, lpBytesRead, nTimeOut);
  else
    return CallNamedPipeA ((LPCSTR) lpNamedPipeName, lpInBuffer, nInBufferSize, lpOutBuffer, nOutBufferSize, lpBytesRead, nTimeOut);
}

BOOL
qxeWaitNamedPipe (const Extbyte * lpNamedPipeName, DWORD nTimeOut)
{
  if (XEUNICODE_P)
    return WaitNamedPipeW ((LPCWSTR) lpNamedPipeName, nTimeOut);
  else
    return WaitNamedPipeA ((LPCSTR) lpNamedPipeName, nTimeOut);
}

BOOL
qxeSetVolumeLabel (const Extbyte * lpRootPathName, const Extbyte * lpVolumeName)
{
  if (XEUNICODE_P)
    return SetVolumeLabelW ((LPCWSTR) lpRootPathName, (LPCWSTR) lpVolumeName);
  else
    return SetVolumeLabelA ((LPCSTR) lpRootPathName, (LPCSTR) lpVolumeName);
}

BOOL
qxeGetVolumeInformation (const Extbyte * lpRootPathName, Extbyte * lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, Extbyte * lpFileSystemNameBuffer, DWORD nFileSystemNameSize)
{
  if (XEUNICODE_P)
    return GetVolumeInformationW ((LPCWSTR) lpRootPathName, (LPWSTR) lpVolumeNameBuffer, nVolumeNameSize, lpVolumeSerialNumber, lpMaximumComponentLength, lpFileSystemFlags, (LPWSTR) lpFileSystemNameBuffer, nFileSystemNameSize);
  else
    return GetVolumeInformationA ((LPCSTR) lpRootPathName, (LPSTR) lpVolumeNameBuffer, nVolumeNameSize, lpVolumeSerialNumber, lpMaximumComponentLength, lpFileSystemFlags, (LPSTR) lpFileSystemNameBuffer, nFileSystemNameSize);
}

BOOL
qxeClearEventLog (HANDLE hEventLog, const Extbyte * lpBackupFileName)
{
  if (XEUNICODE_P)
    return ClearEventLogW (hEventLog, (LPCWSTR) lpBackupFileName);
  else
    return ClearEventLogA (hEventLog, (LPCSTR) lpBackupFileName);
}

BOOL
qxeBackupEventLog (HANDLE hEventLog, const Extbyte * lpBackupFileName)
{
  if (XEUNICODE_P)
    return BackupEventLogW (hEventLog, (LPCWSTR) lpBackupFileName);
  else
    return BackupEventLogA (hEventLog, (LPCSTR) lpBackupFileName);
}

HANDLE
qxeOpenEventLog (const Extbyte * lpUNCServerName, const Extbyte * lpSourceName)
{
  if (XEUNICODE_P)
    return OpenEventLogW ((LPCWSTR) lpUNCServerName, (LPCWSTR) lpSourceName);
  else
    return OpenEventLogA ((LPCSTR) lpUNCServerName, (LPCSTR) lpSourceName);
}

HANDLE
qxeRegisterEventSource (const Extbyte * lpUNCServerName, const Extbyte * lpSourceName)
{
  if (XEUNICODE_P)
    return RegisterEventSourceW ((LPCWSTR) lpUNCServerName, (LPCWSTR) lpSourceName);
  else
    return RegisterEventSourceA ((LPCSTR) lpUNCServerName, (LPCSTR) lpSourceName);
}

HANDLE
qxeOpenBackupEventLog (const Extbyte * lpUNCServerName, const Extbyte * lpFileName)
{
  if (XEUNICODE_P)
    return OpenBackupEventLogW ((LPCWSTR) lpUNCServerName, (LPCWSTR) lpFileName);
  else
    return OpenBackupEventLogA ((LPCSTR) lpUNCServerName, (LPCSTR) lpFileName);
}

BOOL
qxeReadEventLog (HANDLE hEventLog, DWORD dwReadFlags, DWORD dwRecordOffset, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, DWORD      * pnBytesRead, DWORD      * pnMinNumberOfBytesNeeded)
{
  if (XEUNICODE_P)
    return ReadEventLogW (hEventLog, dwReadFlags, dwRecordOffset, lpBuffer, nNumberOfBytesToRead, pnBytesRead, pnMinNumberOfBytesNeeded);
  else
    return ReadEventLogA (hEventLog, dwReadFlags, dwRecordOffset, lpBuffer, nNumberOfBytesToRead, pnBytesRead, pnMinNumberOfBytesNeeded);
}

BOOL
qxeReportEvent (HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID, PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, const Extbyte *   * lpStrings, LPVOID lpRawData)
{
  if (XEUNICODE_P)
    return ReportEventW (hEventLog, wType, wCategory, dwEventID, lpUserSid, wNumStrings, dwDataSize, (LPCWSTR   *) lpStrings, lpRawData);
  else
    return ReportEventA (hEventLog, wType, wCategory, dwEventID, lpUserSid, wNumStrings, dwDataSize, (LPCSTR   *) lpStrings, lpRawData);
}

BOOL
qxeAccessCheckAndAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, Extbyte * ObjectTypeName, Extbyte * ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, DWORD DesiredAccess, PGENERIC_MAPPING GenericMapping, BOOL ObjectCreation, LPDWORD GrantedAccess, LPBOOL AccessStatus, LPBOOL pfGenerateOnClose)
{
  if (XEUNICODE_P)
    return AccessCheckAndAuditAlarmW ((LPCWSTR) SubsystemName, HandleId, (LPWSTR) ObjectTypeName, (LPWSTR) ObjectName, SecurityDescriptor, DesiredAccess, GenericMapping, ObjectCreation, GrantedAccess, AccessStatus, pfGenerateOnClose);
  else
    return AccessCheckAndAuditAlarmA ((LPCSTR) SubsystemName, HandleId, (LPSTR) ObjectTypeName, (LPSTR) ObjectName, SecurityDescriptor, DesiredAccess, GenericMapping, ObjectCreation, GrantedAccess, AccessStatus, pfGenerateOnClose);
}

/* Error if AccessCheckByTypeAndAuditAlarm used: NT 5.0+ only */

/* Error if AccessCheckByTypeResultListAndAuditAlarm used: NT 5.0+ only */

BOOL
qxeObjectOpenAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, Extbyte * ObjectTypeName, Extbyte * ObjectName, PSECURITY_DESCRIPTOR pSecurityDescriptor, HANDLE ClientToken, DWORD DesiredAccess, DWORD GrantedAccess, PPRIVILEGE_SET Privileges, BOOL ObjectCreation, BOOL AccessGranted, LPBOOL GenerateOnClose)
{
  if (XEUNICODE_P)
    return ObjectOpenAuditAlarmW ((LPCWSTR) SubsystemName, HandleId, (LPWSTR) ObjectTypeName, (LPWSTR) ObjectName, pSecurityDescriptor, ClientToken, DesiredAccess, GrantedAccess, Privileges, ObjectCreation, AccessGranted, GenerateOnClose);
  else
    return ObjectOpenAuditAlarmA ((LPCSTR) SubsystemName, HandleId, (LPSTR) ObjectTypeName, (LPSTR) ObjectName, pSecurityDescriptor, ClientToken, DesiredAccess, GrantedAccess, Privileges, ObjectCreation, AccessGranted, GenerateOnClose);
}

BOOL
qxeObjectPrivilegeAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, HANDLE ClientToken, DWORD DesiredAccess, PPRIVILEGE_SET Privileges, BOOL AccessGranted)
{
  if (XEUNICODE_P)
    return ObjectPrivilegeAuditAlarmW ((LPCWSTR) SubsystemName, HandleId, ClientToken, DesiredAccess, Privileges, AccessGranted);
  else
    return ObjectPrivilegeAuditAlarmA ((LPCSTR) SubsystemName, HandleId, ClientToken, DesiredAccess, Privileges, AccessGranted);
}

BOOL
qxeObjectCloseAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, BOOL GenerateOnClose)
{
  if (XEUNICODE_P)
    return ObjectCloseAuditAlarmW ((LPCWSTR) SubsystemName, HandleId, GenerateOnClose);
  else
    return ObjectCloseAuditAlarmA ((LPCSTR) SubsystemName, HandleId, GenerateOnClose);
}

BOOL
qxeObjectDeleteAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, BOOL GenerateOnClose)
{
  if (XEUNICODE_P)
    return ObjectDeleteAuditAlarmW ((LPCWSTR) SubsystemName, HandleId, GenerateOnClose);
  else
    return ObjectDeleteAuditAlarmA ((LPCSTR) SubsystemName, HandleId, GenerateOnClose);
}

BOOL
qxePrivilegedServiceAuditAlarm (const Extbyte * SubsystemName, const Extbyte * ServiceName, HANDLE ClientToken, PPRIVILEGE_SET Privileges, BOOL AccessGranted)
{
  if (XEUNICODE_P)
    return PrivilegedServiceAuditAlarmW ((LPCWSTR) SubsystemName, (LPCWSTR) ServiceName, ClientToken, Privileges, AccessGranted);
  else
    return PrivilegedServiceAuditAlarmA ((LPCSTR) SubsystemName, (LPCSTR) ServiceName, ClientToken, Privileges, AccessGranted);
}

BOOL
qxeSetFileSecurity (const Extbyte * lpFileName, SECURITY_INFORMATION SecurityInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor)
{
  if (XEUNICODE_P)
    return SetFileSecurityW ((LPCWSTR) lpFileName, SecurityInformation, pSecurityDescriptor);
  else
    return SetFileSecurityA ((LPCSTR) lpFileName, SecurityInformation, pSecurityDescriptor);
}

BOOL
qxeGetFileSecurity (const Extbyte * lpFileName, SECURITY_INFORMATION RequestedInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor, DWORD nLength, LPDWORD lpnLengthNeeded)
{
  if (XEUNICODE_P)
    return GetFileSecurityW ((LPCWSTR) lpFileName, RequestedInformation, pSecurityDescriptor, nLength, lpnLengthNeeded);
  else
    return GetFileSecurityA ((LPCSTR) lpFileName, RequestedInformation, pSecurityDescriptor, nLength, lpnLengthNeeded);
}

HANDLE
qxeFindFirstChangeNotification (const Extbyte * lpPathName, BOOL bWatchSubtree, DWORD dwNotifyFilter)
{
  if (XEUNICODE_P)
    return FindFirstChangeNotificationW ((LPCWSTR) lpPathName, bWatchSubtree, dwNotifyFilter);
  else
    return FindFirstChangeNotificationA ((LPCSTR) lpPathName, bWatchSubtree, dwNotifyFilter);
}

/* Error if ReadDirectoryChanges used: Unicode-only */

BOOL
qxeIsBadStringPtr (const Extbyte * lpsz, UINT ucchMax)
{
  if (XEUNICODE_P)
    return IsBadStringPtrW ((LPCWSTR) lpsz, ucchMax);
  else
    return IsBadStringPtrA ((LPCSTR) lpsz, ucchMax);
}

BOOL
qxeLookupAccountSid (const Extbyte * lpSystemName, PSID Sid, Extbyte * Name, LPDWORD cbName, Extbyte * ReferencedDomainName, LPDWORD cbReferencedDomainName, PSID_NAME_USE peUse)
{
  if (XEUNICODE_P)
    return LookupAccountSidW ((LPCWSTR) lpSystemName, Sid, (LPWSTR) Name, cbName, (LPWSTR) ReferencedDomainName, cbReferencedDomainName, peUse);
  else
    return LookupAccountSidA ((LPCSTR) lpSystemName, Sid, (LPSTR) Name, cbName, (LPSTR) ReferencedDomainName, cbReferencedDomainName, peUse);
}

BOOL
qxeLookupAccountName (const Extbyte * lpSystemName, const Extbyte * lpAccountName, PSID Sid, LPDWORD cbSid, Extbyte * ReferencedDomainName, LPDWORD cbReferencedDomainName, PSID_NAME_USE peUse)
{
  if (XEUNICODE_P)
    return LookupAccountNameW ((LPCWSTR) lpSystemName, (LPCWSTR) lpAccountName, Sid, cbSid, (LPWSTR) ReferencedDomainName, cbReferencedDomainName, peUse);
  else
    return LookupAccountNameA ((LPCSTR) lpSystemName, (LPCSTR) lpAccountName, Sid, cbSid, (LPSTR) ReferencedDomainName, cbReferencedDomainName, peUse);
}

BOOL
qxeLookupPrivilegeValue (const Extbyte * lpSystemName, const Extbyte * lpName, PLUID lpLuid)
{
  if (XEUNICODE_P)
    return LookupPrivilegeValueW ((LPCWSTR) lpSystemName, (LPCWSTR) lpName, lpLuid);
  else
    return LookupPrivilegeValueA ((LPCSTR) lpSystemName, (LPCSTR) lpName, lpLuid);
}

BOOL
qxeLookupPrivilegeName (const Extbyte * lpSystemName, PLUID lpLuid, Extbyte * lpName, LPDWORD cbName)
{
  if (XEUNICODE_P)
    return LookupPrivilegeNameW ((LPCWSTR) lpSystemName, lpLuid, (LPWSTR) lpName, cbName);
  else
    return LookupPrivilegeNameA ((LPCSTR) lpSystemName, lpLuid, (LPSTR) lpName, cbName);
}

BOOL
qxeLookupPrivilegeDisplayName (const Extbyte * lpSystemName, const Extbyte * lpName, Extbyte * lpDisplayName, LPDWORD cbDisplayName, LPDWORD lpLanguageId)
{
  if (XEUNICODE_P)
    return LookupPrivilegeDisplayNameW ((LPCWSTR) lpSystemName, (LPCWSTR) lpName, (LPWSTR) lpDisplayName, cbDisplayName, lpLanguageId);
  else
    return LookupPrivilegeDisplayNameA ((LPCSTR) lpSystemName, (LPCSTR) lpName, (LPSTR) lpDisplayName, cbDisplayName, lpLanguageId);
}

BOOL
qxeBuildCommDCB (const Extbyte * lpDef, LPDCB lpDCB)
{
  if (XEUNICODE_P)
    return BuildCommDCBW ((LPCWSTR) lpDef, lpDCB);
  else
    return BuildCommDCBA ((LPCSTR) lpDef, lpDCB);
}

BOOL
qxeBuildCommDCBAndTimeouts (const Extbyte * lpDef, LPDCB lpDCB, LPCOMMTIMEOUTS lpCommTimeouts)
{
  if (XEUNICODE_P)
    return BuildCommDCBAndTimeoutsW ((LPCWSTR) lpDef, lpDCB, lpCommTimeouts);
  else
    return BuildCommDCBAndTimeoutsA ((LPCSTR) lpDef, lpDCB, lpCommTimeouts);
}

BOOL
qxeCommConfigDialog (const Extbyte * lpszName, HWND hWnd, LPCOMMCONFIG lpCC)
{
  if (XEUNICODE_P)
    return CommConfigDialogW ((LPCWSTR) lpszName, hWnd, lpCC);
  else
    return CommConfigDialogA ((LPCSTR) lpszName, hWnd, lpCC);
}

BOOL
qxeGetDefaultCommConfig (const Extbyte * lpszName, LPCOMMCONFIG lpCC, LPDWORD lpdwSize)
{
  if (XEUNICODE_P)
    return GetDefaultCommConfigW ((LPCWSTR) lpszName, lpCC, lpdwSize);
  else
    return GetDefaultCommConfigA ((LPCSTR) lpszName, lpCC, lpdwSize);
}

BOOL
qxeSetDefaultCommConfig (const Extbyte * lpszName, LPCOMMCONFIG lpCC, DWORD dwSize)
{
  if (XEUNICODE_P)
    return SetDefaultCommConfigW ((LPCWSTR) lpszName, lpCC, dwSize);
  else
    return SetDefaultCommConfigA ((LPCSTR) lpszName, lpCC, dwSize);
}

BOOL
qxeGetComputerName (Extbyte * lpBuffer, LPDWORD nSize)
{
  if (XEUNICODE_P)
    return GetComputerNameW ((LPWSTR) lpBuffer, nSize);
  else
    return GetComputerNameA ((LPSTR) lpBuffer, nSize);
}

BOOL
qxeSetComputerName (const Extbyte * lpComputerName)
{
  if (XEUNICODE_P)
    return SetComputerNameW ((LPCWSTR) lpComputerName);
  else
    return SetComputerNameA ((LPCSTR) lpComputerName);
}

BOOL
qxeGetUserName (Extbyte * lpBuffer, LPDWORD nSize)
{
  if (XEUNICODE_P)
    return GetUserNameW ((LPWSTR) lpBuffer, nSize);
  else
    return GetUserNameA ((LPSTR) lpBuffer, nSize);
}

BOOL
qxeLogonUser (Extbyte * lpszUsername, Extbyte * lpszDomain, Extbyte * lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken)
{
  if (XEUNICODE_P)
    return LogonUserW ((LPWSTR) lpszUsername, (LPWSTR) lpszDomain, (LPWSTR) lpszPassword, dwLogonType, dwLogonProvider, phToken);
  else
    return LogonUserA ((LPSTR) lpszUsername, (LPSTR) lpszDomain, (LPSTR) lpszPassword, dwLogonType, dwLogonProvider, phToken);
}

BOOL
qxeCreateProcessAsUser (HANDLE hToken, const Extbyte * lpApplicationName, Extbyte * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, const Extbyte * lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation)
{
  if (XEUNICODE_P)
    return CreateProcessAsUserW (hToken, (LPCWSTR) lpApplicationName, (LPWSTR) lpCommandLine, lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags, lpEnvironment, (LPCWSTR) lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
  else
    return CreateProcessAsUserA (hToken, (LPCSTR) lpApplicationName, (LPSTR) lpCommandLine, lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags, lpEnvironment, (LPCSTR) lpCurrentDirectory, (LPSTARTUPINFOA) lpStartupInfo, lpProcessInformation);
}

/* Error if GetCurrentHwProfile used: split-sized LPHW_PROFILE_INFO; NT 4.0+ only */

/* Error if GetVersionEx used: split-sized LPOSVERSIONINFO */

/* Error if CreateJobObject used: NT 5.0+ only */

/* Error if OpenJobObject used: NT 5.0+ only */


/*----------------------------------------------------------------------*/
/*                      Processing file COMMDLG.H                       */
/*----------------------------------------------------------------------*/

BOOL  
qxeGetOpenFileName (LPOPENFILENAMEW arg1)
{
  if (XEUNICODE_P)
    return GetOpenFileNameW (arg1);
  else
    return GetOpenFileNameA ((LPOPENFILENAMEA) arg1);
}

BOOL  
qxeGetSaveFileName (LPOPENFILENAMEW arg1)
{
  if (XEUNICODE_P)
    return GetSaveFileNameW (arg1);
  else
    return GetSaveFileNameA ((LPOPENFILENAMEA) arg1);
}

short 
qxeGetFileTitle (const Extbyte * arg1, Extbyte * arg2, WORD arg3)
{
  if (XEUNICODE_P)
    return GetFileTitleW ((LPCWSTR) arg1, (LPWSTR) arg2, arg3);
  else
    return GetFileTitleA ((LPCSTR) arg1, (LPSTR) arg2, arg3);
}

BOOL  
qxeChooseColor (LPCHOOSECOLORW arg1)
{
  if (XEUNICODE_P)
    return ChooseColorW (arg1);
  else
    return ChooseColorA ((LPCHOOSECOLORA) arg1);
}

HWND  
qxeFindText (LPFINDREPLACEW arg1)
{
  if (XEUNICODE_P)
    return FindTextW (arg1);
  else
    return FindTextA ((LPFINDREPLACEA) arg1);
}

HWND  
qxeReplaceText (LPFINDREPLACEW arg1)
{
  if (XEUNICODE_P)
    return ReplaceTextW (arg1);
  else
    return ReplaceTextA ((LPFINDREPLACEA) arg1);
}

/* Error if AfxReplaceText used: mac only */

/* Error if ChooseFont used: split-sized LPLOGFONT in LPCHOOSEFONT */

/* Skipping PrintDlg because LPPRINTDLG with split-sized DEVMODE handle */

/* Skipping PageSetupDlg because LPPAGESETUPDLG with split-sized DEVMODE handle */


/*----------------------------------------------------------------------*/
/*                       Processing file SHLOBJ.H                       */
/*----------------------------------------------------------------------*/

BOOL
qxeSHGetPathFromIDList (LPCITEMIDLIST pidl, Extbyte * pszPath)
{
  if (XEUNICODE_P)
    return SHGetPathFromIDListW (pidl, (LPWSTR) pszPath);
  else
    return SHGetPathFromIDListA (pidl, (LPSTR) pszPath);
}

/* Skipping SHGetSpecialFolderPath because error in Cygwin prototype */

/* Skipping SHBrowseForFolder because need to intercept callback for SendMessage */

/* Skipping SHGetDataFromIDList because split-sized WIN32_FIND_DATA or split-simple NETRESOURCE */


/*----------------------------------------------------------------------*/
/*                        Processing file IME.H                         */
/*----------------------------------------------------------------------*/

/* Error if SendIMEMessageEx used: obsolete, no docs available */


/*----------------------------------------------------------------------*/
/*                       Processing file WINGDI.H                       */
/*----------------------------------------------------------------------*/

int
qxeAddFontResource (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return AddFontResourceW ((LPCWSTR) arg1);
  else
    return AddFontResourceA ((LPCSTR) arg1);
}

HMETAFILE
qxeCopyMetaFile (HMETAFILE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return CopyMetaFileW (arg1, (LPCWSTR) arg2);
  else
    return CopyMetaFileA (arg1, (LPCSTR) arg2);
}

/* Skipping CreateDC because split-sized DEVMODE */

/* Skipping CreateFontIndirect because split-sized LOGFONT */

HFONT
qxeCreateFont (int arg1, int arg2, int arg3, int arg4, int arg5, DWORD arg6, DWORD arg7, DWORD arg8, DWORD arg9, DWORD arg10, DWORD arg11, DWORD arg12, DWORD arg13, const Extbyte * arg14)
{
  if (XEUNICODE_P)
    return CreateFontW (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, (LPCWSTR) arg14);
  else
    return CreateFontA (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, (LPCSTR) arg14);
}

/* Skipping CreateIC because split-sized DEVMODE */

HDC
qxeCreateMetaFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return CreateMetaFileW ((LPCWSTR) arg1);
  else
    return CreateMetaFileA ((LPCSTR) arg1);
}

BOOL
qxeCreateScalableFontResource (DWORD arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return CreateScalableFontResourceW (arg1, (LPCWSTR) arg2, (LPCWSTR) arg3, (LPCWSTR) arg4);
  else
    return CreateScalableFontResourceA (arg1, (LPCSTR) arg2, (LPCSTR) arg3, (LPCSTR) arg4);
}

/* Skipping DeviceCapabilities because split-sized DEVMODE */

/* Skipping EnumFontFamiliesEx because split-complex FONTENUMPROC; NT 4.0+ only */

/* Error if EnumFontFamilies used: split-complex FONTENUMPROC */

/* Error if EnumFonts used: split-complex FONTENUMPROC */

BOOL
qxeGetCharWidth (HDC arg1, UINT arg2, UINT arg3, LPINT arg4)
{
  if (XEUNICODE_P)
    return GetCharWidthW (arg1, arg2, arg3, arg4);
  else
    return GetCharWidthA (arg1, arg2, arg3, arg4);
}

BOOL
qxeGetCharWidth32 (HDC arg1, UINT arg2, UINT arg3, LPINT arg4)
{
  if (XEUNICODE_P)
    return GetCharWidth32W (arg1, arg2, arg3, arg4);
  else
    return GetCharWidth32A (arg1, arg2, arg3, arg4);
}

BOOL  
qxeGetCharWidthFloat (HDC arg1, UINT arg2, UINT arg3, PFLOAT arg4)
{
  if (XEUNICODE_P)
    return GetCharWidthFloatW (arg1, arg2, arg3, arg4);
  else
    return GetCharWidthFloatA (arg1, arg2, arg3, arg4);
}

BOOL  
qxeGetCharABCWidths (HDC arg1, UINT arg2, UINT arg3, LPABC arg4)
{
  if (XEUNICODE_P)
    return GetCharABCWidthsW (arg1, arg2, arg3, arg4);
  else
    return GetCharABCWidthsA (arg1, arg2, arg3, arg4);
}

BOOL  
qxeGetCharABCWidthsFloat (HDC arg1, UINT arg2, UINT arg3, LPABCFLOAT arg4)
{
  if (XEUNICODE_P)
    return GetCharABCWidthsFloatW (arg1, arg2, arg3, arg4);
  else
    return GetCharABCWidthsFloatA (arg1, arg2, arg3, arg4);
}

DWORD
qxeGetGlyphOutline (HDC arg1, UINT arg2, UINT arg3, LPGLYPHMETRICS arg4, DWORD arg5, LPVOID arg6, CONST MAT2 * arg7)
{
  if (XEUNICODE_P)
    return GetGlyphOutlineW (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return GetGlyphOutlineA (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

HMETAFILE
qxeGetMetaFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetMetaFileW ((LPCWSTR) arg1);
  else
    return GetMetaFileA ((LPCSTR) arg1);
}

/* Error if GetOutlineTextMetrics used: split-sized LPOUTLINETEXTMETRIC */

BOOL  
qxeGetTextExtentPoint (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4)
{
  if (XEUNICODE_P)
    return GetTextExtentPointW (arg1, (LPCWSTR) arg2, arg3, arg4);
  else
    return GetTextExtentPointA (arg1, (LPCSTR) arg2, arg3, arg4);
}

BOOL  
qxeGetTextExtentPoint32 (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4)
{
  if (XEUNICODE_P)
    return GetTextExtentPoint32W (arg1, (LPCWSTR) arg2, arg3, arg4);
  else
    return GetTextExtentPoint32A (arg1, (LPCSTR) arg2, arg3, arg4);
}

BOOL  
qxeGetTextExtentExPoint (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPINT arg5, LPINT arg6, LPSIZE arg7)
{
  if (XEUNICODE_P)
    return GetTextExtentExPointW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6, arg7);
  else
    return GetTextExtentExPointA (arg1, (LPCSTR) arg2, arg3, arg4, arg5, arg6, arg7);
}

/* NOTE: NT 4.0+ only */
DWORD
qxeGetCharacterPlacement (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPGCP_RESULTSW arg5, DWORD arg6)
{
  if (XEUNICODE_P)
    return GetCharacterPlacementW (arg1, (LPCWSTR) arg2, arg3, arg4, arg5, arg6);
  else
    return GetCharacterPlacementA (arg1, (LPCSTR) arg2, arg3, arg4, (LPGCP_RESULTSA) arg5, arg6);
}

/* Error if GetGlyphIndices used: NT 5.0+ only */

/* Error if AddFontResourceEx used: NT 5.0+ only */

/* Error if RemoveFontResourceEx used: NT 5.0+ only */

/* Error if CreateFontIndirectEx used: split-sized ENUMLOGFONTEXDV; NT 5.0+ only */

/* Skipping ResetDC because split-sized DEVMODE */

BOOL
qxeRemoveFontResource (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return RemoveFontResourceW ((LPCWSTR) arg1);
  else
    return RemoveFontResourceA ((LPCSTR) arg1);
}

HENHMETAFILE
qxeCopyEnhMetaFile (HENHMETAFILE arg1, const Extbyte * arg2)
{
  if (XEUNICODE_P)
    return CopyEnhMetaFileW (arg1, (LPCWSTR) arg2);
  else
    return CopyEnhMetaFileA (arg1, (LPCSTR) arg2);
}

HDC
qxeCreateEnhMetaFile (HDC arg1, const Extbyte * arg2, CONST RECT * arg3, const Extbyte * arg4)
{
  if (XEUNICODE_P)
    return CreateEnhMetaFileW (arg1, (LPCWSTR) arg2, arg3, (LPCWSTR) arg4);
  else
    return CreateEnhMetaFileA (arg1, (LPCSTR) arg2, arg3, (LPCSTR) arg4);
}

HENHMETAFILE
qxeGetEnhMetaFile (const Extbyte * arg1)
{
  if (XEUNICODE_P)
    return GetEnhMetaFileW ((LPCWSTR) arg1);
  else
    return GetEnhMetaFileA ((LPCSTR) arg1);
}

UINT
qxeGetEnhMetaFileDescription (HENHMETAFILE arg1, UINT arg2, Extbyte * arg3)
{
  if (XEUNICODE_P)
    return GetEnhMetaFileDescriptionW (arg1, arg2, (LPWSTR) arg3);
  else
    return GetEnhMetaFileDescriptionA (arg1, arg2, (LPSTR) arg3);
}

/* Skipping GetTextMetrics because split-sized LPTEXTMETRIC */

int
qxeStartDoc (HDC arg1, CONST DOCINFOW * arg2)
{
  if (XEUNICODE_P)
    return StartDocW (arg1, arg2);
  else
    return StartDocA (arg1, (CONST DOCINFOA *) arg2);
}

/* Skipping GetObject because split-sized LOGFONT */

BOOL
qxeTextOut (HDC arg1, int arg2, int arg3, const Extbyte * arg4, int arg5)
{
  if (XEUNICODE_P)
    return TextOutW (arg1, arg2, arg3, (LPCWSTR) arg4, arg5);
  else
    return TextOutA (arg1, arg2, arg3, (LPCSTR) arg4, arg5);
}

BOOL
qxeExtTextOut (HDC arg1, int arg2, int arg3, UINT arg4, CONST RECT * arg5, const Extbyte * arg6, UINT arg7, CONST INT * arg8)
{
  if (XEUNICODE_P)
    return ExtTextOutW (arg1, arg2, arg3, arg4, arg5, (LPCWSTR) arg6, arg7, arg8);
  else
    return ExtTextOutA (arg1, arg2, arg3, arg4, arg5, (LPCSTR) arg6, arg7, arg8);
}

BOOL
qxePolyTextOut (HDC arg1, CONST POLYTEXTW * arg2, int arg3)
{
  if (XEUNICODE_P)
    return PolyTextOutW (arg1, arg2, arg3);
  else
    return PolyTextOutA (arg1, (CONST POLYTEXTA *) arg2, arg3);
}

int
qxeGetTextFace (HDC arg1, int arg2, Extbyte * arg3)
{
  if (XEUNICODE_P)
    return GetTextFaceW (arg1, arg2, (LPWSTR) arg3);
  else
    return GetTextFaceA (arg1, arg2, (LPSTR) arg3);
}

DWORD
qxeGetKerningPairs (HDC arg1, DWORD arg2, LPKERNINGPAIR arg3)
{
  if (XEUNICODE_P)
    return GetKerningPairsW (arg1, arg2, arg3);
  else
    return GetKerningPairsA (arg1, arg2, arg3);
}

/* Error if GetLogColorSpace used: split-sized LPLOGCOLORSPACE; NT 4.0+ only */

/* Error if CreateColorSpace used: split-sized LPLOGCOLORSPACE; NT 4.0+ only */

/* Skipping GetICMProfile because NT 4.0+ only, error in Cygwin prototype */

/* NOTE: NT 4.0+ only */
BOOL
qxeSetICMProfile (HDC arg1, Extbyte * arg2)
{
  if (XEUNICODE_P)
    return SetICMProfileW (arg1, (LPWSTR) arg2);
  else
    return SetICMProfileA (arg1, (LPSTR) arg2);
}

/* NOTE: NT 4.0+ only */
int
qxeEnumICMProfiles (HDC arg1, ICMENUMPROCW arg2, LPARAM arg3)
{
  if (XEUNICODE_P)
    return EnumICMProfilesW (arg1, arg2, arg3);
  else
    return EnumICMProfilesA (arg1, (ICMENUMPROCA) arg2, arg3);
}

/* Skipping UpdateICMRegKey because NT 4.0+ only, error in Cygwin prototype */

/* Error if wglUseFontBitmaps used: causes link error */

/* Error if wglUseFontOutlines used: causes link error */

