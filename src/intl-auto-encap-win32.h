/* Automatically-generated Unicode-encapsulation header file.
   Do not edit.  See `make-mswin-unicode.pl'.
*/


/* Processing file WINNLS.H */

#undef GetCPInfoEx
#define GetCPInfoEx error not used, not examined yet

#undef CompareString
#define CompareString error not used, not examined yet

#undef LCMapString
#define LCMapString error not used, not examined yet

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetLocaleInfo
#define GetLocaleInfo error use qxeGetLocaleInfo or GetLocaleInfoA/GetLocaleInfoW
#endif
int qxeGetLocaleInfo (LCID Locale, LCTYPE LCType, Extbyte * lpLCData, int cchData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetLocaleInfo
#define SetLocaleInfo error use qxeSetLocaleInfo or SetLocaleInfoA/SetLocaleInfoW
#endif
BOOL qxeSetLocaleInfo (LCID Locale, LCTYPE LCType, const Extbyte * lpLCData);

#undef GetTimeFormat
#define GetTimeFormat error not used, not examined yet

#undef GetDateFormat
#define GetDateFormat error not used, not examined yet

#undef GetNumberFormat
#define GetNumberFormat error not used, not examined yet

#undef GetCurrencyFormat
#define GetCurrencyFormat error not used, not examined yet

#undef EnumCalendarInfo
#define EnumCalendarInfo error not used, not examined yet

#undef EnumCalendarInfoEx
#define EnumCalendarInfoEx error not used, not examined yet

#undef EnumTimeFormats
#define EnumTimeFormats error not used, not examined yet

#undef EnumDateFormats
#define EnumDateFormats error not used, not examined yet

#undef EnumDateFormatsEx
#define EnumDateFormatsEx error not used, not examined yet

#undef GetStringTypeEx
#define GetStringTypeEx error not used, not examined yet

#undef GetStringType
#define GetStringType error no such fun; A and W versions have different nos. of args

#undef FoldString
#define FoldString error not used, not examined yet

#undef EnumSystemLocales
#define EnumSystemLocales error not used, not examined yet

#undef EnumSystemCodePages
#define EnumSystemCodePages error not used, not examined yet


/* Processing file WINSPOOL.H */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumPrinters
#define EnumPrinters error use qxeEnumPrinters or EnumPrintersA/EnumPrintersW
#endif
BOOL qxeEnumPrinters (DWORD Flags, Extbyte * Name, DWORD Level, LPBYTE pPrinterEnum, DWORD cbBuf, LPDWORD pcbNeeded, LPDWORD pcReturned);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
/* Skipping OpenPrinter because split-sized DEVMODE pointer in split PRINTER_DEFAULTS */
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ResetPrinter
#define ResetPrinter error split-sized DEVMODE pointer in split PRINTER_DEFAULTS
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetJob
#define SetJob error split-sized DEVMODE pointer in split JOB_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetJob
#define GetJob error split-sized DEVMODE pointer in split JOB_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumJobs
#define EnumJobs error split-sized DEVMODE pointer in split JOB_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinter
#define AddPrinter error split-sized DEVMODE pointer in split PRINTER_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinter
#define SetPrinter error split-sized DEVMODE pointer in split PRINTER_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinter
#define GetPrinter error split-sized DEVMODE pointer in split PRINTER_INFO_2
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinterDriver
#define AddPrinterDriver error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinterDriverEx
#define AddPrinterDriverEx error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterDrivers
#define EnumPrinterDrivers error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterDriver
#define GetPrinterDriver error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterDriverDirectory
#define GetPrinterDriverDirectory error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterDriver
#define DeletePrinterDriver error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterDriverEx
#define DeletePrinterDriverEx error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPerMachineConnection
#define AddPerMachineConnection error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePerMachineConnection
#define DeletePerMachineConnection error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPerMachineConnections
#define EnumPerMachineConnections error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrintProcessor
#define AddPrintProcessor error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrintProcessors
#define EnumPrintProcessors error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrintProcessorDirectory
#define GetPrintProcessorDirectory error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrintProcessorDatatypes
#define EnumPrintProcessorDatatypes error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrintProcessor
#define DeletePrintProcessor error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef StartDocPrinter
#define StartDocPrinter error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddJob
#define AddJob error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
/* Skipping DocumentProperties because split-sized DEVMODE, error in Cygwin prototype */
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AdvancedDocumentProperties
#define AdvancedDocumentProperties error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterData
#define GetPrinterData error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterDataEx
#define GetPrinterDataEx error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterData
#define EnumPrinterData error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterDataEx
#define EnumPrinterDataEx error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPrinterKey
#define EnumPrinterKey error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinterData
#define SetPrinterData error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinterDataEx
#define SetPrinterDataEx error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterData
#define DeletePrinterData error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterDataEx
#define DeletePrinterDataEx error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterKey
#define DeletePrinterKey error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef PrinterMessageBox
#define PrinterMessageBox error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddForm
#define AddForm error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeleteForm
#define DeleteForm error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetForm
#define GetForm error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetForm
#define SetForm error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumForms
#define EnumForms error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumMonitors
#define EnumMonitors error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddMonitor
#define AddMonitor error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeleteMonitor
#define DeleteMonitor error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef EnumPorts
#define EnumPorts error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPort
#define AddPort error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ConfigurePort
#define ConfigurePort error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePort
#define DeletePort error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef XcvData
#define XcvData error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPort
#define SetPort error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrinterConnection
#define AddPrinterConnection error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrinterConnection
#define DeletePrinterConnection error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef AddPrintProvidor
#define AddPrintProvidor error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef DeletePrintProvidor
#define DeletePrintProvidor error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef SetPrinterHTMLView
#define SetPrinterHTMLView error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef GetPrinterHTMLView
#define GetPrinterHTMLView error not used, complicated interface with split structures
#endif /* defined (HAVE_MS_WINDOWS) */


/* Processing file WINNETWK.H */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetAddConnection
#define WNetAddConnection error use qxeWNetAddConnection or WNetAddConnectionA/WNetAddConnectionW
#endif
DWORD  qxeWNetAddConnection (const Extbyte * lpRemoteName, const Extbyte * lpPassword, const Extbyte * lpLocalName);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetAddConnection2
#define WNetAddConnection2 error use qxeWNetAddConnection2 or WNetAddConnection2A/WNetAddConnection2W
#endif
DWORD  qxeWNetAddConnection2 (LPNETRESOURCEW lpNetResource, const Extbyte * lpPassword, const Extbyte * lpUserName, DWORD dwFlags);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetAddConnection3
#define WNetAddConnection3 error use qxeWNetAddConnection3 or WNetAddConnection3A/WNetAddConnection3W
#endif
DWORD  qxeWNetAddConnection3 (HWND hwndOwner, LPNETRESOURCEW lpNetResource, const Extbyte * lpPassword, const Extbyte * lpUserName, DWORD dwFlags);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetCancelConnection
#define WNetCancelConnection error use qxeWNetCancelConnection or WNetCancelConnectionA/WNetCancelConnectionW
#endif
DWORD  qxeWNetCancelConnection (const Extbyte * lpName, BOOL fForce);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetCancelConnection2
#define WNetCancelConnection2 error use qxeWNetCancelConnection2 or WNetCancelConnection2A/WNetCancelConnection2W
#endif
DWORD  qxeWNetCancelConnection2 (const Extbyte * lpName, DWORD dwFlags, BOOL fForce);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetConnection
#define WNetGetConnection error use qxeWNetGetConnection or WNetGetConnectionA/WNetGetConnectionW
#endif
DWORD  qxeWNetGetConnection (const Extbyte * lpLocalName, Extbyte * lpRemoteName, LPDWORD lpnLength);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetUseConnection
#define WNetUseConnection error use qxeWNetUseConnection or WNetUseConnectionA/WNetUseConnectionW
#endif
DWORD  qxeWNetUseConnection (HWND hwndOwner, LPNETRESOURCEW lpNetResource, const Extbyte * lpUserID, const Extbyte * lpPassword, DWORD dwFlags, Extbyte * lpAccessName, LPDWORD lpBufferSize, LPDWORD lpResult);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetConnectionDialog1
#define WNetConnectionDialog1 error use qxeWNetConnectionDialog1 or WNetConnectionDialog1A/WNetConnectionDialog1W
#endif
DWORD  qxeWNetConnectionDialog1 (LPCONNECTDLGSTRUCTW lpConnDlgStruct);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetDisconnectDialog1
#define WNetDisconnectDialog1 error use qxeWNetDisconnectDialog1 or WNetDisconnectDialog1A/WNetDisconnectDialog1W
#endif
DWORD  qxeWNetDisconnectDialog1 (LPDISCDLGSTRUCTW lpConnDlgStruct);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetOpenEnum
#define WNetOpenEnum error use qxeWNetOpenEnum or WNetOpenEnumA/WNetOpenEnumW
#endif
DWORD  qxeWNetOpenEnum (DWORD dwScope, DWORD dwType, DWORD dwUsage, LPNETRESOURCEW lpNetResource, LPHANDLE lphEnum);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetEnumResource
#define WNetEnumResource error use qxeWNetEnumResource or WNetEnumResourceA/WNetEnumResourceW
#endif
DWORD  qxeWNetEnumResource (HANDLE hEnum, LPDWORD lpcCount, LPVOID lpBuffer, LPDWORD lpBufferSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetUniversalName
#define WNetGetUniversalName error use qxeWNetGetUniversalName or WNetGetUniversalNameA/WNetGetUniversalNameW
#endif
DWORD  qxeWNetGetUniversalName (const Extbyte * lpLocalPath, DWORD dwInfoLevel, LPVOID lpBuffer, LPDWORD lpBufferSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetUser
#define WNetGetUser error use qxeWNetGetUser or WNetGetUserA/WNetGetUserW
#endif
DWORD  qxeWNetGetUser (const Extbyte * lpName, Extbyte * lpUserName, LPDWORD lpnLength);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetProviderName
#define WNetGetProviderName error use qxeWNetGetProviderName or WNetGetProviderNameA/WNetGetProviderNameW
#endif
DWORD  qxeWNetGetProviderName (DWORD dwNetType, Extbyte * lpProviderName, LPDWORD lpBufferSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetNetworkInformation
#define WNetGetNetworkInformation error use qxeWNetGetNetworkInformation or WNetGetNetworkInformationA/WNetGetNetworkInformationW
#endif
DWORD  qxeWNetGetNetworkInformation (const Extbyte * lpProvider, LPNETINFOSTRUCT lpNetInfoStruct);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WNetGetLastError
#define WNetGetLastError error use qxeWNetGetLastError or WNetGetLastErrorA/WNetGetLastErrorW
#endif
DWORD  qxeWNetGetLastError (LPDWORD lpError, Extbyte * lpErrorBuf, DWORD nErrorBufSize, Extbyte * lpNameBuf, DWORD nNameBufSize);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MultinetGetConnectionPerformance
#define MultinetGetConnectionPerformance error use qxeMultinetGetConnectionPerformance or MultinetGetConnectionPerformanceA/MultinetGetConnectionPerformanceW
#endif
DWORD  qxeMultinetGetConnectionPerformance (LPNETRESOURCEW lpNetResource, LPNETCONNECTINFOSTRUCT lpNetConnectInfoStruct);
#endif /* defined (HAVE_MS_WINDOWS) */


/* Processing file WINREG.H */

/* Skipping RegConnectRegistry because error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegCreateKey
#define RegCreateKey error use qxeRegCreateKey or RegCreateKeyA/RegCreateKeyW
#endif
LONG
 qxeRegCreateKey (HKEY hKey, const Extbyte * lpSubKey, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegCreateKeyEx
#define RegCreateKeyEx error use qxeRegCreateKeyEx or RegCreateKeyExA/RegCreateKeyExW
#endif
LONG
 qxeRegCreateKeyEx (HKEY hKey, const Extbyte * lpSubKey, DWORD Reserved, Extbyte * lpClass, DWORD dwOptions, REGSAM samDesired, LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult, LPDWORD lpdwDisposition);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegDeleteKey
#define RegDeleteKey error use qxeRegDeleteKey or RegDeleteKeyA/RegDeleteKeyW
#endif
LONG
 qxeRegDeleteKey (HKEY hKey, const Extbyte * lpSubKey);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegDeleteValue
#define RegDeleteValue error use qxeRegDeleteValue or RegDeleteValueA/RegDeleteValueW
#endif
LONG
 qxeRegDeleteValue (HKEY hKey, const Extbyte * lpValueName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegEnumKey
#define RegEnumKey error use qxeRegEnumKey or RegEnumKeyA/RegEnumKeyW
#endif
LONG
 qxeRegEnumKey (HKEY hKey, DWORD dwIndex, Extbyte * lpName, DWORD cbName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegEnumKeyEx
#define RegEnumKeyEx error use qxeRegEnumKeyEx or RegEnumKeyExA/RegEnumKeyExW
#endif
LONG
 qxeRegEnumKeyEx (HKEY hKey, DWORD dwIndex, Extbyte * lpName, LPDWORD lpcbName, LPDWORD lpReserved, Extbyte * lpClass, LPDWORD lpcbClass, PFILETIME lpftLastWriteTime);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegEnumValue
#define RegEnumValue error use qxeRegEnumValue or RegEnumValueA/RegEnumValueW
#endif
LONG
 qxeRegEnumValue (HKEY hKey, DWORD dwIndex, Extbyte * lpValueName, LPDWORD lpcbValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegLoadKey
#define RegLoadKey error use qxeRegLoadKey or RegLoadKeyA/RegLoadKeyW
#endif
LONG
 qxeRegLoadKey (HKEY hKey, const Extbyte * lpSubKey, const Extbyte * lpFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegOpenKey
#define RegOpenKey error use qxeRegOpenKey or RegOpenKeyA/RegOpenKeyW
#endif
LONG
 qxeRegOpenKey (HKEY hKey, const Extbyte * lpSubKey, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegOpenKeyEx
#define RegOpenKeyEx error use qxeRegOpenKeyEx or RegOpenKeyExA/RegOpenKeyExW
#endif
LONG
 qxeRegOpenKeyEx (HKEY hKey, const Extbyte * lpSubKey, DWORD ulOptions, REGSAM samDesired, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryInfoKey
#define RegQueryInfoKey error use qxeRegQueryInfoKey or RegQueryInfoKeyA/RegQueryInfoKeyW
#endif
LONG
 qxeRegQueryInfoKey (HKEY hKey, Extbyte * lpClass, LPDWORD lpcbClass, LPDWORD lpReserved, LPDWORD lpcSubKeys, LPDWORD lpcbMaxSubKeyLen, LPDWORD lpcbMaxClassLen, LPDWORD lpcValues, LPDWORD lpcbMaxValueNameLen, LPDWORD lpcbMaxValueLen, LPDWORD lpcbSecurityDescriptor, PFILETIME lpftLastWriteTime);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryValue
#define RegQueryValue error use qxeRegQueryValue or RegQueryValueA/RegQueryValueW
#endif
LONG
 qxeRegQueryValue (HKEY hKey, const Extbyte * lpSubKey, Extbyte * lpValue, PLONG lpcbValue);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryMultipleValues
#define RegQueryMultipleValues error use qxeRegQueryMultipleValues or RegQueryMultipleValuesA/RegQueryMultipleValuesW
#endif
LONG
 qxeRegQueryMultipleValues (HKEY hKey, PVALENTW val_list, DWORD num_vals, Extbyte * lpValueBuf, LPDWORD ldwTotsize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegQueryValueEx
#define RegQueryValueEx error use qxeRegQueryValueEx or RegQueryValueExA/RegQueryValueExW
#endif
LONG
 qxeRegQueryValueEx (HKEY hKey, const Extbyte * lpValueName, LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegReplaceKey
#define RegReplaceKey error use qxeRegReplaceKey or RegReplaceKeyA/RegReplaceKeyW
#endif
LONG
 qxeRegReplaceKey (HKEY hKey, const Extbyte * lpSubKey, const Extbyte * lpNewFile, const Extbyte * lpOldFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegRestoreKey
#define RegRestoreKey error use qxeRegRestoreKey or RegRestoreKeyA/RegRestoreKeyW
#endif
LONG
 qxeRegRestoreKey (HKEY hKey, const Extbyte * lpFile, DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegSaveKey
#define RegSaveKey error use qxeRegSaveKey or RegSaveKeyA/RegSaveKeyW
#endif
LONG
 qxeRegSaveKey (HKEY hKey, const Extbyte * lpFile, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegSetValue
#define RegSetValue error use qxeRegSetValue or RegSetValueA/RegSetValueW
#endif
LONG
 qxeRegSetValue (HKEY hKey, const Extbyte * lpSubKey, DWORD dwType, const Extbyte * lpData, DWORD cbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegSetValueEx
#define RegSetValueEx error use qxeRegSetValueEx or RegSetValueExA/RegSetValueExW
#endif
LONG
 qxeRegSetValueEx (HKEY hKey, const Extbyte * lpValueName, DWORD Reserved, DWORD dwType, CONST BYTE* lpData, DWORD cbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegUnLoadKey
#define RegUnLoadKey error use qxeRegUnLoadKey or RegUnLoadKeyA/RegUnLoadKeyW
#endif
LONG
 qxeRegUnLoadKey (HKEY hKey, const Extbyte * lpSubKey);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef InitiateSystemShutdown
#define InitiateSystemShutdown error use qxeInitiateSystemShutdown or InitiateSystemShutdownA/InitiateSystemShutdownW
#endif
BOOL
 qxeInitiateSystemShutdown (Extbyte * lpMachineName, Extbyte * lpMessage, DWORD dwTimeout, BOOL bForceAppsClosed, BOOL bRebootAfterShutdown);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AbortSystemShutdown
#define AbortSystemShutdown error use qxeAbortSystemShutdown or AbortSystemShutdownA/AbortSystemShutdownW
#endif
BOOL
 qxeAbortSystemShutdown (Extbyte * lpMachineName);


/* Processing file SHELLAPI.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DragQueryFile
#define DragQueryFile error use qxeDragQueryFile or DragQueryFileA/DragQueryFileW
#endif
UINT  qxeDragQueryFile (HDROP arg1, UINT arg2, Extbyte * arg3, UINT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ShellExecute
#define ShellExecute error use qxeShellExecute or ShellExecuteA/ShellExecuteW
#endif
HINSTANCE  qxeShellExecute (HWND hwnd, const Extbyte * lpOperation, const Extbyte * lpFile, const Extbyte * lpParameters, const Extbyte * lpDirectory, INT nShowCmd);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindExecutable
#define FindExecutable error use qxeFindExecutable or FindExecutableA/FindExecutableW
#endif
HINSTANCE  qxeFindExecutable (const Extbyte * lpFile, const Extbyte * lpDirectory, Extbyte * lpResult);

#undef CommandLineToArgv
#define CommandLineToArgv error Unicode-only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ShellAbout
#define ShellAbout error use qxeShellAbout or ShellAboutA/ShellAboutW
#endif
INT        qxeShellAbout (HWND hWnd, const Extbyte * szApp, const Extbyte * szOtherStuff, HICON hIcon);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtractAssociatedIcon
#define ExtractAssociatedIcon error use qxeExtractAssociatedIcon or ExtractAssociatedIconA/ExtractAssociatedIconW
#endif
HICON      qxeExtractAssociatedIcon (HINSTANCE hInst, Extbyte * lpIconPath, LPWORD lpiIcon);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtractIcon
#define ExtractIcon error use qxeExtractIcon or ExtractIconA/ExtractIconW
#endif
HICON      qxeExtractIcon (HINSTANCE hInst, const Extbyte * lpszExeFileName, UINT nIconIndex);

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DoEnvironmentSubst
#define DoEnvironmentSubst error use qxeDoEnvironmentSubst or DoEnvironmentSubstA/DoEnvironmentSubstW
#endif
DWORD    qxeDoEnvironmentSubst (Extbyte * szString, UINT cbString);
#endif /* !defined (CYGWIN_HEADERS) */

#undef FindEnvironmentString
#define FindEnvironmentString error causes link error; NT 4.0+ only

/* Skipping ExtractIconEx because NT 4.0+ only, error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHFileOperation
#define SHFileOperation error use qxeSHFileOperation or SHFileOperationA/SHFileOperationW
#endif
int qxeSHFileOperation (LPSHFILEOPSTRUCTW lpFileOp);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ShellExecuteEx
#define ShellExecuteEx error use qxeShellExecuteEx or ShellExecuteExA/ShellExecuteExW
#endif
BOOL qxeShellExecuteEx (LPSHELLEXECUTEINFOW lpExecInfo);

#undef WinExecError
#define WinExecError error causes link error; NT 4.0+ only

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHQueryRecycleBin
#define SHQueryRecycleBin error use qxeSHQueryRecycleBin or SHQueryRecycleBinA/SHQueryRecycleBinW
#endif
HRESULT qxeSHQueryRecycleBin (const Extbyte * pszRootPath, LPSHQUERYRBINFO pSHQueryRBInfo);
#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHEmptyRecycleBin
#define SHEmptyRecycleBin error use qxeSHEmptyRecycleBin or SHEmptyRecycleBinA/SHEmptyRecycleBinW
#endif
HRESULT qxeSHEmptyRecycleBin (HWND hwnd, const Extbyte * pszRootPath, DWORD dwFlags);
#endif /* !defined (CYGWIN_HEADERS) */

#undef Shell_NotifyIcon
#define Shell_NotifyIcon error split-sized NOTIFYICONDATA, NT 4.0+ only

/* Skipping SHGetFileInfo because split-sized SHFILEINFO, NT 4.0+ only */

#undef SHGetDiskFreeSpace
#define SHGetDiskFreeSpace error causes link error; NT 4.0+ only

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHGetNewLinkInfo
#define SHGetNewLinkInfo error use qxeSHGetNewLinkInfo or SHGetNewLinkInfoA/SHGetNewLinkInfoW
#endif
BOOL qxeSHGetNewLinkInfo (const Extbyte * pszLinkTo, const Extbyte * pszDir, Extbyte * pszName, BOOL * pfMustCopy, UINT uFlags);
#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHInvokePrinterCommand
#define SHInvokePrinterCommand error use qxeSHInvokePrinterCommand or SHInvokePrinterCommandA/SHInvokePrinterCommandW
#endif
BOOL qxeSHInvokePrinterCommand (HWND hwnd, UINT uAction, const Extbyte * lpBuf1, const Extbyte * lpBuf2, BOOL fModal);
#endif /* !defined (CYGWIN_HEADERS) */


/* Processing file WINCON.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PeekConsoleInput
#define PeekConsoleInput error use qxePeekConsoleInput or PeekConsoleInputA/PeekConsoleInputW
#endif
BOOL qxePeekConsoleInput (HANDLE hConsoleInput, PINPUT_RECORD lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsRead);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsoleInput
#define ReadConsoleInput error use qxeReadConsoleInput or ReadConsoleInputA/ReadConsoleInputW
#endif
BOOL qxeReadConsoleInput (HANDLE hConsoleInput, PINPUT_RECORD lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsRead);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsoleInput
#define WriteConsoleInput error use qxeWriteConsoleInput or WriteConsoleInputA/WriteConsoleInputW
#endif
BOOL qxeWriteConsoleInput (HANDLE hConsoleInput, CONST INPUT_RECORD * lpBuffer, DWORD nLength, LPDWORD lpNumberOfEventsWritten);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsoleOutput
#define ReadConsoleOutput error use qxeReadConsoleOutput or ReadConsoleOutputA/ReadConsoleOutputW
#endif
BOOL qxeReadConsoleOutput (HANDLE hConsoleOutput, PCHAR_INFO lpBuffer, COORD dwBufferSize, COORD dwBufferCoord, PSMALL_RECT lpReadRegion);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsoleOutput
#define WriteConsoleOutput error use qxeWriteConsoleOutput or WriteConsoleOutputA/WriteConsoleOutputW
#endif
BOOL qxeWriteConsoleOutput (HANDLE hConsoleOutput, CONST CHAR_INFO * lpBuffer, COORD dwBufferSize, COORD dwBufferCoord, PSMALL_RECT lpWriteRegion);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsoleOutputCharacter
#define ReadConsoleOutputCharacter error use qxeReadConsoleOutputCharacter or ReadConsoleOutputCharacterA/ReadConsoleOutputCharacterW
#endif
BOOL qxeReadConsoleOutputCharacter (HANDLE hConsoleOutput, Extbyte * lpCharacter, DWORD nLength, COORD dwReadCoord, LPDWORD lpNumberOfCharsRead);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsoleOutputCharacter
#define WriteConsoleOutputCharacter error use qxeWriteConsoleOutputCharacter or WriteConsoleOutputCharacterA/WriteConsoleOutputCharacterW
#endif
BOOL qxeWriteConsoleOutputCharacter (HANDLE hConsoleOutput, const Extbyte * lpCharacter, DWORD nLength, COORD dwWriteCoord, LPDWORD lpNumberOfCharsWritten);

#undef FillConsoleOutputCharacter
#define FillConsoleOutputCharacter error split CHAR

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ScrollConsoleScreenBuffer
#define ScrollConsoleScreenBuffer error use qxeScrollConsoleScreenBuffer or ScrollConsoleScreenBufferA/ScrollConsoleScreenBufferW
#endif
BOOL qxeScrollConsoleScreenBuffer (HANDLE hConsoleOutput, CONST SMALL_RECT * lpScrollRectangle, CONST SMALL_RECT * lpClipRectangle, COORD dwDestinationOrigin, CONST CHAR_INFO * lpFill);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetConsoleTitle
#define GetConsoleTitle error use qxeGetConsoleTitle or GetConsoleTitleA/GetConsoleTitleW
#endif
DWORD qxeGetConsoleTitle (Extbyte * lpConsoleTitle, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetConsoleTitle
#define SetConsoleTitle error use qxeSetConsoleTitle or SetConsoleTitleA/SetConsoleTitleW
#endif
BOOL qxeSetConsoleTitle (const Extbyte * lpConsoleTitle);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadConsole
#define ReadConsole error use qxeReadConsole or ReadConsoleA/ReadConsoleW
#endif
BOOL qxeReadConsole (HANDLE hConsoleInput, LPVOID lpBuffer, DWORD nNumberOfCharsToRead, LPDWORD lpNumberOfCharsRead, LPVOID lpReserved);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteConsole
#define WriteConsole error use qxeWriteConsole or WriteConsoleA/WriteConsoleW
#endif
BOOL qxeWriteConsole (HANDLE hConsoleOutput, CONST VOID * lpBuffer, DWORD nNumberOfCharsToWrite, LPDWORD lpNumberOfCharsWritten, LPVOID lpReserved);


/* Processing file IMM.H */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmInstallIME
#define ImmInstallIME error use qxeImmInstallIME or ImmInstallIMEA/ImmInstallIMEW
#endif
HKL qxeImmInstallIME (const Extbyte * lpszIMEFileName, const Extbyte * lpszLayoutText);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetDescription
#define ImmGetDescription error use qxeImmGetDescription or ImmGetDescriptionA/ImmGetDescriptionW
#endif
UINT qxeImmGetDescription (HKL arg1, Extbyte * arg2, UINT uBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetIMEFileName
#define ImmGetIMEFileName error use qxeImmGetIMEFileName or ImmGetIMEFileNameA/ImmGetIMEFileNameW
#endif
UINT qxeImmGetIMEFileName (HKL arg1, Extbyte * arg2, UINT uBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCompositionString
#define ImmGetCompositionString error use qxeImmGetCompositionString or ImmGetCompositionStringA/ImmGetCompositionStringW
#endif
LONG qxeImmGetCompositionString (HIMC arg1, DWORD arg2, LPVOID arg3, DWORD arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmSetCompositionString
#define ImmSetCompositionString error use qxeImmSetCompositionString or ImmSetCompositionStringA/ImmSetCompositionStringW
#endif
BOOL qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPCVOID lpComp, DWORD arg4, LPCVOID lpRead, DWORD arg6);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCandidateListCount
#define ImmGetCandidateListCount error use qxeImmGetCandidateListCount or ImmGetCandidateListCountA/ImmGetCandidateListCountW
#endif
DWORD qxeImmGetCandidateListCount (HIMC arg1, LPDWORD lpdwListCount);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCandidateList
#define ImmGetCandidateList error use qxeImmGetCandidateList or ImmGetCandidateListA/ImmGetCandidateListW
#endif
DWORD qxeImmGetCandidateList (HIMC arg1, DWORD deIndex, LPCANDIDATELIST arg3, DWORD dwBufLen);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetGuideLine
#define ImmGetGuideLine error use qxeImmGetGuideLine or ImmGetGuideLineA/ImmGetGuideLineW
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
#define ImmConfigureIME error use qxeImmConfigureIME or ImmConfigureIMEA/ImmConfigureIMEW
#endif
BOOL qxeImmConfigureIME (HKL arg1, HWND arg2, DWORD arg3, LPVOID arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmEscape
#define ImmEscape error use qxeImmEscape or ImmEscapeA/ImmEscapeW
#endif
LRESULT qxeImmEscape (HKL arg1, HIMC arg2, UINT arg3, LPVOID arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetConversionList
#define ImmGetConversionList error use qxeImmGetConversionList or ImmGetConversionListA/ImmGetConversionListW
#endif
DWORD qxeImmGetConversionList (HKL arg1, HIMC arg2, const Extbyte * arg3, LPCANDIDATELIST arg4, DWORD dwBufLen, UINT uFlag);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmIsUIMessage
#define ImmIsUIMessage error use qxeImmIsUIMessage or ImmIsUIMessageA/ImmIsUIMessageW
#endif
BOOL qxeImmIsUIMessage (HWND arg1, UINT arg2, WPARAM arg3, LPARAM arg4);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmRegisterWord
#define ImmRegisterWord error use qxeImmRegisterWord or ImmRegisterWordA/ImmRegisterWordW
#endif
BOOL qxeImmRegisterWord (HKL arg1, const Extbyte * lpszReading, DWORD arg3, const Extbyte * lpszRegister);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmUnregisterWord
#define ImmUnregisterWord error use qxeImmUnregisterWord or ImmUnregisterWordA/ImmUnregisterWordW
#endif
BOOL qxeImmUnregisterWord (HKL arg1, const Extbyte * lpszReading, DWORD arg3, const Extbyte * lpszUnregister);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ImmGetRegisterWordStyle
#define ImmGetRegisterWordStyle error split-sized STYLEBUF
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmEnumRegisterWord
#define ImmEnumRegisterWord error use qxeImmEnumRegisterWord or ImmEnumRegisterWordA/ImmEnumRegisterWordW
#endif
UINT qxeImmEnumRegisterWord (HKL arg1, REGISTERWORDENUMPROCW arg2, const Extbyte * lpszReading, DWORD arg4, const Extbyte * lpszRegister, LPVOID arg6);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#undef ImmGetImeMenuItems
#define ImmGetImeMenuItems error split-sized IMEMENUITEMINFO
#endif /* defined (HAVE_MS_WINDOWS) */


/* Processing file DDEML.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DdeInitialize
#define DdeInitialize error use qxeDdeInitialize or DdeInitializeA/DdeInitializeW
#endif
UINT qxeDdeInitialize (LPDWORD pidInst, PFNCALLBACK pfnCallback, DWORD afCmd, DWORD ulRes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DdeCreateStringHandle
#define DdeCreateStringHandle error use qxeDdeCreateStringHandle or DdeCreateStringHandleA/DdeCreateStringHandleW
#endif
HSZ qxeDdeCreateStringHandle (DWORD idInst, const Extbyte * psz, int iCodePage);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DdeQueryString
#define DdeQueryString error use qxeDdeQueryString or DdeQueryStringA/DdeQueryStringW
#endif
DWORD qxeDdeQueryString (DWORD idInst, HSZ hsz, Extbyte * psz, DWORD cchMax, int iCodePage);


/* Processing file WINUSER.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef wvsprintf
#define wvsprintf error use qxewvsprintf or wvsprintfA/wvsprintfW
#endif
int qxewvsprintf (Extbyte * arg1, const Extbyte * arg2, va_list arglist);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadKeyboardLayout
#define LoadKeyboardLayout error use qxeLoadKeyboardLayout or LoadKeyboardLayoutA/LoadKeyboardLayoutW
#endif
HKL qxeLoadKeyboardLayout (const Extbyte * pwszKLID, UINT Flags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetKeyboardLayoutName
#define GetKeyboardLayoutName error use qxeGetKeyboardLayoutName or GetKeyboardLayoutNameA/GetKeyboardLayoutNameW
#endif
BOOL qxeGetKeyboardLayoutName (Extbyte * pwszKLID);

#undef CreateDesktop
#define CreateDesktop error split-sized LPDEVMODE

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenDesktop
#define OpenDesktop error use qxeOpenDesktop or OpenDesktopA/OpenDesktopW
#endif
HDESK qxeOpenDesktop (Extbyte * lpszDesktop, DWORD dwFlags, BOOL fInherit, ACCESS_MASK dwDesiredAccess);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumDesktops
#define EnumDesktops error use qxeEnumDesktops or EnumDesktopsA/EnumDesktopsW
#endif
BOOL qxeEnumDesktops (HWINSTA hwinsta, DESKTOPENUMPROCW lpEnumFunc, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWindowStation
#define CreateWindowStation error use qxeCreateWindowStation or CreateWindowStationA/CreateWindowStationW
#endif
HWINSTA qxeCreateWindowStation (Extbyte * lpwinsta, DWORD dwReserved, ACCESS_MASK dwDesiredAccess, LPSECURITY_ATTRIBUTES lpsa);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenWindowStation
#define OpenWindowStation error use qxeOpenWindowStation or OpenWindowStationA/OpenWindowStationW
#endif
HWINSTA qxeOpenWindowStation (Extbyte * lpszWinSta, BOOL fInherit, ACCESS_MASK dwDesiredAccess);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumWindowStations
#define EnumWindowStations error use qxeEnumWindowStations or EnumWindowStationsA/EnumWindowStationsW
#endif
BOOL qxeEnumWindowStations (WINSTAENUMPROCW lpEnumFunc, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetUserObjectInformation
#define GetUserObjectInformation error use qxeGetUserObjectInformation or GetUserObjectInformationA/GetUserObjectInformationW
#endif
BOOL qxeGetUserObjectInformation (HANDLE hObj, int nIndex, PVOID pvInfo, DWORD nLength, LPDWORD lpnLengthNeeded);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetUserObjectInformation
#define SetUserObjectInformation error use qxeSetUserObjectInformation or SetUserObjectInformationA/SetUserObjectInformationW
#endif
BOOL qxeSetUserObjectInformation (HANDLE hObj, int nIndex, PVOID pvInfo, DWORD nLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterWindowMessage
#define RegisterWindowMessage error use qxeRegisterWindowMessage or RegisterWindowMessageA/RegisterWindowMessageW
#endif
UINT qxeRegisterWindowMessage (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMessage
#define GetMessage error use qxeGetMessage or GetMessageA/GetMessageW
#endif
BOOL qxeGetMessage (LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DispatchMessage
#define DispatchMessage error use qxeDispatchMessage or DispatchMessageA/DispatchMessageW
#endif
LONG qxeDispatchMessage (CONST MSG * lpMsg);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PeekMessage
#define PeekMessage error use qxePeekMessage or PeekMessageA/PeekMessageW
#endif
BOOL qxePeekMessage (LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax, UINT wRemoveMsg);

/* Skipping SendMessage because split messages and structures */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendMessageTimeout
#define SendMessageTimeout error use qxeSendMessageTimeout or SendMessageTimeoutA/SendMessageTimeoutW
#endif
LRESULT qxeSendMessageTimeout (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam, UINT fuFlags, UINT uTimeout, LPDWORD lpdwResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendNotifyMessage
#define SendNotifyMessage error use qxeSendNotifyMessage or SendNotifyMessageA/SendNotifyMessageW
#endif
BOOL qxeSendNotifyMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendMessageCallback
#define SendMessageCallback error use qxeSendMessageCallback or SendMessageCallbackA/SendMessageCallbackW
#endif
BOOL qxeSendMessageCallback (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam, SENDASYNCPROC lpResultCallBack, DWORD dwData);

#undef BroadcastSystemMessage
#define BroadcastSystemMessage error win95 version not split; NT 4.0+ only

#undef RegisterDeviceNotification
#define RegisterDeviceNotification error NT 5.0+ only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PostMessage
#define PostMessage error use qxePostMessage or PostMessageA/PostMessageW
#endif
BOOL qxePostMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PostThreadMessage
#define PostThreadMessage error use qxePostThreadMessage or PostThreadMessageA/PostThreadMessageW
#endif
BOOL qxePostThreadMessage (DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam);

/* Skipping DefWindowProc because return value is conditionalized on _MAC, messes up parser */

#undef CallWindowProc
#define CallWindowProc error two versions, STRICT and non-STRICT

#undef CallWindowProc
#define CallWindowProc error two versions, STRICT and non-STRICT

/* Skipping RegisterClass because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASS */

/* Skipping UnregisterClass because need to intercept for reasons related to RegisterClass */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassInfo
#define GetClassInfo error use qxeGetClassInfo or GetClassInfoA/GetClassInfoW
#endif
BOOL qxeGetClassInfo (HINSTANCE hInstance, const Extbyte * lpClassName, LPWNDCLASSW lpWndClass);

/* Skipping RegisterClassEx because need to intercept so we can provide our own window procedure and handle split notify messages; split-simple WNDCLASSEX; NT 4.0+ only */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassInfoEx
#define GetClassInfoEx error use qxeGetClassInfoEx or GetClassInfoExA/GetClassInfoExW
#endif
BOOL qxeGetClassInfoEx (HINSTANCE arg1, const Extbyte * arg2, LPWNDCLASSEXW arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWindowEx
#define CreateWindowEx error use qxeCreateWindowEx or CreateWindowExA/CreateWindowExW
#endif
HWND qxeCreateWindowEx (DWORD dwExStyle, const Extbyte * lpClassName, const Extbyte * lpWindowName, DWORD dwStyle, int X, int Y, int nWidth, int nHeight, HWND hWndParent, HMENU hMenu, HINSTANCE hInstance, LPVOID lpParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDialogParam
#define CreateDialogParam error use qxeCreateDialogParam or CreateDialogParamA/CreateDialogParamW
#endif
HWND qxeCreateDialogParam (HINSTANCE hInstance, const Extbyte * lpTemplateName, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDialogIndirectParam
#define CreateDialogIndirectParam error use qxeCreateDialogIndirectParam or CreateDialogIndirectParamA/CreateDialogIndirectParamW
#endif
HWND qxeCreateDialogIndirectParam (HINSTANCE hInstance, LPCDLGTEMPLATEW lpTemplate, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DialogBoxParam
#define DialogBoxParam error use qxeDialogBoxParam or DialogBoxParamA/DialogBoxParamW
#endif
int qxeDialogBoxParam (HINSTANCE hInstance, const Extbyte * lpTemplateName, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DialogBoxIndirectParam
#define DialogBoxIndirectParam error use qxeDialogBoxIndirectParam or DialogBoxIndirectParamA/DialogBoxIndirectParamW
#endif
int qxeDialogBoxIndirectParam (HINSTANCE hInstance, LPCDLGTEMPLATEW hDialogTemplate, HWND hWndParent, DLGPROC lpDialogFunc, LPARAM dwInitParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetDlgItemText
#define SetDlgItemText error use qxeSetDlgItemText or SetDlgItemTextA/SetDlgItemTextW
#endif
BOOL qxeSetDlgItemText (HWND hDlg, int nIDDlgItem, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDlgItemText
#define GetDlgItemText error use qxeGetDlgItemText or GetDlgItemTextA/GetDlgItemTextW
#endif
UINT qxeGetDlgItemText (HWND hDlg, int nIDDlgItem, Extbyte * lpString, int nMaxCount);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendDlgItemMessage
#define SendDlgItemMessage error use qxeSendDlgItemMessage or SendDlgItemMessageA/SendDlgItemMessageW
#endif
LONG qxeSendDlgItemMessage (HWND hDlg, int nIDDlgItem, UINT Msg, WPARAM wParam, LPARAM lParam);

#undef DefDlgProc
#define DefDlgProc error return value is conditionalized on _MAC, messes up parser

#if !defined (CYGWIN_HEADERS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CallMsgFilter
#define CallMsgFilter error use qxeCallMsgFilter or CallMsgFilterA/CallMsgFilterW
#endif
BOOL qxeCallMsgFilter (LPMSG lpMsg, int nCode);
#endif /* !defined (CYGWIN_HEADERS) */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterClipboardFormat
#define RegisterClipboardFormat error use qxeRegisterClipboardFormat or RegisterClipboardFormatA/RegisterClipboardFormatW
#endif
UINT qxeRegisterClipboardFormat (const Extbyte * lpszFormat);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClipboardFormatName
#define GetClipboardFormatName error use qxeGetClipboardFormatName or GetClipboardFormatNameA/GetClipboardFormatNameW
#endif
int qxeGetClipboardFormatName (UINT format, Extbyte * lpszFormatName, int cchMaxCount);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharToOem
#define CharToOem error use qxeCharToOem or CharToOemA/CharToOemW
#endif
BOOL qxeCharToOem (const Extbyte * lpszSrc, LPSTR lpszDst);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OemToChar
#define OemToChar error use qxeOemToChar or OemToCharA/OemToCharW
#endif
BOOL qxeOemToChar (LPCSTR lpszSrc, Extbyte * lpszDst);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharToOemBuff
#define CharToOemBuff error use qxeCharToOemBuff or CharToOemBuffA/CharToOemBuffW
#endif
BOOL qxeCharToOemBuff (const Extbyte * lpszSrc, LPSTR lpszDst, DWORD cchDstLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OemToCharBuff
#define OemToCharBuff error use qxeOemToCharBuff or OemToCharBuffA/OemToCharBuffW
#endif
BOOL qxeOemToCharBuff (LPCSTR lpszSrc, Extbyte * lpszDst, DWORD cchDstLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharUpper
#define CharUpper error use qxeCharUpper or CharUpperA/CharUpperW
#endif
Extbyte * qxeCharUpper (Extbyte * lpsz);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharUpperBuff
#define CharUpperBuff error use qxeCharUpperBuff or CharUpperBuffA/CharUpperBuffW
#endif
DWORD qxeCharUpperBuff (Extbyte * lpsz, DWORD cchLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharLower
#define CharLower error use qxeCharLower or CharLowerA/CharLowerW
#endif
Extbyte * qxeCharLower (Extbyte * lpsz);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharLowerBuff
#define CharLowerBuff error use qxeCharLowerBuff or CharLowerBuffA/CharLowerBuffW
#endif
DWORD qxeCharLowerBuff (Extbyte * lpsz, DWORD cchLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharNext
#define CharNext error use qxeCharNext or CharNextA/CharNextW
#endif
Extbyte * qxeCharNext (const Extbyte * lpsz);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CharPrev
#define CharPrev error use qxeCharPrev or CharPrevA/CharPrevW
#endif
Extbyte * qxeCharPrev (const Extbyte * lpszStart, const Extbyte * lpszCurrent);

#undef IsCharAlpha
#define IsCharAlpha error split CHAR

#undef IsCharAlphaNumeric
#define IsCharAlphaNumeric error split CHAR

#undef IsCharUpper
#define IsCharUpper error split CHAR

#undef IsCharLower
#define IsCharLower error split CHAR

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetKeyNameText
#define GetKeyNameText error use qxeGetKeyNameText or GetKeyNameTextA/GetKeyNameTextW
#endif
int qxeGetKeyNameText (LONG lParam, Extbyte * lpString, int nSize);

/* Skipping VkKeyScan because split CHAR */

#undef VkKeyScanEx
#define VkKeyScanEx error split CHAR; NT 4.0+ only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MapVirtualKey
#define MapVirtualKey error use qxeMapVirtualKey or MapVirtualKeyA/MapVirtualKeyW
#endif
UINT qxeMapVirtualKey (UINT uCode, UINT uMapType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MapVirtualKeyEx
#define MapVirtualKeyEx error use qxeMapVirtualKeyEx or MapVirtualKeyExA/MapVirtualKeyExW
#endif
UINT qxeMapVirtualKeyEx (UINT uCode, UINT uMapType, HKL dwhkl);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadAccelerators
#define LoadAccelerators error use qxeLoadAccelerators or LoadAcceleratorsA/LoadAcceleratorsW
#endif
HACCEL qxeLoadAccelerators (HINSTANCE hInstance, const Extbyte * lpTableName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateAcceleratorTable
#define CreateAcceleratorTable error use qxeCreateAcceleratorTable or CreateAcceleratorTableA/CreateAcceleratorTableW
#endif
HACCEL qxeCreateAcceleratorTable (LPACCEL arg1, int arg2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyAcceleratorTable
#define CopyAcceleratorTable error use qxeCopyAcceleratorTable or CopyAcceleratorTableA/CopyAcceleratorTableW
#endif
int qxeCopyAcceleratorTable (HACCEL hAccelSrc, LPACCEL lpAccelDst, int cAccelEntries);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef TranslateAccelerator
#define TranslateAccelerator error use qxeTranslateAccelerator or TranslateAcceleratorA/TranslateAcceleratorW
#endif
int qxeTranslateAccelerator (HWND hWnd, HACCEL hAccTable, LPMSG lpMsg);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadMenu
#define LoadMenu error use qxeLoadMenu or LoadMenuA/LoadMenuW
#endif
HMENU qxeLoadMenu (HINSTANCE hInstance, const Extbyte * lpMenuName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadMenuIndirect
#define LoadMenuIndirect error use qxeLoadMenuIndirect or LoadMenuIndirectA/LoadMenuIndirectW
#endif
HMENU qxeLoadMenuIndirect (CONST MENUTEMPLATEW * lpMenuTemplate);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ChangeMenu
#define ChangeMenu error use qxeChangeMenu or ChangeMenuA/ChangeMenuW
#endif
BOOL qxeChangeMenu (HMENU hMenu, UINT cmd, const Extbyte * lpszNewItem, UINT cmdInsert, UINT flags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMenuString
#define GetMenuString error use qxeGetMenuString or GetMenuStringA/GetMenuStringW
#endif
int qxeGetMenuString (HMENU hMenu, UINT uIDItem, Extbyte * lpString, int nMaxCount, UINT uFlag);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef InsertMenu
#define InsertMenu error use qxeInsertMenu or InsertMenuA/InsertMenuW
#endif
BOOL qxeInsertMenu (HMENU hMenu, UINT uPosition, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AppendMenu
#define AppendMenu error use qxeAppendMenu or AppendMenuA/AppendMenuW
#endif
BOOL qxeAppendMenu (HMENU hMenu, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ModifyMenu
#define ModifyMenu error use qxeModifyMenu or ModifyMenuA/ModifyMenuW
#endif
BOOL qxeModifyMenu (HMENU hMnu, UINT uPosition, UINT uFlags, UINT uIDNewItem, const Extbyte * lpNewItem);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef InsertMenuItem
#define InsertMenuItem error use qxeInsertMenuItem or InsertMenuItemA/InsertMenuItemW
#endif
BOOL qxeInsertMenuItem (HMENU arg1, UINT arg2, BOOL arg3, LPCMENUITEMINFOW arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMenuItemInfo
#define GetMenuItemInfo error use qxeGetMenuItemInfo or GetMenuItemInfoA/GetMenuItemInfoW
#endif
BOOL qxeGetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPMENUITEMINFOW arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetMenuItemInfo
#define SetMenuItemInfo error use qxeSetMenuItemInfo or SetMenuItemInfoA/SetMenuItemInfoW
#endif
BOOL qxeSetMenuItemInfo (HMENU arg1, UINT arg2, BOOL arg3, LPCMENUITEMINFOW arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DrawText
#define DrawText error use qxeDrawText or DrawTextA/DrawTextW
#endif
int qxeDrawText (HDC hDC, const Extbyte * lpString, int nCount, LPRECT lpRect, UINT uFormat);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DrawTextEx
#define DrawTextEx error use qxeDrawTextEx or DrawTextExA/DrawTextExW
#endif
int qxeDrawTextEx (HDC arg1, Extbyte * arg2, int arg3, LPRECT arg4, UINT arg5, LPDRAWTEXTPARAMS arg6);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GrayString
#define GrayString error use qxeGrayString or GrayStringA/GrayStringW
#endif
BOOL qxeGrayString (HDC hDC, HBRUSH hBrush, GRAYSTRINGPROC lpOutputFunc, LPARAM lpData, int nCount, int X, int Y, int nWidth, int nHeight);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DrawState
#define DrawState error use qxeDrawState or DrawStateA/DrawStateW
#endif
BOOL qxeDrawState (HDC arg1, HBRUSH arg2, DRAWSTATEPROC arg3, LPARAM arg4, WPARAM arg5, int arg6, int arg7, int arg8, int arg9, UINT arg10);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef TabbedTextOut
#define TabbedTextOut error use qxeTabbedTextOut or TabbedTextOutA/TabbedTextOutW
#endif
LONG qxeTabbedTextOut (HDC hDC, int X, int Y, const Extbyte * lpString, int nCount, int nTabPositions, LPINT lpnTabStopPositions, int nTabOrigin);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTabbedTextExtent
#define GetTabbedTextExtent error use qxeGetTabbedTextExtent or GetTabbedTextExtentA/GetTabbedTextExtentW
#endif
DWORD qxeGetTabbedTextExtent (HDC hDC, const Extbyte * lpString, int nCount, int nTabPositions, LPINT lpnTabStopPositions);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetProp
#define SetProp error use qxeSetProp or SetPropA/SetPropW
#endif
BOOL qxeSetProp (HWND hWnd, const Extbyte * lpString, HANDLE hData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProp
#define GetProp error use qxeGetProp or GetPropA/GetPropW
#endif
HANDLE qxeGetProp (HWND hWnd, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RemoveProp
#define RemoveProp error use qxeRemoveProp or RemovePropA/RemovePropW
#endif
HANDLE qxeRemoveProp (HWND hWnd, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumPropsEx
#define EnumPropsEx error use qxeEnumPropsEx or EnumPropsExA/EnumPropsExW
#endif
int qxeEnumPropsEx (HWND hWnd, PROPENUMPROCEXW lpEnumFunc, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumProps
#define EnumProps error use qxeEnumProps or EnumPropsA/EnumPropsW
#endif
int qxeEnumProps (HWND hWnd, PROPENUMPROCW lpEnumFunc);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowText
#define SetWindowText error use qxeSetWindowText or SetWindowTextA/SetWindowTextW
#endif
BOOL qxeSetWindowText (HWND hWnd, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowText
#define GetWindowText error use qxeGetWindowText or GetWindowTextA/GetWindowTextW
#endif
int qxeGetWindowText (HWND hWnd, Extbyte * lpString, int nMaxCount);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowTextLength
#define GetWindowTextLength error use qxeGetWindowTextLength or GetWindowTextLengthA/GetWindowTextLengthW
#endif
int qxeGetWindowTextLength (HWND hWnd);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MessageBox
#define MessageBox error use qxeMessageBox or MessageBoxA/MessageBoxW
#endif
int qxeMessageBox (HWND hWnd, const Extbyte * lpText, const Extbyte * lpCaption, UINT uType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MessageBoxEx
#define MessageBoxEx error use qxeMessageBoxEx or MessageBoxExA/MessageBoxExW
#endif
int qxeMessageBoxEx (HWND hWnd, const Extbyte * lpText, const Extbyte * lpCaption, UINT uType, WORD wLanguageId);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MessageBoxIndirect
#define MessageBoxIndirect error use qxeMessageBoxIndirect or MessageBoxIndirectA/MessageBoxIndirectW
#endif
int qxeMessageBoxIndirect (LPMSGBOXPARAMSW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowLong
#define GetWindowLong error use qxeGetWindowLong or GetWindowLongA/GetWindowLongW
#endif
LONG qxeGetWindowLong (HWND hWnd, int nIndex);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowLong
#define SetWindowLong error use qxeSetWindowLong or SetWindowLongA/SetWindowLongW
#endif
LONG qxeSetWindowLong (HWND hWnd, int nIndex, LONG dwNewLong);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassLong
#define GetClassLong error use qxeGetClassLong or GetClassLongA/GetClassLongW
#endif
DWORD qxeGetClassLong (HWND hWnd, int nIndex);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetClassLong
#define SetClassLong error use qxeSetClassLong or SetClassLongA/SetClassLongW
#endif
DWORD qxeSetClassLong (HWND hWnd, int nIndex, LONG dwNewLong);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindWindow
#define FindWindow error use qxeFindWindow or FindWindowA/FindWindowW
#endif
HWND qxeFindWindow (const Extbyte * lpClassName, const Extbyte * lpWindowName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindWindowEx
#define FindWindowEx error use qxeFindWindowEx or FindWindowExA/FindWindowExW
#endif
HWND qxeFindWindowEx (HWND arg1, HWND arg2, const Extbyte * arg3, const Extbyte * arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetClassName
#define GetClassName error use qxeGetClassName or GetClassNameA/GetClassNameW
#endif
int qxeGetClassName (HWND hWnd, Extbyte * lpClassName, int nMaxCount);

#undef SetWindowsHook
#define SetWindowsHook error obsolete; two versions, STRICT and non-STRICT

#undef SetWindowsHook
#define SetWindowsHook error obsolete; two versions, STRICT and non-STRICT

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowsHookEx
#define SetWindowsHookEx error use qxeSetWindowsHookEx or SetWindowsHookExA/SetWindowsHookExW
#endif
HHOOK qxeSetWindowsHookEx (int idHook, HOOKPROC lpfn, HINSTANCE hmod, DWORD dwThreadId);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadBitmap
#define LoadBitmap error use qxeLoadBitmap or LoadBitmapA/LoadBitmapW
#endif
HBITMAP qxeLoadBitmap (HINSTANCE hInstance, const Extbyte * lpBitmapName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadCursor
#define LoadCursor error use qxeLoadCursor or LoadCursorA/LoadCursorW
#endif
HCURSOR qxeLoadCursor (HINSTANCE hInstance, const Extbyte * lpCursorName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadCursorFromFile
#define LoadCursorFromFile error use qxeLoadCursorFromFile or LoadCursorFromFileA/LoadCursorFromFileW
#endif
HCURSOR qxeLoadCursorFromFile (const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadIcon
#define LoadIcon error use qxeLoadIcon or LoadIconA/LoadIconW
#endif
HICON qxeLoadIcon (HINSTANCE hInstance, const Extbyte * lpIconName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadImage
#define LoadImage error use qxeLoadImage or LoadImageA/LoadImageW
#endif
HANDLE qxeLoadImage (HINSTANCE arg1, const Extbyte * arg2, UINT arg3, int arg4, int arg5, UINT arg6);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadString
#define LoadString error use qxeLoadString or LoadStringA/LoadStringW
#endif
int qxeLoadString (HINSTANCE hInstance, UINT uID, Extbyte * lpBuffer, int nBufferMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef IsDialogMessage
#define IsDialogMessage error use qxeIsDialogMessage or IsDialogMessageA/IsDialogMessageW
#endif
BOOL qxeIsDialogMessage (HWND hDlg, LPMSG lpMsg);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirList
#define DlgDirList error use qxeDlgDirList or DlgDirListA/DlgDirListW
#endif
int qxeDlgDirList (HWND hDlg, Extbyte * lpPathSpec, int nIDListBox, int nIDStaticPath, UINT uFileType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirSelectEx
#define DlgDirSelectEx error use qxeDlgDirSelectEx or DlgDirSelectExA/DlgDirSelectExW
#endif
BOOL qxeDlgDirSelectEx (HWND hDlg, Extbyte * lpString, int nCount, int nIDListBox);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirListComboBox
#define DlgDirListComboBox error use qxeDlgDirListComboBox or DlgDirListComboBoxA/DlgDirListComboBoxW
#endif
int qxeDlgDirListComboBox (HWND hDlg, Extbyte * lpPathSpec, int nIDComboBox, int nIDStaticPath, UINT uFiletype);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DlgDirSelectComboBoxEx
#define DlgDirSelectComboBoxEx error use qxeDlgDirSelectComboBoxEx or DlgDirSelectComboBoxExA/DlgDirSelectComboBoxExW
#endif
BOOL qxeDlgDirSelectComboBoxEx (HWND hDlg, Extbyte * lpString, int nCount, int nIDComboBox);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefFrameProc
#define DefFrameProc error use qxeDefFrameProc or DefFrameProcA/DefFrameProcW
#endif
LRESULT qxeDefFrameProc (HWND hWnd, HWND hWndMDIClient, UINT uMsg, WPARAM wParam, LPARAM lParam);

#undef DefMDIChildProc
#define DefMDIChildProc error return value is conditionalized on _MAC, messes up parser

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMDIWindow
#define CreateMDIWindow error use qxeCreateMDIWindow or CreateMDIWindowA/CreateMDIWindowW
#endif
HWND qxeCreateMDIWindow (Extbyte * lpClassName, Extbyte * lpWindowName, DWORD dwStyle, int X, int Y, int nWidth, int nHeight, HWND hWndParent, HINSTANCE hInstance, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WinHelp
#define WinHelp error use qxeWinHelp or WinHelpA/WinHelpW
#endif
BOOL qxeWinHelp (HWND hWndMain, const Extbyte * lpszHelp, UINT uCommand, DWORD dwData);

#undef ChangeDisplaySettings
#define ChangeDisplaySettings error split-sized LPDEVMODE

#undef ChangeDisplaySettingsEx
#define ChangeDisplaySettingsEx error split-sized LPDEVMODE; NT 5.0/Win98+ only

#undef EnumDisplaySettings
#define EnumDisplaySettings error split-sized LPDEVMODE

#undef EnumDisplayDevices
#define EnumDisplayDevices error split-sized PDISPLAY_DEVICE; NT 5.0+ only, no Win98

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SystemParametersInfo
#define SystemParametersInfo error use qxeSystemParametersInfo or SystemParametersInfoA/SystemParametersInfoW
#endif
BOOL qxeSystemParametersInfo (UINT uiAction, UINT uiParam, PVOID pvParam, UINT fWinIni);

#undef GetMonitorInfo
#define GetMonitorInfo error NT 5.0/Win98+ only

#undef GetWindowModuleFileName
#define GetWindowModuleFileName error NT 5.0+ only

#undef RealGetWindowClass
#define RealGetWindowClass error NT 5.0+ only

#undef GetAltTabInfo
#define GetAltTabInfo error NT 5.0+ only


/* Processing file MMSYSTEM.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef sndPlaySound
#define sndPlaySound error use qxesndPlaySound or sndPlaySoundA/sndPlaySoundW
#endif
BOOL qxesndPlaySound (const Extbyte * pszSound, UINT fuSound);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PlaySound
#define PlaySound error use qxePlaySound or PlaySoundA/PlaySoundW
#endif
BOOL qxePlaySound (const Extbyte * pszSound, HMODULE hmod, DWORD fdwSound);

#undef waveOutGetDevCaps
#define waveOutGetDevCaps error split-sized LPWAVEOUTCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef waveOutGetErrorText
#define waveOutGetErrorText error use qxewaveOutGetErrorText or waveOutGetErrorTextA/waveOutGetErrorTextW
#endif
MMRESULT qxewaveOutGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef waveInGetDevCaps
#define waveInGetDevCaps error split-sized LPWAVEINCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef waveInGetErrorText
#define waveInGetErrorText error use qxewaveInGetErrorText or waveInGetErrorTextA/waveInGetErrorTextW
#endif
MMRESULT qxewaveInGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef midiOutGetDevCaps
#define midiOutGetDevCaps error split-sized LPMIDIOUTCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef midiOutGetErrorText
#define midiOutGetErrorText error use qxemidiOutGetErrorText or midiOutGetErrorTextA/midiOutGetErrorTextW
#endif
MMRESULT qxemidiOutGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef midiInGetDevCaps
#define midiInGetDevCaps error split-sized LPMIDIOUTCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef midiInGetErrorText
#define midiInGetErrorText error use qxemidiInGetErrorText or midiInGetErrorTextA/midiInGetErrorTextW
#endif
MMRESULT qxemidiInGetErrorText (MMRESULT mmrError, Extbyte * pszText, UINT cchText);

#undef auxGetDevCaps
#define auxGetDevCaps error split-sized LPAUXCAPS

#undef mixerGetDevCaps
#define mixerGetDevCaps error split-sized LPMIXERCAPS

#undef mixerGetLineInfo
#define mixerGetLineInfo error split-sized LPMIXERLINE

#undef mixerGetLineControls
#define mixerGetLineControls error split-sized LPMIXERCONTROL

#undef mixerGetControlDetails
#define mixerGetControlDetails error split-sized LPMIXERCONTROL in LPMIXERLINECONTROLS in LPMIXERCONTROLDETAILS

#undef joyGetDevCaps
#define joyGetDevCaps error split-sized LPJOYCAPS

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioStringToFOURCC
#define mmioStringToFOURCC error use qxemmioStringToFOURCC or mmioStringToFOURCCA/mmioStringToFOURCCW
#endif
FOURCC qxemmioStringToFOURCC (const Extbyte * sz, UINT uFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioInstallIOProc
#define mmioInstallIOProc error use qxemmioInstallIOProc or mmioInstallIOProcA/mmioInstallIOProcW
#endif
LPMMIOPROC qxemmioInstallIOProc (FOURCC fccIOProc, LPMMIOPROC pIOProc, DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioOpen
#define mmioOpen error use qxemmioOpen or mmioOpenA/mmioOpenW
#endif
HMMIO qxemmioOpen (Extbyte * pszFileName, LPMMIOINFO pmmioinfo, DWORD fdwOpen);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mmioRename
#define mmioRename error use qxemmioRename or mmioRenameA/mmioRenameW
#endif
MMRESULT qxemmioRename (const Extbyte * pszFileName, const Extbyte * pszNewFileName, LPCMMIOINFO pmmioinfo, DWORD fdwRename);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciSendCommand
#define mciSendCommand error use qxemciSendCommand or mciSendCommandA/mciSendCommandW
#endif
MCIERROR qxemciSendCommand (MCIDEVICEID mciId, UINT uMsg, DWORD dwParam1, DWORD dwParam2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciSendString
#define mciSendString error use qxemciSendString or mciSendStringA/mciSendStringW
#endif
MCIERROR qxemciSendString (const Extbyte * lpstrCommand, Extbyte * lpstrReturnString, UINT uReturnLength, HWND hwndCallback);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciGetDeviceID
#define mciGetDeviceID error use qxemciGetDeviceID or mciGetDeviceIDA/mciGetDeviceIDW
#endif
MCIDEVICEID qxemciGetDeviceID (const Extbyte * pszDevice);

#if !defined (MINGW)
#undef mciGetDeviceIDFromElementID
#define mciGetDeviceIDFromElementID error missing from Win98se version of ADVAPI32.dll
#endif /* !defined (MINGW) */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef mciGetErrorString
#define mciGetErrorString error use qxemciGetErrorString or mciGetErrorStringA/mciGetErrorStringW
#endif
BOOL qxemciGetErrorString (MCIERROR mcierr, Extbyte * pszText, UINT cchText);


/* Processing file WINBASE.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetBinaryType
#define GetBinaryType error use qxeGetBinaryType or GetBinaryTypeA/GetBinaryTypeW
#endif
BOOL qxeGetBinaryType (const Extbyte * lpApplicationName, LPDWORD lpBinaryType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetShortPathName
#define GetShortPathName error use qxeGetShortPathName or GetShortPathNameA/GetShortPathNameW
#endif
DWORD qxeGetShortPathName (const Extbyte * lpszLongPath, Extbyte * lpszShortPath, DWORD cchBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetLongPathName
#define GetLongPathName error use qxeGetLongPathName or GetLongPathNameA/GetLongPathNameW
#endif
DWORD qxeGetLongPathName (const Extbyte * lpszShortPath, Extbyte * lpszLongPath, DWORD cchBuffer);

/* Skipping GetEnvironmentStrings because misnamed ANSI version of the function */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FreeEnvironmentStrings
#define FreeEnvironmentStrings error use qxeFreeEnvironmentStrings or FreeEnvironmentStringsA/FreeEnvironmentStringsW
#endif
BOOL qxeFreeEnvironmentStrings (Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FormatMessage
#define FormatMessage error use qxeFormatMessage or FormatMessageA/FormatMessageW
#endif
DWORD qxeFormatMessage (DWORD dwFlags, LPCVOID lpSource, DWORD dwMessageId, DWORD dwLanguageId, Extbyte * lpBuffer, DWORD nSize, va_list * Arguments);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMailslot
#define CreateMailslot error use qxeCreateMailslot or CreateMailslotA/CreateMailslotW
#endif
HANDLE qxeCreateMailslot (const Extbyte * lpName, DWORD nMaxMessageSize, DWORD lReadTimeout, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#if !defined (CYGWIN_HEADERS)
#undef EncryptFile
#define EncryptFile error Win2K+ only
#endif /* !defined (CYGWIN_HEADERS) */

#if !defined (CYGWIN_HEADERS)
#undef DecryptFile
#define DecryptFile error Win2K+ only
#endif /* !defined (CYGWIN_HEADERS) */

#undef OpenRaw
#define OpenRaw error error "The procedure entry point OpenRawW could not be located in the dynamic link library ADVAPI32.dll."

#undef QueryRecoveryAgents
#define QueryRecoveryAgents error split-sized LPRECOVERY_AGENT_INFORMATION

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcmp
#define lstrcmp error use qxelstrcmp or lstrcmpA/lstrcmpW
#endif
int qxelstrcmp (const Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcmpi
#define lstrcmpi error use qxelstrcmpi or lstrcmpiA/lstrcmpiW
#endif
int qxelstrcmpi (const Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcpyn
#define lstrcpyn error use qxelstrcpyn or lstrcpynA/lstrcpynW
#endif
Extbyte * qxelstrcpyn (Extbyte * lpString1, const Extbyte * lpString2, int iMaxLength);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcpy
#define lstrcpy error use qxelstrcpy or lstrcpyA/lstrcpyW
#endif
Extbyte * qxelstrcpy (Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrcat
#define lstrcat error use qxelstrcat or lstrcatA/lstrcatW
#endif
Extbyte * qxelstrcat (Extbyte * lpString1, const Extbyte * lpString2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef lstrlen
#define lstrlen error use qxelstrlen or lstrlenA/lstrlenW
#endif
int qxelstrlen (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMutex
#define CreateMutex error use qxeCreateMutex or CreateMutexA/CreateMutexW
#endif
HANDLE qxeCreateMutex (LPSECURITY_ATTRIBUTES lpMutexAttributes, BOOL bInitialOwner, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenMutex
#define OpenMutex error use qxeOpenMutex or OpenMutexA/OpenMutexW
#endif
HANDLE qxeOpenMutex (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateEvent
#define CreateEvent error use qxeCreateEvent or CreateEventA/CreateEventW
#endif
HANDLE qxeCreateEvent (LPSECURITY_ATTRIBUTES lpEventAttributes, BOOL bManualReset, BOOL bInitialState, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenEvent
#define OpenEvent error use qxeOpenEvent or OpenEventA/OpenEventW
#endif
HANDLE qxeOpenEvent (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateSemaphore
#define CreateSemaphore error use qxeCreateSemaphore or CreateSemaphoreA/CreateSemaphoreW
#endif
HANDLE qxeCreateSemaphore (LPSECURITY_ATTRIBUTES lpSemaphoreAttributes, LONG lInitialCount, LONG lMaximumCount, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenSemaphore
#define OpenSemaphore error use qxeOpenSemaphore or OpenSemaphoreA/OpenSemaphoreW
#endif
HANDLE qxeOpenSemaphore (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWaitableTimer
#define CreateWaitableTimer error use qxeCreateWaitableTimer or CreateWaitableTimerA/CreateWaitableTimerW
#endif
HANDLE qxeCreateWaitableTimer (LPSECURITY_ATTRIBUTES lpTimerAttributes, BOOL bManualReset, const Extbyte * lpTimerName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenWaitableTimer
#define OpenWaitableTimer error use qxeOpenWaitableTimer or OpenWaitableTimerA/OpenWaitableTimerW
#endif
HANDLE qxeOpenWaitableTimer (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpTimerName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFileMapping
#define CreateFileMapping error use qxeCreateFileMapping or CreateFileMappingA/CreateFileMappingW
#endif
HANDLE qxeCreateFileMapping (HANDLE hFile, LPSECURITY_ATTRIBUTES lpFileMappingAttributes, DWORD flProtect, DWORD dwMaximumSizeHigh, DWORD dwMaximumSizeLow, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenFileMapping
#define OpenFileMapping error use qxeOpenFileMapping or OpenFileMappingA/OpenFileMappingW
#endif
HANDLE qxeOpenFileMapping (DWORD dwDesiredAccess, BOOL bInheritHandle, const Extbyte * lpName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetLogicalDriveStrings
#define GetLogicalDriveStrings error use qxeGetLogicalDriveStrings or GetLogicalDriveStringsA/GetLogicalDriveStringsW
#endif
DWORD qxeGetLogicalDriveStrings (DWORD nBufferLength, Extbyte * lpBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadLibrary
#define LoadLibrary error use qxeLoadLibrary or LoadLibraryA/LoadLibraryW
#endif
HMODULE qxeLoadLibrary (const Extbyte * lpLibFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LoadLibraryEx
#define LoadLibraryEx error use qxeLoadLibraryEx or LoadLibraryExA/LoadLibraryExW
#endif
HMODULE qxeLoadLibraryEx (const Extbyte * lpLibFileName, HANDLE hFile, DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetModuleFileName
#define GetModuleFileName error use qxeGetModuleFileName or GetModuleFileNameA/GetModuleFileNameW
#endif
DWORD qxeGetModuleFileName (HMODULE hModule, Extbyte * lpFilename, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetModuleHandle
#define GetModuleHandle error use qxeGetModuleHandle or GetModuleHandleA/GetModuleHandleW
#endif
HMODULE qxeGetModuleHandle (const Extbyte * lpModuleName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateProcess
#define CreateProcess error use qxeCreateProcess or CreateProcessA/CreateProcessW
#endif
BOOL qxeCreateProcess (const Extbyte * lpApplicationName, Extbyte * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, const Extbyte * lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FatalAppExit
#define FatalAppExit error use qxeFatalAppExit or FatalAppExitA/FatalAppExitW
#endif
VOID qxeFatalAppExit (UINT uAction, const Extbyte * lpMessageText);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetStartupInfo
#define GetStartupInfo error use qxeGetStartupInfo or GetStartupInfoA/GetStartupInfoW
#endif
VOID qxeGetStartupInfo (LPSTARTUPINFOW lpStartupInfo);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCommandLine
#define GetCommandLine error use qxeGetCommandLine or GetCommandLineA/GetCommandLineW
#endif
Extbyte * qxeGetCommandLine (void);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetEnvironmentVariable
#define GetEnvironmentVariable error use qxeGetEnvironmentVariable or GetEnvironmentVariableA/GetEnvironmentVariableW
#endif
DWORD qxeGetEnvironmentVariable (const Extbyte * lpName, Extbyte * lpBuffer, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetEnvironmentVariable
#define SetEnvironmentVariable error use qxeSetEnvironmentVariable or SetEnvironmentVariableA/SetEnvironmentVariableW
#endif
BOOL qxeSetEnvironmentVariable (const Extbyte * lpName, const Extbyte * lpValue);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExpandEnvironmentStrings
#define ExpandEnvironmentStrings error use qxeExpandEnvironmentStrings or ExpandEnvironmentStringsA/ExpandEnvironmentStringsW
#endif
DWORD qxeExpandEnvironmentStrings (const Extbyte * lpSrc, Extbyte * lpDst, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OutputDebugString
#define OutputDebugString error use qxeOutputDebugString or OutputDebugStringA/OutputDebugStringW
#endif
VOID qxeOutputDebugString (const Extbyte * lpOutputString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindResource
#define FindResource error use qxeFindResource or FindResourceA/FindResourceW
#endif
HRSRC qxeFindResource (HMODULE hModule, const Extbyte * lpName, const Extbyte * lpType);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindResourceEx
#define FindResourceEx error use qxeFindResourceEx or FindResourceExA/FindResourceExW
#endif
HRSRC qxeFindResourceEx (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, WORD wLanguage);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceTypes
#define EnumResourceTypes error use qxeEnumResourceTypes or EnumResourceTypesA/EnumResourceTypesW
#endif
BOOL qxeEnumResourceTypes (HMODULE hModule, ENUMRESTYPEPROC lpEnumFunc, LONG lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceNames
#define EnumResourceNames error use qxeEnumResourceNames or EnumResourceNamesA/EnumResourceNamesW
#endif
BOOL qxeEnumResourceNames (HMODULE hModule, const Extbyte * lpType, ENUMRESNAMEPROC lpEnumFunc, LONG lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceLanguages
#define EnumResourceLanguages error use qxeEnumResourceLanguages or EnumResourceLanguagesA/EnumResourceLanguagesW
#endif
BOOL qxeEnumResourceLanguages (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, ENUMRESLANGPROC lpEnumFunc, LONG lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BeginUpdateResource
#define BeginUpdateResource error use qxeBeginUpdateResource or BeginUpdateResourceA/BeginUpdateResourceW
#endif
HANDLE qxeBeginUpdateResource (const Extbyte * pFileName, BOOL bDeleteExistingResources);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef UpdateResource
#define UpdateResource error use qxeUpdateResource or UpdateResourceA/UpdateResourceW
#endif
BOOL qxeUpdateResource (HANDLE hUpdate, const Extbyte * lpType, const Extbyte * lpName, WORD wLanguage, LPVOID lpData, DWORD cbData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EndUpdateResource
#define EndUpdateResource error use qxeEndUpdateResource or EndUpdateResourceA/EndUpdateResourceW
#endif
BOOL qxeEndUpdateResource (HANDLE hUpdate, BOOL fDiscard);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GlobalAddAtom
#define GlobalAddAtom error use qxeGlobalAddAtom or GlobalAddAtomA/GlobalAddAtomW
#endif
ATOM qxeGlobalAddAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GlobalFindAtom
#define GlobalFindAtom error use qxeGlobalFindAtom or GlobalFindAtomA/GlobalFindAtomW
#endif
ATOM qxeGlobalFindAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GlobalGetAtomName
#define GlobalGetAtomName error use qxeGlobalGetAtomName or GlobalGetAtomNameA/GlobalGetAtomNameW
#endif
UINT qxeGlobalGetAtomName (ATOM nAtom, Extbyte * lpBuffer, int nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AddAtom
#define AddAtom error use qxeAddAtom or AddAtomA/AddAtomW
#endif
ATOM qxeAddAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindAtom
#define FindAtom error use qxeFindAtom or FindAtomA/FindAtomW
#endif
ATOM qxeFindAtom (const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetAtomName
#define GetAtomName error use qxeGetAtomName or GetAtomNameA/GetAtomNameW
#endif
UINT qxeGetAtomName (ATOM nAtom, Extbyte * lpBuffer, int nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProfileInt
#define GetProfileInt error use qxeGetProfileInt or GetProfileIntA/GetProfileIntW
#endif
UINT qxeGetProfileInt (const Extbyte * lpAppName, const Extbyte * lpKeyName, INT nDefault);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProfileString
#define GetProfileString error use qxeGetProfileString or GetProfileStringA/GetProfileStringW
#endif
DWORD qxeGetProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpDefault, Extbyte * lpReturnedString, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteProfileString
#define WriteProfileString error use qxeWriteProfileString or WriteProfileStringA/WriteProfileStringW
#endif
BOOL qxeWriteProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetProfileSection
#define GetProfileSection error use qxeGetProfileSection or GetProfileSectionA/GetProfileSectionW
#endif
DWORD qxeGetProfileSection (const Extbyte * lpAppName, Extbyte * lpReturnedString, DWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WriteProfileSection
#define WriteProfileSection error use qxeWriteProfileSection or WriteProfileSectionA/WriteProfileSectionW
#endif
BOOL qxeWriteProfileSection (const Extbyte * lpAppName, const Extbyte * lpString);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileInt
#define GetPrivateProfileInt error use qxeGetPrivateProfileInt or GetPrivateProfileIntA/GetPrivateProfileIntW
#endif
UINT qxeGetPrivateProfileInt (const Extbyte * lpAppName, const Extbyte * lpKeyName, INT nDefault, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileString
#define GetPrivateProfileString error use qxeGetPrivateProfileString or GetPrivateProfileStringA/GetPrivateProfileStringW
#endif
DWORD qxeGetPrivateProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpDefault, Extbyte * lpReturnedString, DWORD nSize, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WritePrivateProfileString
#define WritePrivateProfileString error use qxeWritePrivateProfileString or WritePrivateProfileStringA/WritePrivateProfileStringW
#endif
BOOL qxeWritePrivateProfileString (const Extbyte * lpAppName, const Extbyte * lpKeyName, const Extbyte * lpString, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileSection
#define GetPrivateProfileSection error use qxeGetPrivateProfileSection or GetPrivateProfileSectionA/GetPrivateProfileSectionW
#endif
DWORD qxeGetPrivateProfileSection (const Extbyte * lpAppName, Extbyte * lpReturnedString, DWORD nSize, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WritePrivateProfileSection
#define WritePrivateProfileSection error use qxeWritePrivateProfileSection or WritePrivateProfileSectionA/WritePrivateProfileSectionW
#endif
BOOL qxeWritePrivateProfileSection (const Extbyte * lpAppName, const Extbyte * lpString, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileSectionNames
#define GetPrivateProfileSectionNames error use qxeGetPrivateProfileSectionNames or GetPrivateProfileSectionNamesA/GetPrivateProfileSectionNamesW
#endif
DWORD qxeGetPrivateProfileSectionNames (Extbyte * lpszReturnBuffer, DWORD nSize, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetPrivateProfileStruct
#define GetPrivateProfileStruct error use qxeGetPrivateProfileStruct or GetPrivateProfileStructA/GetPrivateProfileStructW
#endif
BOOL qxeGetPrivateProfileStruct (const Extbyte * lpszSection, const Extbyte * lpszKey, LPVOID lpStruct, UINT uSizeStruct, const Extbyte * szFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WritePrivateProfileStruct
#define WritePrivateProfileStruct error use qxeWritePrivateProfileStruct or WritePrivateProfileStructA/WritePrivateProfileStructW
#endif
BOOL qxeWritePrivateProfileStruct (const Extbyte * lpszSection, const Extbyte * lpszKey, LPVOID lpStruct, UINT uSizeStruct, const Extbyte * szFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDriveType
#define GetDriveType error use qxeGetDriveType or GetDriveTypeA/GetDriveTypeW
#endif
UINT qxeGetDriveType (const Extbyte * lpRootPathName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetSystemDirectory
#define GetSystemDirectory error use qxeGetSystemDirectory or GetSystemDirectoryA/GetSystemDirectoryW
#endif
UINT qxeGetSystemDirectory (Extbyte * lpBuffer, UINT uSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTempPath
#define GetTempPath error use qxeGetTempPath or GetTempPathA/GetTempPathW
#endif
DWORD qxeGetTempPath (DWORD nBufferLength, Extbyte * lpBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTempFileName
#define GetTempFileName error use qxeGetTempFileName or GetTempFileNameA/GetTempFileNameW
#endif
UINT qxeGetTempFileName (const Extbyte * lpPathName, const Extbyte * lpPrefixString, UINT uUnique, Extbyte * lpTempFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetWindowsDirectory
#define GetWindowsDirectory error use qxeGetWindowsDirectory or GetWindowsDirectoryA/GetWindowsDirectoryW
#endif
UINT qxeGetWindowsDirectory (Extbyte * lpBuffer, UINT uSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetCurrentDirectory
#define SetCurrentDirectory error use qxeSetCurrentDirectory or SetCurrentDirectoryA/SetCurrentDirectoryW
#endif
BOOL qxeSetCurrentDirectory (const Extbyte * lpPathName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCurrentDirectory
#define GetCurrentDirectory error use qxeGetCurrentDirectory or GetCurrentDirectoryA/GetCurrentDirectoryW
#endif
DWORD qxeGetCurrentDirectory (DWORD nBufferLength, Extbyte * lpBuffer);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDiskFreeSpace
#define GetDiskFreeSpace error use qxeGetDiskFreeSpace or GetDiskFreeSpaceA/GetDiskFreeSpaceW
#endif
BOOL qxeGetDiskFreeSpace (const Extbyte * lpRootPathName, LPDWORD lpSectorsPerCluster, LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, LPDWORD lpTotalNumberOfClusters);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDiskFreeSpaceEx
#define GetDiskFreeSpaceEx error use qxeGetDiskFreeSpaceEx or GetDiskFreeSpaceExA/GetDiskFreeSpaceExW
#endif
BOOL qxeGetDiskFreeSpaceEx (const Extbyte * lpDirectoryName, PULARGE_INTEGER lpFreeBytesAvailableToCaller, PULARGE_INTEGER lpTotalNumberOfBytes, PULARGE_INTEGER lpTotalNumberOfFreeBytes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDirectory
#define CreateDirectory error use qxeCreateDirectory or CreateDirectoryA/CreateDirectoryW
#endif
BOOL qxeCreateDirectory (const Extbyte * lpPathName, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDirectoryEx
#define CreateDirectoryEx error use qxeCreateDirectoryEx or CreateDirectoryExA/CreateDirectoryExW
#endif
BOOL qxeCreateDirectoryEx (const Extbyte * lpTemplateDirectory, const Extbyte * lpNewDirectory, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RemoveDirectory
#define RemoveDirectory error use qxeRemoveDirectory or RemoveDirectoryA/RemoveDirectoryW
#endif
BOOL qxeRemoveDirectory (const Extbyte * lpPathName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFullPathName
#define GetFullPathName error use qxeGetFullPathName or GetFullPathNameA/GetFullPathNameW
#endif
DWORD qxeGetFullPathName (const Extbyte * lpFileName, DWORD nBufferLength, Extbyte * lpBuffer, Extbyte * * lpFilePart);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefineDosDevice
#define DefineDosDevice error use qxeDefineDosDevice or DefineDosDeviceA/DefineDosDeviceW
#endif
BOOL qxeDefineDosDevice (DWORD dwFlags, const Extbyte * lpDeviceName, const Extbyte * lpTargetPath);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef QueryDosDevice
#define QueryDosDevice error use qxeQueryDosDevice or QueryDosDeviceA/QueryDosDeviceW
#endif
DWORD qxeQueryDosDevice (const Extbyte * lpDeviceName, Extbyte * lpTargetPath, DWORD ucchMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFile
#define CreateFile error use qxeCreateFile or CreateFileA/CreateFileW
#endif
HANDLE qxeCreateFile (const Extbyte * lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode, LPSECURITY_ATTRIBUTES lpSecurityAttributes, DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes, HANDLE hTemplateFile);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetFileAttributes
#define SetFileAttributes error use qxeSetFileAttributes or SetFileAttributesA/SetFileAttributesW
#endif
BOOL qxeSetFileAttributes (const Extbyte * lpFileName, DWORD dwFileAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileAttributes
#define GetFileAttributes error use qxeGetFileAttributes or GetFileAttributesA/GetFileAttributesW
#endif
DWORD qxeGetFileAttributes (const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileAttributesEx
#define GetFileAttributesEx error use qxeGetFileAttributesEx or GetFileAttributesExA/GetFileAttributesExW
#endif
BOOL qxeGetFileAttributesEx (const Extbyte * lpFileName, GET_FILEEX_INFO_LEVELS fInfoLevelId, LPVOID lpFileInformation);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCompressedFileSize
#define GetCompressedFileSize error use qxeGetCompressedFileSize or GetCompressedFileSizeA/GetCompressedFileSizeW
#endif
DWORD qxeGetCompressedFileSize (const Extbyte * lpFileName, LPDWORD lpFileSizeHigh);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DeleteFile
#define DeleteFile error use qxeDeleteFile or DeleteFileA/DeleteFileW
#endif
BOOL qxeDeleteFile (const Extbyte * lpFileName);

#undef FindFirstFileEx
#define FindFirstFileEx error split-sized LPWIN32_FIND_DATA; not used, NT 4.0+ only

/* Skipping FindFirstFile because split-sized LPWIN32_FIND_DATA */

/* Skipping FindNextFile because split-sized LPWIN32_FIND_DATA */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SearchPath
#define SearchPath error use qxeSearchPath or SearchPathA/SearchPathW
#endif
DWORD qxeSearchPath (const Extbyte * lpPath, const Extbyte * lpFileName, const Extbyte * lpExtension, DWORD nBufferLength, Extbyte * lpBuffer, Extbyte * * lpFilePart);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyFile
#define CopyFile error use qxeCopyFile or CopyFileA/CopyFileW
#endif
BOOL qxeCopyFile (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, BOOL bFailIfExists);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyFileEx
#define CopyFileEx error use qxeCopyFileEx or CopyFileExA/CopyFileExW
#endif
BOOL qxeCopyFileEx (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, LPPROGRESS_ROUTINE lpProgressRoutine, LPVOID lpData, LPBOOL pbCancel, DWORD dwCopyFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MoveFile
#define MoveFile error use qxeMoveFile or MoveFileA/MoveFileW
#endif
BOOL qxeMoveFile (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef MoveFileEx
#define MoveFileEx error use qxeMoveFileEx or MoveFileExA/MoveFileExW
#endif
BOOL qxeMoveFileEx (const Extbyte * lpExistingFileName, const Extbyte * lpNewFileName, DWORD dwFlags);

#undef MoveFileWithProgress
#define MoveFileWithProgress error NT 5.0+ only

#undef CreateHardLink
#define CreateHardLink error NT 5.0+ only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateNamedPipe
#define CreateNamedPipe error use qxeCreateNamedPipe or CreateNamedPipeA/CreateNamedPipeW
#endif
HANDLE qxeCreateNamedPipe (const Extbyte * lpName, DWORD dwOpenMode, DWORD dwPipeMode, DWORD nMaxInstances, DWORD nOutBufferSize, DWORD nInBufferSize, DWORD nDefaultTimeOut, LPSECURITY_ATTRIBUTES lpSecurityAttributes);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetNamedPipeHandleState
#define GetNamedPipeHandleState error use qxeGetNamedPipeHandleState or GetNamedPipeHandleStateA/GetNamedPipeHandleStateW
#endif
BOOL qxeGetNamedPipeHandleState (HANDLE hNamedPipe, LPDWORD lpState, LPDWORD lpCurInstances, LPDWORD lpMaxCollectionCount, LPDWORD lpCollectDataTimeout, Extbyte * lpUserName, DWORD nMaxUserNameSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CallNamedPipe
#define CallNamedPipe error use qxeCallNamedPipe or CallNamedPipeA/CallNamedPipeW
#endif
BOOL qxeCallNamedPipe (const Extbyte * lpNamedPipeName, LPVOID lpInBuffer, DWORD nInBufferSize, LPVOID lpOutBuffer, DWORD nOutBufferSize, LPDWORD lpBytesRead, DWORD nTimeOut);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef WaitNamedPipe
#define WaitNamedPipe error use qxeWaitNamedPipe or WaitNamedPipeA/WaitNamedPipeW
#endif
BOOL qxeWaitNamedPipe (const Extbyte * lpNamedPipeName, DWORD nTimeOut);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetVolumeLabel
#define SetVolumeLabel error use qxeSetVolumeLabel or SetVolumeLabelA/SetVolumeLabelW
#endif
BOOL qxeSetVolumeLabel (const Extbyte * lpRootPathName, const Extbyte * lpVolumeName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetVolumeInformation
#define GetVolumeInformation error use qxeGetVolumeInformation or GetVolumeInformationA/GetVolumeInformationW
#endif
BOOL qxeGetVolumeInformation (const Extbyte * lpRootPathName, Extbyte * lpVolumeNameBuffer, DWORD nVolumeNameSize, LPDWORD lpVolumeSerialNumber, LPDWORD lpMaximumComponentLength, LPDWORD lpFileSystemFlags, Extbyte * lpFileSystemNameBuffer, DWORD nFileSystemNameSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ClearEventLog
#define ClearEventLog error use qxeClearEventLog or ClearEventLogA/ClearEventLogW
#endif
BOOL qxeClearEventLog (HANDLE hEventLog, const Extbyte * lpBackupFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BackupEventLog
#define BackupEventLog error use qxeBackupEventLog or BackupEventLogA/BackupEventLogW
#endif
BOOL qxeBackupEventLog (HANDLE hEventLog, const Extbyte * lpBackupFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenEventLog
#define OpenEventLog error use qxeOpenEventLog or OpenEventLogA/OpenEventLogW
#endif
HANDLE qxeOpenEventLog (const Extbyte * lpUNCServerName, const Extbyte * lpSourceName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterEventSource
#define RegisterEventSource error use qxeRegisterEventSource or RegisterEventSourceA/RegisterEventSourceW
#endif
HANDLE qxeRegisterEventSource (const Extbyte * lpUNCServerName, const Extbyte * lpSourceName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenBackupEventLog
#define OpenBackupEventLog error use qxeOpenBackupEventLog or OpenBackupEventLogA/OpenBackupEventLogW
#endif
HANDLE qxeOpenBackupEventLog (const Extbyte * lpUNCServerName, const Extbyte * lpFileName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReadEventLog
#define ReadEventLog error use qxeReadEventLog or ReadEventLogA/ReadEventLogW
#endif
BOOL qxeReadEventLog (HANDLE hEventLog, DWORD dwReadFlags, DWORD dwRecordOffset, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, DWORD      * pnBytesRead, DWORD      * pnMinNumberOfBytesNeeded);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReportEvent
#define ReportEvent error use qxeReportEvent or ReportEventA/ReportEventW
#endif
BOOL qxeReportEvent (HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID, PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, const Extbyte *   * lpStrings, LPVOID lpRawData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AccessCheckAndAuditAlarm
#define AccessCheckAndAuditAlarm error use qxeAccessCheckAndAuditAlarm or AccessCheckAndAuditAlarmA/AccessCheckAndAuditAlarmW
#endif
BOOL qxeAccessCheckAndAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, Extbyte * ObjectTypeName, Extbyte * ObjectName, PSECURITY_DESCRIPTOR SecurityDescriptor, DWORD DesiredAccess, PGENERIC_MAPPING GenericMapping, BOOL ObjectCreation, LPDWORD GrantedAccess, LPBOOL AccessStatus, LPBOOL pfGenerateOnClose);

#undef AccessCheckByTypeAndAuditAlarm
#define AccessCheckByTypeAndAuditAlarm error NT 5.0+ only

#undef AccessCheckByTypeResultListAndAuditAlarm
#define AccessCheckByTypeResultListAndAuditAlarm error NT 5.0+ only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectOpenAuditAlarm
#define ObjectOpenAuditAlarm error use qxeObjectOpenAuditAlarm or ObjectOpenAuditAlarmA/ObjectOpenAuditAlarmW
#endif
BOOL qxeObjectOpenAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, Extbyte * ObjectTypeName, Extbyte * ObjectName, PSECURITY_DESCRIPTOR pSecurityDescriptor, HANDLE ClientToken, DWORD DesiredAccess, DWORD GrantedAccess, PPRIVILEGE_SET Privileges, BOOL ObjectCreation, BOOL AccessGranted, LPBOOL GenerateOnClose);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectPrivilegeAuditAlarm
#define ObjectPrivilegeAuditAlarm error use qxeObjectPrivilegeAuditAlarm or ObjectPrivilegeAuditAlarmA/ObjectPrivilegeAuditAlarmW
#endif
BOOL qxeObjectPrivilegeAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, HANDLE ClientToken, DWORD DesiredAccess, PPRIVILEGE_SET Privileges, BOOL AccessGranted);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectCloseAuditAlarm
#define ObjectCloseAuditAlarm error use qxeObjectCloseAuditAlarm or ObjectCloseAuditAlarmA/ObjectCloseAuditAlarmW
#endif
BOOL qxeObjectCloseAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, BOOL GenerateOnClose);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ObjectDeleteAuditAlarm
#define ObjectDeleteAuditAlarm error use qxeObjectDeleteAuditAlarm or ObjectDeleteAuditAlarmA/ObjectDeleteAuditAlarmW
#endif
BOOL qxeObjectDeleteAuditAlarm (const Extbyte * SubsystemName, LPVOID HandleId, BOOL GenerateOnClose);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PrivilegedServiceAuditAlarm
#define PrivilegedServiceAuditAlarm error use qxePrivilegedServiceAuditAlarm or PrivilegedServiceAuditAlarmA/PrivilegedServiceAuditAlarmW
#endif
BOOL qxePrivilegedServiceAuditAlarm (const Extbyte * SubsystemName, const Extbyte * ServiceName, HANDLE ClientToken, PPRIVILEGE_SET Privileges, BOOL AccessGranted);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetFileSecurity
#define SetFileSecurity error use qxeSetFileSecurity or SetFileSecurityA/SetFileSecurityW
#endif
BOOL qxeSetFileSecurity (const Extbyte * lpFileName, SECURITY_INFORMATION SecurityInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileSecurity
#define GetFileSecurity error use qxeGetFileSecurity or GetFileSecurityA/GetFileSecurityW
#endif
BOOL qxeGetFileSecurity (const Extbyte * lpFileName, SECURITY_INFORMATION RequestedInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor, DWORD nLength, LPDWORD lpnLengthNeeded);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindFirstChangeNotification
#define FindFirstChangeNotification error use qxeFindFirstChangeNotification or FindFirstChangeNotificationA/FindFirstChangeNotificationW
#endif
HANDLE qxeFindFirstChangeNotification (const Extbyte * lpPathName, BOOL bWatchSubtree, DWORD dwNotifyFilter);

#undef ReadDirectoryChanges
#define ReadDirectoryChanges error Unicode-only

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef IsBadStringPtr
#define IsBadStringPtr error use qxeIsBadStringPtr or IsBadStringPtrA/IsBadStringPtrW
#endif
BOOL qxeIsBadStringPtr (const Extbyte * lpsz, UINT ucchMax);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupAccountSid
#define LookupAccountSid error use qxeLookupAccountSid or LookupAccountSidA/LookupAccountSidW
#endif
BOOL qxeLookupAccountSid (const Extbyte * lpSystemName, PSID Sid, Extbyte * Name, LPDWORD cbName, Extbyte * ReferencedDomainName, LPDWORD cbReferencedDomainName, PSID_NAME_USE peUse);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupAccountName
#define LookupAccountName error use qxeLookupAccountName or LookupAccountNameA/LookupAccountNameW
#endif
BOOL qxeLookupAccountName (const Extbyte * lpSystemName, const Extbyte * lpAccountName, PSID Sid, LPDWORD cbSid, Extbyte * ReferencedDomainName, LPDWORD cbReferencedDomainName, PSID_NAME_USE peUse);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupPrivilegeValue
#define LookupPrivilegeValue error use qxeLookupPrivilegeValue or LookupPrivilegeValueA/LookupPrivilegeValueW
#endif
BOOL qxeLookupPrivilegeValue (const Extbyte * lpSystemName, const Extbyte * lpName, PLUID lpLuid);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupPrivilegeName
#define LookupPrivilegeName error use qxeLookupPrivilegeName or LookupPrivilegeNameA/LookupPrivilegeNameW
#endif
BOOL qxeLookupPrivilegeName (const Extbyte * lpSystemName, PLUID lpLuid, Extbyte * lpName, LPDWORD cbName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LookupPrivilegeDisplayName
#define LookupPrivilegeDisplayName error use qxeLookupPrivilegeDisplayName or LookupPrivilegeDisplayNameA/LookupPrivilegeDisplayNameW
#endif
BOOL qxeLookupPrivilegeDisplayName (const Extbyte * lpSystemName, const Extbyte * lpName, Extbyte * lpDisplayName, LPDWORD cbDisplayName, LPDWORD lpLanguageId);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BuildCommDCB
#define BuildCommDCB error use qxeBuildCommDCB or BuildCommDCBA/BuildCommDCBW
#endif
BOOL qxeBuildCommDCB (const Extbyte * lpDef, LPDCB lpDCB);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef BuildCommDCBAndTimeouts
#define BuildCommDCBAndTimeouts error use qxeBuildCommDCBAndTimeouts or BuildCommDCBAndTimeoutsA/BuildCommDCBAndTimeoutsW
#endif
BOOL qxeBuildCommDCBAndTimeouts (const Extbyte * lpDef, LPDCB lpDCB, LPCOMMTIMEOUTS lpCommTimeouts);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CommConfigDialog
#define CommConfigDialog error use qxeCommConfigDialog or CommConfigDialogA/CommConfigDialogW
#endif
BOOL qxeCommConfigDialog (const Extbyte * lpszName, HWND hWnd, LPCOMMCONFIG lpCC);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetDefaultCommConfig
#define GetDefaultCommConfig error use qxeGetDefaultCommConfig or GetDefaultCommConfigA/GetDefaultCommConfigW
#endif
BOOL qxeGetDefaultCommConfig (const Extbyte * lpszName, LPCOMMCONFIG lpCC, LPDWORD lpdwSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetDefaultCommConfig
#define SetDefaultCommConfig error use qxeSetDefaultCommConfig or SetDefaultCommConfigA/SetDefaultCommConfigW
#endif
BOOL qxeSetDefaultCommConfig (const Extbyte * lpszName, LPCOMMCONFIG lpCC, DWORD dwSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetComputerName
#define GetComputerName error use qxeGetComputerName or GetComputerNameA/GetComputerNameW
#endif
BOOL qxeGetComputerName (Extbyte * lpBuffer, LPDWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetComputerName
#define SetComputerName error use qxeSetComputerName or SetComputerNameA/SetComputerNameW
#endif
BOOL qxeSetComputerName (const Extbyte * lpComputerName);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetUserName
#define GetUserName error use qxeGetUserName or GetUserNameA/GetUserNameW
#endif
BOOL qxeGetUserName (Extbyte * lpBuffer, LPDWORD nSize);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef LogonUser
#define LogonUser error use qxeLogonUser or LogonUserA/LogonUserW
#endif
BOOL qxeLogonUser (Extbyte * lpszUsername, Extbyte * lpszDomain, Extbyte * lpszPassword, DWORD dwLogonType, DWORD dwLogonProvider, PHANDLE phToken);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateProcessAsUser
#define CreateProcessAsUser error use qxeCreateProcessAsUser or CreateProcessAsUserA/CreateProcessAsUserW
#endif
BOOL qxeCreateProcessAsUser (HANDLE hToken, const Extbyte * lpApplicationName, Extbyte * lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes, LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles, DWORD dwCreationFlags, LPVOID lpEnvironment, const Extbyte * lpCurrentDirectory, LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation);

#undef GetCurrentHwProfile
#define GetCurrentHwProfile error split-sized LPHW_PROFILE_INFO; NT 4.0+ only

#undef GetVersionEx
#define GetVersionEx error split-sized LPOSVERSIONINFO

#undef CreateJobObject
#define CreateJobObject error NT 5.0+ only

#undef OpenJobObject
#define OpenJobObject error NT 5.0+ only


/* Processing file COMMDLG.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetOpenFileName
#define GetOpenFileName error use qxeGetOpenFileName or GetOpenFileNameA/GetOpenFileNameW
#endif
BOOL   qxeGetOpenFileName (LPOPENFILENAMEW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetSaveFileName
#define GetSaveFileName error use qxeGetSaveFileName or GetSaveFileNameA/GetSaveFileNameW
#endif
BOOL   qxeGetSaveFileName (LPOPENFILENAMEW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetFileTitle
#define GetFileTitle error use qxeGetFileTitle or GetFileTitleA/GetFileTitleW
#endif
short  qxeGetFileTitle (const Extbyte * arg1, Extbyte * arg2, WORD arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ChooseColor
#define ChooseColor error use qxeChooseColor or ChooseColorA/ChooseColorW
#endif
BOOL   qxeChooseColor (LPCHOOSECOLORW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindText
#define FindText error use qxeFindText or FindTextA/FindTextW
#endif
HWND   qxeFindText (LPFINDREPLACEW arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ReplaceText
#define ReplaceText error use qxeReplaceText or ReplaceTextA/ReplaceTextW
#endif
HWND   qxeReplaceText (LPFINDREPLACEW arg1);

#undef AfxReplaceText
#define AfxReplaceText error mac only

#undef ChooseFont
#define ChooseFont error split-sized LPLOGFONT in LPCHOOSEFONT

/* Skipping PrintDlg because LPPRINTDLG with split-sized DEVMODE handle */

/* Skipping PageSetupDlg because LPPAGESETUPDLG with split-sized DEVMODE handle */


/* Processing file SHLOBJ.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHGetPathFromIDList
#define SHGetPathFromIDList error use qxeSHGetPathFromIDList or SHGetPathFromIDListA/SHGetPathFromIDListW
#endif
BOOL qxeSHGetPathFromIDList (LPCITEMIDLIST pidl, Extbyte * pszPath);

/* Skipping SHGetSpecialFolderPath because error in Cygwin prototype, missing from Cygwin libraries */

/* Skipping SHBrowseForFolder because need to intercept callback for SendMessage */

/* Skipping SHGetDataFromIDList because split-sized WIN32_FIND_DATA or split-simple NETRESOURCE, missing from Cygwin libraries */


/* Processing file IME.H */

#undef SendIMEMessageEx
#define SendIMEMessageEx error obsolete, no docs available


/* Processing file WINGDI.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef AddFontResource
#define AddFontResource error use qxeAddFontResource or AddFontResourceA/AddFontResourceW
#endif
int qxeAddFontResource (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyMetaFile
#define CopyMetaFile error use qxeCopyMetaFile or CopyMetaFileA/CopyMetaFileW
#endif
HMETAFILE qxeCopyMetaFile (HMETAFILE arg1, const Extbyte * arg2);

/* Skipping CreateDC because split-sized DEVMODE */

/* Skipping CreateFontIndirect because split-sized LOGFONT */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFont
#define CreateFont error use qxeCreateFont or CreateFontA/CreateFontW
#endif
HFONT qxeCreateFont (int arg1, int arg2, int arg3, int arg4, int arg5, DWORD arg6, DWORD arg7, DWORD arg8, DWORD arg9, DWORD arg10, DWORD arg11, DWORD arg12, DWORD arg13, const Extbyte * arg14);

/* Skipping CreateIC because split-sized DEVMODE */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateMetaFile
#define CreateMetaFile error use qxeCreateMetaFile or CreateMetaFileA/CreateMetaFileW
#endif
HDC qxeCreateMetaFile (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateScalableFontResource
#define CreateScalableFontResource error use qxeCreateScalableFontResource or CreateScalableFontResourceA/CreateScalableFontResourceW
#endif
BOOL qxeCreateScalableFontResource (DWORD arg1, const Extbyte * arg2, const Extbyte * arg3, const Extbyte * arg4);

/* Skipping DeviceCapabilities because split-sized DEVMODE */

/* Skipping EnumFontFamiliesEx because split-complex FONTENUMPROC; NT 4.0+ only */

#undef EnumFontFamilies
#define EnumFontFamilies error split-complex FONTENUMPROC

#undef EnumFonts
#define EnumFonts error split-complex FONTENUMPROC

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharWidth
#define GetCharWidth error use qxeGetCharWidth or GetCharWidthA/GetCharWidthW
#endif
BOOL qxeGetCharWidth (HDC arg1, UINT arg2, UINT arg3, LPINT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharWidth32
#define GetCharWidth32 error use qxeGetCharWidth32 or GetCharWidth32A/GetCharWidth32W
#endif
BOOL qxeGetCharWidth32 (HDC arg1, UINT arg2, UINT arg3, LPINT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharWidthFloat
#define GetCharWidthFloat error use qxeGetCharWidthFloat or GetCharWidthFloatA/GetCharWidthFloatW
#endif
BOOL   qxeGetCharWidthFloat (HDC arg1, UINT arg2, UINT arg3, PFLOAT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharABCWidths
#define GetCharABCWidths error use qxeGetCharABCWidths or GetCharABCWidthsA/GetCharABCWidthsW
#endif
BOOL   qxeGetCharABCWidths (HDC arg1, UINT arg2, UINT arg3, LPABC arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharABCWidthsFloat
#define GetCharABCWidthsFloat error use qxeGetCharABCWidthsFloat or GetCharABCWidthsFloatA/GetCharABCWidthsFloatW
#endif
BOOL   qxeGetCharABCWidthsFloat (HDC arg1, UINT arg2, UINT arg3, LPABCFLOAT arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetGlyphOutline
#define GetGlyphOutline error use qxeGetGlyphOutline or GetGlyphOutlineA/GetGlyphOutlineW
#endif
DWORD qxeGetGlyphOutline (HDC arg1, UINT arg2, UINT arg3, LPGLYPHMETRICS arg4, DWORD arg5, LPVOID arg6, CONST MAT2 * arg7);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetMetaFile
#define GetMetaFile error use qxeGetMetaFile or GetMetaFileA/GetMetaFileW
#endif
HMETAFILE qxeGetMetaFile (const Extbyte * arg1);

#undef GetOutlineTextMetrics
#define GetOutlineTextMetrics error split-sized LPOUTLINETEXTMETRIC

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextExtentPoint
#define GetTextExtentPoint error use qxeGetTextExtentPoint or GetTextExtentPointA/GetTextExtentPointW
#endif
BOOL   qxeGetTextExtentPoint (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextExtentPoint32
#define GetTextExtentPoint32 error use qxeGetTextExtentPoint32 or GetTextExtentPoint32A/GetTextExtentPoint32W
#endif
BOOL   qxeGetTextExtentPoint32 (HDC arg1, const Extbyte * arg2, int arg3, LPSIZE arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextExtentExPoint
#define GetTextExtentExPoint error use qxeGetTextExtentExPoint or GetTextExtentExPointA/GetTextExtentExPointW
#endif
BOOL   qxeGetTextExtentExPoint (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPINT arg5, LPINT arg6, LPSIZE arg7);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetCharacterPlacement
#define GetCharacterPlacement error use qxeGetCharacterPlacement or GetCharacterPlacementA/GetCharacterPlacementW
#endif
DWORD qxeGetCharacterPlacement (HDC arg1, const Extbyte * arg2, int arg3, int arg4, LPGCP_RESULTSW arg5, DWORD arg6);

#undef GetGlyphIndices
#define GetGlyphIndices error NT 5.0+ only

#undef AddFontResourceEx
#define AddFontResourceEx error NT 5.0+ only

#undef RemoveFontResourceEx
#define RemoveFontResourceEx error NT 5.0+ only

#undef CreateFontIndirectEx
#define CreateFontIndirectEx error split-sized ENUMLOGFONTEXDV; NT 5.0+ only

/* Skipping ResetDC because split-sized DEVMODE */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RemoveFontResource
#define RemoveFontResource error use qxeRemoveFontResource or RemoveFontResourceA/RemoveFontResourceW
#endif
BOOL qxeRemoveFontResource (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CopyEnhMetaFile
#define CopyEnhMetaFile error use qxeCopyEnhMetaFile or CopyEnhMetaFileA/CopyEnhMetaFileW
#endif
HENHMETAFILE qxeCopyEnhMetaFile (HENHMETAFILE arg1, const Extbyte * arg2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateEnhMetaFile
#define CreateEnhMetaFile error use qxeCreateEnhMetaFile or CreateEnhMetaFileA/CreateEnhMetaFileW
#endif
HDC qxeCreateEnhMetaFile (HDC arg1, const Extbyte * arg2, CONST RECT * arg3, const Extbyte * arg4);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetEnhMetaFile
#define GetEnhMetaFile error use qxeGetEnhMetaFile or GetEnhMetaFileA/GetEnhMetaFileW
#endif
HENHMETAFILE qxeGetEnhMetaFile (const Extbyte * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetEnhMetaFileDescription
#define GetEnhMetaFileDescription error use qxeGetEnhMetaFileDescription or GetEnhMetaFileDescriptionA/GetEnhMetaFileDescriptionW
#endif
UINT qxeGetEnhMetaFileDescription (HENHMETAFILE arg1, UINT arg2, Extbyte * arg3);

/* Skipping GetTextMetrics because split-sized LPTEXTMETRIC */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef StartDoc
#define StartDoc error use qxeStartDoc or StartDocA/StartDocW
#endif
int qxeStartDoc (HDC arg1, CONST DOCINFOW * arg2);

/* Skipping GetObject because split-sized LOGFONT */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef TextOut
#define TextOut error use qxeTextOut or TextOutA/TextOutW
#endif
BOOL qxeTextOut (HDC arg1, int arg2, int arg3, const Extbyte * arg4, int arg5);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtTextOut
#define ExtTextOut error use qxeExtTextOut or ExtTextOutA/ExtTextOutW
#endif
BOOL qxeExtTextOut (HDC arg1, int arg2, int arg3, UINT arg4, CONST RECT * arg5, const Extbyte * arg6, UINT arg7, CONST INT * arg8);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PolyTextOut
#define PolyTextOut error use qxePolyTextOut or PolyTextOutA/PolyTextOutW
#endif
BOOL qxePolyTextOut (HDC arg1, CONST POLYTEXTW * arg2, int arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextFace
#define GetTextFace error use qxeGetTextFace or GetTextFaceA/GetTextFaceW
#endif
int qxeGetTextFace (HDC arg1, int arg2, Extbyte * arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetKerningPairs
#define GetKerningPairs error use qxeGetKerningPairs or GetKerningPairsA/GetKerningPairsW
#endif
DWORD qxeGetKerningPairs (HDC arg1, DWORD arg2, LPKERNINGPAIR arg3);

#undef GetLogColorSpace
#define GetLogColorSpace error split-sized LPLOGCOLORSPACE; NT 4.0+ only

#undef CreateColorSpace
#define CreateColorSpace error split-sized LPLOGCOLORSPACE; NT 4.0+ only

/* Skipping GetICMProfile because NT 4.0+ only, error in Cygwin prototype */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetICMProfile
#define SetICMProfile error use qxeSetICMProfile or SetICMProfileA/SetICMProfileW
#endif
BOOL qxeSetICMProfile (HDC arg1, Extbyte * arg2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumICMProfiles
#define EnumICMProfiles error use qxeEnumICMProfiles or EnumICMProfilesA/EnumICMProfilesW
#endif
int qxeEnumICMProfiles (HDC arg1, ICMENUMPROCW arg2, LPARAM arg3);

/* Skipping UpdateICMRegKey because NT 4.0+ only, error in Cygwin prototype */

#undef wglUseFontBitmaps
#define wglUseFontBitmaps error causes link error

#undef wglUseFontOutlines
#define wglUseFontOutlines error causes link error

