/* Copyright (C) 2000 Free Software Foundation, Inc.
   Copyright (C) 2000, 2001, 2002, 2004 Ben Wing.

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
   
   Created May 2000 by Andy Piper.
   Windows-Mule stuff added by Ben Wing, 2000-2001.
   September 2001 Ben Wing reorganized and included nt.h and ntheap.h into
   this file; comments in those files say:
      * Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au>
      * Sync'ed with Emacs 19.34.6 by Marc Paquette <marcpa@cam.org>
        (Note: Sync messages from Marc Paquette may indicate
        incomplete synching, so beware.)
      (in ntheap.h only) Geoff Voelker (voelker@cs.washington.edu) 7-29-94

*/

#ifndef INCLUDED_syswindows_h_
#define INCLUDED_syswindows_h_

/* See win32.c for info about the different Windows files in XEmacs. */

/* ------------------------- Basic includes ------------------------- */

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#if defined (CYGWIN) || defined (MINGW)
# define CYGWIN_HEADERS
# ifndef _WIN32_IE
#  define _WIN32_IE 0x0400
# endif

BEGIN_C_DECLS

/* Fucking GCC complains about "no previous prototype" for inline
   functions.  DUH!  See DECLARE_INLINE_HEADER. */
extern __inline void *GetCurrentFiber (void);
extern __inline void *GetFiberData (void);

END_C_DECLS

#endif

/* Defines for COM so it's usable in both C and C++.  XECOMCALLn() calls a
   method with the specified number of parameters.  XECOMID() surrounds a
   class or interface name when passed to CoCreateInstance, a
   QueryInterface method, or the like. */
#ifdef __cplusplus
#define XECOMCALL0(cl, meth) ((cl)->meth ())
#define XECOMCALL1(cl, meth, a1) ((cl)->meth (a1))
#define XECOMCALL2(cl, meth, a1, a2) ((cl)->meth (a1, a2))
#define XECOMCALL3(cl, meth, a1, a2, a3) ((cl)->meth (a1, a2, a3))
#define XECOMCALL4(cl, meth, a1, a2, a3, a4) ((cl)->meth (a1, a2, a3, a4))
#define XECOMID(id) (id)
#else
#define XECOMCALL0(cl, meth) ((cl)->lpVtbl->meth (cl))
#define XECOMCALL1(cl, meth, a1) ((cl)->lpVtbl->meth (cl, a1))
#define XECOMCALL2(cl, meth, a1, a2) ((cl)->lpVtbl->meth (cl, a1, a2))
#define XECOMCALL3(cl, meth, a1, a2, a3) ((cl)->lpVtbl->meth (cl, a1, a2, a3))
#define XECOMCALL4(cl, meth, a1, a2, a3, a4) \
		((cl)->lpVtbl->meth (cl, a1, a2, a3, a4))
#define XECOMID(id) (&(id))
#endif		

#define OEMRESOURCE /* Define OCR_ and friend constants */
#include <windows.h>

#if defined (WIN32_LEAN_AND_MEAN)
# ifdef HAVE_X_WINDOWS
/* Christ almighty.  The problems you get when combining two large code bases,
   neither with any respect for namespace purity. */
#  undef Status
# endif
# include <winspool.h>
# ifdef HAVE_X_WINDOWS
#  define Status int
# endif
# include <mmsystem.h>
# include <shlobj.h>
# include <shellapi.h>
# include <ddeml.h>
#endif

#include <lmaccess.h> /* next three for NetUserEnum and friends */
#include <lmapibuf.h>
#include <lmerr.h>
#include <lmcons.h> /* for UNLEN and possibly other constants */

#include <cderr.h>
#include <commdlg.h>

#ifdef NEED_MSWINDOWS_COMMCTRL
#include <commctrl.h>
#endif
#ifdef NEED_MSWINDOWS_OBJBASE
#include <objbase.h>
#endif
#ifdef NEED_MSWINDOWS_SHLOBJ
#include <shlobj.h>
#endif

#include <zmouse.h> /* WHEEL_PAGESCROLL under Cygwin */

#include <wchar.h>

/* ------------------------- Cygwin header brokenness ---------------------- */

#ifdef CYGWIN_HEADERS

#include <cygwin/stat.h> /* for struct stat */
#include <w32api.h> /* for version info */

/* Test for a specific version of w32api */
#define W32API_VER(major,minor) (((major) << 16) + (minor))
#define W32API_INSTALLED_VER \
  W32API_VER (__W32API_MAJOR_VERSION, __W32API_MINOR_VERSION)

/* Various brokennesses in various versions of Cygwin */

/* windows.h defines. */
#ifndef SPI_GETWHEELSCROLLLINES
#define SPI_GETWHEELSCROLLLINES 104
#endif

/* commctrl.h defines. */
#ifndef TB_SETIMAGELIST
#define TB_SETIMAGELIST (WM_USER + 48)
#endif
#ifndef TB_GETIMAGELIST
#define TB_GETIMAGELIST (WM_USER + 49)
#endif
#ifndef TB_SETDISABLEDIMAGELIST
#define TB_SETDISABLEDIMAGELIST (WM_USER + 54)
#endif
#ifndef TB_GETDISABLEDIMAGELIST
#define TB_GETDISABLEDIMAGELIST (WM_USER + 55)
#endif
#ifndef TB_SETPADDING
#define TB_SETPADDING   (WM_USER + 87)
#endif
#ifndef TB_GETBUTTONINFOA
#define TB_GETBUTTONINFOA  (WM_USER + 65)
#endif
#ifndef TB_GETBUTTONINFOW
#define TB_GETBUTTONINFOW  (WM_USER + 63)
#endif
#ifndef TB_SETBUTTONINFOA
#define TB_SETBUTTONINFOA  (WM_USER + 66)
#endif
#ifndef TB_SETBUTTONINFOW
#define TB_SETBUTTONINFOW  (WM_USER + 64)
#endif
#ifndef TB_INSERTBUTTONA
#define TB_INSERTBUTTONA  (WM_USER + 21)
#endif
#ifndef TB_INSERTBUTTONW
#define TB_INSERTBUTTONW  (WM_USER + 67)
#endif
#ifndef TB_ADDBUTTONSA
#define TB_ADDBUTTONSA  (WM_USER + 20)
#endif
#ifndef TB_ADDBUTTONSW
#define TB_ADDBUTTONSW  (WM_USER + 68)
#endif
#ifndef LVM_SETBKIMAGEA
#define LVM_SETBKIMAGEA  (LVM_FIRST + 68)
#endif
#ifndef LVM_SETBKIMAGEW
#define LVM_SETBKIMAGEW  (LVM_FIRST + 138)
#endif
#ifndef LVM_GETBKIMAGEA
#define LVM_GETBKIMAGEA  (LVM_FIRST + 69)
#endif
#ifndef LVM_GETBKIMAGEW
#define LVM_GETBKIMAGEW  (LVM_FIRST + 139)
#endif
#ifndef WC_COMBOBOXEXW
#define WC_COMBOBOXEXW L"ComboBoxEx32"
#endif
#ifndef CBEM_INSERTITEMA
#define CBEM_INSERTITEMA  (WM_USER + 1)
#endif
#ifndef CBEM_INSERTITEMW
#define CBEM_INSERTITEMW  (WM_USER + 11)
#endif
#ifndef CBEM_SETITEMA
#define CBEM_SETITEMA  (WM_USER + 5)
#endif
#ifndef CBEM_SETITEMW
#define CBEM_SETITEMW  (WM_USER + 12)
#endif
#ifndef CBEM_GETITEMA
#define CBEM_GETITEMA  (WM_USER + 4)
#endif
#ifndef CBEM_GETITEMW
#define CBEM_GETITEMW  (WM_USER + 13)
#endif
#ifndef HDN_GETDISPINFOA
#define HDN_GETDISPINFOA        (HDN_FIRST - 9)
#endif
#ifndef HDN_GETDISPINFOW
#define HDN_GETDISPINFOW        (HDN_FIRST - 29)
#endif
#ifndef TBN_GETDISPINFOA
#define TBN_GETDISPINFOA        (TBN_FIRST - 16)
#endif
#ifndef TBN_GETDISPINFOW
#define TBN_GETDISPINFOW        (TBN_FIRST - 17)
#endif
#ifndef TBN_GETINFOTIPA
#define TBN_GETINFOTIPA         (TBN_FIRST - 18)
#endif
#ifndef TBN_GETINFOTIPW
#define TBN_GETINFOTIPW         (TBN_FIRST - 19)
#endif
#ifndef TTN_GETDISPINFOA
#define TTN_GETDISPINFOA        (TTN_FIRST - 0)
#endif
#ifndef TTN_GETDISPINFOW
#define TTN_GETDISPINFOW        (TTN_FIRST - 10)
#endif

#if (_WIN32_IE >= 0x0400)

#ifndef LVN_GETINFOTIPA
#define LVN_GETINFOTIPA         (LVN_FIRST - 57)
#endif
#ifndef LVN_GETINFOTIPW
#define LVN_GETINFOTIPW         (LVN_FIRST - 58)
#endif
#ifndef TVN_GETINFOTIPA
#define TVN_GETINFOTIPA         (TVN_FIRST - 13)
#endif
#ifndef TVN_GETINFOTIPW
#define TVN_GETINFOTIPW         (TVN_FIRST - 14)
#endif
#ifndef CBEN_GETDISPINFOA
#define CBEN_GETDISPINFOA       (CBEN_FIRST - 0)
#endif
#ifndef CBEN_GETDISPINFOW
#define CBEN_GETDISPINFOW       (CBEN_FIRST - 7)
#endif
#ifndef CBEN_DRAGBEGINA
#define CBEN_DRAGBEGINA			 (CBEN_FIRST - 8)
#endif
#ifndef CBEN_DRAGBEGINW
#define CBEN_DRAGBEGINW			 (CBEN_FIRST - 9)
#endif

#endif /* (_WIN32_IE >= 0x0400) */

#ifndef CBEN_ENDEDITA
#define CBEN_ENDEDITA            (CBEN_FIRST - 5)
#endif
#ifndef CBEN_ENDEDITW
#define CBEN_ENDEDITW            (CBEN_FIRST - 6)
#endif

#ifndef CBEMAXSTRLEN
#define CBEMAXSTRLEN 260
#endif

#ifndef NMCBEENDEDIT

typedef struct
{
  NMHDR hdr;
  BOOL fChanged;
  int iNewSelection;
  WCHAR szText[CBEMAXSTRLEN];
  int iWhy;
} NMCBEENDEDITW, *LPNMCBEENDEDITW, *PNMCBEENDEDITW;

typedef struct
{
  NMHDR hdr;
  BOOL fChanged;
  int iNewSelection;
  char szText[CBEMAXSTRLEN];
  int iWhy;
} NMCBEENDEDITA, *LPNMCBEENDEDITA,*PNMCBEENDEDITA;

#endif /* not NMCBEENDEDIT */

#if (_WIN32_IE >= 0x0400)

#ifndef NMCBEDRAGBEGIN

typedef struct
{
  NMHDR hdr;
  int   iItemid;
  WCHAR szText[CBEMAXSTRLEN];
} NMCBEDRAGBEGINW, *LPNMCBEDRAGBEGINW, *PNMCBEDRAGBEGINW;

typedef struct
{
  NMHDR hdr;
  int   iItemid;
  char szText[CBEMAXSTRLEN];
} NMCBEDRAGBEGINA, *LPNMCBEDRAGBEGINA, *PNMCBEDRAGBEGINA;

#endif /* not NMCBEDRAGBEGIN */

typedef struct tagNMDATETIMEFORMATA
{
  NMHDR nmhdr;
  LPCSTR  pszFormat;
  SYSTEMTIME st;
  LPCSTR pszDisplay;
  CHAR szDisplay[64];
} NMDATETIMEFORMATA, FAR * LPNMDATETIMEFORMATA;

typedef struct tagNMDATETIMEFORMATW
{
  NMHDR nmhdr;
  LPCWSTR pszFormat;
  SYSTEMTIME st;
  LPCWSTR pszDisplay;
  WCHAR szDisplay[64];
} NMDATETIMEFORMATW, FAR * LPNMDATETIMEFORMATW;

#if W32API_INSTALLED_VER < W32API_VER(2,2)

typedef struct tagNMTTDISPIFNOA
{
  NMHDR hdr;
  LPSTR lpszText;
  char szText[80];
  HINSTANCE hinst;
  UINT uFlags;
#if (_WIN32_IE >= 0x0300)
  LPARAM lParam;
#endif
} NMTTDISPINFOA, FAR *LPNMTTDISPINFOA;

typedef struct tagNMTTDISPINFOW
{
  NMHDR hdr;
  LPWSTR lpszText;
  WCHAR szText[80];
  HINSTANCE hinst;
  UINT uFlags;
#if (_WIN32_IE >= 0x0300)
  LPARAM lParam;
#endif
} NMTTDISPINFOW, FAR *LPNMTTDISPINFOW;

#endif /* W32API_INSTALLED_VER < W32API_VER(2,2) */

#endif /* (_WIN32_IE >= 0x0400) */

/* shlobj.h defines */
#ifndef BFFM_VALIDATEFAILEDA
#define BFFM_VALIDATEFAILEDA    3
#endif
#ifndef BFFM_VALIDATEFAILEDW
#define BFFM_VALIDATEFAILEDW    4
#endif
#ifndef BFFM_SETSELECTIONA
#define BFFM_SETSELECTIONA      (WM_USER + 102)
#endif
#ifndef BFFM_SETSELECTIONW
#define BFFM_SETSELECTIONW      (WM_USER + 103)
#endif
#ifndef BFFM_SETSTATUSTEXTA
#define BFFM_SETSTATUSTEXTA     (WM_USER + 100)
#endif
#ifndef BFFM_SETSTATUSTEXTW
#define BFFM_SETSTATUSTEXTW     (WM_USER + 104)
#endif
#ifndef SHARD_PATHA
#define SHARD_PATHA     2
#endif
#ifndef SHARD_PATHW
#define SHARD_PATHW     3
#endif
#ifndef SHCNF_PATHA
#define SHCNF_PATHA     1
#endif
#ifndef SHCNF_PATHW
#define SHCNF_PATHW     5
#endif
#ifndef SHCNF_PRINTERA
#define SHCNF_PRINTERA     2
#endif
#ifndef SHCNF_PRINTERW
#define SHCNF_PRINTERW     6
#endif
#ifndef BFFM_VALIDATEFAILED
#ifdef UNICODE
#define BFFM_VALIDATEFAILED BFFM_VALIDATEFAILEDW
#else
#define BFFM_VALIDATEFAILED BFFM_VALIDATEFAILEDA
#endif
#endif

/* winnls.h defines */
#ifndef MAC_CHARSET
#define MAC_CHARSET 		77
#endif
#ifndef LOCALE_RETURN_NUMBER
#define LOCALE_RETURN_NUMBER	0x20000000
#endif

/* OEM resources */
#ifndef OCR_ICOCUR
#define OCR_ICOCUR          32647
#define OIC_SAMPLE          32512
#define OIC_HAND            32513
#define OIC_QUES            32514
#define OIC_BANG            32515
#define OIC_NOTE            32516
#define OIC_WINLOGO         32517
#endif

/* More Cygwin stupidity: Current w32api's winuser.h has IME message
   constants and they conflict with imm.h. (NOTE: Currently fixed, but
   I'm sure the problems were present post 1.0.) */
#undef WM_IME_STARTCOMPOSITION
#undef WM_IME_ENDCOMPOSITION
#undef WM_IME_COMPOSITION
#undef WM_IME_KEYLAST
#undef WM_IME_SETCONTEXT
#undef WM_IME_NOTIFY
#undef WM_IME_CONTROL
#undef WM_IME_COMPOSITIONFULL
#undef WM_IME_SELECT
#undef WM_IME_CHAR
#undef WM_IME_KEYDOWN
#undef WM_IME_KEYUP

#include <imm.h>

#if W32API_INSTALLED_VER < W32API_VER(2,4)
typedef struct _SHQUERYRBINFO
{
  DWORD cbSize;
  __int64 i64Size;
  __int64 i64NumItems;
} SHQUERYRBINFO, *LPSHQUERYRBINFO;
#endif

typedef LPCDLGTEMPLATE LPCDLGTEMPLATEW;
typedef LPCDLGTEMPLATE LPCDLGTEMPLATEA;

#else /* !CYGWIN_HEADERS */
#define W32API_VER(major,minor) 0
#define W32API_INSTALLED_VER 0
#endif /* CYGWIN_HEADERS */

/* Not in VC 6 */
#ifndef BIF_NEWDIALOGSTYLE
#define BIF_NEWDIALOGSTYLE 64
#endif

#ifdef CYGWIN

/* All but wcscmp and wcslen left out of Cygwin headers -- but present
   in /usr/include/mingw/string.h! */
wchar_t* wcscat (wchar_t*, const wchar_t*);
wchar_t* wcschr (const wchar_t*, wchar_t);
int	wcscoll (const wchar_t*, const wchar_t*);
wchar_t* wcscpy (wchar_t*, const wchar_t*);
wchar_t* wcsdup (const wchar_t*);
size_t	wcscspn (const wchar_t*, const wchar_t*);
/* Note: No wcserror in CRTDLL. */
wchar_t* wcsncat (wchar_t*, const wchar_t*, size_t);
int	wcsncmp (const wchar_t*, const wchar_t*, size_t);
wchar_t* wcsncpy (wchar_t*, const wchar_t*, size_t);
wchar_t* wcspbrk (const wchar_t*, const wchar_t*);
wchar_t* wcsrchr (const wchar_t*, wchar_t);
size_t	wcsspn (const wchar_t*, const wchar_t*);
wchar_t* wcsstr (const wchar_t*, const wchar_t*);
wchar_t* wcstok (wchar_t*, const wchar_t*);
size_t	wcsxfrm (wchar_t*, const wchar_t*, size_t);

#endif /* CYGWIN */

/* ------------------------- Unicode encapsulation ------------------------- */

/* See intl-win32.c for more information about Unicode-encapsulation */

#define ERROR_WHEN_NONINTERCEPTED_FUNS_USED

#include "intl-auto-encap-win32.h"

/* would be encapsulatable but for parsing problems */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefWindowProc
#define DefWindowProc error use qxeDefWindowProc or DefWindowProcA/DefWindowProcW
#endif
LRESULT qxeDefWindowProc (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CallWindowProc
#define CallWindowProc error use qxeCallWindowProc or CallWindowProcA/CallWindowProcW
#endif
LRESULT qxeCallWindowProc (WNDPROC lpPrevWndFunc, HWND hWnd, UINT Msg,
			   WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefDlgProc
#define DefDlgProc error use qxeDefDlgProc or DefDlgProcA/DefDlgProcW
#endif
LRESULT qxeDefDlgProc (HWND hDlg, UINT Msg, WPARAM wParam, LPARAM lParam);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SetWindowsHook
#define SetWindowsHook error use qxeSetWindowsHook or SetWindowsHookA/SetWindowsHookW
#endif
HHOOK qxeSetWindowsHook (int nFilterType, HOOKPROC pfnFilterProc);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DefMDIChildProc
#define DefMDIChildProc error use qxeDefMDIChildProc or DefMDIChildProcA/DefMDIChildProcW
#endif
LRESULT qxeDefMDIChildProc (HWND hWnd, UINT uMsg, WPARAM wParam,
			    LPARAM lParam);

#undef GetEnvironmentStrings
#undef GetEnvironmentStringsA
#define GetEnvironmentStringsA GetEnvironmentStrings
Extbyte * qxeGetEnvironmentStrings (void);

/* would be encapsulatable but for Cygwin problems */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DdeCreateStringHandle
#define DdeCreateStringHandle error use qxeDdeCreateStringHandle or DdeCreateStringHandleA/DdeCreateStringHandleW
#endif
HSZ qxeDdeCreateStringHandle (DWORD idInst, const Extbyte * psz, int iCodePage);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegConnectRegistry
#define RegConnectRegistry error use qxeRegConnectRegistry or RegConnectRegistryA/RegConnectRegistryW
#endif
LONG qxeRegConnectRegistry (const Extbyte * lpMachineName, HKEY hKey, PHKEY phkResult);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ExtractIconEx
#define ExtractIconEx error use qxeExtractIconEx or ExtractIconExA/ExtractIconExW
#endif
UINT qxeExtractIconEx (const Extbyte * lpszFile, int nIconIndex, HICON FAR * phiconLarge, HICON FAR * phiconSmall, UINT nIcons);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetICMProfile
#define GetICMProfile error use qxeGetICMProfile or GetICMProfileA/GetICMProfileW
#endif
BOOL qxeGetICMProfile (HDC arg1, LPDWORD arg2, Extbyte * arg3);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef UpdateICMRegKey
#define UpdateICMRegKey error use qxeUpdateICMRegKey or UpdateICMRegKeyA/UpdateICMRegKeyW
#endif
BOOL qxeUpdateICMRegKey (DWORD arg1, Extbyte * arg2, Extbyte * arg3, UINT arg4);

/* would be encapsulatable but for header changes in different versions of VC++ */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceTypes
#define EnumResourceTypes error_use_qxeEnumResourceTypes_or_EnumResourceTypesA_and_EnumResourceTypesW
#endif
#if MSC_VERSION >= 1300
BOOL qxeEnumResourceTypes (HMODULE hModule, ENUMRESTYPEPROCW lpEnumFunc, LONG lParam);
#else
BOOL qxeEnumResourceTypes (HMODULE hModule, ENUMRESTYPEPROC lpEnumFunc, LONG lParam);
#endif

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceNames
#define EnumResourceNames error_use_qxeEnumResourceNames_or_EnumResourceNamesA_and_EnumResourceNamesW
#endif
#if MSC_VERSION >= 1300
BOOL qxeEnumResourceNames (HMODULE hModule, const Extbyte * lpType, ENUMRESNAMEPROCW lpEnumFunc, LONG lParam);
#else
BOOL qxeEnumResourceNames (HMODULE hModule, const Extbyte * lpType, ENUMRESNAMEPROC lpEnumFunc, LONG lParam);
#endif

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumResourceLanguages
#define EnumResourceLanguages error_use_qxeEnumResourceLanguages_or_EnumResourceLanguagesA_and_EnumResourceLanguagesW
#endif
#if MSC_VERSION >= 1300
BOOL qxeEnumResourceLanguages (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, ENUMRESLANGPROCW lpEnumFunc, LONG lParam);
#else
BOOL qxeEnumResourceLanguages (HMODULE hModule, const Extbyte * lpType, const Extbyte * lpName, ENUMRESLANGPROC lpEnumFunc, LONG lParam);
#endif

/* files */
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindFirstFile
#define FindFirstFile error use qxeFindFirstFile or FindFirstFileA/FindFirstFileW
#endif
HANDLE qxeFindFirstFile (const Extbyte *lpFileName,
			 WIN32_FIND_DATAW *lpFindFileData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef FindNextFile
#define FindNextFile error use qxeFindNextFile or FindNextFileA/FindNextFileW
#endif
BOOL qxeFindNextFile (HANDLE hFindFile, WIN32_FIND_DATAW *lpFindFileData);

/* shell */
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHGetFileInfo
#define SHGetFileInfo error use qxeSHGetFileInfo or SHGetFileInfoA/SHGetFileInfoW
#endif
DWORD qxeSHGetFileInfo (const Extbyte *pszPath, DWORD dwFileAttributes,
			SHFILEINFOW *psfi, UINT cbFileInfo, UINT uFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHBrowseForFolder
#define SHBrowseForFolder error use qxeSHBrowseForFolder or SHBrowseForFolderA/SHBrowseForFolderW
#endif
LPITEMIDLIST qxeSHBrowseForFolder (LPBROWSEINFOW lpbi);

/* Not Unicode-split */
VOID qxeSHAddToRecentDocs (UINT uFlags, LPCVOID pv);

/* Not Unicode-split */
VOID qxeSHChangeNotify (LONG wEventId, UINT uFlags, LPCVOID dwItem1,
			LPCVOID dwItem2);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SHGetDataFromIDList
#define SHGetDataFromIDList error use qxeSHGetDataFromIDList or SHGetDataFromIDListA/SHGetDataFromIDListW
#endif
HRESULT qxeSHGetDataFromIDList (IShellFolder *psf, LPCITEMIDLIST pidl,
				int nFormat, PVOID pv, int cb);

/* devmode */
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateDC
#define CreateDC error use qxeCreateDC or CreateDCA/CreateDCW
#endif
HDC qxeCreateDC (const Extbyte *lpszDriver, const Extbyte *lpszDevice,
		 const Extbyte *lpszOutput, CONST DEVMODEW *lpInitData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ResetDC
#define ResetDC error use qxeResetDC or ResetDCA/ResetDCW
#endif
HDC qxeResetDC (HDC hdc, CONST DEVMODEW *lpInitData);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef OpenPrinter
#define OpenPrinter error use qxeOpenPrinter or OpenPrinterA/OpenPrinterW
#endif
DWORD qxeOpenPrinter (Extbyte *pPrinterName, LPHANDLE phPrinter,
		      LPPRINTER_DEFAULTSW pDefaultconst);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef DocumentProperties
#define DocumentProperties error use qxeDocumentProperties or DocumentPropertiesA/DocumentPropertiesW
#endif
LONG qxeDocumentProperties (HWND hWnd, HANDLE hPrinter, Extbyte *pDeviceName,
			    DEVMODEW *pDevModeOutput, DEVMODEW *pDevModeInput,
			    DWORD fMode);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PrintDlg
#define PrintDlg error use qxePrintDlg or PrintDlgA/PrintDlgW
#endif
BOOL qxePrintDlg (PRINTDLGW *lppd);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef PageSetupDlg
#define PageSetupDlg error use qxePageSetupDlg or PageSetupDlgA/PageSetupDlgW
#endif
BOOL qxePageSetupDlg (PAGESETUPDLGW *lppd);

/* fonts */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef EnumFontFamiliesEx
#define EnumFontFamiliesEx error use qxeEnumFontFamiliesEx or EnumFontFamiliesExA/EnumFontFamiliesExW
#endif
int qxeEnumFontFamiliesEx (HDC hdc, LOGFONTW *lpLogfont,
			   FONTENUMPROCW lpEnumFontFamProc, LPARAM lParam,
			   DWORD dwFlags);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateFontIndirect
#define CreateFontIndirect error use qxeCreateFontIndirect or CreateFontIndirectA/CreateFontIndirectW
#endif
HFONT qxeCreateFontIndirect (CONST LOGFONTW *lplf);

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmSetCompositionFont
#define ImmSetCompositionFont error use qxeImmSetCompositionFont or ImmSetCompositionFontA/ImmSetCompositionFontW
#endif
BOOL qxeImmSetCompositionFont (HIMC imc, LOGFONTW *lplf);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmGetCompositionFont
#define ImmGetCompositionFont error use qxeImmGetCompositionFont or ImmGetCompositionFontA/ImmGetCompositionFontW
#endif
BOOL qxeImmGetCompositionFont (HIMC imc, LOGFONTW *lplf);
#endif /* defined (HAVE_MS_WINDOWS) */

#if defined (HAVE_MS_WINDOWS)
#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef ImmSetCompositionString
#define ImmSetCompositionString error_use_qxeImmSetCompositionString_or_ImmSetCompositionStringA_and_ImmSetCompositionStringW
#endif
#if MSC_VERSION >= 1300
BOOL qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPVOID lpComp, DWORD arg4, LPCVOID lpRead, DWORD arg6);
#else
BOOL qxeImmSetCompositionString (HIMC arg1, DWORD dwIndex, LPCVOID lpComp, DWORD arg4, LPCVOID lpRead, DWORD arg6);
#endif
#endif /* defined (HAVE_MS_WINDOWS) */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetObject
#define GetObject error use qxeGetObject or GetObjectA/GetObjectW
#endif
int qxeGetObject (HGDIOBJ hgdiobj, int cbBuffer, LPVOID lpvObject);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef GetTextMetrics
#define GetTextMetrics error use qxeGetTextMetrics or GetTextMetricsA/GetTextMetricsW
#endif
BOOL qxeGetTextMetrics (HDC hdc, LPTEXTMETRICW lptm);

/* COMMCTRL.H */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef SendMessage
#define SendMessage error use qxeSendMessage or SendMessageA/SendMessageW
#endif
LRESULT qxeSendMessage (HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

/* windows */

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterClass
#define RegisterClass error use qxeRegisterClass or RegisterClassA/RegisterClassW
#endif
ATOM qxeRegisterClass (CONST WNDCLASSW * lpWndClass);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef UnregisterClass
#define UnregisterClass error use qxeUnregisterClass or UnregisterClassA/UnregisterClassW
#endif
BOOL qxeUnregisterClass (const Extbyte * lpClassName, HINSTANCE hInstance);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef RegisterClassEx
#define RegisterClassEx error use qxeRegisterClassEx or RegisterClassExA/RegisterClassExW
#endif
ATOM qxeRegisterClassEx (CONST WNDCLASSEXW * arg1);

#ifdef ERROR_WHEN_NONINTERCEPTED_FUNS_USED
#undef CreateWindow
#define CreateWindow error use qxeCreateWindow or CreateWindowA/CreateWindowW
#endif
#define qxeCreateWindow(lpClassName, lpWindowName, dwStyle, x, y,	     \
                        nWidth, nHeight, hWndParent, hMenu, hInstance,	     \
			lpParam)					     \
  qxeCreateWindowEx (0L, lpClassName, lpWindowName, dwStyle, x, y,	     \
                     nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam)

/* ------------------------- Unicode conversion ------------------------- */

/* Set early in command-line processing, when -nuni or
   --no-unicode-lib-calls is seen. */
extern int no_mswin_unicode_lib_calls;
/* Set early, in init_win32_very_very_early(). */
extern int mswindows_windows9x_p;
#define XEUNICODE_P (!mswindows_windows9x_p && !no_mswin_unicode_lib_calls)

#define XELPTSTR LPWSTR
#define XELPCTSTR LPCWSTR

#define XETCHAR_SIZE (XEUNICODE_P ? sizeof (WCHAR) : sizeof (CHAR))
#define MAX_XETCHAR_SIZE sizeof (WCHAR)
#define XETEXT1(arg) (XEUNICODE_P ? ((char *) (L##arg)) : (arg))
/* We need to do this indirection in case ARG is also a manifest constant.
   I don't really understand why. --ben */
#define XETEXT(arg) XETEXT1(arg)
#define XECOPY_TCHAR(ptr, ch) \
  (XEUNICODE_P ? (* (LPWSTR) (ptr) = L##ch) : (* (LPSTR) (ptr) = (ch)))
#define qxetcslen(arg) \
  (XEUNICODE_P ? wcslen ((wchar_t *) arg) : strlen (arg))
#define qxetcsbytelen(arg) \
  (XEUNICODE_P ? wcslen ((wchar_t *) arg) * XETCHAR_SIZE : strlen (arg))
#define qxetcscmp(s1, s2) \
  (XEUNICODE_P ? wcscmp ((wchar_t *) s1, (wchar_t *) s2) \
   : strcmp (s1, s2))
#define qxetcscpy(s1, s2) \
  (XEUNICODE_P ? (char *) wcscpy ((wchar_t *) s1, (wchar_t *) s2) \
   : strcpy (s1, s2))
#define qxetcsncpy(s1, s2, n) \
  (XEUNICODE_P ? (char *) wcsncpy ((wchar_t *) s1, (wchar_t *) s2, n) \
   : strncpy (s1, s2, n))
#define qxetcschr(s, ch) \
  (XEUNICODE_P ? (char *) wcschr ((wchar_t *) s, (WCHAR) ch) \
   : strchr (s, ch))
#define qxetcsrchr(s, ch) \
  (XEUNICODE_P ? (char *) wcsrchr ((wchar_t *) s, (WCHAR) ch) \
   : strrchr (s, ch))
#define qxetcsdup(s) \
  (XEUNICODE_P ? (char *) wcsdup ((wchar_t *) s) \
   : xstrdup (s))

#define C_STRING_TO_TSTR(in, out) \
  C_STRING_TO_EXTERNAL (in, out, Qmswindows_tstr)
#define LISP_STRING_TO_TSTR(in, out) \
  LISP_STRING_TO_EXTERNAL (in, out, Qmswindows_tstr)
#define TSTR_TO_C_STRING(in, out) \
  EXTERNAL_TO_C_STRING (in, out, Qmswindows_tstr)
#define TSTR_TO_C_STRING_MALLOC(in, out) \
  EXTERNAL_TO_C_STRING_MALLOC (in, out, Qmswindows_tstr)

#define build_tstr_string(in) \
  make_ext_string (in, qxetcsbytelen ((Extbyte *) in), Qmswindows_tstr)

#define MAX_ANSI_CHAR_LEN 1
#define MAX_UNICODE_CHAR_LEN 2

DECLARE_INLINE_HEADER (int ansi_char_to_text (int ch, Extbyte *t))
{
  ch &= 0xFF;
  t[0] = ch;
  return 1;
}

DECLARE_INLINE_HEADER (int unicode_char_to_text (int ch, Extbyte *t))
{
  t[0] = ch & 0xFF;
  t[1] = (ch >> 8) & 0xFF;
  return 2;
}

Extbyte *convert_multibyte_to_unicode_malloc (const Extbyte *src,
					      Bytecount n,
					      int cp, Bytecount *size_out);
Extbyte *convert_unicode_to_multibyte_malloc (const Extbyte *src,
					      Bytecount n,
					      int cp, Bytecount *size_out);
Ibyte *convert_multibyte_to_internal_malloc (const Extbyte *src,
					       Bytecount n,
					       int cp, Bytecount *size_out);
void convert_multibyte_to_unicode_dynarr (const Extbyte *src, Bytecount n,
					  int cp, unsigned_char_dynarr *dst);
void convert_unicode_to_multibyte_dynarr (const Extbyte *src, Bytecount n,
					  int cp, unsigned_char_dynarr *dst);

Bytecount unicode_multibyte_convert_size (const char *srctext, const void *src,
					  Bytecount src_size, int to_unicode,
					  int cp);
void *unicode_multibyte_convert_copy_data (const char *srctext,
					   void *alloca_data);

/* NOTE: If you make two invocations of the following functions in the same
   subexpression and use the exact same expression for the source in both
   cases, you will lose.  In this unlikely case, you may get an abort, and
   need to rewrite the code.

   We need to use ALLOCA_FUNCALL_OK here, see NEW_DFC in text.h.
*/

#ifdef WEXTTEXT_IS_WIDE
#define MULTIBYTE_TO_WEXTTEXT(str)					 \
  ((Wexttext *)								 \
   unicode_multibyte_convert_copy_data					 \
   (#str, ALLOCA_FUNCALL_OK (unicode_multibyte_convert_size		 \
			     (#str, str, strlen (str) + 1, 1, CP_ACP))))

#define WEXTTEXT_TO_MULTIBYTE(str)					\
  ((Extbyte *)								\
   unicode_multibyte_convert_copy_data					\
   (#str, ALLOCA_FUNCALL_OK (unicode_multibyte_convert_size		\
			     (#str, str,				\
			      (wcslen (str) + 1) * sizeof (WCHAR), 0,	\
			      CP_ACP))))
#else
#define MULTIBYTE_TO_WEXTTEXT(str) ((Wexttext *) (str))
#define WEXTTEXT_TO_MULTIBYTE(str) ((Extbyte *) (str))
#endif

/* #### mbstowcs() uses MB_ERR_INVALID_CHARS in addition to MB_PRECOMPOSED.
   Should we do this?  But then we have to handle errors.
   #### Do we already check for invalid sequences in the coding system? */
#define MBTOWC_OPTIONS MB_PRECOMPOSED /* | MB_ERR_INVALID_CHARS */
  /* The following options are what wcstombs() uses in the CRT.  It uses
     NULL in place of "~". */
#define WCTOMB_OPTIONS WC_COMPOSITECHECK | WC_SEPCHARS
#define WCTOMB_INVALID_STRING "~"

/* ------------------------- Other Mule stuff ------------------------- */

LCID mswindows_current_locale (void);
int mswindows_locale_to_code_page (LCID lcid);
int mswindows_locale_to_oem_code_page (LCID lcid);

/* ------------------------- Filename conversion ------------------------- */

#ifdef CYGWIN

BEGIN_C_DECLS

void cygwin_win32_to_posix_path_list (const char *, char *);
int cygwin_win32_to_posix_path_list_buf_size (const char *);
void cygwin_posix_to_win32_path_list (const char *, char *);
int cygwin_posix_to_win32_path_list_buf_size (const char *);

END_C_DECLS

#endif

#define LOCAL_FILE_FORMAT_TO_TSTR(path, out)			\
do {								\
  Ibyte *lttff;						\
								\
  LOCAL_TO_WIN32_FILE_FORMAT (XSTRING_DATA (path), lttff);	\
  C_STRING_TO_TSTR (lttff, out);				\
} while (0)

Lisp_Object tstr_to_local_file_format (Extbyte *pathout);

/* Convert from local file format, as used in XEmacs, to valid win32
   filenames as can be given to Windows API routines.  Under native XEmacs,
   this is a no-op, but under Cygwin, the local names look different --
   Cygwin mount points, forward slashes, etc.  Currently, under Cygwin, we
   actually allow local names to be of both formats, i.e. Cygwin or Win32
   native.  So we check to see if we have Win32 native already (a cheesy
   check, look for letter plus colon at beginning of name) and do nothing
   in that case. */

#ifdef CYGWIN
#define LOCAL_TO_WIN32_FILE_FORMAT(path, pathout)			   \
do {									   \
  /* NOTE: It is a bit evil that here and below we are passing		   \
     internal-format data to a function that (nominally) should work	   \
     with external-format data.  But in point of fact, the Cygwin	   \
     conversion functions are *NOT* localized, and will fail if they	   \
     get 7-bit ISO2022-encoded data.  We know that our internal format	   \
     is ASCII-compatible, and so these functions will work fine with	   \
     this data. */							   \
  Ibyte *ltwffp = (path);						   \
  if (isalpha (ltwffp[0]) && (IS_DEVICE_SEP (ltwffp[1])))		   \
    pathout = ltwffp;							   \
  else									   \
    {									   \
      int ltwff2 =							   \
        cygwin_posix_to_win32_path_list_buf_size ((char *) ltwffp);	   \
      pathout = alloca_ibytes (ltwff2);				   \
      cygwin_posix_to_win32_path_list ((char *) ltwffp, (char *) pathout); \
    }									   \
} while (0)
#else
#define LOCAL_TO_WIN32_FILE_FORMAT(path, pathout)	\
do {							\
  (pathout) = (path);					\
} while (0)
#endif

#ifdef CYGWIN
#define WIN32_TO_LOCAL_FILE_FORMAT(path, pathout)			\
do {									\
  Ibyte *wtlff1 = (path);						\
  int wtlff2 =								\
    cygwin_win32_to_posix_path_list_buf_size ((char *) wtlff1);		\
  Ibyte *wtlff3 = alloca_ibytes (wtlff2);				\
  cygwin_win32_to_posix_path_list ((char *) wtlff1, (char *) wtlff3);	\
  (pathout) = wtlff3;							\
} while (0)
#else
#define WIN32_TO_LOCAL_FILE_FORMAT(path, pathout)	\
do {							\
  (pathout) = (path);					\
} while (0)
#endif

/* Convert a local-format file name or URL in internal format into a Win32
   file name or URL in tstr format. */

#ifdef CYGWIN

#define LOCAL_FILE_FORMAT_MAYBE_URL_TO_TSTR(lispstr, pathout)		     \
do									     \
{									     \
  Ibyte *lffmutt_fname1;						     \
  Ibyte *lffmutt_pathint = XSTRING_DATA (lispstr);			     \
									     \
  if ((lffmutt_fname1 = qxestrchr (lffmutt_pathint, ':')) != NULL	     \
      && *++lffmutt_fname1 == '/' && *++lffmutt_fname1 == '/')		     \
    {									     \
      /* If URL style file, the innards may have Cygwin mount points and     \
	 the like.  so separate out the innards, process them, and put back  \
	 together. */							     \
      if (qxestrncasecmp_ascii (lffmutt_pathint, "file://", 7) == 0)	     \
	{								     \
	  Ibyte *lffmutt_path1, *lffmutt_path2;			     \
	  LOCAL_TO_WIN32_FILE_FORMAT (lffmutt_pathint + 7, lffmutt_path1);   \
	  if (lffmutt_path1 == lffmutt_pathint + 7) /* Optimization */	     \
	    lffmutt_path2 = lffmutt_pathint;				     \
	  else								     \
	    {								     \
	      lffmutt_path2 = alloca_ibytes (7 + qxestrlen (lffmutt_path1) \
					       + 1);			     \
	      qxestrncpy (lffmutt_path2, lffmutt_pathint, 7);		     \
	      qxestrcpy (lffmutt_path2 + 7, lffmutt_path1);		     \
	    }								     \
	  C_STRING_TO_TSTR (lffmutt_path2, pathout);			     \
	}								     \
      else								     \
	/* A straight URL, just convert */				     \
	LISP_STRING_TO_TSTR (lispstr, pathout);				     \
    }									     \
  else									     \
    /* Not URL-style, must be a straight filename. */			     \
    LOCAL_FILE_FORMAT_TO_TSTR (lispstr, pathout);			     \
} while (0)

#else /* not CYGWIN */

  /* URL's (and everything else) are already in the right format */
#define LOCAL_FILE_FORMAT_MAYBE_URL_TO_TSTR(lispstr, pathout) \
   LOCAL_FILE_FORMAT_TO_TSTR (lispstr, pathout)

#endif /* not CYGWIN */


Ibyte *urlify_filename (Ibyte *filename);
Ibyte *mswindows_canonicalize_filename (Ibyte *name);
#define MSWINDOWS_NORMALIZE_FILENAME(name) \
  IBYTE_STRING_TO_ALLOCA (mswindows_canonicalize_filename (name), name)

/* ------------------- Functions needed dynamic binding ------------------- */

typedef BOOL (WINAPI *pfSwitchToThread_t) (VOID);

typedef NET_API_STATUS (NET_API_FUNCTION *pfNetUserEnum_t)
     (LPCWSTR, DWORD, DWORD, LPBYTE *, DWORD, LPDWORD, LPDWORD, LPDWORD);
typedef NET_API_STATUS (NET_API_FUNCTION *pfNetApiBufferFree_t) (LPVOID);

extern pfSwitchToThread_t xSwitchToThread;

extern pfNetUserEnum_t xNetUserEnum;
extern pfNetApiBufferFree_t xNetApiBufferFree;

/* --------- Useful routines for manipulating memory-mapped files -------- */

typedef struct file_data
{
  const Ibyte  *name;
  unsigned long  size;
  HANDLE         file;
  HANDLE         file_mapping;
  void           *file_base;
} file_data;

#define OFFSET_TO_RVA(var,section)			\
	  (section->VirtualAddress +			\
	   ((DWORD)(var) - section->PointerToRawData))

#define RVA_TO_OFFSET(var,section)			\
	  (section->PointerToRawData +			\
	   ((DWORD)(var) - section->VirtualAddress))

#define RVA_TO_PTR(var,section,filedata)		\
	  ((void *)(RVA_TO_OFFSET(var,section) +	\
		    (char *)(filedata).file_base))

int open_input_file (file_data *p_file, const Ibyte *name);
int open_output_file (file_data *p_file, const Ibyte *name,
		      unsigned long size);
void close_file_data (file_data *p_file);

/* ------------------------- Heap related stuff ------------------------- */

#ifdef WIN32_NATIVE

#define get_reserved_heap_size()	reserved_heap_size
#define get_committed_heap_size()	(get_data_end () - get_data_start ())
#define get_heap_start()		get_data_start ()
#define get_heap_end()			get_data_end ()
#define get_page_size()			sysinfo_cache.dwPageSize
#define get_allocation_unit()		sysinfo_cache.dwAllocationGranularity
#define get_processor_type()		sysinfo_cache.dwProcessorType
#define get_nt_major_version()  	nt_major_version
#define get_nt_minor_version()  	nt_minor_version

unsigned char *get_data_start (void);
unsigned char *get_data_end (void);
extern unsigned long  data_region_size;
extern unsigned long  reserved_heap_size;
extern SYSTEM_INFO    sysinfo_cache;
extern int    	      nt_major_version;
extern int    	      nt_minor_version;

/* To prevent zero-initialized variables from being placed into the bss
   section, use non-zero values to represent an uninitialized state.  */
#define UNINIT_PTR ((unsigned char*) 0xF0A0F0A0)
#define UNINIT_LONG (0xF0A0F0A0L)

/* Recreate the heap created during dumping.  */
void recreate_heap (Extbyte *executable_path);

/* Round the heap to this size.  */
void round_heap (unsigned long size);

/* Load in the dumped .bss section.  */
void read_in_bss (Extbyte *name);

/* Map in the dumped heap.  */
void map_in_heap (Extbyte *name);

/* Cache system info, e.g., the NT page size.  */
void cache_system_info (void);

/* Round ADDRESS up to be aligned with ALIGN.  */
unsigned char *round_to_next (unsigned char *address, 
			      unsigned long align);
#endif /* WIN32_NATIVE */

/* ------------------------- Misc prototypes ------------------------- */

#ifdef WIN32_NATIVE
DECLARE_INLINE_HEADER (int strcasecmp (const char *a, const char *b))
{
  return qxestrcasecmp ((const Ibyte *) a, (const Ibyte *) b);
}
#endif /* WIN32_NATIVE */

/* in nt.c */
int mswindows_access (const Ibyte *path, int mode);
int mswindows_link (const Ibyte *old, const Ibyte *new);
int mswindows_rename (const Ibyte *oldname, const Ibyte *newname);
int mswindows_unlink (const Ibyte *path);
int mswindows_stat (const Ibyte *path, struct stat *buf);
int mswindows_fstat (int desc, struct stat *buf);
time_t mswindows_convert_time (FILETIME ft);
int mswindows_is_executable (const Ibyte *filename);
void mswindows_executable_type (const Ibyte *filename, int *is_dos_app,
				int *is_cygnus_app);
Ibyte *mswindows_getdcwd (int drivelet);

/* in process-nt.c */
extern int mswindows_compare_env (const void *strp1, const void *strp2);

/* in win32.c */
Extbyte *mswindows_get_module_file_name (void);
void mswindows_output_last_error (char *frob);
DECLARE_DOESNT_RETURN (mswindows_report_process_error (const char *string,
						       Lisp_Object data,
						       int errnum));
Lisp_Object mswindows_lisp_error (int errnum);

/* in intl-win32.c */
extern Lisp_Object Qmswindows_tstr, Qmswindows_unicode;
extern Lisp_Object Qmswindows_multibyte, Qmswindows_multibyte_to_unicode;

#endif /* INCLUDED_syswindows_h_ */
