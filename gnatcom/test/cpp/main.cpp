#include <windows.h>
#include <comdef.h>
#include <tchar.h>
#include "Factory.h"
#include "beep.h"

#include "beep_i.c"

/////////////////////////////////////////////////////////////////////////////
// COM reference globals

long g_cComponents=0;
long g_cServer=0;

/////////////////////////////////////////////////////////////////////////////
// Special entry points required for inproc servers

HRESULT __stdcall DllGetClassObject(REFCLSID clsid, REFIID riid, void** ppv)
{
	if(clsid!=CLSID_BeepClass)
		return CLASS_E_CLASSNOTAVAILABLE;

	CFactory* pFactory = new CFactory;
	if(pFactory==NULL)
		return E_OUTOFMEMORY;

	HRESULT hr=pFactory->QueryInterface(riid, ppv);
	pFactory->Release();

	return hr;
}

HRESULT __stdcall DllCanUnloadNow()
{
	if(g_cServer==0 && g_cComponents ==0)
		return S_OK;
	else
		return S_FALSE;
}

const TCHAR *g_RegTable[][3] = {
	{ _T("CLSID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}"), 0, _T("Beep Class") },
	{ _T("CLSID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}"), _T("AppID"), _T("{0FE0EE21-8AA2-11d2-81AA-444553540001}") },
	{ _T("CLSID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}\\InProcServer32"),0,(const TCHAR*)-1 },
	{ _T("CLSID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}\\InProcServer32"), _T("ThreadingModel"), _T("Apartment") },
	{ _T("CLSID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}\\ProgID"),0,_T("BeepLibrary.BeepClass.1")},
	{ _T("CLSID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}\\VersionIndependentProgID"),0,_T("BeepLibrary.BeepClass")},
	{ _T("BeepLibrary.BeepClass"), 0, _T("Beep Class") },
	{ _T("BeepLibrary.BeepClass\\CLSID"), 0, _T("{0FE0EE21-8AA2-11d2-81AA-444553540001}") },
	{ _T("BeepLibrary.BeepClass\\CurVer"), 0, _T("BeepLibrary.BeepClass.1") },
	{ _T("BeepLibrary.BeepClass.1"), 0, _T("Beep Class") },
	{ _T("BeepLibrary.BeepClass.1\\CLSID"), 0, _T("{0FE0EE21-8AA2-11d2-81AA-444553540001}") },
	{ _T("AppID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}"), 0, _T("Beep Class") },
	{ _T("AppID\\{0FE0EE21-8AA2-11d2-81AA-444553540001}"), _T("DllSurrogate"), _T("") }
};

STDAPI UnRegisterServer()
{
	HRESULT hr=UnRegisterTypeLib(LIBID_BeepLibrary, 1, 0, LANG_NEUTRAL, SYS_WIN32);
	if(FAILED(hr))
		return hr;

	int numEntries=sizeof(g_RegTable)/sizeof(*g_RegTable);
	for(int i=numEntries-1; i>=0; i--) {
		long err=RegDeleteKey(HKEY_CLASSES_ROOT, g_RegTable[i][0]);
		if(err!=ERROR_SUCCESS) hr=S_FALSE;
	}

	return hr;
}

STDAPI RegisterServer(HINSTANCE hInstance)
{
	TCHAR FilePath[MAX_PATH];
	GetModuleFileName(hInstance, FilePath, MAX_PATH);
	_bstr_t wFilePath(FilePath);

	ITypeLib* pTypeLib;
	HRESULT hr=LoadTypeLibEx(wFilePath, REGKIND_REGISTER, &pTypeLib);
	if(FAILED(hr))
		return hr;
	pTypeLib->Release();

	int numEntries=sizeof(g_RegTable)/sizeof(*g_RegTable);

	for(int i=0; SUCCEEDED(hr) && i<numEntries; i++) {
		const TCHAR *KeyName = g_RegTable[i][0];
		const TCHAR *ValueName = g_RegTable[i][1];
		const TCHAR *Value = g_RegTable[i][2];

		if(Value == (const TCHAR*)-1) Value=FilePath;

		HKEY hkey;
		long err=RegCreateKey(HKEY_CLASSES_ROOT, KeyName, &hkey);
		if (err == ERROR_SUCCESS) {
			err=RegSetValueEx(hkey, ValueName, 0, REG_SZ, (const BYTE*)Value, (_tcslen(Value)+1)*sizeof(TCHAR));
			RegCloseKey(hkey);
		} else {
			UnRegisterServer();
			hr=SELFREG_E_CLASS;
		}
	}
	return hr;
}

HINSTANCE hinst;

HRESULT __stdcall DllRegisterServer()
{
	return RegisterServer(hinst);
}

HRESULT __stdcall DllUnregisterServer()
{
	return UnRegisterServer();
}

BOOL WINAPI DllMain(
  HINSTANCE hinstDLL,  // handle to DLL module
  DWORD fdwReason,     // reason for calling function
  LPVOID lpvReserved   // reserved
)
{
	hinst = hinstDLL;

	return 1;
}
 