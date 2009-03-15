#ifndef __Factory_h__
#define __Factory_h__

/////////////////////////////////////////////////////////////////////////////
// COM reference globals

extern long g_cComponents;
extern long g_cServer;

/////////////////////////////////////////////////////////////////////////////
// CFactory Definition

class CFactory : public IClassFactory
{
public:

	//IUnkown
	ULONG __stdcall AddRef();
	ULONG __stdcall Release();
	HRESULT __stdcall QueryInterface(REFIID riid, void** ppv);

	//ClassFactory
	HRESULT __stdcall CreateInstance(IUnknown* pUnknownOuter, REFIID riid, void** ppv);
	HRESULT __stdcall LockServer(BOOL bLock);

	CFactory() : m_cRef(1) {}
	~CFactory() {}

private:

	LONG m_cRef;
};

#endif