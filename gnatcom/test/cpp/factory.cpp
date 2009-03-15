#include <windows.h>
#include <comdef.h>
#include "Factory.h"
#include "beep.h"
#include "BeepCom.h"



/////////////////////////////////////////////////////////////////////////////
// CFactory Implimentation
/////////////////////////////////////////////////////////////////////////////
//IUnknown

ULONG CFactory::AddRef()
{
	return InterlockedIncrement(&m_cRef);
}

ULONG CFactory::Release()
{
	LONG res=InterlockedDecrement(&m_cRef);
	if(res!=0) return res;
	delete this;
	return 0;
}

HRESULT CFactory::QueryInterface(REFIID riid, void** ppv)
{
	if(riid == IID_IUnknown)
		*ppv=(IUnknown*)this;
	else if(riid == IID_IClassFactory)
		*ppv=(IClassFactory*)this;
	else {
		*ppv=NULL;
		return E_NOINTERFACE;
	}
	AddRef();
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
//IClassFactory

HRESULT CFactory::CreateInstance(IUnknown* pUnknownOuter, REFIID riid, void** ppv)
{
	if(pUnknownOuter!=NULL)
		return CLASS_E_NOAGGREGATION;

	CBeepCom *pObject = new CBeepCom;
	if(pObject==NULL)
		return E_OUTOFMEMORY;

	HRESULT hr=pObject->QueryInterface(riid, ppv);
	pObject->Release();
	return hr;
}

HRESULT CFactory::LockServer(BOOL bLock)
{
	if(bLock)
		InterlockedIncrement(&g_cServer);
	else
		InterlockedDecrement(&g_cServer);
	return S_OK;
}
