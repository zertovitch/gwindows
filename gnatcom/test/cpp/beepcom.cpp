#include <windows.h>
#include <comdef.h>
#include "Factory.h"
#include "beep.h"
#include "BeepCom.h"
#include <stdio.h>



CBeepCom::CBeepCom() : m_cRef(1)
{
	InterlockedIncrement(&g_cComponents);
	_LoadTypeInfo(&m_pTInfo, IID_IBeep);
}

CBeepCom::~CBeepCom() {
	InterlockedDecrement(&g_cComponents);
};

void CBeepCom::_LoadTypeInfo(ITypeInfo **ppTInfo, REFCLSID riid)
{
	LPTYPELIB pTLib;

	if (!SUCCEEDED(LoadRegTypeLib(LIBID_BeepLibrary, 1, 0, 0, &pTLib))) {
		::MessageBox(NULL, "Failed to load type library", "TypeLib Error", MB_OK);
	}

	if (!SUCCEEDED(pTLib->GetTypeInfoOfGuid(riid, ppTInfo))) {
		::MessageBox(NULL, "Failed to load type Type Info", "TypeLib Error", MB_OK);
	}

	pTLib->Release();
}


/////////////////////////////////////////////////////////////////////////////
//IUnknown

ULONG CBeepCom::AddRef()
{
	return InterlockedIncrement(&m_cRef);
}

ULONG CBeepCom::Release()
{
	LONG res=InterlockedDecrement(&m_cRef);

	if(res!=0) return res;
	delete this;
	return 0;
}

HRESULT CBeepCom::QueryInterface(REFIID riid, void** ppv)
{
	if(riid == IID_IUnknown)
		*ppv=static_cast<IBeep*>(this);
	else if(riid == IID_IDispatch)
		*ppv=static_cast<IBeep*>(this);
	else if(riid == IID_IBeep)
		*ppv=static_cast<IBeep*>(this);
	else {
		*ppv=NULL;
		return E_NOINTERFACE;
	}
	AddRef();
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
//IDispatch

HRESULT CBeepCom::GetTypeInfoCount(UINT *pctinfo)
{
	*pctinfo=1;
	return S_OK;
}

HRESULT CBeepCom::GetTypeInfo(UINT itinfo, LCID lcid, ITypeInfo **ppTInfo)
{
	*ppTInfo=NULL;

	if (itinfo !=0)
	{
		return DISP_E_BADINDEX;
	}

	m_pTInfo->AddRef();
	*ppTInfo = m_pTInfo;
	
	return S_OK;
}

HRESULT CBeepCom::GetIDsOfNames(REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgDispId)
{
	return DispGetIDsOfNames(m_pTInfo, rgszNames, cNames, rgDispId);
}

HRESULT CBeepCom::Invoke(DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags,
		DISPPARAMS *pDispParams, VARIANT *pVarResult, EXCEPINFO *pExcepInfo, UINT *puArgError)
{
	return DispInvoke(this, m_pTInfo, dispIdMember, wFlags,
		pDispParams, pVarResult, pExcepInfo, puArgError);
}


/////////////////////////////////////////////////////////////////////////////
//IBeep

HRESULT CBeepCom::Beep()
{
	::MessageBeep(MB_ICONEXCLAMATION);
	printf("++ PASS\n");
	return S_OK;
}

HRESULT CBeepCom::Display(/*[in]*/ BSTR text)
{
	printf((LPCTSTR)_bstr_t(text));
	return S_OK;
}

HRESULT CBeepCom::GetText(/*[out, retval]*/ BSTR *text)
{
	*text = SysAllocString(L"++ PASS");
	return S_OK;
}

HRESULT CBeepCom::SetGetText(/*[in]*/ BSTR inText, /*[out, retval]*/ BSTR *outText)
{
	*outText = SysAllocString(inText);
	return S_OK;
}

HRESULT CBeepCom::InDoubleOut(/*[in]*/ BSTR inText, /*[out]*/ BSTR *out1Text, /*[out, retval]*/ BSTR *out2Text)
{
	*out1Text = SysAllocString(L"++ PASS");
	*out2Text = SysAllocString(inText);
	return S_OK;
}

HRESULT CBeepCom::InDoubleOutVar(/*[in]*/ VARIANT inVar, /*[out]*/ VARIANT *out1Var, /*[out, retval]*/ VARIANT *out2Var)
{
	_variant_t out1 ("++ PASS");
	_variant_t out2 ("++ PASS");

	*out1Var = out1.Detach();
	*out2Var = out2.Detach();

	return S_OK;
}
