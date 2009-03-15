#ifndef __BeepCom_h__
#define __BeepCom_h__

/////////////////////////////////////////////////////////////////////////////
// COM reference globals
extern long g_cComponents;
extern HANDLE g_hEvent;

/////////////////////////////////////////////////////////////////////////////
// CCampaignMgr Definition

class CBeepCom : public IBeep
{
public:

	//IUnknown
	ULONG __stdcall AddRef();
	ULONG __stdcall Release();
	HRESULT __stdcall QueryInterface(REFIID riid, void** ppv);

	//IDispatch

	HRESULT __stdcall GetTypeInfoCount(UINT *pctinfo);
	HRESULT __stdcall GetTypeInfo(UINT itinfo, LCID lcid, ITypeInfo **ppTInfo);
	HRESULT __stdcall GetIDsOfNames(REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgDispId);
	HRESULT __stdcall Invoke(DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags,
		DISPPARAMS *pDispParams, VARIANT *pVarResult, EXCEPINFO *pExcepInfo, UINT *puArgError);

	//IBeep
	HRESULT __stdcall Beep();
	HRESULT __stdcall Display(/*[in]*/ BSTR text);
	HRESULT __stdcall GetText(/*[out, retval]*/ BSTR *text);
	HRESULT __stdcall SetGetText(/*[in]*/ BSTR inText, /*[out, retval]*/ BSTR *outText);
	HRESULT __stdcall InDoubleOut(/*[in]*/ BSTR inText, /*[out]*/ BSTR *out1Text, /*[out, retval]*/ BSTR *out2Text);
	HRESULT __stdcall InDoubleOutVar(/*[in]*/ VARIANT inVar, /*[out]*/ VARIANT *out1Var, /*[out, retval]*/ VARIANT *out2Var);

	CBeepCom();
	~CBeepCom();
private:
	void _LoadTypeInfo(ITypeInfo **ppTInfo, REFCLSID riid);
	ITypeInfo *m_pTInfo;
	LONG m_cRef;
};

#endif
