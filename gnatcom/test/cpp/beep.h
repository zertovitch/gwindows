/* this ALWAYS GENERATED file contains the definitions for the interfaces */


/* File created by MIDL compiler version 5.01.0164 */
/* at Mon Jan 10 18:05:51 2000
 */
/* Compiler settings for F:\Work\gnatcom\test\cpp\beep.idl:
    Os (OptLev=s), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __beep_h__
#define __beep_h__

#ifdef __cplusplus
extern "C"{
#endif 

/* Forward Declarations */ 

#ifndef __IBeep_FWD_DEFINED__
#define __IBeep_FWD_DEFINED__
typedef interface IBeep IBeep;
#endif 	/* __IBeep_FWD_DEFINED__ */


#ifndef __IBeep_FWD_DEFINED__
#define __IBeep_FWD_DEFINED__
typedef interface IBeep IBeep;
#endif 	/* __IBeep_FWD_DEFINED__ */


#ifndef __BeepClass_FWD_DEFINED__
#define __BeepClass_FWD_DEFINED__

#ifdef __cplusplus
typedef class BeepClass BeepClass;
#else
typedef struct BeepClass BeepClass;
#endif /* __cplusplus */

#endif 	/* __BeepClass_FWD_DEFINED__ */


/* header files for imported files */
#include "unknwn.h"
#include "oaidl.h"

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void __RPC_FAR * ); 

#ifndef __IBeep_INTERFACE_DEFINED__
#define __IBeep_INTERFACE_DEFINED__

/* interface IBeep */
/* [dual][oleautomation][helpstring][uuid][object] */ 


EXTERN_C const IID IID_IBeep;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("0FE0EE22-8AA2-11d2-81AA-444553540001")
    IBeep : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Beep( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Display( 
            /* [in] */ BSTR text) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetText( 
            /* [retval][out] */ BSTR __RPC_FAR *text) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetGetText( 
            /* [in] */ BSTR inText,
            /* [retval][out] */ BSTR __RPC_FAR *outText) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InDoubleOut( 
            /* [in] */ BSTR inText,
            /* [out] */ BSTR __RPC_FAR *out1Text,
            /* [retval][out] */ BSTR __RPC_FAR *out2Text) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InDoubleOutVar( 
            /* [in] */ VARIANT inVar,
            /* [out] */ VARIANT __RPC_FAR *out1Var,
            /* [retval][out] */ VARIANT __RPC_FAR *out2Var) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IBeepVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IBeep __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IBeep __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IBeep __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IBeep __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IBeep __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IBeep __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IBeep __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Beep )( 
            IBeep __RPC_FAR * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Display )( 
            IBeep __RPC_FAR * This,
            /* [in] */ BSTR text);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetText )( 
            IBeep __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *text);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *SetGetText )( 
            IBeep __RPC_FAR * This,
            /* [in] */ BSTR inText,
            /* [retval][out] */ BSTR __RPC_FAR *outText);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *InDoubleOut )( 
            IBeep __RPC_FAR * This,
            /* [in] */ BSTR inText,
            /* [out] */ BSTR __RPC_FAR *out1Text,
            /* [retval][out] */ BSTR __RPC_FAR *out2Text);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *InDoubleOutVar )( 
            IBeep __RPC_FAR * This,
            /* [in] */ VARIANT inVar,
            /* [out] */ VARIANT __RPC_FAR *out1Var,
            /* [retval][out] */ VARIANT __RPC_FAR *out2Var);
        
        END_INTERFACE
    } IBeepVtbl;

    interface IBeep
    {
        CONST_VTBL struct IBeepVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IBeep_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IBeep_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IBeep_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IBeep_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IBeep_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IBeep_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IBeep_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IBeep_Beep(This)	\
    (This)->lpVtbl -> Beep(This)

#define IBeep_Display(This,text)	\
    (This)->lpVtbl -> Display(This,text)

#define IBeep_GetText(This,text)	\
    (This)->lpVtbl -> GetText(This,text)

#define IBeep_SetGetText(This,inText,outText)	\
    (This)->lpVtbl -> SetGetText(This,inText,outText)

#define IBeep_InDoubleOut(This,inText,out1Text,out2Text)	\
    (This)->lpVtbl -> InDoubleOut(This,inText,out1Text,out2Text)

#define IBeep_InDoubleOutVar(This,inVar,out1Var,out2Var)	\
    (This)->lpVtbl -> InDoubleOutVar(This,inVar,out1Var,out2Var)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IBeep_Beep_Proxy( 
    IBeep __RPC_FAR * This);


void __RPC_STUB IBeep_Beep_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IBeep_Display_Proxy( 
    IBeep __RPC_FAR * This,
    /* [in] */ BSTR text);


void __RPC_STUB IBeep_Display_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IBeep_GetText_Proxy( 
    IBeep __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *text);


void __RPC_STUB IBeep_GetText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IBeep_SetGetText_Proxy( 
    IBeep __RPC_FAR * This,
    /* [in] */ BSTR inText,
    /* [retval][out] */ BSTR __RPC_FAR *outText);


void __RPC_STUB IBeep_SetGetText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IBeep_InDoubleOut_Proxy( 
    IBeep __RPC_FAR * This,
    /* [in] */ BSTR inText,
    /* [out] */ BSTR __RPC_FAR *out1Text,
    /* [retval][out] */ BSTR __RPC_FAR *out2Text);


void __RPC_STUB IBeep_InDoubleOut_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IBeep_InDoubleOutVar_Proxy( 
    IBeep __RPC_FAR * This,
    /* [in] */ VARIANT inVar,
    /* [out] */ VARIANT __RPC_FAR *out1Var,
    /* [retval][out] */ VARIANT __RPC_FAR *out2Var);


void __RPC_STUB IBeep_InDoubleOutVar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IBeep_INTERFACE_DEFINED__ */



#ifndef __BeepLibrary_LIBRARY_DEFINED__
#define __BeepLibrary_LIBRARY_DEFINED__

/* library BeepLibrary */
/* [version][helpstring][uuid] */ 



EXTERN_C const IID LIBID_BeepLibrary;

EXTERN_C const CLSID CLSID_BeepClass;

#ifdef __cplusplus

class DECLSPEC_UUID("0FE0EE21-8AA2-11d2-81AA-444553540001")
BeepClass;
#endif
#endif /* __BeepLibrary_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long __RPC_FAR *, unsigned long            , BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long __RPC_FAR *, BSTR __RPC_FAR * ); 

unsigned long             __RPC_USER  VARIANT_UserSize(     unsigned long __RPC_FAR *, unsigned long            , VARIANT __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  VARIANT_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, VARIANT __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  VARIANT_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, VARIANT __RPC_FAR * ); 
void                      __RPC_USER  VARIANT_UserFree(     unsigned long __RPC_FAR *, VARIANT __RPC_FAR * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif
