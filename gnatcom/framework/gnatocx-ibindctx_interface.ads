with GNATCOM.Iinterface;

package GNATOCX.IBindCtx_Interface is

   type IBindCtx_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IBindCtx_Type);

   function Pointer (This : IBindCtx_Type)
     return Pointer_To_IBindCtx;

   procedure Attach (This    : in out IBindCtx_Type;
                     Pointer : in     Pointer_To_IBindCtx);

   procedure RegisterObjectBound
     (This : IBindCtx_Type;
      punk : GNATCOM.Types.Pointer_To_IUnknown);

   procedure RevokeObjectBound
     (This : IBindCtx_Type;
      punk : GNATCOM.Types.Pointer_To_IUnknown);

   procedure ReleaseBoundObjects
     (This : IBindCtx_Type);

   procedure RemoteSetBindOptions
     (This      : IBindCtx_Type;
      pbindopts : Pointer_To_BIND_OPTS2);

   procedure RemoteGetBindOptions
     (This      : IBindCtx_Type;
      pbindopts : Pointer_To_BIND_OPTS2);

   procedure GetRunningObjectTable
     (This  : IBindCtx_Type;
      pprot : Pointer_To_Pointer_To_IRunningObjectTable);

   procedure RegisterObjectParam
     (This   : IBindCtx_Type;
      pszKey : GNATCOM.Types.LPWSTR;
      punk   : GNATCOM.Types.Pointer_To_IUnknown);

   procedure GetObjectParam
     (This   : IBindCtx_Type;
      pszKey : GNATCOM.Types.LPWSTR;
      ppunk  : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown);

   procedure EnumObjectParam
     (This   : IBindCtx_Type;
      ppenum : Pointer_To_Pointer_To_IEnumString);

   procedure RevokeObjectParam
     (This   : IBindCtx_Type;
      pszKey : GNATCOM.Types.LPWSTR);

end GNATOCX.IBindCtx_Interface;
