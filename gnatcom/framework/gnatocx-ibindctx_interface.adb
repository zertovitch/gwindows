with GNATCOM.Errors;

package body GNATOCX.IBindCtx_Interface is

   procedure Initialize (This : in out IBindCtx_Type) is
   begin
      Set_IID (This, IID_IBindCtx);
   end Initialize;

   function Pointer (This : IBindCtx_Type)
     return Pointer_To_IBindCtx
   is
   begin
      return To_Pointer_To_IBindCtx (Address (This));
   end Pointer;

   procedure Attach (This    : in out IBindCtx_Type;
                     Pointer : in     Pointer_To_IBindCtx)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RegisterObjectBound
     (This : IBindCtx_Type;
      punk : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RegisterObjectBound
         (Pointer (This),
          punk));

   end RegisterObjectBound;

   procedure RevokeObjectBound
     (This : IBindCtx_Type;
      punk : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RevokeObjectBound
         (Pointer (This),
          punk));

   end RevokeObjectBound;

   procedure ReleaseBoundObjects
     (This : IBindCtx_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ReleaseBoundObjects
         (Pointer (This)));

   end ReleaseBoundObjects;

   procedure RemoteSetBindOptions
     (This      : IBindCtx_Type;
      pbindopts : Pointer_To_BIND_OPTS2)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteSetBindOptions
         (Pointer (This),
          pbindopts));

   end RemoteSetBindOptions;

   procedure RemoteGetBindOptions
     (This      : IBindCtx_Type;
      pbindopts : Pointer_To_BIND_OPTS2)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteGetBindOptions
         (Pointer (This),
          pbindopts));

   end RemoteGetBindOptions;

   procedure GetRunningObjectTable
     (This  : IBindCtx_Type;
      pprot : Pointer_To_Pointer_To_IRunningObjectTable)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetRunningObjectTable
         (Pointer (This),
          pprot));

   end GetRunningObjectTable;

   procedure RegisterObjectParam
     (This   : IBindCtx_Type;
      pszKey : GNATCOM.Types.LPWSTR;
      punk   : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RegisterObjectParam
         (Pointer (This),
          pszKey,
          punk));

   end RegisterObjectParam;

   procedure GetObjectParam
     (This   : IBindCtx_Type;
      pszKey : GNATCOM.Types.LPWSTR;
      ppunk  : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetObjectParam
         (Pointer (This),
          pszKey,
          ppunk));

   end GetObjectParam;

   procedure EnumObjectParam
     (This   : IBindCtx_Type;
      ppenum : Pointer_To_Pointer_To_IEnumString)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumObjectParam
         (Pointer (This),
          ppenum));

   end EnumObjectParam;

   procedure RevokeObjectParam
     (This   : IBindCtx_Type;
      pszKey : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RevokeObjectParam
         (Pointer (This),
          pszKey));

   end RevokeObjectParam;

end GNATOCX.IBindCtx_Interface;
