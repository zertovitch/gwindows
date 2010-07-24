
with GNATCOM.Errors;
package body GNATOCX.IAdviseSink_Interface is

   procedure Initialize (This : in out IAdviseSink_Type) is
   begin
      Set_IID (This, IID_IAdviseSink);
   end Initialize;

   function Pointer (This : IAdviseSink_Type)
     return Pointer_To_IAdviseSink
   is
   begin
      return To_Pointer_To_IAdviseSink (Address (This));
   end Pointer;

   procedure Attach (This    : in out IAdviseSink_Type;
                     Pointer : in     Pointer_To_IAdviseSink)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure RemoteOnDataChange
     (This       : IAdviseSink_Type;
      pformatetc : Pointer_To_FORMATETC;
      pStgmed    : Pointer_To_wireASYNC_STGMEDIUM)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteOnDataChange
         (Pointer (This),
          pformatetc,
          pStgmed));

   end RemoteOnDataChange;

   procedure RemoteOnViewChange
     (This     : IAdviseSink_Type;
      dwAspect : Interfaces.C.unsigned_long;
      lindex   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteOnViewChange
         (Pointer (This),
          dwAspect,
          lindex));

   end RemoteOnViewChange;

   procedure RemoteOnRename
     (This : IAdviseSink_Type;
      pmk  : Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteOnRename
         (Pointer (This),
          pmk));

   end RemoteOnRename;

   procedure RemoteOnSave
     (This : IAdviseSink_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteOnSave
         (Pointer (This)));

   end RemoteOnSave;

   procedure RemoteOnClose
     (This : IAdviseSink_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RemoteOnClose
         (Pointer (This)));

   end RemoteOnClose;

end GNATOCX.IAdviseSink_Interface;
