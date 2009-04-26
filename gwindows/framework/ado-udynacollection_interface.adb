with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.uDynaCollection_Interface is

   procedure Initialize (This : in out uDynaCollection_Type) is
   begin
      Set_IID (This, IID_uDynaCollection);
   end Initialize;

   function Pointer (This : uDynaCollection_Type)
     return Pointer_To_uDynaCollection
   is
   begin
      return To_Pointer_To_uDynaCollection (Address (This));
   end Pointer;

   procedure Attach (This    : in out uDynaCollection_Type;
                     Pointer : in     Pointer_To_uDynaCollection)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
     (This : uDynaCollection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Count
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Count;

   function uNewEnum
     (This      : uDynaCollection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uNewEnum
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end uNewEnum;

   procedure Refresh
     (This : uDynaCollection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   procedure Append
     (This   : uDynaCollection_Type;
      Object : GNATCOM.Types.Pointer_To_IDispatch)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Append
         (Pointer (This),
          Object));

   end Append;

   procedure Delete
     (This  : uDynaCollection_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Delete
         (Pointer (This),
          Index));

      if Free then
               GNATCOM.Iinterface.Free (Index);

      end if;

   end Delete;

end ADO.uDynaCollection_Interface;
