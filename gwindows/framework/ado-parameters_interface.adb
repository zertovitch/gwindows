with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Parameters_Interface is

   procedure Initialize (This : in out Parameters_Type) is
   begin
      Set_IID (This, IID_Parameters);
   end Initialize;

   function Pointer (This : Parameters_Type)
     return Pointer_To_Parameters
   is
   begin
      return To_Pointer_To_Parameters (Address (This));
   end Pointer;

   procedure Attach (This    : in out Parameters_Type;
                     Pointer : in     Pointer_To_Parameters)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
     (This : Parameters_Type)
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
     (This      : Parameters_Type)
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
     (This : Parameters_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   procedure Append
     (This   : Parameters_Type;
      Object : GNATCOM.Types.Pointer_To_IDispatch)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Append
         (Pointer (This),
          Object));

   end Append;

   procedure Delete
     (This  : Parameters_Type;
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

   function Get_Item
     (This      : Parameters_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_uParameter
   is
       RetVal : aliased Pointer_To_uParameter;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Item
         (Pointer (This),
          Index,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.Iinterface.Free (Index);

      end if;

      return RetVal;
   end Get_Item;

end ADO.Parameters_Interface;
